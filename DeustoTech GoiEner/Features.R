library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


#descropmir carpeta de 200 csv
path <- "Transformers.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina
unzip(path, exdir = tempdir) # descomprime. Tarda un poco
plan(multisession)


folder <- "TransformersV2/"
# Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)
metadata_file <- fread("metadata.csv")

# csv_files <- csv_files[201:length(csv_files)]

N <- csv_files[!grepl("-CT\\.csv$", csv_files) & !grepl("-L\\.csv$", csv_files)]
CT <- csv_files[grepl("-CT\\.csv$", csv_files)]
L <- csv_files[grepl("-L\\.csv$", csv_files)]


# Agregar el prefijo 'folder' a las rutas en N, CT y L
N <- paste(folder, N, sep = "")
CT <- paste(folder, CT, sep = "")
L <- paste(folder, L, sep = "")

horas <- data.frame(
  hora = 0:23,
  TARIFA_2.0 = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano", "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  ),
  TARIFA_SOLAR = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "solar pico", "solar pico", "solar pico", "solar pico",
    "solar llano", "solar llano",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  )
)
MC  <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error


B <- foreach(NAME = N,
             .combine = rbind,
             .errorhandling = "remove", .options.future = list(packages = librerias)) %dofuture% { 
               
               csv_actual <- fread(NAME)
               
               
               if ("time" %in% colnames(csv_actual)) {
                 # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
                 colnames(csv_actual)[colnames(csv_actual) == "time"] <- "timestamp"
                 csv_actual$imputed <- 0
                 csv_actual <- csv_actual %>% select(timestamp, kWh, imputed)
               }
               
               
               a <- csv_actual %>%
                 mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
                 mutate(Hora = hour(timestamp))
               
               T2.0_VALLE <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "valle"]])
               T2.0_LLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "llano"]])
               T2.0_PICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "pico"]])
               
               T_SOLAR_LLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "llano"]])
               T_SOLAR_PICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "pico"]])
               T_SOLAR_SPICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "solar pico"]])
               T_SOLAR_SLLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "solar llano"]])
               
               
               
               LENGTH <- length(a$kWh)
               
               QQ     <- as.numeric(quantile(a$kWh,c(0,0.25,0.5,0.75,1),na.rm=T))
               
               nombre     <- tools::file_path_sans_ext(NAME)
               ID <-  sub("TransformersV2/", "", nombre)
               # ID <- tools::file_path_sans_ext(ID)
               
               
               metadatos <- metadata_file[metadata_file$user == ID, ]
               
               POT_NOM <- max(metadatos$p1, metadatos$p2, metadatos$p3, metadatos$p4, metadatos$p5, metadatos$p6, na.rm = T)
               ECDF   <- ecdf(a$kWh)(MC*POT_NOM) 
               
               aux <- data.frame(
                 ID=     ID,
                 LENGTH= LENGTH,
                 ZERO=   sum(a$kWh==0)/LENGTH,
                 IMPUTED=sum(a$issue)/LENGTH,
                 AVG=    mean(a$kWh,na.rm=T),
                 SD=     sd(a$kWh,na.rm=T),
                 MIN=    QQ[1],
                 Q1=     QQ[2],
                 MEDIAN= QQ[3],
                 Q3=     QQ[4],
                 MAX=    QQ[5],
                 
                 POT_1 = metadatos$p1,
                 POT_2 = metadatos$p2,
                 POT_3 = metadatos$p3,
                 POT_4 = metadatos$p4,
                 POT_5 = metadatos$p5,
                 POT_6 = metadatos$p6,
                 POT_NOM = POT_NOM,
                 
                 MC25=   ECDF[1],
                 MC50=   ECDF[2],
                 MC80=   ECDF[3],
                 MC90=   ECDF[4],
                 MC95=   ECDF[5],
                 
                 P_T2.0_VALLE = T2.0_VALLE,
                 P_T2.0_LLANO = T2.0_LLANO,
                 P_T2.0_PICO = T2.0_PICO,
                 P_T_SOLAR_PICO = T_SOLAR_PICO,
                 P_T_SOLAR_LLANO = T_SOLAR_LLANO,
                 P_T_SOLAR_SPICO = T_SOLAR_SPICO,
                 P_T_SOLAR_SLLANO = T_SOLAR_SLLANO
                 
               )
             }

write.csv(B,file="features.csv",row.names = F)

B <- read.csv("features.csv")


boxplot(B$P_T2.0_VALLE,B$P_T2.0_LLANO,B$P_T2.0_PICO,B$P_T_SOLAR_PICO,
        B$P_T_SOLAR_LLANO,B$P_T_SOLAR_SPICO, B$P_T_SOLAR_SLLANO,outline=F )

colnames(B)

summary(data.frame(B$P_T2.0_VALLE,B$P_T2.0_LLANO,B$P_T2.0_PICO,B$P_T_SOLAR_PICO,
                   B$P_T_SOLAR_LLANO,B$P_T_SOLAR_SPICO, B$P_T_SOLAR_SLLANO))


B$POT_NOM <- max(B$POT_1, B$POT_2, B$POT_3, B$POT_4, B$POT_5, B$POT_6, na.rm = T)

plot(ecdf(B$ZERO))
plot(ecdf(B$IMPUTED))
plot(ecdf(B$MAX/B$POT_NOM))
ecdf(B$MAX/B$POT_NOM)(0.8)


aggr_data <- stats::aggregate(
  x   = as.numeric(tseries),
  by  = list(date_time = sum_factor),
  FUN = sum
)

get_seasonal_features_from_timeseries <- function(csv_actual, maxmin = FALSE) {
  
  tseries <- fread(csv_actual)
  # Initialize results list
  o <- list()
  # DEFINITION OF FUNCTION TO AVOID NaN WHEN VECTOR LENGTH IS 1
  sd_ <- function(x) ifelse(length(x)==1, 0, stats::sd(x))
  # Initial date
  ini_date <- 
  # Date sequence
  samples_per_day <- attr(tseries, "msts")[1]
  date_by <- as.difftime(24 / samples_per_day, units = "hours")
  t <- seq(from = ini_date, length.out = length(tseries), by = date_by)
  # Variable names
  name <- c(
    as.name("hour_1"),           #  1
    as.name("hour_4"),           #  2
    as.name("hour_6"),           #  3
    as.name("day"),              #  4
    as.name("weekday"),          #  5
    as.name("month"),            #  6
    as.name("season"),           #  7
    as.name("hour4_season"),     #  8
    as.name("td2.0_p6"),         #  9 
    as.name("td2.0_p7"),         # 10
    as.name("td2.0_p6_season"),  # 11
    as.name("td2.0_p7_season"),  # 12
    as.name("td2.0_p3"),         # 13
    as.name("td2.0_p3_ym"),      # 14 
    as.name("weekday_drm"),      # 15 >> drm = day-referenced mean
    as.name("month_drm"),        # 16 
    as.name("season_drm"),       # 17
    as.name("hour4_season_drm"), # 18
    as.name("new_feature_prueba")# 19
  )
  # Loop for the 8 different bins
  for (bb in 1:19) {
    # Get the bins to compute the sum
    sum_factor <- get_bins(t, bb)
    # Aggregate data (sum) according to the bins
    aggr_data <- stats::aggregate(
      x   = as.numeric(tseries),
      by  = list(date_time = sum_factor),
      FUN = sum
    )
    # There's no need to check the completeness of the bins. Just remove first
    # and last bins by default. In case there's a unique bin when computing
    # means, standard deviation is set to 0. This may only happen with
    # meteorological seasons
    if (bb %in% 1:8) {
      aggr_data <- aggr_data[c(-1, -nrow(aggr_data)), ]
    }
    # Get the new bins to compute the mean
    # Hours
    if (bb == 1 | bb == 2 | bb == 3) {
      sum_factor <- 
        as.factor(lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # Days
    if (bb == 4 | bb == 5) {
      sum_factor <- 
        as.factor(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # Months
    if (bb == 6 | bb == 7) {
      sum_factor <- 
        as.factor(lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # Hours & seasons
    if (bb == 8) {
      sum_factor <- as.factor(
        lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC")) + 
          lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) * 100
      )
    }
    # 2.0TD with 6 periods
    if (bb == 9) {
      sum_factor <- 
        as.factor(lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC")))
    }
    # 2.0TD with 7 periods
    if (bb == 10) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Weekend detection
      idx <- which(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")) == 7)
      sum_factor[idx] <- 25
      sum_factor <- as.factor(sum_factor)
    }
    # 2.0TD with 6 periods per season (24 bins)
    if (bb == 11) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Season detection
      idx <- (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(100 * idx + sum_factor)
    }
    # 2.0TD with 7 periods per season (28 bins)
    if (bb == 12) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Weekend detection
      idx <- which(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")) == 7)
      sum_factor[idx] <- 25
      idx <- (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(100 * idx + sum_factor)
    }
    # 2.0TD with 6 periods
    if (bb == 13) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor[sum_factor ==  0] <- 3
      sum_factor[sum_factor ==  8] <- 2
      sum_factor[sum_factor == 10] <- 1
      sum_factor <- as.factor(sum_factor)
    }
    # 2.0TD year x month x period (28 bins)
    if (bb == 14) {
      sum_factor <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      # Weekend detection (P3)
      idx <- which(lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC")) == 7)
      sum_factor[idx] <- 3
      sum_factor[sum_factor == 0] <- 3
      # P2
      sum_factor[sum_factor %in% c(8, 14, 22)] <- 2
      # P1 
      sum_factor[sum_factor %in% c(10, 18)] <- 1
      idx <- lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor <- 10 * idx + sum_factor
      idx <- lubridate::year(as.POSIXct(aggr_data[,1], tz="UTC")) - 2000
      sum_factor <- as.factor(1000 * idx + sum_factor)
    }
    # Day-referenced mean: weekends/workdays 
    if (bb == 15) {
      sum_factor <- lubridate::wday(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor[sum_factor %in% c(1,7)] <- 7
      sum_factor[sum_factor %in% c(2:6)] <- 1
      sum_factor <- as.factor(sum_factor)
    }
    # Day-referenced mean: month 
    if (bb == 16) {
      sum_factor <- lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor <- as.factor(sum_factor)
    }
    # Day-referenced mean: season 
    if (bb == 17) {
      sum_factor <-
        (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(sum_factor)
    }
    # Day-referenced mean: 4-hour period & season
    if (bb == 18) {
      sum_factor_1 <- lubridate::hour(as.POSIXct(aggr_data[,1], tz="UTC"))
      sum_factor_2 <- 
        (lubridate::month(as.POSIXct(aggr_data[,1], tz="UTC")) %/% 3) %% 4 + 1
      sum_factor <- as.factor(100 * sum_factor_2 + sum_factor_1)
    }
    if (bb == 19) {
      #aqui deberiamos de añadir algo para nuestra nueva feature
    }
    # Aggregate data (mean) according to the bins
    o[[name[[bb]]]]$"mean" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = sum_factor),
      FUN = mean
    )
    # Aggregate data (sd) according to the bins
    o[[name[[bb]]]]$"sd" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = sum_factor),
      FUN = sd_
    )
    # Aggregate data (sum) according to the bins
    o[[name[[bb]]]]$"sum" <- stats::aggregate(
      x   = aggr_data$x,
      by  = list(bin = sum_factor),
      FUN = sum
    )
    if (maxmin) {
      # Aggregate data (max) according to the bins
      o[[name[[bb]]]]$"max" <- stats::aggregate(
        x   = aggr_data$x,
        by  = list(bin = sum_factor),
        FUN = max
      )
      # Aggregate data (min) according to the bins
      o[[name[[bb]]]]$"min" <- stats::aggregate(
        x   = aggr_data$x,
        by  = list(bin = sum_factor),
        FUN = min
      )
    }
  }
  return(o)
}


foreach(csv_file = csv_files, 
        .packages = librerias) %dopar% get_seasonal_features_from_timeseries(csv_file)


# PRUEBAS 
prueba <- msts(fread(CT[1]))

out <- get_seasonal_features_from_timeseries(prueba)






# pruebas

csv_actual <- fread(N[1])

if ("time" %in% colnames(csv_actual)) {
  # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
  colnames(csv_actual)[colnames(csv_actual) == "time"] <- "timestamp"
  csv_actual$imputed <- 0
  csv_actual <- csv_actual %>% select(timestamp, kWh, imputed)
}


a <- csv_actual %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(Hora = hour(timestamp))

T2.0_VALLE <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "valle"]])
T2.0_LLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "llano"]])
T2.0_PICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "pico"]])

T_SOLAR_LLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "llano"]])
T_SOLAR_PICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "pico"]])
T_SOLAR_SPICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "solar pico"]])
T_SOLAR_SLLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "solar llano"]])



LENGTH <- length(a$kWh)

QQ     <- as.numeric(quantile(a$kWh,c(0,0.25,0.5,0.75,1),na.rm=T))

nombre     <- tools::file_path_sans_ext(N[1])

ID <-  sub("TransformersV2/", "", nombre)


metadatos <- metadata_file[metadata_file$user == ID, ]

POT_NOM <- max(metadatos$p1, metadatos$p2, metadatos$p3, metadatos$p4, metadatos$p5, metadatos$p6, na.rm = T)
ECDF   <- ecdf(a$kWh)(MC*POT_NOM) 

aux <- data.frame(
  ID=     ID,
  LENGTH= LENGTH,
  ZERO=   sum(a$kWh==0)/LENGTH,
  IMPUTED=sum(a$issue)/LENGTH,
  AVG=    mean(a$kWh,na.rm=T),
  SD=     sd(a$kWh,na.rm=T),
  MIN=    QQ[1],
  Q1=     QQ[2],
  MEDIAN= QQ[3],
  Q3=     QQ[4],
  MAX=    QQ[5],
  
  POT_1 = metadatos$p1,
  POT_2 = metadatos$p2,
  POT_3 = metadatos$p3,
  POT_4 = metadatos$p4,
  POT_5 = metadatos$p5,
  POT_6 = metadatos$p6,
  
  MC25=   ECDF[1],
  MC50=   ECDF[2],
  MC80=   ECDF[3],
  MC90=   ECDF[4],
  MC95=   ECDF[5],
  
  P_T2.0_VALLE = T2.0_VALLE,
  P_T2.0_LLANO = T2.0_LLANO,
  P_T2.0_PICO = T2.0_PICO,
  P_T_SOLAR_PICO = T_SOLAR_PICO,
  P_T_SOLAR_LLANO = T_SOLAR_LLANO,
  P_T_SOLAR_SPICO = T_SOLAR_SPICO,
  P_T_SOLAR_SLLANO = T_SOLAR_SLLANO
  
)



