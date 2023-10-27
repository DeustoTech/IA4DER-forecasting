library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet') 

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

# csv_files <- csv_files[201:length(csv_files)]

N <- csv_files[!grepl("-CT\\.csv$", csv_files) & !grepl("-L\\.csv$", csv_files)]
CT <- csv_files[grepl("-CT\\.csv$", csv_files)]
L <- csv_files[grepl("-L\\.csv$", csv_files)]

# Agregar el prefijo 'folder' a las rutas en N, CT y L
N <- paste(folder, N, sep = "")
CT <- paste(folder, CT, sep = "")
L <- paste(folder, L, sep = "")


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
  ini_date <- get_extrema_dates_from_timeseries(tseries)
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
