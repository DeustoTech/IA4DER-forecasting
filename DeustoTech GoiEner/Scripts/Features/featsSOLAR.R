library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "mice", "tsfeatures") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


#read csvs
{
  roseta <- fread("SOLAR/roseta.csv")
  featsNoPV <- fread("SOLAR/featuresNoPV.csv")
  archivos <- list.files("SOLAR/SOLAR", full.names = TRUE, pattern = "\\.csv$")
 
}



datos_sin_auto <- list()
datos_con_auto <- list()
datos_totales <- list()
entropy_sin_auto <- data.frame()
entropy_con_auto <- data.frame()
entropy_total <- data.frame()

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



calcular_features <- function(serie, ID1, firstPanel, val_pot) {
  # print(names(serie))
  serie$VAL_AI <- serie$VAL_AI / val_pot
  serie$VAL_AE <- serie$VAL_AE / val_pot
  
  # NOMBRES DE COLUMNAS 
  column_names <- c(
    "ID", "LENGTH","NAs","ZEROS", "AVG", "SD", "MIN", "Q1", "MEDIAN", "Q3", "MAX", "TOTAL", "VAR","ENTROPY", 
    "P_T2.0_VALLE", "P_T2.0_LLANO", "P_T2.0_PICO", "P_T_SOLAR_PICO", "P_T_SOLAR_LLANO",
    "P_T_SOLAR_SPICO", "P_T_SOLAR_SLLANO",
    paste("mean.AE.weekdays", 0:23, sep = "."),
    paste("mean.AE.weekend", 0:23, sep = "."),
    paste("AI.week", 1:52, sep = "."),
    paste("AE.week", 1:52, sep = "."),
    paste("AI.month", 1:12, sep = "."),
    paste("AE.month", 1:12, sep = "."),
    "DIFF_HOURS", "INSTALLATION_TIMESTAMP" , "mean.mape.week", "mean.mape.month"

  )
  
  namesEntropy <- c(paste("Entropy.w", 1:52, sep = "."))
  # limipiamos valores no finitos
  
  serie <- serie %>%
    rowwise() %>%
    mutate(
      VAL_AI = ifelse(!is.finite(VAL_AI), 0, VAL_AI),
      VAL_AE = ifelse(!is.finite(VAL_AE), 0, VAL_AE)
    )
  
  # SEASONAL AGGREGATES
  serie$season <- case_when(
    month(serie$timestamp) %in% c(12, 1, 2) ~ "winter",
    month(serie$timestamp) %in% c(3, 4, 5) ~ "spring",
    month(serie$timestamp) %in% c(6, 7, 8) ~ "summer",
    month(serie$timestamp) %in% c(9, 10, 11) ~ "autum"
  )
  
  serie$day_period <- cut(
    hour(serie$timestamp),
    breaks = c(0, 4, 8, 12, 16, 20, 24),
    labels = c("0-4", "5-8", "9-12", "13-16", "17-20", "21-24"),
    include.lowest = TRUE
  )
  
  serie$week_day <- wday(serie$timestamp)
  serie$hour <- hour(serie$timestamp)
  serie$week <- week(serie$timestamp)
  serie$month <- month(serie$timestamp)
  
  laborable <- serie %>% filter(week_day %in% c(1, 2, 3, 4, 5))
  finde <- serie %>% filter(week_day %in% c(6, 7))
  
  features_semana <- laborable %>%
    group_by(season, day_period) %>%
    summarise(Total = sum(VAL_AI),
              Max = max(VAL_AI, na.rm = T)) %>%
    pivot_wider(names_from = c(season, day_period), values_from = c(Total, Max))
  
  features_fin_de_semana <- finde %>%
    group_by(season) %>%
    summarise(Total = sum(VAL_AI),
              Max = max(VAL_AI, na.rm = T))%>%
    pivot_wider(names_from = season, values_from = c(Total, Max))
  

  
  colnames(features_fin_de_semana) <- paste(colnames(features_fin_de_semana), "finde", sep = ".")
  
  column_names <- c(column_names, colnames(features_semana) ,colnames(features_fin_de_semana))
  
  # print(features_fin_de_semana)
  
  
  # INICIALIZAR EL DF
  
  feats <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  entropyDf <- data.frame(matrix(ncol = length(namesEntropy), nrow = 0))
  colnames(feats) <- column_names
  colnames(entropyDf) <- namesEntropy
  
  numeric_columns <- setdiff(column_names, c("ID", "INSTALLATION_TIMESTAMP"))
  feats[, numeric_columns] <- lapply(feats[, numeric_columns], as.numeric)
  feats$INSTALLATION_TIMESTAMP <- as_datetime(feats$INSTALLATION_TIMESTAMP)
  feats$ID <- as.character(feats$ID)
  
  
  # # INYECCION  POR HORAS 
  injection_by_hour_weekdays <- laborable %>%
    group_by(hour) %>%
    summarise(mean_VAL_AE = mean(VAL_AE, na.rm = TRUE))

  injection_by_hour_weekend <- finde %>%
    group_by(hour) %>%
    summarise(mean_VAL_AE = mean(VAL_AE, na.rm = TRUE))
  for (i in 0:23){
    weekday_col <- paste("mean.AE.weekdays", i, sep = ".")
    weekend_col <- paste("mean.AE.weekend", i, sep = ".")
    weekday_value <- ifelse(length(which(injection_by_hour_weekdays$hour == i)) > 0,
                            injection_by_hour_weekdays$mean_VAL_AE[which(injection_by_hour_weekdays$hour == i)],
                            NA)
    
    weekend_value <- ifelse(length(which(injection_by_hour_weekend$hour == i)) > 0,
                            injection_by_hour_weekend$mean_VAL_AE[which(injection_by_hour_weekend$hour == i)],
                            NA)
    feats[1, weekday_col] <- weekday_value
    feats[1, weekend_col] <- weekend_value
  }
  
  # POR MES INYECCION Y CONSUMO
  
  consumption_by_month <- serie %>%
        group_by(month) %>%
        summarise(total_VAL_AI = sum(VAL_AI, na.rm = TRUE))
  

  inyection_by_month <- serie %>%
    group_by(month) %>%
    summarise(total_VAL_AE = sum(VAL_AE, na.rm = TRUE))
  
  for (i in 1:12){
    consumption_col <- paste("AI.month", i, sep = ".")
    injection_col <- paste("AE.month", i, sep = ".")
    
    consumption_value <- ifelse(length(which(!is.na(consumption_by_month$total_VAL_AI[i]))) > 0,
                                consumption_by_month$total_VAL_AI[i],
                                NA)
    
    injection_value <- ifelse(length(which(!is.na(inyection_by_month$total_VAL_AE[i]))) > 0,
                              inyection_by_month$total_VAL_AE[i],
                              NA)
    
    feats[1, consumption_col] <- consumption_value
    feats[1, injection_col] <- injection_value
  }
  
  # POR SEMANA INYECCION Y CONSUMO
  nWeeks <- max(serie$week, na.rm = T)
  
  inyection_by_week <- serie %>%
    group_by(week) %>%
    summarise(total_VAL_AE = sum(VAL_AE, na.rm = TRUE))

  consumption_by_week <- serie %>%
    group_by(week) %>%
    summarise(total_VAL_AI = sum(VAL_AI, na.rm = TRUE))
  
  entropy_by_week <- serie %>% group_by(week) %>% 
    summarise(entropy = entropy(VAL_AI))
  # print(entropy_by_week)

  for (i in 1:nrow(entropy_by_week)){
    entropyCol <- paste("Entropy.w", i, sep = ".")
    entropyVal <- entropy_by_week$entropy[i]
    entropyDf[1, entropyCol] <- entropyVal
    
  }
  
  for (i in 1:nWeeks){
    consumption_col <- paste("AI.week", i, sep = ".")
    injection_col <- paste("AE.week", i, sep = ".")

    consumption_value <- ifelse(length(which(!is.na(consumption_by_week$total_VAL_AI[i]))) > 0,
                                consumption_by_week$total_VAL_AI[i],
                                NA)
    
    injection_value <- ifelse(length(which(!is.na(inyection_by_week$total_VAL_AE[i]))) > 0,
                              inyection_by_week$total_VAL_AE[i],
                              NA)
    
    
    feats[1, consumption_col] <- consumption_value
    feats[1, injection_col] <- injection_value
    
  }
  
  entropyDf$ID <- ID1
  
  
  # print(entropyDf)
  
  # Mape by week and mean week mape
  colsMapeW <- c()
  for (i in 1:(nWeeks - 1)){
    j <- i + 1
    
    # i semana pasada, j semana de ahora.
    col_name_i <- paste("AI.week", i, sep = ".")
    col_name_j <- paste("AI.week", j, sep = ".")
    mape_col_name <- paste("mape.AI.week.", j, "-", i, sep = "")
    
    mapeVal <- mape(feats[1, col_name_j], feats[1, col_name_i]) * 100
    feats[1, mape_col_name] <- mapeVal
    colsMapeW[i] <- mapeVal
  }
  
  # Mean month mape
  
  colsMapeM <- c()
  for (i in 1:11){
    j <- i + 1
    col_name_i <- paste("AI.month", i, sep = ".")
    col_name_j <- paste("AI.month", j, sep = ".")
    colsMapeM[i] <- mape(feats[1, col_name_j], feats[1, col_name_i]) * 100
  }

  


  # TARIFAS 
  T2.0_VALLE <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_2.0 == "valle"]])
  T2.0_LLANO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_2.0 == "llano"]])
  T2.0_PICO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_2.0 == "pico"]])
  
  T_SOLAR_LLANO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "llano"]])
  T_SOLAR_PICO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "pico"]])
  T_SOLAR_SPICO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "solar pico"]])
  T_SOLAR_SLLANO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "solar llano"]])
  
 
  
  
  LENGTH <- nrow(serie)
  QQ     <- as.numeric(quantile(serie$VAL_AI,c(0,0.25,0.5,0.75,1),na.rm=T))
  
  primer_no_cero <- which(serie$VAL_AE != 0)[1]
  
  # Encuentra el índice del primer 1 en la columna AUTO
  primer_uno <- which(serie$AUTO == 1)[1]
  
  # Calcula la diferencia en horas
  diff_horas <- as.double(difftime(serie$timestamp[primer_uno], serie$timestamp[primer_no_cero], units = "hours"))
  

  feats <- feats %>% 
    add_row(
      ID = ID1,  # Convertir a caracter
      LENGTH = LENGTH,
      ZEROS = sum(serie$VAL_AI == 0) / LENGTH,
      NAs = sum(is.na(serie$VAL_AI)),
      AVG = mean(serie$VAL_AI, na.rm = TRUE),
      SD = sd(serie$VAL_AI, na.rm = TRUE),
      MIN = QQ[1],
      Q1 = QQ[2],
      MEDIAN = QQ[3],
      Q3 = QQ[4],
      MAX = QQ[5],
      TOTAL = sum(serie$VAL_AI, na.rm = TRUE),
      VAR = var(serie$VAL_AI, na.rm = TRUE),
      ENTROPY = entropy(serie$VAL_AI),
      P_T2.0_VALLE = T2.0_VALLE,
      P_T2.0_LLANO = T2.0_LLANO,
      P_T2.0_PICO = T2.0_PICO,
      P_T_SOLAR_PICO = T_SOLAR_PICO,
      P_T_SOLAR_LLANO = T_SOLAR_LLANO,
      P_T_SOLAR_SPICO = T_SOLAR_SPICO,
      P_T_SOLAR_SLLANO = T_SOLAR_SLLANO,
      INSTALLATION_TIMESTAMP = unique(serie$firstPanel),
      DIFF_HOURS = abs(diff_horas),
      mean.mape.week = mean(colsMapeW, na.rm = T),
      mean.mape.month = mean(colsMapeM, na.rm = T)
    )

  # feats <- cbind(feats, features_semana, features_fin_de_semana)
  
  for (col_name in names(features_semana)){
    feats[1, col_name] <- features_semana[1, col_name]
  }
  for (col_name in names(features_fin_de_semana)){
    feats[1, col_name] <- features_fin_de_semana[1, col_name]
  }
  data <- data.frame(matrix(ncol = length(names(feats)), nrow = 0))
  colnames(data) <- names(feats)
  for (col in names(feats)){
    if (is.numeric(feats[, col])){
      data[1,col] <- mean(feats[, col], na.rm = T)
    } else{
      data[1,col] <- feats[1, col]
    }
  }
  
  # print(head(entropyDf))
  return(list(data = data, entropyDf = entropyDf))
}


# Para vaciar si ya se han ejecutado cosas
datos_sin_auto <- list()
datos_con_auto <- list()
datos_totales <- list()
entropy_sin_auto <- data.frame()
entropy_con_auto <- data.frame()
entropy_total <- data.frame()




# Bucle para procesar cada archivo
for(archivo in archivos) {
 
  serie <- fread(archivo)
  trampa <- F
 
  serie$timestamp <- ymd_hms(serie$timestamp) # Convertir timestamp a tipo fecha
  
  id_serie <- basename(archivo)
  id_serie <- gsub("\\.csv$", "", id_serie)
  
  if (!(id_serie %in% roseta$CUPS)){ trampa <- T}
  
  if (trampa == T){ # Es trampa, elegimos las feats de otro archivo
    info_serie <- featsNoPV %>% filter(ID == id_serie)
    info_serie <- info_serie %>% select(POT_NOM) 
    setnames(info_serie, old ="POT_NOM", new = "VAL_POT_AUTORIZADA")
  } else{
    info_serie <- roseta %>% filter(CUPS == id_serie) %>% select(-CUPS)
  }
  # print(summary(info_serie))
  # Calcular features
  firstPanel <- serie %>%
    filter(AUTO == 1) %>%
    slice(1) %>% select(timestamp)
  
  firstPanel <- ifelse(is.null(firstPanel) || all(is.na(firstPanel)), NA, firstPanel)
  
  serie$firstPanel <- firstPanel
  serie$ID <- id_serie

  
  if (trampa == T){
    sin_auto <-  calcular_features(serie = serie %>% filter(AUTO == 0), ID1 = gsub("\\.csv$", "", basename(archivo)),
                                   firstPanel, val_pot = info_serie$VAL_POT_AUTORIZADA)
  } else{
    con_auto <- if (any(serie$AUTO == 1)) calcular_features(serie = serie %>% filter(AUTO == 1), ID1 = gsub("\\.csv$", "", basename(archivo)),
                                                                          firstPanel,  val_pot = info_serie$VAL_POT_AUTORIZADA)
  }
  
  # Ahora sin auto es solo la parte de series trampa
  
# 
#   total <-  calcular_features(serie = serie, ID1 = gsub("\\.csv$", "", basename(archivo)),
#                                        firstPanel,  val_pot = info_serie$VAL_POT_AUTORIZADA)

  
  # info_serie <- roseta %>% filter(CUPS == id_serie) %>% select(-CUPS) # Excluir CUPS para evitar duplicados
  
  features_sin_auto <- sin_auto$data
  features_con_auto <- con_auto$data
  # features_total <- total$data
  
  entsin <- intersect(colnames(entropy_sin_auto), colnames(sin_auto))
  entropy_sin_auto <- rbind(entropy_sin_auto[entsin], sin_auto$entropy[entsin])
  
  entcon <- intersect(colnames(entropy_con_auto), colnames(con_auto))
  entropy_con_auto <- rbind(entropy_con_auto[entcon], con_auto$entropy[entcon])
  
  
  # entTot <- intersect(colnames(entropy_total), colnames(entropy_total))
  # entropy_total <- rbind(entropy_total[entTot], total$entropy[entTot])
  
   
  # Unir con información de roseta
  if (trampa == T){  datos_sin_auto[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_sin_auto, info_serie, .name_repair = "minimal")
}
  else{
  datos_con_auto[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_con_auto, info_serie, .name_repair = "minimal")}
  # datos_totales[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_total, info_serie, .name_repair = "minimal")
}

df_sin_auto <- bind_rows(datos_sin_auto) 
df_sin_auto$ID <- df_sin_auto$ID_SERIE
df_sin_auto <- df_sin_auto %>%  select(-ID_SERIE) %>% distinct(ID, .keep_all = T)

df_con_auto <- bind_rows(datos_con_auto)
df_con_auto$ID <- df_con_auto$ID_SERIE
df_con_auto <- df_con_auto %>% select(-ID_SERIE) %>% distinct(ID, .keep_all = T)

df_totales <- bind_rows(datos_totales)
df_totales$ID <- df_totales$ID_SERIE
df_totales <- df_totales %>% select(-ID_SERIE) %>% distinct(ID, .keep_all = T)



fwrite(df_sin_auto, "SOLAR/features_sin_autoconsumo_Trampa.csv")
fwrite(df_con_auto, "SOLAR/features_con_autoconsumo_ConPV.csv")
fwrite(df_totales, "SOLAR/features_totales.csv_Trampa_PV")


fwrite(entropy_sin_auto, "SOLAR/Entropy_sin_autoconsumo_Trampa.csv")
fwrite(entropy_con_auto, "SOLAR/Entropy_con_autoconsumo_ConPV.csv")
fwrite(entropy_total, "SOLAR/Entropy_totales.csv_Trampa_PV")

# Combina todos los datos en un data.frame (este paso depende de cómo quieras estructurar tus datos finales)
datos_combinados <- bind_rows(lapply(datos_finales, bind_rows), .id = "ID_SERIE")

# Guardar el data.frame combinado en un archivo CSV
fwrite(datos_combinados, "SOLAR/newFeatsSolar.csv")
