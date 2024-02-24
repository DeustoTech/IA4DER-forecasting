library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "mice", "mltools") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


#read csvs
{
  roseta <- fread("SOLAR/roseta.csv")
  
  archivos <- list.files("SOLAR/SOLAR", full.names = TRUE, pattern = "\\.csv$")
 
}



datos_sin_auto <- list()
datos_con_auto <- list()
datos_totales <- list()

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
calcular_features <- function(serie, ID1, firstPanel) {
  
  # print(paste("DENTRO: ", ID1))
  
  
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
              Max = max(VAL_AI)) %>%
    pivot_wider(names_from = c(season, day_period), values_from = c(Total, Max))
  
  features_fin_de_semana <- finde %>%
    group_by(season) %>%
    summarise(Total = sum(VAL_AI),
              Max = max(VAL_AI))%>%
    pivot_wider(names_from = season, values_from = c(Total, Max))
  
  
  colnames(features_fin_de_semana) <- paste(colnames(features_fin_de_semana), "finde", sep = "_")
  
  
  # INYECCION POR HORAS 
  injection_by_hour_weekdays <- laborable %>%
    group_by(hour) %>%
    summarise(mean_VAL_AE = mean(VAL_AE, na.rm = TRUE)) %>% pivot_wider(names_from = hour, values_from = mean_VAL_AE)
  colnames(injection_by_hour_weekdays) <- paste("mean_VAL_AE_weekdays", 0:23, sep = "_")
  
  # Calcular la inyección media cada hora en fin de semana
  injection_by_hour_weekend <- finde %>%
    group_by(hour) %>%
    summarise(mean_VAL_AE = mean(VAL_AE, na.rm = TRUE)) %>% pivot_wider(names_from = hour, values_from = mean_VAL_AE)
  
  colnames(injection_by_hour_weekend) <- paste("mean_VAL_AE_weekend", 0:23,sep = "_")
  
  
  # CONSUMO POR SEMANA Y MES
  nWeeks <- max(serie$week)
  
  consumption_by_week <- serie %>%
    group_by(week) %>%
    summarise(total_VAL_AI = sum(VAL_AI, na.rm = TRUE)) %>% pivot_wider(names_from = week, values_from = total_VAL_AI)
  
  colnames(consumption_by_week) <- paste("total_VAL_AI_week", 1:nWeeks,sep = "_")
  
  consumption_by_month <- serie %>%
    group_by(month) %>%
    summarise(total_VAL_AI = sum(VAL_AI, na.rm = TRUE)) %>% pivot_wider(names_from = month, values_from = total_VAL_AI)
  colnames(consumption_by_month) <- paste("total_VAL_AI_month", 1:12,sep = "_")
  
  
  # INYECCION POR SEMANA Y MES
  inyection_by_week <- serie %>%
    group_by(week) %>%
    summarise(total_VAL_AE = sum(VAL_AE, na.rm = TRUE)) %>% pivot_wider(names_from = week, values_from = total_VAL_AE)
  colnames(inyection_by_week) <- paste("total_VAL_AI_week", 1:nWeeks,sep = "_")   
  
  inyection_by_month <- serie %>%
    group_by(month) %>%
    summarise(total_VAL_AE = sum(VAL_AE, na.rm = TRUE)) %>% pivot_wider(names_from = month, values_from = total_VAL_AE)
  colnames(inyection_by_month) <- paste("total_VAL_AI_month", 1:12,sep = "_")
  
  # por alguna razon pone numeros a las columnas, los quitamos luego
  
  # TARIFAS 
  T2.0_VALLE <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_2.0 == "valle"]])
  T2.0_LLANO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_2.0 == "llano"]])
  T2.0_PICO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_2.0 == "pico"]])
  
  T_SOLAR_LLANO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "llano"]])
  T_SOLAR_PICO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "pico"]])
  T_SOLAR_SPICO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "solar pico"]])
  T_SOLAR_SLLANO <- sum(serie$VAL_AI[serie$hour %in% horas$hora[horas$TARIFA_SOLAR == "solar llano"]])
  
  # TODO entropía (??????????)
  # TODO mape del SNaive. el mape del consumo horario de una semana comparado con el consumo horario de la semana pasada. Una por semana

  LENGTH <- nrow(serie)
  QQ     <- as.numeric(quantile(serie$VAL_AI,c(0,0.25,0.5,0.75,1),na.rm=T))
  
  feats <- data.frame(

    ID = unique(serie$ID),
    LENGTH= LENGTH,
    AVG=    mean(serie$VAL_AI,na.rm=T),
    SD=     sd(serie$VAL_AI,na.rm=T),
    MIN=    QQ[1],
    Q1=     QQ[2],
    MEDIAN= QQ[3],
    Q3=     QQ[4],
    MAX=    QQ[5],
    TOTAL = sum(serie$VAL_AI, na.rm = T),
    VAR = var(serie$VAL_AI, na.rm = T),
    
    P_T2.0_VALLE = T2.0_VALLE,
    P_T2.0_LLANO = T2.0_LLANO,
    P_T2.0_PICO = T2.0_PICO,
    P_T_SOLAR_PICO = T_SOLAR_PICO,
    P_T_SOLAR_LLANO = T_SOLAR_LLANO,
    P_T_SOLAR_SPICO = T_SOLAR_SPICO,
    P_T_SOLAR_SLLANO = T_SOLAR_SLLANO,
    firstPanel_timestamp =  unique(serie$firstPanel)
    
  )
  
  data <- cbind(feats, features_semana, features_fin_de_semana, injection_by_hour_weekend, injection_by_hour_weekdays,
                inyection_by_week, inyection_by_month, consumption_by_week, consumption_by_month, inyection_by_week, inyection_by_month)
  
  return(data)
}

# Bucle para procesar cada archivo
for(archivo in archivos) {
 
  serie <- fread(archivo)
  serie$timestamp <- ymd_hms(serie$timestamp)# Convertir timestamp a tipo fecha
  
  id_serie <- basename(archivo)
  id_serie <- gsub("\\.csv$", "", id_serie)
  
  info_serie <- roseta %>% filter(CUPS == id_serie)
  # Calcular features
  firstPanel <- serie %>%
    filter(AUTO == 1) %>%
    slice(1) %>% select(timestamp)
  
  serie$firstPanel <- firstPanel
  serie$ID <- id_serie
  
  features_sin_auto <- if (any(serie$AUTO == 0)) calcular_features(serie = serie %>% filter(AUTO == 0), ID1 = gsub("\\.csv$", "", basename(archivo)), firstPanel)
  features_con_auto <- if (any(serie$AUTO == 1)) calcular_features(serie = serie %>% filter(AUTO == 1), ID1 = gsub("\\.csv$", "", basename(archivo)),  firstPanel)
  features_total <- calcular_features(serie)
  
  # Para quitar ...numeritos que saca el pivot
  #
  # Error in `colnames<-`(`*tmp*`, value = gsub("\\.\\.\\..*$", "", colnames(features_sin_auto))) : 
  #   attempt to set 'colnames' on an object with less than two dimensions
  
  colnames(features_sin_auto) <- gsub("\\.\\.\\..*$", '', colnames(features_sin_auto))
  colnames(features_con_auto) <- gsub("\\.\\.\\..*$", '', colnames(features_con_auto))
  colnames(features_total) <- gsub("\\.\\.\\..*$", '', colnames(features_total))
  
  # Unir con información de roseta
  info_serie <- roseta %>% filter(CUPS == id_serie) %>% select(-CUPS) # Excluir CUPS para evitar duplicados
  
  datos_sin_auto[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_sin_auto, info_serie)
  datos_con_auto[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_con_auto, info_serie)
  datos_totales[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_total, info_serie)
}

df_sin_auto <- bind_rows(datos_sin_auto) 
df_sin_auto <- df_sin_auto[, 1:246] %>% select(-ID_SERIE) %>% distinct(ID, .keep_all = T)

df_con_auto <- bind_rows(datos_con_auto)
df_con_auto <- df_con_auto[, 1:311] %>% select(-ID_SERIE) %>% distinct(ID, .keep_all = T)

df_totales <- bind_rows(datos_totales)
df_totales <- df_totales[, 1:338] %>% select(-ID_SERIE) %>% distinct(ID, .keep_all = T)



fwrite(df_sin_auto, "SOLAR/features_sin_autoconsumo.csv")
fwrite(df_con_auto, "SOLAR/features_con_autoconsumo.csv")
fwrite(df_totales, "SOLAR/features_totales.csv")
# Combina todos los datos en un data.frame (este paso depende de cómo quieras estructurar tus datos finales)
datos_combinados <- bind_rows(lapply(datos_finales, bind_rows), .id = "ID_SERIE")

# Guardar el data.frame combinado en un archivo CSV
fwrite(datos_combinados, "SOLAR/newFeatsSolar.csv")
