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

# Bucle para procesar cada archivo
for(archivo in archivos) {
 
  serie <- fread(archivo)
  serie$timestamp <- ymd_hms(serie$timestamp)# Convertir timestamp a tipo fecha
  
  id_serie <- basename(archivo)
  id_serie <- gsub("\\.csv$", "", id_serie)
  
  info_serie <- roseta %>% filter(CUPS == id_serie)
  
  # Función para calcular estadísticas descriptivas
  calcular_features <- function(data) {
    data %>% summarise(AVG = mean(VAL_AI, na.rm = TRUE),
                       SD = sd(VAL_AI, na.rm = TRUE),
                       MIN = min(VAL_AI, na.rm = TRUE),
                       Q1 = quantile(VAL_AI, 0.25, na.rm = TRUE),
                       MEDIAN = median(VAL_AI, na.rm = TRUE),
                       Q3 = quantile(VAL_AI, 0.75, na.rm = TRUE),
                       MAX = max(VAL_AI, na.rm = TRUE),
                       TOTAL = sum(VAL_AI, na.rm = TRUE),
                       VAR = var(VAL_AI, na.rm = TRUE))
  }
  
  
  # Calcular features
  features_sin_auto <- if (any(serie$AUTO == 0)) calcular_features(serie %>% filter(AUTO == 0))
  features_con_auto <- if (any(serie$AUTO == 1)) calcular_features(serie %>% filter(AUTO == 1))
  features_total <- calcular_features(serie)
  
  # Unir con información de roseta
  info_serie <- roseta %>% filter(CUPS == id_serie) %>% select(-CUPS) # Excluir CUPS para evitar duplicados
  
  datos_sin_auto[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_sin_auto, info_serie)
  datos_con_auto[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_con_auto, info_serie)
  datos_totales[[id_serie]] <- tibble(ID_SERIE = id_serie) %>% bind_cols(features_total, info_serie)
}

df_sin_auto <- bind_rows(datos_sin_auto)
df_con_auto <- bind_rows(datos_con_auto)
df_totales <- bind_rows(datos_totales)

fwrite(df_sin_auto, "SOLAR/features_sin_autoconsumo.csv")
fwrite(df_con_auto, "SOLAR/features_con_autoconsumo.csv")
fwrite(df_totales, "SOLAR/features_totales.csv")
# Combina todos los datos en un data.frame (este paso depende de cómo quieras estructurar tus datos finales)
datos_combinados <- bind_rows(lapply(datos_finales, bind_rows), .id = "ID_SERIE")

# Guardar el data.frame combinado en un archivo CSV
fwrite(datos_combinados, "SOLAR/newFeatsSolar.csv")
