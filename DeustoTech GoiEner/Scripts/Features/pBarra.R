library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

# Leer
{
folder <- "Resultados/PrediccionError/"


# Lista de modelos y sus correspondientes archivos
model_files <- list(
  lm = list.files(folder, pattern = "_lm.csv$", recursive = TRUE, full.names = TRUE),
  rf = list.files(folder, pattern = "_rf.csv$", recursive = TRUE, full.names = TRUE),
  gbm = list.files(folder, pattern = "_gbm.csv$", recursive = TRUE, full.names = TRUE),
  nn = list.files(folder, pattern = "_nn.csv$", recursive = TRUE, full.names = TRUE),
  svm = list.files(folder, pattern = "_svm.csv$", recursive = TRUE, full.names = TRUE)
)

# Inicializar un dataframe vacío
result_df <- data.frame()

# Iterar sobre cada modelo y sus archivos correspondientes
for (model_name in names(model_files)) {
  # Leer todos los archivos para un modelo y combinarlos
  model_data <- lapply(model_files[[model_name]], read.csv)
  
  # Extraer el nombre del modelo
  model_name_extracted <- gsub("_.*$", "", model_name)
  
  # Renombrar las columnas
  for (i in 1:length(lm)){
  new_col_names <- ifelse(names(model_data[[i]]) %in% c("ID", "Real"), names(model_data[[i]]), paste0(names(model_data[[i]]), "_", model_name_extracted))
  names(model_data[[i]]) <- new_col_names
  }
  
  # Añadir el dataframe al resultado final
  result_df <- dplyr::bind_rows(result_df, model_data)
}
result_df <- result_df %>%
  select(-starts_with("MAE"))

# Verificar el resultado
head(result_df)
fwrite(result_df, "Resultados/PrediccionError/combinedPreds.csv")
}

# Calcular p barra 
{
  
  mapes <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana",
              "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana", "mapeEnsemble_mediana")
  
  datos <- feats[which(!is.na(feats$mapeEnsemble_mediana)), ]
  preds <- read.csv("Resultados/PrediccionError/combinedPreds.csv")
  sumMape <- rowSums(datos[, mapes])
  (1 / sumMape)
  
  
  
}
