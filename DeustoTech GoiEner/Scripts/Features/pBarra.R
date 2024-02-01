library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "purrr") 

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

df_list <- c()

for (file in model_files){
  for (i in 1:length(file)){
  df <- read.csv(file[i])
  df_list <- append(df_list, list(df))}
  
}


combined <- df_list %>% reduce(inner_join, by = "ID")


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
