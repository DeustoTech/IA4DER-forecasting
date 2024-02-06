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


combined <- df_list %>% reduce(full_join, by = "ID")
colnames(combined)[colnames(combined) == "Real.x"] <- "Real"
combined <- combined %>%
  select(-starts_with("MAE")) %>% select(c("ID","Real", starts_with("Predicted"))) 

fwrite(combined, "Resultados/PrediccionError/combinedPreds.csv")

}

# Calcular p barra 
{
  combined <- read.csv("Resultados/PrediccionError/combinedPreds.csv")
  feats <- read.csv("featuresPredicciones_3.csv")
  
  modelos <- c("rf", "lm", "svm", "nn", "gbm")
  featuresets <- c("s1", "s2","s3")
  
  mapes <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana",
              "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana", "mapeEnsemble_mediana")
  modelosOG <- c("Media", "Naive", "Arima", "SN", "NN", "ETS", "SVM", "Ensemble")
  
  
  columns_to_select <- c("ID", mapes)  # Excluimos la primera columna de combined que es "ID"
  
  # Unir los dataframes por la columna "ID"
  df <- merge(feats[columns_to_select], combined, by = "ID", all.x = TRUE)
  df<- df[df$ID %in% combined$ID, ]
  sumMapes <- rowSums(df[, mapes], na.rm = T)
  df <- df[which(sumMapes != 0), ]
  sumMapes <- sumMapes[which(sumMapes != 0)]
  
  for (i in 1:nrow(df)){
    
    sumMape <- sumMapes[i]
    
    for (modelo in modelos){ # lm, gbm, svm, rf y nn
      
      for (set in featuresets){
        
        weighted_sum <- 0
        
        for (modeloOG in modelosOG){
          pred_col <- paste("Predicted", modeloOG, set, modelo, sep = "_")
          mape_col <- paste("mape", modeloOG, "_mediana", sep = "")
          
          weighted_sum <- weighted_sum + ifelse(is.na(df[i, mape_col] * df[i, pred_col]), 0, df[i, mape_col] * df[i, pred_col])
        }
        
        pBarra_col <- paste("pBarra", set, modelo, sep = "_")
        df[i, pBarra_col] <- (1 / sumMape) * weighted_sum
      }
    }
    
    
  }

  
  pbarras <- df %>% select(ID, starts_with("pBarra"))
  
  fwrite(pbarras, "sumaPonderada.csv")
 
  
  columnas_nuevas <- colnames(pbarras)[-which(colnames(pbarras) == "ID")]
  feats <- merge(feats, pbarras, by = "ID", all.x = TRUE, suffixes = c("", ""))
  
  
  fwrite(feats, "featuresPredicciones_3.csv")
  
}
