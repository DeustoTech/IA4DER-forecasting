library(foreach)
library(doParallel)

# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS",  
               'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable", "tidyverse") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

###### BOXPLOT SIMPLE CON LOS DATOS REDUCIDOS POR DIA ##########
feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
feats$ID <- feats$id
feats <- feats %>% select(-id)
feats <- feats %>% select(ID, dia, hora, real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred, mean_mape, rw_mape, naive_mape, simple_mape, lr_mape, ann_mape, svm_mape, arima_mape, ses_mape, ens_mape)
feats$Real <- feats$real
feats <- feats %>% select(-real)

combinedPredsDIA <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combinedPreds.csv")
combinedPredsDIA <- combinedPredsDIA %>% select(ID, starts_with("Real_"), starts_with("Predicted_"), starts_with("MAPE_"))
combinedPredsDIA <- combinedPredsDIA %>% distinct(ID, .keep_all = TRUE)

datosCombinadosDIA <- feats %>%
  left_join(combinedPredsDIA, by = "ID")
setDT(datosCombinadosDIA)

mape_lm <- grep("MAPE_.*_lm$", names(datosCombinadosDIA), value = TRUE)
mape_rf <- grep("MAPE_.*_rf$", names(datosCombinadosDIA), value = TRUE)
mape_knn <- grep("MAPE_.*_knn$", names(datosCombinadosDIA), value = TRUE)
mape_xgb <- grep("MAPE_.*_xgboost$", names(datosCombinadosDIA), value = TRUE)

graficar_boxplot <- function(columnas) {
  data <- datosCombinadosDIA[, ..columnas]
  matrix_data <- as.matrix(data)
  matrix_data <- matrix_data[complete.cases(matrix_data) & rowSums(is.finite(matrix_data)) == length(columnas), ]
  par(mar = c(10, 4, 4, 2))
  b <- boxplot(matrix_data, outline = FALSE, las = 3, ylab = "MAPE")
  medianas <- apply(matrix_data, 2, median, na.rm = TRUE)
  text(
    x = 1:length(medianas),
    y = medianas + 7,  # Posición justo encima del upper whisker
    labels = round(medianas, 1),  # Redondear si lo deseas
    cex = 1.5
  )
}

graficar_boxplot(mape_lm)
graficar_boxplot(mape_rf)
graficar_boxplot(mape_knn)
graficar_boxplot(mape_xgb)

