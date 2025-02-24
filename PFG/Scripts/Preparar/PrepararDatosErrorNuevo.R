library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'car', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mice", "mltools", "zoo") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


#JUNTAR DATOS DE PREDICCIONES
folder <- "NUEVOS DATOS/goi4_pst_preds"
files_2 <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
data_list <- list()

for (file in files_2) {
  
  df <- fread(file)
  
  original_cols <- names(df)
  pred_cols <- setdiff(original_cols, "real")
  
  id <- tools::file_path_sans_ext(basename(file))
  df[, id := id]
  
  df[, dia := rep(1:7, each = 24)]  # Días del 1 al 7
  df[, hora := rep(0:23, times = 7)]  # Horas de 0 a 23
  
  setnames(df, old = pred_cols, new = paste0(pred_cols, "_pred"))
  data_list[[length(data_list) + 1]] <- df
}

combined_df <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
column_order <- c("id", "dia", "hora", "real", setdiff(names(combined_df), c("id", "dia", "hora", "real")))
setcolorder(combined_df, column_order)
fwrite(combined_df, "NUEVOS DATOS/DATOS ERROR NUEVO/goi4_pst_preds.csv")


#VOLVER A CALCULAR MAPES Y RMSE
preds <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/goi4_pst_preds.csv")

modelos <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")

foreach(model = modelos) %do% {
  mape_col <- paste0(model, "_mape")
  rmse_col <- paste0(model, "_rmse")
  pred_col <- paste0(model, "_pred")
  
  preds[[mape_col]] <- abs((preds[[pred_col]] - preds$real) / preds$real) * 100
  preds[[rmse_col]] <- sqrt((preds[[pred_col]] - preds$real)^2)
}
fwrite(preds, "NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")


#JUNTAR CON METADATA (features)
combined <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
metadata <- fread("NUEVOS DATOS/OriginalData/metadata.csv")

colnames(metadata)[colnames(metadata) == "user"] <- "id"

allMetadata <- merge(combined, metadata, by = "id")
fwrite(allMetadata, "NUEVOS DATOS/DATOS ERROR NUEVO/allMetadataDEF.csv", row.names = FALSE)
