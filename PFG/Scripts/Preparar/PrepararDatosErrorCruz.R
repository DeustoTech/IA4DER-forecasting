library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", 
                 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mltools", "zoo") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}
library(foreach)
library(doParallel)

# Vector con todas las librerías necesarias
librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "forecast", "Metrics", "fable", "data.table", "xts", "future",
               "foreach", "doParallel", "RSNNS", "TTR", "quantmod", "car", "e1071",
               "nnet", "tools", "doFuture", "neuralnet", "gbm", "randomForest", 
               "mltools", "zoo", "mlr3", "mlr3tuning", "paradox", "mlr3learners", "stringr")

# Instalar y cargar las librerías
foreach(lib = librerias) %do% {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
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
fwrite(combined_df, "NUEVOS DATOS/DATOS ERROR CRUZ/goi4_pst_preds.csv")


#JUNTAR DATOS DE ERRORES
folder <- "NUEVOS DATOS/goi4_pst_mape"
files_1 <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
data_list <- list()

for (file in files_1) {
  df <- fread(file)
  if ("V1" %in% names(df)) {
    df[, V1 := NULL]
  }
  setnames(df, old = names(df), new = paste0(names(df), "_error"))
  id <- tools::file_path_sans_ext(basename(file))
  df[, id := id]
  data_list[[length(data_list) + 1]] <- df
}

combined_df <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
fwrite(combined_df, "NUEVOS DATOS/DATOS ERROR CRUZ/goi4_pst_mape.csv")


#JUNTAR AMBOS

errors <- fread("NUEVOS DATOS/DATOS ERROR CRUZ/goi4_pst_mape.csv")
preds <- fread("NUEVOS DATOS/DATOS ERROR CRUZ/goi4_pst_preds.csv")
colnames(preds)[colnames(preds) == "real_pred"] <- "real"
errorsYpreds <- merge(errors, preds, by = "id")
errorsYpreds <- na.omit(errorsYpreds)

fwrite(errorsYpreds, "NUEVOS DATOS/DATOS ERROR CRUZ/combined_data_NEW.csv", row.names = FALSE)

#JUNTAR CON METADATA (features)
combined <- fread("NUEVOS DATOS/DATOS ERROR CRUZ/combined_data_NEW.csv")
metadata <- fread("NUEVOS DATOS/OriginalData/metadata.csv")

colnames(metadata)[colnames(metadata) == "user"] <- "id"

allMetadata <- merge(combined, metadata, by = "id")
fwrite(allMetadata, "NUEVOS DATOS/DATOS ERROR CRUZ/allMetadata.csv", row.names = FALSE)
