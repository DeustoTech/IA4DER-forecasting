library(foreach)
library(doParallel)

# FFORMA SCRIPT: given the predicted error of each base forecast model, predict 
# consumption using fforma

# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "purrr", "matrixStats", "stringr") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


# Leer
folder <- "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA"

# Función para buscar archivos por patrón en una carpeta (sin subcarpetas)
buscar_archivos_por_modelo <- function(folder, modelo) {
  patron <- paste0("^Pred_.*_mape_", modelo, "\\.csv$")
  archivos_modelo <- list.files(folder, pattern = patron, full.names = TRUE)
  return(archivos_modelo)
}

# Buscar archivos por cada modelo directamente en la carpeta base
model_files <- list(
  lm = buscar_archivos_por_modelo(folder, "lm"),
  rf = buscar_archivos_por_modelo(folder, "rf"),
  xgboost = buscar_archivos_por_modelo(folder, "xgboost"),
  knn = buscar_archivos_por_modelo(folder, "knn")
)

leer_y_renombrar <- function(filepath) {
  df <- fread(filepath)
  
  filename <- basename(filepath)
  
  # Extraer tipo y modelo
  nombre_sin_ext <- sub("\\.csv$", "", filename)
  partes <- strsplit(nombre_sin_ext, "_")[[1]]
  tipo <- partes[2]
  modelo <- partes[4]
  
  # Verificación
  print(paste("Archivo:", filename, "→ Tipo:", tipo, ", Modelo:", modelo))
  
  # Renombrar columnas usando posiciones
  new_names <- c("ID", "dia",
                 paste0("Real_", tipo),
                 paste0("Predicted_", tipo, "_", modelo),
                 paste0("MAPE_", tipo, "_", modelo))
  
  setnames(df, old = names(df)[1:5], new = new_names)
  return(df)
}


combinar_archivos_en_df <- function(archivos_modelo) {
  lista_dfs <- lapply(archivos_modelo, leer_y_renombrar)
  
  # Verificar que los IDs sean iguales en todos
  ids_iguales <- all(sapply(lista_dfs[-1], function(df) all(df$ID == lista_dfs[[1]]$ID)))
  if (!ids_iguales) {
    stop("Los IDs no están en el mismo orden en todos los archivos.")
  }
  
  # Eliminar columnas duplicadas (ID y dia) al combinar
  lista_dfs_sin_id <- lapply(lista_dfs[-1], function(df) df[, !c("ID", "dia"), with = FALSE])
  df_combinado <- cbind(lista_dfs[[1]], do.call(cbind, lista_dfs_sin_id))
  return(df_combinado)
}


lm_df <- combinar_archivos_en_df(model_files[[1]])
fwrite(lm_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combined_lm.csv")
rf_df <- combinar_archivos_en_df(model_files[[2]])
fwrite(rf_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combined_rf.csv")
xgboost_df <- combinar_archivos_en_df(model_files[[3]])
fwrite(xgboost_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combined_xgboost.csv")
knn_df <- combinar_archivos_en_df(model_files[[4]])
fwrite(knn_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combined_knn.csv")


combinedPreds <- lm_df %>%
  full_join(rf_df, by = c("ID", "dia", "Real_ann", "Real_arima", "Real_ens", "Real_lr", "Real_mean", "Real_naive", "Real_rw", "Real_ses", "Real_simple", "Real_svm")) %>%
  full_join(xgboost_df, by = c("ID", "dia", "Real_ann", "Real_arima", "Real_ens", "Real_lr", "Real_mean", "Real_naive", "Real_rw", "Real_ses", "Real_simple", "Real_svm")) %>%
  full_join(knn_df, by = c("ID", "dia", "Real_ann", "Real_arima", "Real_ens", "Real_lr", "Real_mean", "Real_naive", "Real_rw", "Real_ses", "Real_simple", "Real_svm")) 


fwrite(combinedPreds, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combinedPreds.csv")

#LEER ARCHIVOS PARA FFORMA

feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
combinedPreds <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combinedPreds.csv")
feats$ID <- feats$id
feats <- feats %>% select(-id)
feats <- feats %>% select(ID, dia, hora, real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred, mean_mape, rw_mape, naive_mape, simple_mape, lr_mape, ann_mape, svm_mape, arima_mape, ses_mape, ens_mape)
combinedPreds <- combinedPreds %>% select(ID, starts_with("Real_"), starts_with("Predicted_"), starts_with("MAPE_"))
feats$Real <- feats$real
feats <- feats %>% select(-real)

combinedPreds <- combinedPreds %>% distinct(ID, .keep_all = TRUE)

datosCombinadosENTERO <- feats %>%
  left_join(combinedPreds, by = "ID")

datosCombinados <- datosCombinadosENTERO
setDT(datosCombinados)
cols <- c("ID", "Real", "dia", "hora", "mean_pred", "rw_pred", "naive_pred", "simple_pred", "lr_pred", "ann_pred", "svm_pred", "arima_pred", "ses_pred", "ens_pred",
          "mean_mape", "rw_mape", "naive_mape", "simple_mape", "lr_mape", "ann_mape", "svm_mape", "arima_mape", "ses_mape", "ens_mape")
pb <- datosCombinados[, ..cols]

modelosC <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses")
#modelosC <- c( "rw", "naive", "simple", "svm", "ens")
modelosP <- c("lm", "rf", "xgboost", "knn")
pred_cols <- paste0(modelosC, "_pred")

for (modeloP in modelosP) {
  
  mape_cols <- paste0("Predicted_", modelosC, "_", modeloP) #prediccion del error
  #mape_cols <- paste0(modelosC, "_mape") #error normal
  aux <- datosCombinados[, c(pred_cols, mape_cols), with = FALSE]  # Extraemos todas las columnas necesarias en una tabla auxiliar
  
  setnames(aux, pred_cols, paste0("consumo_", modelosC))
  setnames(aux, mape_cols, paste0("mape_", modelosC))
  
  # Calcular los pesos = 1 / mape (para cada columna de modeloC)
  for (mc in modelosC) {
    aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
  }
  
  numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
  denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
  pbarra_name <- paste0("FFORMA_", modeloP)
  pb[, (pbarra_name) := numerador / denominador]
  
  cat("Calculado FFORMA para modelo", modeloP, "\n")
}


#añadir ensemble
pb_cols <- paste0("FFORMA_", modelosP)
pb[, FFORMA_Ensemble := rowMeans(.SD, na.rm = TRUE), .SDcols = pb_cols]


#error real
mape_cols <- paste0(modelosC, "_mape") #error normal
aux <- datosCombinados[, c(pred_cols, mape_cols), with = FALSE]  # Extraemos todas las columnas necesarias en una tabla auxiliar
  
setnames(aux, pred_cols, paste0("consumo_", modelosC))
setnames(aux, mape_cols, paste0("mape_", modelosC))
  
# Calcular los pesos = 1 / mape (para cada columna de modeloC)
for (mc in modelosC) {
  aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
}
  
numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
pbarra_name <- paste0("FFORMA_", "min")
pb[, (pbarra_name) := numerador / denominador]

#fforma_hist
mape_cols <- paste0(modelosC, "_mape")  # columnas de error
aux <- datosCombinados[, c(pred_cols, mape_cols), with = FALSE]  # Extraer columnas necesarias

setnames(aux, pred_cols, paste0("consumo_", modelosC))
setnames(aux, mape_cols, paste0("mape_", modelosC))

for (mc in modelosC) {
  mape_col <- paste0("mape_", mc)
  mediana_val <- median(aux[[mape_col]], na.rm = TRUE)
  aux[[mape_col]] <- mediana_val  # Rellenar toda la columna con la mediana
}

for (mc in modelosC) {
  aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
}

numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
pbarra_name <- paste0("FFORMA_", "hist")
pb[, (pbarra_name) := numerador / denominador]


fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA.csv")



#calcular MAPE
pb_cols <- paste0("FFORMA_", modelosP)
pb_cols <- c(pb_cols, "FFORMA_Ensemble", "FFORMA_min", "FFORMA_hist")  # También incluimos el ensemble

for (col_name in pb_cols) {
  mape_col <- paste0(col_name, "_MAPE")
  
  pb[, (mape_col) := abs((Real - get(col_name)) / Real) * 100]
}


primarias <- c("ID", "Real", "dia", "hora")
pred_cols <- grep("_pred$", names(pb), value = TRUE)
pred_cols <- setdiff(pred_cols, "ens_pred")
fforma_cols <- grep("^FFORMA_.*[^MAPE]$", names(pb), value = TRUE)
fforma_cols <- setdiff(fforma_cols, c("FFORMA_min", "FFORMA_hist", "FFORMA_Ensemble"))  # quitamos los especiales
fforma_ordered <- c("FFORMA_min", "FFORMA_hist", "FFORMA_Ensemble", fforma_cols)
mape_cols <- grep("_mape$", names(pb), value = TRUE)
mape_cols <- setdiff(mape_cols, "ens_mape")
fforma_mape_cols <- grep("^FFORMA_.*_MAPE$", names(pb), value = TRUE)
fforma_mape_cols <- setdiff(fforma_mape_cols, c("FFORMA_min_MAPE", "FFORMA_hist_MAPE", "FFORMA_Ensemble_MAPE"))
fforma_mape_ordered <- c("FFORMA_min_MAPE", "FFORMA_hist_MAPE", "FFORMA_Ensemble_MAPE", fforma_mape_cols)
errores_ordered <- c(mape_cols, "ens_mape", fforma_mape_ordered)
nuevo_orden <- c(primarias, pred_cols, fforma_ordered, errores_ordered)
pb <- pb[, ..nuevo_orden]

fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")
