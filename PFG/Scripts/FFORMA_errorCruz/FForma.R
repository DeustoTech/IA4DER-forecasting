library(foreach)
library(doParallel)

# FFORMA SCRIPT: given the predicted error of each base forecast model, predict 
# consumption using fforma



# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "purrr", "matrixStats") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


# Leer
folder <- "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE"

# Función para buscar archivos por patrón en una carpeta (sin subcarpetas)
buscar_archivos_por_modelo <- function(folder, pattern) {
  archivos_modelo <- list.files(folder, pattern = pattern, full.names = TRUE)
  return(archivos_modelo)
}

# Buscar archivos por cada modelo directamente en la carpeta base
model_files <- list(
  lm = buscar_archivos_por_modelo(folder, "_lm_.*\\.csv$")
  #rf = buscar_archivos_por_modelo(folder, "_rf_.*\\.csv$"),
  #gbm = buscar_archivos_por_modelo(folder, "_gbm_.*\\.csv$"),
  #svm = buscar_archivos_por_modelo(folder, "_svm_tarifa.*\\.csv$"),
  #nn = buscar_archivos_por_modelo(folder, "_nn_.*\\.csv$")
)


combinar_archivos_en_df <- function(archivos_modelo) {
  lista_dfs <- lapply(archivos_modelo, fread)
  ids_iguales <- all(sapply(lista_dfs[-1], function(df) all(df[[1]] == lista_dfs[[1]][[1]])))
  if (!ids_iguales) {
    stop("Los IDs no están en el mismo orden en todos los archivos.")
  }
  lista_dfs_sin_id <- lapply(lista_dfs[-1], function(df) df[, -1, with = FALSE])
  df_combinado <- cbind(lista_dfs[[1]], do.call(cbind, lista_dfs_sin_id))
  return(df_combinado)
}
  
## COMPROBACION ##
prueba1 <- fread("PFG/Resultados/PrediccionError/ann/PredError_ann_lm_tarifa.csv")
prueba2 <- fread("PFG/Resultados/PrediccionError/arima/PredError_arima_nn_tarifa.csv")
mezcla <- merge(prueba1, prueba2, by="ID")
##


lm_df <- combinar_archivos_en_df(model_files[[1]])
fwrite(lm_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/combined_lm.csv")
rf_df <- combinar_archivos_en_df(model_files[[2]])
fwrite(rf_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_rf.csv")
gbm_df <- combinar_archivos_en_df(model_files[[3]])
fwrite(gbm_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_gbm.csv")
svm_df <- combinar_archivos_en_df(model_files[[4]])
fwrite(svm_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_svm.csv")
nn_df <- combinar_archivos_en_df(model_files[[5]])
fwrite(nn_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_nn.csv")



combinedPreds <- lm_df %>%
  full_join(gbm_df, by = c("ID", "Real_ann", "Real_arima", "Real_ens", "Real_lr", "Real_mean", "Real_naive", "Real_rw", "Real_ses", "Real_simple", "Real_svm")) %>%
  full_join(nn_df, by = c("ID", "Real_ann", "Real_arima", "Real_ens", "Real_lr", "Real_mean", "Real_naive", "Real_rw", "Real_ses", "Real_simple", "Real_svm")) %>%
  full_join(svm_df, by = c("ID", "Real_ann", "Real_arima", "Real_ens", "Real_lr", "Real_mean", "Real_naive", "Real_rw", "Real_ses", "Real_simple", "Real_svm")) %>%
  full_join(rf_df, by = c("ID", "Real_ann", "Real_arima", "Real_ens", "Real_lr", "Real_mean", "Real_naive", "Real_rw", "Real_ses", "Real_simple", "Real_svm"))


fwrite(combinedPreds, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combinedPreds.csv")

#LEER ARCHIVOS PARA PBARRA
feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE_reducido.csv")
combinedPreds <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combinedPreds.csv")
feats$ID <- feats$id
feats <- feats %>% select(-id)

#colsY <- grep("\\.y$", names(combinedPreds), value = TRUE)
#combinedPreds <- combinedPreds[, !(names(combinedPreds) %in% colsY), with = FALSE]
#colsX <- grep("\\.x$", names(combinedPreds), value = TRUE)
#combinedPreds <- combinedPreds[, !(names(combinedPreds) %in% colsX), with = FALSE]

feats$Real <- feats$real
feats <- feats %>% select(-real)
datosCombinados <- merge(combinedPreds, feats, by = "ID")
#fwrite(datosCombinados, "PFG/datosParaPBarra.csv")
#datosCombinados <- fread("datosParaPBarra.csv")


#CALCULAR P BARRA

modelosC <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
modelosP <- c("lm", "rf", "gbm", "nn", "svm")
features <- c("tarifa")


columnasPBarra <- paste("PBarra", rep(modelosP, each = length(features)), rep(features, times = length(modelosP)), sep = "_")
pb <- data.frame(matrix(ncol = length(columnasPBarra), nrow = nrow(datosCombinados)))
names(pb) <- columnasPBarra
pb$ID <- datosCombinados$ID
pb$Real <- datosCombinados$Real
pb$Real_mean <- datosCombinados$Real_mean
pb$Real_rw <- datosCombinados$Real_rw
pb$Real_naive <- datosCombinados$Real_naive
pb$Real_simple <- datosCombinados$Real_simple
pb$Real_lr <- datosCombinados$Real_lr
pb$Real_ann <- datosCombinados$Real_ann
pb$Real_svm <- datosCombinados$Real_svm
pb$Real_arima <- datosCombinados$Real_arima
pb$Real_ses <- datosCombinados$Real_ses
pb$Real_ens <- datosCombinados$Real_ens


#BUCLE QUE HACE PBARRA
for (i in 1:nrow(datosCombinados)) {
  for (modeloP in modelosP) {
    for (feature in features) {
      # Restablecer numerador y denominador para cada combinación de modeloP y feature
      numerador <- 0
      denominador <- 0
      for (modeloC in modelosC) {
        
        #en vez de predicted column hay que poner el mape de verdad
        predicted_column <- paste("Predicted", modeloC, feature, modeloP, sep = "_")
        pred_median_column <- paste(modeloC, "_pred", sep = "")
        
        predicted_value <- datosCombinados[i, ..predicted_column, with = FALSE][[1]]
        pred_median_value <- datosCombinados[i, ..pred_median_column, with = FALSE][[1]]
        
        # Sumar al denominador, ignorando NA
        if (!is.na(predicted_value)) {
          denominador <- denominador + predicted_value
        }
        
        # Sumar al numerador, solo si ambos valores no son NA
        if (!is.na(predicted_value) && !is.na(pred_median_value)) {
          numerador <- numerador + (predicted_value * pred_median_value)
        }
      }
      
      pBarra_name <- paste("PBarra", modeloP, feature, sep = "_")
      
      # Asignar el valor calculado de pBarra en pb
      if(denominador != 0) {
        pb[i, pBarra_name] <- numerador / denominador
      } else {
        pb[i, pBarra_name] <- NA  # Asignar NA si el denominador es cero
      }
    }
  }
}


for (i in 1:nrow(datosCombinados)) {
      # Restablecer numerador y denominador para cada combinación de modeloP y feature
      numerador <- 0
      denominador <- 0
      for (modeloC in modelosC) {
        
        #en vez de predicted column hay que poner el mape de verdad
        predicted_column <- paste(modeloC, "_mape", sep = "")
        pred_median_column <- paste(modeloC, "_pred", sep = "")
        
        predicted_value <- datosCombinados[i, ..predicted_column, with = FALSE][[1]]
        pred_median_value <- datosCombinados[i, ..pred_median_column, with = FALSE][[1]]
        
        # Sumar al denominador, ignorando NA
        if (!is.na(predicted_value)) {
          denominador <- denominador + predicted_value
        }
        
        # Sumar al numerador, solo si ambos valores no son NA
        if (!is.na(predicted_value) && !is.na(pred_median_value)) {
          numerador <- numerador + (predicted_value * pred_median_value)
        }
      }
      
      pBarra_name <- paste("PBarra_errorMape")
      
      # Asignar el valor calculado de pBarra en pb
      if(denominador != 0) {
        pb[i, pBarra_name] <- numerador / denominador
      } else {
        pb[i, pBarra_name] <- NA  # Asignar NA si el denominador es cero
      }
}


fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarras.csv")



#AÑADIR ENSEMBLE
pBarra_df <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarras.csv")

# Calculamos PBarra_Ensemble para cada conjunto de features
features <- c("tarifa")
modelos <- c("lm", "rf", "gbm", "nn", "svm")

for(feature in features){
  columnas <- paste0("PBarra_", modelos, "_", feature) # Nombres de las columnas actuales para la feature
  pBarra_df[, paste0("PBarra_Ensemble_", feature) := rowMeans(.SD, na.rm = TRUE), .SDcols = columnas] # Calcula la media y añade la nueva columna
}

fwrite(pBarra_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarras.csv", row.names = FALSE)


#CALCULAR MAPE
pBarra_df <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarras.csv")

calculate_mape <- function(actual, predicted) {
  error <- abs((actual - predicted) / actual) * 100
  ifelse(is.nan(error) | is.infinite(error), 0, error)  # Manejo de divisiones por cero
}

pBarra_df <- as.data.frame(pBarra_df)
pBarra_df <- pBarra_df %>%
  mutate(real_mediana = rowMedians(as.matrix(select(., starts_with("Real_"))), na.rm = TRUE))

model_names <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
pbarra_columns <- grep("PBarra", names(pBarra_df), value=TRUE)
for (col in pbarra_columns) {
    pBarra_df[paste0(col,"_MAPE")] <- calculate_mape(pBarra_df$Real, pBarra_df[[col]])
}

pBarra_df <- as.data.table(pBarra_df)

fwrite(pBarra_df, 'NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv', row.names = FALSE)

pBarrasMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv")

summary(pBarrasMAPE)

