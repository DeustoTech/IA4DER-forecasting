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
folder <- "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO"

# Función para buscar archivos por patrón en una carpeta (sin subcarpetas)
buscar_archivos_por_modelo <- function(folder, pattern) {
  archivos_modelo <- list.files(folder, pattern = pattern, full.names = TRUE)
  return(archivos_modelo)
}

# Buscar archivos por cada modelo directamente en la carpeta base
model_files <- list(
  lm = buscar_archivos_por_modelo(folder, "_lm_.*\\.csv$"),
  rf = buscar_archivos_por_modelo(folder, "_rf_.*\\.csv$"),
  gbm = buscar_archivos_por_modelo(folder, "_gbm_.*\\.csv$"),
  svm = buscar_archivos_por_modelo(folder, "_svm_tarifa.*\\.csv$"),
  nn = buscar_archivos_por_modelo(folder, "_nn_.*\\.csv$")
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



lm_df <- combinar_archivos_en_df(model_files[[1]])
fwrite(lm_df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_lm.csv")
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
feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
combinedPreds <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combinedPreds.csv")
feats$ID <- feats$id
feats <- feats %>% select(-id)
feats <- feats %>% select(ID, dia, hora, real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred, mean_mape, rw_mape, naive_mape, simple_mape, lr_mape, ann_mape, svm_mape, arima_mape, ses_mape, ens_mape)
combinedPreds <- combinedPreds %>% select(ID, starts_with("Real_"), starts_with("Predicted_"), starts_with("MAPE_"))
feats$Real <- feats$real
feats <- feats %>% select(-real)

combinedPreds <- combinedPreds %>% distinct(ID, .keep_all = TRUE)

datosCombinados <- feats %>%
  left_join(combinedPreds, by = "ID")


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


total_iterations <- nrow(pb) * length(modelosP) * length(modelosC)
pb_progress <- txtProgressBar(min = 0, max = total_iterations, style = 3)
progress_counter <- 0

#BUCLE QUE HACE PBARRA
for (i in 1:nrow(datosCombinados)) {
  for (modeloP in modelosP) {
      # Restablecer numerador y denominador para cada combinación de modeloP 
      numerador <- 0
      denominador <- 0
      for (modeloC in modelosC) {
        
        #en vez de predicted column hay que poner el mape de verdad
        predicted_column <- paste("Predicted", modeloC, "tarifa", modeloP, sep = "_") #la prediccion del error
        pred_median_column <- paste(modeloC, "_pred", sep = "") #la prediccion del consumo
        
        predicted_value <- datosCombinados[i, ..predicted_column, with = FALSE][[1]]
        pred_median_value <- datosCombinados[i, ..pred_median_column, with = FALSE][[1]]
        
        # Sumar al denominador, ignorando NA
        if (!is.na(predicted_value)) {
          denominador <- denominador + (predicted_value ^ -1)
        }
        
        # Sumar al numerador, solo si ambos valores no son NA
        if (!is.na(predicted_value) && !is.na(pred_median_value)) {
          numerador <- numerador + ((predicted_value ^ -1) * pred_median_value)
        }
      }
      
      pBarra_name <- paste("PBarra", modeloP, "tarifa", sep = "_")
      
      # Asignar el valor calculado de pBarra en pb
      if(denominador != 0) {
        pb[i, pBarra_name] <- numerador / denominador
      } else {
        pb[i, pBarra_name] <- NA  # Asignar NA si el denominador es cero
      }
  }
  if (i %% 1000 == 0 || i == nrow(pb)) {
    fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarras.csv")
  }
  
  # Actualizar la barra de progreso
  progress_counter <- progress_counter + length(modelosP) * length(features)
  setTxtProgressBar(pb_progress, progress_counter)
}

close(pb_progress)

fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarras.csv")

ultima_fila <- max(which(is.na(pb$PBarra_lm_tarifa)))
total_iterations <- nrow(pb) * length(modelosP) * length(features)
pb_progress <- txtProgressBar(min = 0, max = total_iterations, style = 3)
progress_counter <- 0

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
  
  progress_counter <- progress_counter + length(modelosP) * length(features)
  setTxtProgressBar(pb_progress, progress_counter)
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




#################3 intentar nueva forma de ffroma ###############3
setDT(datosCombinados)
pb <- data.table(ID = datosCombinados$ID)

modelosC <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
modelosP <- c("lm", "rf", "gbm", "nn", "svm")

# Precalcular todas las columnas de consumo
pred_cols <- paste0(modelosC, "_pred")

# Ahora hacemos todo el proceso vectorizado por cada modelo P:
for (modeloP in modelosP) {
  
  # Construimos las columnas de MAPE para ese modeloP
  mape_cols <- paste0("MAPE_", modelosC, "_tarifa_", modeloP)
  
  # Extraemos todas las columnas necesarias en una tabla auxiliar
  aux <- datosCombinados[, c(pred_cols, mape_cols), with = FALSE]
  
  # Cambiar nombres para facilitar operaciones
  setnames(aux, pred_cols, paste0("consumo_", modelosC))
  setnames(aux, mape_cols, paste0("mape_", modelosC))
  
  # Calcular los pesos = 1 / mape (para cada columna de modeloC)
  for (mc in modelosC) {
    aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
  }
  
  # Numerador = sumatoria de (consumo * w)
  numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
  
  # Denominador = sumatoria de los pesos
  denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
  
  # Calcular pbarra
  pbarra_name <- paste0("PBarra_", modeloP, "_tarifa")
  pb[, (pbarra_name) := numerador / denominador]
  
  cat("Calculado PBarra para modelo", modeloP, "\n")
}

# Guardar resultados
fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarras.csv")



##############3 otra forma de hacer pbarra #############
feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
combinedPreds <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combinedPreds.csv")
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

modelosC <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
modelosP <- c("lm", "rf", "gbm", "nn", "svm")
pred_cols <- paste0(modelosC, "_pred")

for (modeloP in modelosP) {
  
  mape_cols <- paste0("Predicted_", modelosC, "_tarifa_", modeloP)
  #mape_cols <- paste0(modelosC, "_mape")
  aux <- datosCombinados[, c(pred_cols, mape_cols), with = FALSE]  # Extraemos todas las columnas necesarias en una tabla auxiliar
  
  setnames(aux, pred_cols, paste0("consumo_", modelosC))
  setnames(aux, mape_cols, paste0("mape_", modelosC))
  
  # Calcular los pesos = 1 / mape (para cada columna de modeloC)
  for (mc in modelosC) {
    aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
  }
  
  numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
  denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
  pbarra_name <- paste0("FFORMA_", modeloP, "_tarifa")
  pb[, (pbarra_name) := numerador / denominador]
  
  cat("Calculado FFORMA para modelo", modeloP, "\n")
}

#añadir ensemble
pb_cols <- paste0("FFORMA_", modelosP, "_tarifa")
pb[, FFORMA_Ensemble_tarifa := rowMeans(.SD, na.rm = TRUE), .SDcols = pb_cols]

fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/FFORMA.csv")

#calcular MAPE
pb_cols <- paste0("FFORMA_", modelosP, "_tarifa")
pb_cols <- c(pb_cols, "FFORMA_Ensemble_tarifa")  # También incluimos el ensemble

for (col_name in pb_cols) {
  mape_col <- paste0(col_name, "_MAPE")
  
  pb[, (mape_col) := abs((Real - get(col_name)) / Real) * 100]
}

fwrite(pb, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/FFORMA_MAPE.csv")
