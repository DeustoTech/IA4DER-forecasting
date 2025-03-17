
library(foreach)
library(doParallel)
# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mltools", "zoo", "mlr3", "mlr3tuning", "paradox", "mlr3learners",
               "stringr", "parallel") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

metadataNew <- fread("NUEVOS DATOS/DATOS ERROR CRUZ/allMetadata.csv")

datos <- metadataNew
datos$cp.provincia <- substr(datos$zip_code, 1, 2) #reemplazar el zip code por el codigo por provincia
datos$cnae.provincia <- substr(datos$cnae, 1, 1) #reemplazar el cnae por el cnae por provincia
datos <- datos %>% select(- contract_end_date)


num_cols <- names(datos)[sapply(datos, is.numeric)]
datos[, (num_cols) := lapply(.SD, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)), .SDcols = num_cols]

char_cols <- names(datos)[sapply(datos, is.character)]
datos[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]


categoricas <- c( "contracted_tariff", "self_consumption_type", "province", "municipality")

for (col in colnames(datos)){
  if (col %in% categoricas){
    datos[[col]] <- as.factor(datos[[col]])
  }
}


#FUNCION LIMPIAR COLUMNAS que genera el trainset y el dataset
limpiarColumnas <- function(trainIndex, colsDesc, target, dataset) {
  
  #seleccionar del dataset todas las columnas descriptivas (tarifa)
  cleanSet <- dataset %>% select(all_of(colsDesc), !!sym(target), id)
  
  #añade al trainset todas las columnas seleccionadas anteriormente
  trainSet <- cleanSet[0, ]
  
  #asegurarse de que el trainset tiene obseervaciones de cada nivel de cada variable categorica
  #añadir una fila de cada nivel al trainset
  for (col in colsDesc){
    if (col %in% categoricas){
      niveles <- unique(cleanSet[[col]])
      for (nivel in niveles) {
        observacion <- cleanSet %>% filter(cleanSet[[col]] == nivel) %>% slice(1)
        trainSet <- bind_rows(trainSet, observacion)
      }
      print(paste("TrainSet tiene todos los niveles de", col))
    }
  }
  
  #con la filas restantes, añadirlas al train y al test
  cleanSet2 <- anti_join(cleanSet, trainSet) # filas que todavia no hemos añadido al trainset
  clean_nrow <- nrow(cleanSet2)
  set.seed(0)
  trainIndexClean <- sample(1:clean_nrow, index * clean_nrow)
  
  trainSet2 <- cleanSet2[trainIndexClean, ]
  trainSet <- bind_rows(trainSet, trainSet2)
  testSet <- dataset #esto es porque el testset habia que usarlo entero (preguntar!!) cleanSet2[-trainIndexClean, ] 
  
  #asegurarnos que cada columna tiene al menos 3 niveles
  #se eliminan las columnas categoricas que tienen 2 o menos niveles
  for (col in colsDesc){
    if (col %in% categoricas){
      if (length(levels(trainSet[[col]])) <= 2 || length(levels(testSet[[col]])) <=  2){
        cat(paste("Eliminando la columna", col, "debido a dos o menos niveles.\n"))
        trainSet[[col]] <- NULL
        testSet[[col]] <- NULL
      } else{
        # print(summary(testSet))
      }
    }
    
  }
  
  return(list(trainSet = trainSet, testSet = testSet))
}


set.seed(0)
index <- 0.7
feats_nrow <- nrow(datos)
trainIndex <- sample(1:feats_nrow, index * feats_nrow)

modelos <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
target <- c("mean_error", "rw_error", "naive_error", "simple_error",
            "lr_error", "ann_error", "svm_error", "arima_error", "ses_error", "ens_error")

### NORMAL
for (variable in target) {
  modeloE <- gsub("_.*$", "",variable)
  
  datosN <- as.data.frame(datos)
  datosN <- datosN %>% select(all_of(c(tarifa, "id", variable))) #seleccionar id, target y columnas de tarifa
  
  print(names(datosN))
  #arreglos varios
  datosN <- datosN[datosN$contracted_tariff != "6.2TD", ]
  datosN$cp.provincia <- as.integer(datosN$cp.provincia)
  #datos <- datos[, c(set, "id", target_variable), drop = FALSE]
  datosN <- datosN[which(!is.na(datosN[[variable]])), ] %>% as.data.frame()
  
  #usar funcion limpiar columnas para generar trainset y testset limpios
  sets_limpios <- limpiarColumnas(trainIndex, tarifa, variable, datosN)
  
  testID <- sets_limpios$testSet %>% select(id)
  #quitar el id de los sets
  trainSet <- sets_limpios$trainSet %>% select(-id)
  testSet <- sets_limpios$testSet %>% select(-id)
  
  #verificacion
  print(paste("Trainset: ", nrow(trainSet), "filas. Testset: ", nrow(testSet), "filas."))

  modelo <- c("svm")
  
  log_variable <- paste("log", variable, sep = "_")
  trainSet[[log_variable]] <- log(trainSet[[variable]] + 1) 
  testSet[[log_variable]] <- log(testSet[[variable]] + 1)
  
  model <- e1071::tune(e1071::svm, as.formula(paste(log_variable, "~ . - ", variable)),
                       data = trainSet, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
  print(paste("Mejores hiperparametros para el modelo", modeloE))
  print(model$best.model)
  predicciones_log <- exp(predict(model$best.model, newdata = testSet )) - 1
  predicciones_log <- na.approx(predicciones_log, na.rm = FALSE, rule = 2)
  
  
  nameReal <- paste("Real", modeloE, sep = "_" )
  namePred <- paste("Predicted", modeloE, "tarifa", modelo, sep = "_")
  nameMAPE <- paste("MAPE", modeloE, "tarifa", modelo, sep = "_")
  
  mape_values <- c()
  
  for (i in 1:nrow(testSet)) {
    mape_values[i] <- mape(testSet[i, variable], predicciones_log[i]) * 100
  }
  
  resultados <- data.frame(ID = testID$id[1:length(predicciones_log)], Real = testSet[[variable]][1:length(predicciones_log)], 
                           Predicciones = predicciones_log, MAPE = mape_values[1:length(predicciones_log)])
  
  colnames(resultados) <- c("ID", nameReal, namePred, nameMAPE)
  
  
  # Escribir el dataframe en un archivo CSV
  
  write.csv(resultados, file = paste("NuevosResultados/PrediccionError/", "PredError_", modeloE, "_", modelo, "_", "tarifa", ".csv", sep = ""), row.names = FALSE)
  
  
  return(resultados)
}



# Variables objetivo y descriptivas
target <- c("mean_error", "rw_error", "naive_error", "simple_error",
            "lr_error", "ann_error", "svm_error", "arima_error", "ses_error", "ens_error")
tarifa <- c("contracted_tariff", "self_consumption_type", "province", "municipality", "cp.provincia", "cnae.provincia")

# ✅ Función para limpiar columnas
limpiarColumnas <- function(dataset, colsDesc, target) {
  cleanSet <- dataset %>% select(all_of(colsDesc), all_of(target), id)
  trainSet <- cleanSet[0, ]
  
  # Asegurar que el trainSet tenga al menos una observación de cada nivel en variables categóricas
  for (col in colsDesc) {
    if (col %in% categoricas) {
      niveles <- unique(cleanSet[[col]])
      for (nivel in niveles) {
        observacion <- cleanSet %>% filter(cleanSet[[col]] == nivel) %>% slice(1)
        trainSet <- bind_rows(trainSet, observacion)
      }
    }
  }
  
  cleanSet2 <- anti_join(cleanSet, trainSet) 
  set.seed(0)
  trainIndexClean <- sample(1:nrow(cleanSet2), 0.7 * nrow(cleanSet2))
  trainSet2 <- cleanSet2[trainIndexClean, ]
  trainSet <- bind_rows(trainSet, trainSet2)
  testSet <- cleanSet2[-trainIndexClean, ]
  
  # Eliminar columnas categóricas con ≤2 niveles
  for (col in colsDesc) {
    if (col %in% categoricas) {
      if (length(levels(trainSet[[col]])) <= 2 || length(levels(testSet[[col]])) <= 2) {
        cat(paste("Eliminando columna", col, "por tener 2 o menos niveles.\n"))
        trainSet[[col]] <- NULL
        testSet[[col]] <- NULL
      }
    }
  }
  
  return(list(trainSet = trainSet, testSet = testSet))
}

# ✅ Aplicar limpieza antes de dividir en train/val/test
sets_limpios <- limpiarColumnas(datos, tarifa, target)
datos <- sets_limpios$trainSet  # Usamos el train limpio para la siguiente división

# ✅ División en entrenamiento (80%), validación (10%) y test (10%)
set.seed(0)
n <- nrow(datos)
index_train <- sample(1:n, 0.8 * n)
index_val <- sample(setdiff(1:n, index_train), 0.1 * n)
index_test <- setdiff(1:n, c(index_train, index_val))

train_data <- datos[index_train, ]
val_data <- datos[index_val, ]
test_data <- datos[index_test, ]

# ✅ Crear versiones logarítmicas de las variables objetivo
for (variable in target) {
  train_data[[paste0("log_", variable)]] <- log(train_data[[variable]] + 1)
  val_data[[paste0("log_", variable)]] <- log(val_data[[variable]] + 1)
  test_data[[paste0("log_", variable)]] <- log(test_data[[variable]] + 1)
}

# ✅ 1. Tuneo de hiperparámetros con conjunto de validación
formula <- as.formula(paste(paste0("log_", target), collapse = " + ", "~ . - id -", paste(target, collapse = " - ")))

svm_tune <- e1071::tune(
  e1071::svm, 
  log_mean_error ~ . - id - mean_error - rw_error - naive_error - simple_error - 
    lr_error - ann_error - svm_error - arima_error - ses_error - ens_error,
  data = val_data,
  ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4))
)

best_model <- svm_tune$best.model
print("Mejores hiperparámetros encontrados:")
print(svm_tune$best.parameters)

# ✅ 2. Entrenamiento final con el conjunto de entrenamiento completo
final_model <- svm(formula, data = train_data, cost = svm_tune$best.parameters$cost, gamma = svm_tune$best.parameters$gamma)

# ✅ 3. Predicción para cada variable objetivo
resultados <- test_data[, .(id)]

for (variable in target) {
  log_variable <- paste0("log_", variable)
  pred_log <- exp(predict(final_model, newdata = test_data)) - 1
  pred_log <- na.approx(pred_log, na.rm = FALSE, rule = 2)
  
  nameReal <- paste0("Real_", variable)
  namePred <- paste0("Predicted_", variable, "_svm")
  nameMAPE <- paste0("MAPE_", variable, "_svm")
  
  mape_values <- mape(test_data[[variable]], pred_log) * 100
  
  resultados[[nameReal]] <- test_data[[variable]]
  resultados[[namePred]] <- pred_log
  resultados[[nameMAPE]] <- mape_values
}

# ✅ Guardar resultados
write.csv(resultados, file = "NuevosResultados/PrediccionError/PredError_SVM.csv", row.names = FALSE)
