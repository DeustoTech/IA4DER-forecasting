# FFORMA SCRIPT: PREDICT ERROR OF EACH MODEL GIVEN A SET OF FEATURES

library(foreach)
library(doParallel)
# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'car', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mice", "mltools", "zoo", "mlr3", "mlr3tuning", "paradox", "mlr3learners",
               "stringr") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

#EJECUTAR
#combined_data <- fread("PFG/NUEVOS DATOS/combined_data.csv")
#metadataNew <- fread("PFG/NUEVOS DATOS/metadata.csv")

#para que el id se llame igual en ambos datasets
#metadataNew$id <- metadataNew$user
#metadataNew <- metadataNew %>% select(-user)

metadataNew <- fread("PFG/NUEVOS DATOS/DATOS ERROR CRUZ/allMetadata.csv")
#metadataNew <- metadataNew %>%
#  mutate(across(
#    .cols = starts_with("p") & matches("^p[0-9]$"), # seleccionar solo las columnas que comienzan con "p" y tienen un único número
#    .fns = ~ifelse(is.na(.), 0, .) # reemplazar los valores NA por 0
#  ))


#combinar datos y quitar NA
#datos <- merge(combined_data, metadataNew, by = "id")
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


#coolumnas de tarifa
tarifa <- c("cnae.provincia", "cp.provincia","p1", "p2","p3","p4","p5","p6","contracted_tariff")


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


X <- metadataNew %>% 
  select(-id) %>%                           # Excluir ID
  select(-matches("_error$"))               # Excluir variables objetivo

# Crear y_dict (Variables objetivo)
target_cols <- names(metadataNew)[str_detect(names(metadataNew), "_error$")]  # Seleccionar columnas que terminan en _error

y_dict <- list()
for (target in target_cols) {
  y_dict[[target]] <- metadataNew[[target]]
}

# Hiperparámetros a probar
param_grid <- list(
  gamma = 10^seq(-3, 2, by = 1),
  cost = 10^seq(-4, 4, by = 1)
) 

# Fracción de datos para optimizar (Ejemplo: 10%)
sample_fraction <- 0.10
set.seed(42)  # Fijar semilla para reproducibilidad

# Loop sobre cada variable objetivo en y_dict
for (target_variable in names(y_dict)) {
  
  cat("\n🔍 Optimizando para", target_variable, "...\n")
  
  # Tomar una muestra pequeña (10% de los datos) para acelerar el tuning
  sample_size <- floor(nrow(X) * sample_fraction)
  sample_indices <- sample(seq_len(nrow(X)), size = sample_size)
  X_sample <- X[sample_indices, ]
  y_sample <- y_dict[[target_variable]][sample_indices]
  
  # Crear un dataset de entrenamiento con la muestra
  trainSet_sample <- cbind(X_sample, y_sample)
  colnames(trainSet_sample)[ncol(trainSet_sample)] <- "target_var"
  
  formula_svm <- as.formula("target_var ~ .")
  
  # Ejecutar `tune()` para optimizar SVM con los hiperparámetros definidos
  tuned_model <- e1071::tune(
    e1071::svm,
    formula_svm,
    data = trainSet_sample,
    gamma = param_grid$gamma,
    cost = param_grid$cost
  )
  
  # Guardar los mejores hiperparámetros en el diccionario
  best_params[[target_variable]] <- tuned_model$best.parameters
  
  cat("✅ Mejor configuración para", target_variable, ":", best_params[[target_variable]], "\n")
}

# 📌 Mostrar todos los mejores parámetros encontrados
cat("\n📌 Mejores parámetros por variable objetivo:\n")
print(best_params)





# Función para realizar regresión y generar resultados
regresion_model_feats <- function(model_type, modeloE, target_variable, trainSet, testSet, testID, best_params) {
  
  predicciones_log <- c() #vector vacío para almacenar las predicciones en escala logarítmica.
  #transformar target variable a logaritmica para estabilizar la varianza y mejorar la predicción.
  log_variable <- paste("log", target_variable, sep = "_")
  trainSet[[log_variable]] <- log(trainSet[[target_variable]] + 1) 
  testSet[[log_variable]] <- log(testSet[[target_variable]] + 1)
  
  
    ## PREDICCION DE ERROR ##
    if (model_type == "lm") {
      # Regresión Lineal
      model <- lm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = testSet)) - 1
      predicciones_log <- na.approx(predicciones_log, na.rm = FALSE, rule = 2)
      
    } else if (model_type == "rf") {
      # Random Forest
      model <- randomForest(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet, na.action = na.roughfix)
      predicciones_log <- exp(predict(model, newdata = testSet)) - 1
      predicciones_log <- na.approx(predicciones_log, na.rm = FALSE, rule = 2)
      
    } else if (model_type == "gbm") {
      # Gradient Boosting
      model <- gbm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = testSet, n.trees = 100)) - 1
      
    } else if (model_type == "svm"){
      
      model <- e1071::svm(
        as.formula(paste(log_variable, "~ . - ", target_variable)),
        data = trainSet,
        cost = best_params$cost,
        gamma = best_params$gamma
      )
      
      predicciones_log <- exp(predict(model, newdata = testSet )) - 1
      predicciones_log <- na.approx(predicciones_log, na.rm = FALSE, rule = 2)
      
    } else if (model_type == "nn"){
      
      numeric_cols <- names(trainSet)[sapply(trainSet, is.numeric)]
      numeric_cols <- setdiff(numeric_cols, c(target_variable, log_variable))  # Excluir variables objetivo
    
      train_scaled <- trainSet
      test_scaled <- testSet
      
      train_scaled[, numeric_cols] <- scale(train_scaled[, numeric_cols])
      test_scaled[, numeric_cols] <- scale(test_scaled[, numeric_cols])
      
      
      # Neural Network
      model <- nnet(
        as.formula(paste(log_variable, "~ . - ", target_variable)),
        data = train_scaled, #ponia trainset
        size = 15,
        linout = TRUE,
        maxit = 500
      )
      #for (t in 1:nrow(testSet)){
      #  testRow <- testSet[t, , drop = FALSE]  
      #  predicciones_log[t] <- exp(predict(model, newdata = testRow)) - 1
        
      #}
      predicciones_log <- exp(predict(model, newdata = test_scaled)) - 1
      predicciones_log <- na.approx(predicciones_log, na.rm = FALSE, rule = 2)
    }
    
    nameReal <- paste("Real", modeloE, sep = "_" )
    namePred <- paste("Predicted", modeloE, "tarifa", model_type, sep = "_")
    nameMAPE <- paste("MAPE", modeloE, "tarifa", model_type, sep = "_")
    
    mape_values <- c()
    
    for (i in 1:nrow(testSet)) {
      mape_values[i] <- mape(testSet[i, target_variable], predicciones_log[i]) * 100
    }
    
    #print(length(testID$id))
    #print(length(predicciones_log))
    #print(length(testSet[[target_variable]]))
    #print(length(mape_values))
    resultados <- data.frame(ID = testID$id[1:length(predicciones_log)], Real = testSet[[target_variable]][1:length(predicciones_log)], 
                             Predicciones = predicciones_log, MAPE = mape_values[1:length(predicciones_log)])
    
    colnames(resultados) <- c("ID", nameReal, namePred, nameMAPE)
    
    
    # Escribir el dataframe en un archivo CSV

    write.csv(resultados, file = paste("PFG/NuevosResultados/PrediccionError/", "PredError_", modeloE, "_", model_type, "_", "tarifa", ".csv", sep = ""), row.names = FALSE)
    
  
  return(resultados)
}

set.seed(0)
index <- 0.70
feats_nrow <- nrow(datos)
trainIndex <- sample(1:feats_nrow, index * feats_nrow)

modelos <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
model_names <- c("lm", "rf", "gbm", "svm", "nn")
model_names <- c("svm")
target <- c("mean_error", "rw_error", "naive_error", "simple_error",
            "lr_error", "ann_error", "svm_error", "arima_error", "ses_error", "ens_error")



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
  
  
  for (modelo in model_names) {
      regresion_model_feats(modelo, modeloE, variable, trainSet, testSet, testID, NULL)
  }
}

## PARA SVM

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
  
  print(head(trainSet))
  print(head(testSet))
  sample_fraction <- 0.10
  set.seed(42)
  
  sample_size <- floor(nrow(trainSet) * sample_fraction)
  sample_indices <- sample(seq_len(nrow(trainSet)), size = sample_size)
  X_sample <- trainSet[sample_indices, ]
  y_sample <- testSet[sample_indices, ]
  
  best_params <- list()
  # Hiperparámetros a probar
  param_grid <- list(
    gamma = 10^seq(-3, 2, by = 1),
    cost = 10^seq(-4, 4, by = 1)
  ) 

  colnames(X_sample)[ncol(X_sample)] <- "target_var"
  print(names(X_sample))
  formula_svm <- as.formula("target_var ~ .")
    
    # Ejecutar `tune()` para optimizar SVM con los hiperparámetros definidos
  tuned_model <- e1071::tune(
      e1071::svm,
      formula_svm,
      data = X_sample,
      gamma = param_grid$gamma,
      cost = param_grid$cost
  )
    
   best_params <- tuned_model$best.parameters
  
  #verificacion
  print(paste("Trainset: ", nrow(trainSet), "filas. Testset: ", nrow(testSet), "filas."))
  
  regresion_model_feats("svm", modeloE, variable, trainSet, testSet, testID, best_params)
}

##PRUEBAS DE DATOS IGUALES

simpleLM <- fread("PFG/NuevosResultados/PrediccionError/PredError_simple_lm_tarifa.csv") #3990
arimaRF <- fread("PFG/NuevosResultados/PrediccionError/PredError_arima_rf_tarifa.csv") #3990
arimaLM <- fread("PFG/NuevosResultados/PrediccionError/PredError_arima_lm_tarifa.csv")
simpleGBM <- fread("PFG/NuevosResultados/PrediccionError/PredError_simple_gbm_tarifa.csv")
arimaGBM <- fread("PFG/NuevosResultados/PrediccionError/PredError_arima_gbm_tarifa.csv")

merged_LM_RF <- merge(simpleLM, simpleRF, by = "ID")

### PARA METER ARCHIVOS EN CARPETAS ###

# Definir la ruta base
base_path <- "PFG/Resultados/PrediccionErrorTestEntero"

# Lista de archivos en la carpeta base
files <- list.files(base_path, full.names = TRUE)

# Crear una función para obtener la parte de la carpeta de un archivo
get_folder_name <- function(file_name) {
  parts <- unlist(strsplit(basename(file_name), "_"))
  return(parts[2])
}

# Crear las carpetas y mover los archivos
for (file in files) {
  folder <- get_folder_name(file)
  folder_path <- file.path(base_path, folder)
  
  # Crear carpeta si no existe
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
  
  # Mover archivo a la carpeta
  file.rename(file, file.path(folder_path, basename(file)))
}


naiveLMCruz <- fread("PFG/NuevosResultados/PrediccionError/PredError_naive_lm_tarifa.csv")
naiveLMAne <- fread("PFG/NuevosResultados/PrediccionErrorNuevo/PredError_naive_lm_tarifa.csv")
