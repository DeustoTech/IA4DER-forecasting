# FFORMA SCRIPT: PREDICT ERROR OF EACH MODEL GIVEN A SET OF FEATURES

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

#EJECUTAR
combined_data <- fread("PFG/NUEVOS DATOS/combined_data.csv")
metadataNew <- fread("PFG/NUEVOS DATOS/metadata.csv")

#para que el id se llame igual en ambos datasets
metadataNew$id <- metadataNew$user
metadataNew <- metadataNew %>% select(-user)
metadataNew <- metadataNew %>%
  mutate(across(
    .cols = starts_with("p") & matches("^p[0-9]$"), # seleccionar solo las columnas que comienzan con "p" y tienen un único número
    .fns = ~ifelse(is.na(.), 0, .) # reemplazar los valores NA por 0
  ))


#combinar datos y quitar NA
datos <- merge(combined_data, metadataNew, by = "id")
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
  
  cleanSet <- dataset %>% select(all_of(colsDesc), !!sym(target), id)
  
  trainSet <- cleanSet[0, ]
  
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
  trainIndexClean <- sample(1:clean_nrow, index * clean_nrow)
  
  trainSet2 <- cleanSet2[trainIndexClean, ]
  trainSet <- bind_rows(trainSet, trainSet2)
  testSet <- cleanSet2[-trainIndexClean, ]
  
  #asegurarnos que cada columna tiene al menos 3 niveles
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




# Función para realizar regresión y generar resultados
regresion_model_feats <- function(model_type, target_variable, trainIndex) {
  
  predicciones_log <- c()
  
  modelo <- gsub("_.*$", "",target_variable)
  
  columns <- list(tarifa) 
  columns_names <- c("tarifa")
  results_list <- list()
  name_i = 1
  
  for (set in columns) { #bucle por las columnas del grupo de tarifa
    
    col_name <- columns_names[name_i] #col_name es el grupo tarifa
    print(paste("Procesando columnas del set", col_name))
    
    datos <- as.data.frame(datos)
    datos <- datos %>% select(all_of(c(set, "id", target_variable))) #seleccionar id, target y columnas de tarifa
    
    #arreglos varios
    datos <- datos[datos$contracted_tariff != "6.2TD", ]
    datos$cp.provincia <- as.integer(datos$cp.provincia)
    #datos <- datos[, c(set, "id", target_variable), drop = FALSE]
    datos <- datos[which(!is.na(datos[[target_variable]])), ] %>% as.data.frame()
    
    #usar funcion limpiar columnas para generar trainset y testset limpios
    sets_limpios <- limpiarColumnas(trainIndex, set, target_variable, datos)
    
    testID <- sets_limpios$testSet %>% select(id) #contiene todos los ids del testset
    
    #quitar el id de los sets
    trainSet <- sets_limpios$trainSet %>% select(-id)
    testSet <- sets_limpios$testSet %>% select(-id)
    
    #transformar target variable a logaritmica
    log_variable <- paste("log", target_variable, sep = "_")
    trainSet[[log_variable]] <- log(trainSet[[target_variable]] + 1)
    
    testSet[[log_variable]] <- log(testSet[[target_variable]] + 1)
    
    #verificacion
    print(paste("Trainset: ", nrow(trainSet), "filas. Testset: ", nrow(testSet), "filas."))
    
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
      # SVM
      model <- tune(e1071::svm, as.formula(paste(log_variable, "~ . - ", target_variable)),
                    data = trainSet, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
      
      for (t in 1:nrow(testSet)) {
        testRow <- testSet[t, , drop = FALSE]  
        prediccion <- exp(predict(model$best.model, newdata = testRow)) - 1
        
        if (length(prediccion) == 0) {
          predicciones_log[t] <- NA
        } else {
          predicciones_log[t] <- prediccion
        }
      }
      predicciones_log <- na.approx(predicciones_log, na.rm = FALSE, rule = 2)
      
    } else if (model_type == "nn"){
      
      # Neural Network
      model <- nnet(
        as.formula(paste(log_variable, "~ . - ", target_variable)),
        data = trainSet,
        size = 5
      )
      for (t in 1:nrow(testSet)){
        testRow <- testSet[t, , drop = FALSE]  
        predicciones_log[t] <- exp(predict(model, newdata = testRow)) - 1
        
      }
      predicciones_log <- na.approx(predicciones_log, na.rm = FALSE, rule = 2)
    }
    
    nameReal <- paste("Real", modelo, sep = "_" )
    namePred <- paste("Predicted", modelo, col_name, model_type, sep = "_")
    nameMAPE <- paste("MAPE", modelo, col_name, model_type, sep = "_")
    
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
    
    write.csv(resultados, file = paste("PFG/Resultados/PrediccionError/", "PredError_", modelo, "_", model_type, "_", col_name, ".csv", sep = ""), row.names = FALSE)
    
    
    name_i = name_i + 1
    
    
    
  }
  
  return(resultados)
}

set.seed(0)
index <- 0.75
feats_nrow <- nrow(datos)
trainIndex <- sample(1:feats_nrow, index * feats_nrow)

modelos <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
model_names <- c("lm", "rf", "gbm", "svm", "nn")
model_names <- c("lm")
target <- c("mean_error", "rw_error", "naive_error", "simple_error",
            "lr_error", "ann_error", "svm_error", "arima_error", "ses_error", "ens_error")



for (modelo in model_names) {
  for (variable in target) {
    regresion_model_feats(modelo, variable, trainIndex)
  }
}


