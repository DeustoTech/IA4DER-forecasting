# Script para predecir el error que comete cada modelo 
# a partir de las features 

library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "mice", "mltools") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

#EJECUTAR
combined_data <- fread("errors.csv")
metadataNew <- fread("metadata.csv")
metadataNew$id <- metadataNew$user
metadataNew <- metadataNew %>% select(-user)
metadataNew <- metadataNew %>%
  mutate(across(
    .cols = starts_with("p") & matches("^p[0-9]$"), # seleccionar solo las columnas que comienzan con "p" y tienen un único número
    .fns = ~ifelse(is.na(.), 0, .) # reemplazar los valores NA por 0
  ))


datos <- merge(combined_data, metadataNew, by = "id")
datos$cp.provincia <- substr(datos$zip_code, 1, 2)
datos$cnae.provincia <- substr(datos$cnae, 1, 1)
categoricas <- c("cnae", "zip_code", "contracted_tariff", "self_consumption_type", "province", "municipality", "cp.provincia", "cnae.provincia")

for (col in colnames(datos)){
  if (col %in% categoricas){
    datos[[col]] <- as.factor(datos[[col]])
  }
}

tarifa <- c("cnae.provincia", "cp.provincia","p1", "p2","p3","p4","p5","p6","contracted_tariff")

limpiarColumnas <- function(trainIndex, colsDesc, target, dataset) {
  
  
  
  cleanSet <- dataset %>% select(all_of(colsDesc), !!sym(target), id)
  
  trainSet <- cleanSet[0, ]
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
  cleanSet2 <- anti_join(cleanSet, trainSet) # filas que todavia no hemos añadido al trainset
  clean_nrow <- nrow(cleanSet2)
  trainIndexClean <- sample(1:clean_nrow, index * clean_nrow)
  
  trainSet2 <- cleanSet2[trainIndexClean, ]
  trainSet <- bind_rows(trainSet, trainSet2)
  testSet <- cleanSet2[-trainIndexClean, ]
  
  for (col in colsDesc){
    if (col %in% categoricas){
      if (length(levels(trainSet[[col]])) < 2 || length(levels(testSet[[col]])) <=  2){
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
  for (set in columns) {
    col_name <- columns_names[name_i]
    print(paste("Procesando columnas del set", col_name))
    datos <- as.data.frame(datos)
    datos <- datos %>% select(all_of(c(set, "id", target_variable)))
    #datos <- datos[, c(set, "id", target_variable), drop = FALSE]
    datos <- datos[which(!is.na(datos[[target_variable]])), ] %>% as.data.frame()
    sets_limpios <- limpiarColumnas(trainIndex, set, target_variable, datos)
    testID <- sets_limpios$testSet %>% select(id)
    
    trainSet <- sets_limpios$trainSet %>% select(-id)
    testSet <- sets_limpios$testSet %>% select(-id)
    
    # trainSet <- datos[trainIndex, ] %>% select(-ID)
    log_variable <- paste("log", target_variable, sep = "_")
    trainSet[[log_variable]] <- log(trainSet[[target_variable]] + 1)
    
    # testSet <- datos[-trainIndex, ] %>% select(-ID)
    testSet[[log_variable]] <- log(testSet[[target_variable]] + 1)
    
    print(paste("Trainset: ", nrow(trainSet), "filas. Testset: ", nrow(testSet), "filas."))
    
    
    if (model_type == "lm") {
      # Regresión Lineal
      model <- lm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet, na.action = na.roughfix)
      predicciones_log <- exp(predict(model, newdata = testSet)) - 1
      
    } else if (model_type == "rf") {
      # Random Forest
      # print(names(trainSet))
      model <- randomForest(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet, na.action = na.roughfix)
      predicciones_log <- exp(predict(model, newdata = testSet)) - 1
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
      
    }
    
    nameReal <- paste("Real", modelo, sep = "_" )
    namePred <- paste("Predicted", modelo, col_name, model_type, sep = "_")
    nameMAPE <- paste("MAPE", modelo, col_name, model_type, sep = "_")
    
    
    mape_values <- c()
    
    for (i in 1:nrow(testSet)) {
      mape_values[i] <- mape(testSet[i, target_variable], predicciones_log[i]) * 100
    }
    
    print(length(testID$id))
    print(length(predicciones_log))
    print(length(testSet[[target_variable]]))
    print(length(mape_values))
    resultados <- data.frame(ID = testID$id[1:length(predicciones_log)], Real = testSet[[target_variable]][1:length(predicciones_log)], 
                             Predicciones = predicciones_log, MAPE = mape_values[1:length(predicciones_log)])
    
    colnames(resultados) <- c("ID", nameReal, namePred, nameMAPE)
    
    
    # Escribir el dataframe en un archivo CSV

    write.csv(resultados, file = paste("Resultados/PrediccionErrorNew/", "PredError_", modelo, "_", model_type, "_", col_name, ".csv", sep = ""), row.names = FALSE)
    
    
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
target <- c("mean_error", "rw_error", "naive_error", "simple_error",
            "lr_error", "ann_error", "svm_error", "arima_error", "ses_error", "ens_error")



for (modelo in model_names) {
  for (variable in target) {
    regresion_model_feats(modelo, variable, trainIndex)
  }
}
