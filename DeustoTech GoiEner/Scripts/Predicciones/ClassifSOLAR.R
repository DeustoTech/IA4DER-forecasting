library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "purrr", "matrixStats","glmnet", "recipes") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


feats_con_auto <- fread("SOLAR/features_con_autoconsumo_ConPV.CSV") #982 330
feats_trampa <- fread("SOLAR/features_sin_autoconsumo_Trampa.csv") #1451  312
hasPV <- fread("SOLAR/Variation/HasPV.csv") #2434    4

feats_totales <- rbind(feats_con_auto, feats_trampa, fill = T) #2433  330

data_classif <- merge(feats_totales, hasPV, by = "ID") #2433  333

indices <- createDataPartition(data_classif$hasPV, p = 0.7, list = FALSE)
trainSet <- data_classif[indices, ]
testSet <- data_classif[-indices, ]

evaluar_modelo <- function(modelo, nombre_modelo, datos_entrenamiento, datos_prueba) {
  modelo <- modelo
  predicciones <- predict(modelo, datos_prueba)
  
  mae <- mean(abs(predicciones - datos_prueba$hasPV))
  mse <- mean((predicciones - datos_prueba$hasPV)^2)
  rmse <- sqrt(mse)
  
  resultados <- data.frame(Modelo = nombre_modelo, MAE = mae, MSE = mse, RMSE = rmse)
  
  return(resultados)
}


resultados_modelos <- data.frame(Modelo = character(), MAE = numeric(), MSE = numeric(), RMSE = numeric())

preproc_values <- preProcess(trainSet, method = "medianImpute")
trainSet <- predict(preproc_values, newdata = trainSet)

#Random Forest
modelo_rf <- randomForest(hasPV ~ ., data = trainSet, ntree = 500)
resultados_modelos <- rbind(resultados_modelos, evaluar_modelo(modelo_rf, "Random Forest", trainSet, testSet))

# Modelo de regresión logística
modelo_logit <- glm(hasPV ~ ., data = trainSet, family = "binomial")
resultados_modelos <- rbind(resultados_modelos, evaluar_modelo(modelo_logit, "Logistic Regression", trainSet, testSet))

# Modelo de máquinas de vectores de soporte
modelo_svm <- svm(hasPV ~ ., data = trainSet)
resultados_modelos <- rbind(resultados_modelos, evaluar_modelo(modelo_svm, "SVM", trainSet, testSet))

# Guardar resultados en un archivo CSV
write.csv(resultados_modelos, "resultados_modelos.csv", row.names = FALSE)

# Ver los resultados
print(resultados_modelos)