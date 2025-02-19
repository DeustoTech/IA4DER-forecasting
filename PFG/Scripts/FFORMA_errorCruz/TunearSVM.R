
library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'car', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mice", "mltools", "zoo", "mlr3", "mlr3tuning", "paradox", "mlr3learners") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

metadataNew <- fread("PFG/NUEVOS DATOS/DATOS ERROR CRUZ/allMetadata.csv")

# Dividir datos en entrenamiento y prueba
set.seed(123)
index <- sample(1:nrow(metadataNew), 0.7 * nrow(metadataNew))
train_data <- metadataNew[index, ]
test_data  <- metadataNew[-index, ]

# Definir un grid de parámetros
cost_values <- c(0.1, 1, 10, 100)
gamma_values <- c(0.01, 0.1, 1)

best_model <- NULL
best_accuracy <- 0

for (C in cost_values) {
  for (G in gamma_values) {
    model <- svm(Species ~ ., data=train_data, kernel="radial", cost=C, gamma=G)
    pred <- predict(model, test_data)
    accuracy <- mean(pred == test_data$Species)
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model <- model
    }
  }
}

print(best_model)
print(best_accuracy)
