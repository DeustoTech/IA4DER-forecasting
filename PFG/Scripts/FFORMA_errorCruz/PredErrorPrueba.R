
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

library(mlr3)
library(mlr3learners)


datos <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
datos <- datos %>% select("id", "dia", "hora", "real", "mean_pred", "rw_pred", "naive_pred", "simple_pred", "lr_pred",
                "ann_pred", "svm_pred", "arima_pred", "ses_pred", "ens_pred", "mean_mape", "rw_mape", "naive_mape",
                "simple_mape", "lr_mape", "ann_mape", "svm_mape", "arima_mape", "ses_mape", "ens_mape")


predictors <- c("real", "dia", "hora", "mean_pred", "rw_pred", "naive_pred", 
                "simple_pred", "lr_pred", "ann_pred", "svm_pred", 
                "arima_pred", "ses_pred", "ens_pred")
targets <- c("mean_mape", "rw_mape", "naive_mape", 
             "simple_mape", "lr_mape", "ann_mape", "svm_mape", "arima_mape",
             "ses_mape", "ens_mape")

datos <- datos[complete.cases(datos[, ..targets]), ]

# Dividir datos (80% train, 20% test)
set.seed(123)
train_indices <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))

# Dividir datasets (crear subsets, no filtrar tasks)
datos_train <- datos[train_indices, ]
datos_test <- datos[-train_indices, ]

# Crear tasks directamente desde train y test
train_tasks <- lapply(targets, function(target) {
  TaskRegr$new(id = paste0(target, "_train"), backend = datos_train, target = target)
})

test_tasks <- lapply(targets, function(target) {
  TaskRegr$new(id = paste0(target, "_test"), backend = datos_test, target = target)
})

# Comprobar número de filas en cada task de train
for (task in train_tasks) {
  cat("Task", task$id, "tiene", task$nrow, "observaciones\n")
}

# Entrenar modelos
learner <- lrn("regr.ranger")

models <- lapply(train_tasks, function(task) {
  if (task$nrow > 0) {
    learner$train(task)
  } else {
    warning(paste("La tarea", task$id, "no tiene datos suficientes y se omite"))
    NULL
  }
})



# Predecir en el conjunto de test
predictions <- lapply(seq_along(models), function(i) {
  model <- models[[i]]
  test_task <- test_tasks[[i]]
  preds <- model$predict(test_task)$response
  return(preds)
})

# Evaluar (por ejemplo RMSE por cada MAPE)
actuals <- lapply(test_tasks, function(task) task$truth())

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

for (i in seq_along(targets)) {
  cat("RMSE para", targets[i], ":", rmse(actuals[[i]], predictions[[i]]), "\n")
}


# Función para calcular el MAPE
mape <- function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual)) * 100)
}

# Dataframe para guardar resultados
resultados <- data.frame(id = datos_test$id)  # Asegúrate que datos_test tiene el ID original o alguna columna identificadora

# Recorrer cada modelo y hacer predicciones
for (i in seq_along(models)) {
  
  model <- models[[i]]
  test_task <- test_tasks[[i]]
  
  if (!is.null(model)) {
    preds <- model$predict(test_task)$response
    actuals <- test_task$truth()
    
    # Guardar predicciones
    col_pred <- paste0(targets[i], "_pred")
    resultados[[col_pred]] <- preds
    
    # Calcular y guardar el MAPE
    col_mape <- paste0(targets[i], "_mape")
    resultados[[col_mape]] <- abs((actuals - preds) / actuals) * 100
  } else {
    # Si no hay modelo, rellenamos con NA
    col_pred <- paste0(targets[i], "_pred")
    col_mape <- paste0(targets[i], "_mape")
    resultados[[col_pred]] <- NA
    resultados[[col_mape]] <- NA
  }
}
