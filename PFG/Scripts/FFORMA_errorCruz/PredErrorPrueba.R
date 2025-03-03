
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
saveRDS(models, "models_entrenados.rds")
#models <- readRDS("models_entrenados.rds")

# Lista de features excluyendo id y el target de cada task
features <- setdiff(names(datos), c("id", targets))

# Crear tasks asegurando que solo tienen las columnas necesarias (features + target)
train_tasks <- lapply(targets, function(target) {
  TaskRegr$new(
    id = paste0(target, "_train"),
    backend = datos_train[, c(features, target), with = FALSE],
    target = target
  )
})

test_tasks <- lapply(targets, function(target) {
  TaskRegr$new(
    id = paste0(target, "_test"),
    backend = datos_test[, c(features, target), with = FALSE],
    target = target
  )
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






#### VERSION MINI ###

library(mlr3)
library(mlr3learners)
library(data.table)

set.seed(123)  # Reproducibilidad

# Cargar datos
datos <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

# Seleccionar solo columnas relevantes
datos <- datos %>% select("id", "dia", "hora", "real", "mean_pred", "rw_pred", "naive_pred", 
                          "simple_pred", "lr_pred", "ann_pred", "svm_pred", "arima_pred", 
                          "ses_pred", "ens_pred", "mean_mape", "rw_mape", "naive_mape", 
                          "simple_mape", "lr_mape", "ann_mape", "svm_mape", "arima_mape", 
                          "ses_mape", "ens_mape")

# Variables
predictors <- c("real", "dia", "hora", "mean_pred", "rw_pred", "naive_pred", 
                "simple_pred", "lr_pred", "ann_pred", "svm_pred", 
                "arima_pred", "ses_pred", "ens_pred")

targets <- c("mean_mape", "rw_mape", "naive_mape", "simple_mape", "lr_mape", 
             "ann_mape", "svm_mape", "arima_mape", "ses_mape", "ens_mape")

# Quitar filas con NA en las targets
datos <- datos[complete.cases(datos[, ..targets]), ]

# Dividir en train y test (80%-20%)
train_indices <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))
datos_train <- datos[train_indices, ]
datos_test <- datos[-train_indices, ]

# ⚠️ Modo mini (1% de los datos) para pruebas rápidas
datos_train_mini <- datos_train[sample(.N, 0.001 * .N)]  # 1% de train
datos_test_mini <- datos_test[sample(.N, 0.001 * .N)]    # 1% de test

for (target in targets) {
  datos_train_mini <- datos_train_mini[is.finite(datos_train_mini[[target]])]
  datos_train_mini <- datos_train_mini[is.finite(datos_train_mini[[target]])]
}

# Features sin el "id" y sin las targets
features <- setdiff(names(datos), c("id", targets))

# Función para crear tasks
crear_tasks <- function(datos_train, datos_test) {
  train_tasks <- lapply(targets, function(target) {
    TaskRegr$new(
      id = paste0(target, "_train"),
      backend = datos_train[, c(features, target), with = FALSE],
      target = target
    )
  })
  
  test_tasks <- lapply(targets, function(target) {
    TaskRegr$new(
      id = paste0(target, "_test"),
      backend = datos_test[, c(features, target), with = FALSE],
      target = target
    )
  })
  
  return(list(train_tasks = train_tasks, test_tasks = test_tasks))
}

# Crear tasks (modo mini para pruebas)
tasks_mini <- crear_tasks(datos_train_mini, datos_test_mini)

# Comprobación: mismo set de features
comprobar_features <- function(train_tasks, test_tasks) {
  for (i in seq_along(train_tasks)) {
    train_features <- colnames(train_tasks[[i]]$data())
    test_features <- colnames(test_tasks[[i]]$data())
    if (!identical(train_features, test_features)) {
      stop(paste("❌ Features diferentes en", train_tasks[[i]]$id))
    }
  }
  cat("✅ Train y Test tienen las mismas features en todos los targets.\n")
}

# Ejecutar comprobación
comprobar_features(tasks_mini$train_tasks, tasks_mini$test_tasks)

# Definir el learner
models_mini <- lapply(train_tasks, function(task) {
  if (task$nrow > 0) {
    learner <- lrn("regr.ranger")  # Crear un learner nuevo para cada task
    learner$train(task)
    return(learner)  # Devolver el learner entrenado
  } else {
    warning(paste("La tarea", task$id, "no tiene datos suficientes y se omite"))
    NULL
  }
})


# Entrenar modelos (modo mini)
models_mini <- entrenar_modelos(tasks_mini$train_tasks)

# Función para predecir
predecir_modelos <- function(models, test_tasks) {
  predictions <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    if (!is.null(model)) {
      model$predict(test_tasks[[i]])$response
    } else {
      rep(NA, test_tasks[[i]]$nrow)
    }
  })
  return(predictions)
}

# Predecir en el test mini
predictions_mini <- predecir_modelos(models_mini, tasks_mini$test_tasks)

for (i in seq_along(targets)) {
  cat("\nTarget:", targets[i], "\n")
  print(summary(predictions_mini[[i]]))
}


# Función para evaluar RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# Evaluar
actuals_mini <- lapply(tasks_mini$test_tasks, function(task) task$truth())

for (i in seq_along(targets)) {
  cat("Revisando actuals para", targets[i], "\n")
  print(summary(actuals_mini[[i]]))
  if (any(is.infinite(actuals_mini[[i]]))) {
    cat("⚠️ Hay Inf en actuals para", targets[i], "\n")
  }
}

for (i in seq_along(targets)) {
  cat("RMSE mini para", targets[i], ":", rmse(actuals_mini[[i]], predictions_mini[[i]]), "\n")
}

# Si todo sale bien, ejecutas la versión completa

