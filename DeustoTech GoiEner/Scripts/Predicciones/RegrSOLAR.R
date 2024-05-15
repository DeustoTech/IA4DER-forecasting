library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "purrr", "matrixStats","glmnet", "recipes", "pROC", "rje") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


feats_con_auto <- fread("SOLAR/features_con_autoconsumo_ConPV.CSV") #982 330
feats_trampa <- fread("SOLAR/features_sin_autoconsumo_Trampa.csv") #1451  312
hasPV_data <- fread("SOLAR/Variation/HasPV.csv") #2434    4
features <- fread("SOLAR/features.csv") #97028    22

features <- features %>% filter(ASS == "CUPS" | ASS == "SOLAR") #71021 22

feats_totales <- rbind(feats_con_auto, feats_trampa, fill = T) #2433  330

# Ejecutar estas dos lineas para seleccionar de features (cruz) solo las que no tenemos nosotros
columns_to_select <- setdiff(names(features), names(feats_totales))
features <- features %>% select(ID, columns_to_select)

feats_totales <- merge(feats_totales, features, by = "ID")

data_classif <- merge(feats_totales, hasPV_data, by = "ID") #2433  333

# Quitamos todo lo de inyección, identificadores, fechas, repetidas y variables que no cambian

data_classif <- data_classif %>% select(-INSTALLATION_TIMESTAMP, -FEC_BAJA_PUN_SUM,
                                        -TIP_CONTRATO, -TIP_CUALIFICACION,
                                        -FirstInjection, -InstallationDate, -DIFF_HOURS,
                                        -COD_CNAE, -contains("AE."), -COD_PS, -COD_CLIENTE,
                                        -COD_CONTRATO, -ASS, -starts_with("FEC"), -SHARP, -ZERO, -COD_SOCIEDAD, -TIP_SUMINISTRO)

categorical_columns <- c( "TARIF",
                         "CNAE", "COD_TARIF_IBDLA", "TIP_EST_POLIZA", "TIP_PUNTO_MEDIDA")


for (col in categorical_columns) {
  data_classif[[col]] <- ifelse(is.na(data_classif[[col]]) | data_classif[[col]] == "", -1, as.factor(data_classif[[col]]))
  data_classif[[col]] <- (as.factor(data_classif[[col]]))
}


data_classif <- data_classif %>% 
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), NA)))

data_classif_imputed <- data_classif %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)))

data_classif_imputed$POT_AUT <- as.numeric(data_classif_imputed$POT_AUT)

normalize_min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data_classif_imputed$POT_AUT <- normalize_min_max(data_classif_imputed$POT_AUT)

data_classif_imputed <- data_classif_imputed %>% select(-ID)

ensure_levels_in_train <- function(data, categorical_columns) {
  train_indices <- c()
  for (col in categorical_columns) {
    for (level in unique(data[[col]])) {
      level_indices <- which(data[[col]] == level)
      train_indices <- c(train_indices, sample(level_indices, 1))
    }
  }
  return(unique(train_indices))
}

# Obtener índices para el conjunto de entrenamiento
initial_train_indices <- ensure_levels_in_train(data_classif_imputed, categorical_columns)

# Crear el conjunto de entrenamiento inicial
data_train_initial <- data_classif_imputed[initial_train_indices, ]

# Crear el conjunto de datos restante
remaining_data <- data_classif_imputed[-initial_train_indices, ]

set.seed(123)
remaining_train_size <- floor(0.8 * nrow(data_classif_imputed)) - nrow(data_train_initial)

# Seleccionar el resto de los datos para el conjunto de entrenamiento
remaining_train_indices <- sample(seq_len(nrow(remaining_data)), size = remaining_train_size)

# Combinar las dos partes del conjunto de entrenamiento
data_train <- rbind(data_train_initial, remaining_data[remaining_train_indices, ])

# Crear el conjunto de prueba con los datos restantes
data_test <- remaining_data[-remaining_train_indices, ]

for (col in categorical_columns) {
  data_train[[col]] <- as.factor(data_train[[col]])
  data_test[[col]] <- as.factor(data_test[[col]])
  
  levels(data_test[[col]]) <- levels(data_train[[col]])
}

####


set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(data_classif_imputed$POT_AUT, p = .8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data_classif_imputed[trainIndex, ]
data_test  <- data_classif_imputed[-trainIndex, ]


for (col in categorical_columns) {
  data_train[[col]] <- as.factor(data_train[[col]])
  data_test[[col]] <- as.factor(data_test[[col]])
  
  levels(data_test[[col]]) <- union(levels(data_test[[col]]), levels(data_train[[col]]))
  levels(data_train[[col]]) <- union(levels(data_train[[col]]), levels(data_test[[col]]))
}


calculate_mape <- function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual)) * 100)
}

#linear regression
model_lm <- lm(POT_AUT ~ ., data = data_train)
predictions_lm <- predict(model_lm, newdata = data_test)

#random forest
model_rf <- randomForest(POT_AUT ~ ., data = data_train, ntree = 100)
predictions_rf <- predict(model_rf, newdata = data_test)

#svm
model_svm <- train(POT_AUT ~ ., data = data_train, method = 'svmLinear')
predictions_svm <- predict(model_svm, newdata = data_test)

#CALCULATE MAPES
mape_lm <- calculate_mape(data_test$POT_AUT, predictions_lm)
mape_rf <- calculate_mape(data_test$POT_AUT, predictions_rf)
mape_svm <- calculate_mape(data_test$POT_AUT, predictions_svm)

results <- data.frame(
  Model = c("LR", "RF", "SVM"),
  MAPE = c(mape_lm, mape_rf, mape_svm)
)

fwrite(results, "SOLAR/Regression_SOLAR.csv", row.names = FALSE)





results_file <- "SOLAR/Regression_SOLAR.csv"
if (!file.exists(results_file)) {
  fwrite(data.frame(Model = character(), RMSE = numeric()), results_file, row.names = FALSE)
}

set.seed(123)  
train_control <- trainControl(
  method = "cv",        
  number = 10          
)

metric <- "RMSE"

models <- c("lm", "rf", "svmLinear")

results <- list()

for (model in models) {
  
  set.seed(123)
  model_fit <- caret::train(
    POT_AUT ~ .,  
    data = data_classif_imputed,
    method = model,
    trControl = train_control,
    metric = metric
  )
  
  # Guardar el resultado
  #results[[model]] <- model_fit
  rmse_value <- model_fit$results$RMSE
  
  fwrite(data.frame(Model = model, RMSE = rmse_value), results_file, row.names = FALSE, append = TRUE)
}




errors <- data.frame(
  Model = character(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)


for (model in names(results)) {
  errors <- rbind(errors, data.frame(
    Model = model,
    RMSE = results[[model]]$results$RMSE
  ))
}

