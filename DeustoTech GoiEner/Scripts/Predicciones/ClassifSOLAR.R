library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "purrr", "matrixStats","glmnet", "recipes", "pROC") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


feats_con_auto <- fread("SOLAR/features_con_autoconsumo_ConPV.CSV") #982 330
feats_trampa <- fread("SOLAR/features_sin_autoconsumo_Trampa.csv") #1451  312
hasPV_data <- fread("SOLAR/Variation/HasPV.csv") #2434    4
features <- fread("SOLAR/features.csv") #97028    22

feats_totales <- rbind(feats_con_auto, feats_trampa, fill = T) #2433  330

feats_totales <- merge(feats_totales, features, by = "ID")

data_classif <- merge(feats_totales, hasPV_data, by = "ID") #2433  333

data_classif <- data_classif %>% select(-INSTALLATION_TIMESTAMP, -FEC_BAJA_PUN_SUM, -TIP_CONTRATO, -TIP_CUALIFICACION, -FirstInjection)


categorical_columns <- c("COD_CONTRATO", "COD_PS", 
                         "TIP_SUMINISTRO", "FEC_ALTA_PUN_SUM",  
                         "COD_CLIENTE", "FEC_ALTA_CONTRATO", 
                         "COD_CNAE", "FEC_ENG_POLIZA", "FEC_DGCHE_POLIZA", 
                         "COD_TARIF_IBDLA", "TIP_EST_POLIZA",
                         "InstallationDate")


for (col in categorical_columns) {
  data_classif[[col]] <- as.numeric(as.factor(data_classif[[col]]))
}

# Reemplazar infinitos con NA en columnas numéricas
data_classif <- data_classif %>% 
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), NA)))

# Imputar valores faltantes en columnas numéricas utilizando la mediana
data_classif_imputed <- data_classif %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)))

# Convertir la variable objetivo 'hasPV' a factor
data_classif_imputed$hasPV <- as.factor(data_classif_imputed$hasPV)

# Eliminar columnas que contienen 'AE' en el nombre
data_classif_imputed <- data_classif_imputed %>%
  select(-contains("AE"))

colnames(data_classif_imputed) <- gsub("-", "_", colnames(data_classif_imputed))

# Separar los datos en conjunto de entrenamiento y prueba
set.seed(123) # para reproducibilidad
train_index <- sample(1:nrow(data_classif_imputed), 0.7 * nrow(data_classif_imputed))
train_data <- data_classif_imputed[train_index, ]
test_data <- data_classif_imputed[-train_index, ]


#Random Forest
rf_model <- randomForest(hasPV ~ ., data = train_data, treesize = 500)

# SVM
svm_model <- svm(as.factor(hasPV) ~ ., data = train_data, kernel = "radial")

# Regresión logística
glm_model <- glm(as.factor(hasPV) ~ ., data = train_data, family = "binomial")

evaluate_model <- function(model, test_data, predictors, target) {
  predictions <- predict(model, newdata = test_data)
  confusion_matrix <- confusionMatrix(predictions, test_data[[target]])
  auc <- roc(test_data[[target]], as.numeric(predictions))$auc
  return(list(confusion_matrix = confusion_matrix, auc = auc))
}


predictors <- setdiff(names(data_classif), "hasPV")
target <- "hasPV"

# Evaluación de modelos
rf_evaluation <- evaluate_model(rf_model, test_data, predictors, target)
svm_evaluation <- evaluate_model(svm_model, test_data, predictors, target)
glm_evaluation <- evaluate_model(glm_model, test_data, predictors, target)

# Métricas de evaluación
print("Random Forest:")
print(rf_evaluation$confusion_matrix)
print(paste("AUC:", rf_evaluation$auc))

print("SVM:")
print(svm_evaluation$confusion_matrix)
print(paste("AUC:", svm_evaluation$auc))

print("Logistic Regression:")
print(glm_evaluation$confusion_matrix)
print(paste("AUC:", glm_evaluation$auc))