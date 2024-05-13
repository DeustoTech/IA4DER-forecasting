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
                                        -COD_CONTRATO, -ASS, -starts_with("FEC"), -SHARP, -ZERO, -COD_SOCIEDAD)

categorical_columns <- c("COD_POLIZA", "TIP_SUMINISTRO", "TARIF",
                         "CNAE", "COD_TARIF_IBDLA", "TIP_EST_POLIZA", "TIP_PUNTO_MEDIDA", "SUM")


for (col in categorical_columns) {
  data_classif[[col]] <- ifelse(is.na(data_classif[[col]]) | data_classif[[col]] == "", -1, as.factor(data_classif[[col]]))
  data_classif[[col]] <- (as.factor(data_classif[[col]]))
}


data_classif <- data_classif %>% 
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), NA)))

data_classif_imputed <- data_classif %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)))

data_classif_imputed$POT_AUT <- as.numeric(data_classif_imputed$POT_AUT)

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

