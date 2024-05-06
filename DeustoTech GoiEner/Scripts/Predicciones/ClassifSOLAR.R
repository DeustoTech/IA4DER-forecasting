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
hasPV_data <- fread("SOLAR/Variation/HasPV.csv") #2434    4

feats_totales <- rbind(feats_con_auto, feats_trampa, fill = T) #2433  330

data_classif <- merge(feats_totales, hasPV_data, by = "ID") #2433  333

data_classif <- data_classif %>% select(-INSTALLATION_TIMESTAMP, -FEC_BAJA_PUN_SUM, -TIP_CONTRATO, -TIP_CUALIFICACION)


categorical_columns <- c("COD_CONTRATO", "COD_PS", 
                         "TIP_SUMINISTRO", "FEC_ALTA_PUN_SUM",  
                         "COD_CLIENTE", "FEC_ALTA_CONTRATO", 
                         "COD_CNAE", "FEC_ENG_POLIZA", "FEC_DGCHE_POLIZA", 
                         "COD_TARIF_IBDLA", "TIP_EST_POLIZA",
                         "InstallationDate", "FirstInjection")


for (col in categorical_columns) {
  data_classif[[col]] <- as.numeric(as.factor(data_classif[[col]]))
}

data_classif_imputed <- data_classif %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

set.seed(123) # para reproducibilidad
train_index <- sample(1:nrow(data_classif_imputed), 0.7*nrow(data_classif_imputed))
train_data <- data_classif_imputed[train_index, ]
test_data <- data_classif_imputed[-train_index, ]



#Random Forest
rf_model <- randomForest(hasPV ~ ., data = train_data, treesize = 500)

# SVM
svm_model <- svm(as.factor(hasPV) ~ ., data = train_data, kernel = "radial")

# Regresión logística
glm_model <- glm(as.factor(hasPV) ~ ., data = train_data, family = "binomial")


