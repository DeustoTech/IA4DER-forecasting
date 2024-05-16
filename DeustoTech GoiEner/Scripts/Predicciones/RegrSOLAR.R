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
columns_to_select <- setdiff(names(feats_totales), names(features))
feats_totales <- feats_totales %>% select(ID, columns_to_select)

feats_totales <- merge(feats_totales, features, by = "ID")

data_classif <- merge(feats_totales, hasPV_data, by = "ID") #2433  333

# Quitamos todo lo de inyección, identificadores, fechas, repetidas y variables que no cambian

data_classif <- data_classif %>% select(-INSTALLATION_TIMESTAMP, -FEC_BAJA_PUN_SUM,
                                        -TIP_CONTRATO, -TIP_CUALIFICACION,
                                        -FirstInjection, -InstallationDate, -DIFF_HOURS,
                                        -COD_CNAE, -contains("AE."), -COD_PS, -COD_CLIENTE,
                                        -COD_CONTRATO, -ASS, -starts_with("FEC"), -SHARP, -ZERO, -COD_SOCIEDAD)



categorical_columns <- c( "TIP_SUMINISTRO", "COD_TARIF_IBDLA", "TIP_EST_POLIZA", "CNAE", "TARIF", "SUM", "TIP_PUNTO_MEDIDA")

data_classif$TarifCode <- data_classif$TARIF

for (col in categorical_columns) {
  data_classif[[col]] <- ifelse(is.na(data_classif[[col]]) | data_classif[[col]] == "", -1, as.factor(data_classif[[col]]))
  data_classif[[col]] <- (as.factor(data_classif[[col]]))
}

unique_categories_to_filter <- list(
  TIP_SUMINISTRO = c(3, 4, 6, 14, 17),
  TIP_EST_POLIZA = c(1, 3), 
  CNAE = c(2,4),  
  SUM = c(5, 16, 18, 19, 21, 22, 23)  
)

for (col in names(unique_categories_to_filter)) {
    data_classif <- subset(data_classif, !(get(col) %in% unique_categories_to_filter[[col]]))
}

data_classif$TIP_SUMINISTRO <- droplevels(data_classif$TIP_SUMINISTRO)
data_classif$TIP_EST_POLIZA <- droplevels(data_classif$TIP_EST_POLIZA)
data_classif$CNAE <- droplevels(data_classif$CNAE)
data_classif$SUM <- droplevels(data_classif$SUM)

for (col in colnames(data_classif)) {
  # Reemplazar "-" por "."
  new_col_name <- gsub("-", ".", col)
  
  # Renombrar la columna en el dataset
  names(data_classif)[names(data_classif) == col] <- new_col_name
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

###### hasta aqui fijo si


#
initial_train_indices <- ensure_levels_in_train(data_classif_imputed, categorical_columns)
data_train_initial <- data_classif_imputed[initial_train_indices, ]
remaining_data <- data_classif_imputed[-initial_train_indices, ]

set.seed(123)
remaining_train_size <- floor(0.8 * nrow(data_classif_imputed)) - nrow(data_train_initial)
remaining_train_indices <- sample(seq_len(nrow(remaining_data)), size = remaining_train_size)
data_train <- rbind(data_train_initial, remaining_data[remaining_train_indices, ])
data_test <- remaining_data[-remaining_train_indices, ]

for (col in categorical_columns) {
  data_train[[col]] <- as.factor(data_train[[col]])
  data_test[[col]] <- as.factor(data_test[[col]])
  
  levels(data_test[[col]]) <- levels(data_train[[col]])
}

####

'''
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

'''


#linear regression
model_lm <- lm(POT_AUT ~ ., data = data_train)
predictions_lm <- predict(model_lm, newdata = data_test)

#random forest
model_rf <- randomForest(POT_AUT ~ ., data = data_train, ntree = 100)
predictions_rf <- predict(model_rf, newdata = data_test)

#svm
model_gbm <- caret::train(POT_AUT ~ ., data = data_train, method = 'gbm', trControl = trainControl(method = "cv", number = 10), verbose = FALSE)
predictions_gbm <- predict(model_gbm, newdata = data_test)

#CALCULATE MAPES
mape_lm <- mape(data_test$POT_AUT, predictions_lm)
mape_rf <- mape(data_test$POT_AUT, predictions_rf)
mape_gbm <- mape(data_test$POT_AUT, predictions_gbm)

results <- data.frame(
  Model = c("LR", "RF", "GBM"),
  MAPE = c(mape_lm, mape_rf, mape_gbm)
)

fwrite(results, "SOLAR/Regression_SOLAR.csv", row.names = FALSE)




#### lo nuevo con los cambios
group1 <- c("ZEROS","AVG","SD","MIN","Q1","MEDIAN","Q3",
            "ENERGY","ENTROPY")
resultadosPerms <- data.frame()


# Normalize columns
for (col in group1){
  if (col != "ZEROS"){
    data_classif_imputed[[col]] <- data_classif_imputed[[col]] / data_classif_imputed$MAX
    data_classif_imputed[[col]] <- replace_na(data_classif_imputed[[col]], 0)
    
  }
}


evaluar_modelo <- function(grupo_features, train, test) {
  # set.seed(123)
  # setDT(data)
  # print(grupo_features)
  train_labels <- train$POT_AUT
  test_labels <- test$POT_AUT
  
  train <- train %>% select(all_of(grupo_features))
  test <- test %>% select(all_of(grupo_features))
  
  #linear regression
  model_lm <- lm(train_labels ~ ., data = train)
  predictions_lm <- predict(model_lm, newdata = test)
  
  #random forest
  model_rf <- randomForest(train_labels ~ ., data = train, ntree = 100)
  predictions_rf <- predict(model_rf, newdata = test)
  
  #svm
  model_gbm <- caret::train(train_labels ~ ., data = train, method = 'gbm', trControl = trainControl(method = "cv", number = 10), verbose = FALSE)
  predictions_gbm <- predict(model_gbm, newdata = test)
  
  #CALCULATE MAPES
  mape_lm <- mape(test_labels, predictions_lm)
  mape_rf <- mape(test_labels, predictions_rf)
  mape_gbm <- mape(test_labels, predictions_gbm)
  
  return(c(mape_lm, mape_rf, mape_gbm))
}


permutations <- powerSet(group1, 9)
  
  for (i in 2:length(permutations)){ # permutations[[1]] is empty
    grupo <- c(permutations[[i]])
    print(grupo)
    # grupo = group1
    initial_train_indices <- ensure_levels_in_train(data_classif_imputed, categorical_columns)
    data_train_initial <- data_classif_imputed[initial_train_indices, ]
    remaining_data <- data_classif_imputed[-initial_train_indices, ]
    
    set.seed(123)
    remaining_train_size <- floor(0.8 * nrow(data_classif_imputed)) - nrow(data_train_initial)
    remaining_train_indices <- sample(seq_len(nrow(remaining_data)), size = remaining_train_size)
    data_train <- rbind(data_train_initial, remaining_data[remaining_train_indices, ]) %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
    data_test <- remaining_data[-remaining_train_indices, ]
    
    metrics <- evaluar_modelo(grupo, data_train, data_test)
    print(paste("Feature set", i, "/512 completed"))
    resultadosPerms <- rbind(resultadosPerms, c(toString(grupo), metrics[1], metrics[2], metrics[3]))
  }

colnames(resultadosPerms) <- c("Grupo", "MAPE_lm", "MAPE_rf", "MAPE_gbm")