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

data_classif_imputed$hasPV <- as.factor(data_classif_imputed$hasPV)

colnames(data_classif_imputed) <- gsub("-", "_", colnames(data_classif_imputed))



# statistical descriptors
group1 <- c("LENGTH","NAs","ZEROS","AVG","SD","MIN","Q1","MEDIAN","Q3",
            "MAX","TOTAL","VAR","ENTROPY","IMPUTED","ENERGY", "SUM")

# solar periods + tariff
group2 <- c("P_T2.0_VALLE","P_T2.0_LLANO","P_T2.0_PICO","P_T_SOLAR_PICO","P_T_SOLAR_LLANO",
            "P_T_SOLAR_SPICO","P_T_SOLAR_SLLANO", "TARIF")

# contracted powers an potencies
group3 <- c("TIP_SUMINISTRO","VAL_POT_AUTORIZADA",
             "COD_TARIF_IBDLA","TIP_EST_POLIZA","TIP_PUNTO_MEDIDA",
            "POT_CON","POT_EST","POT_NOM","POT_AUT", "ENERGY")

group4 <- c(
  "AI.week.1", "AI.week.2", "AI.week.3", "AI.week.4", "AI.week.5",
  "AI.week.6", "AI.week.7", "AI.week.8", "AI.week.9", "AI.week.10",
  "AI.week.11", "AI.week.12", "AI.week.13", "AI.week.14", "AI.week.15",
  "AI.week.16", "AI.week.17", "AI.week.18", "AI.week.19", "AI.week.20",
  "AI.week.21", "AI.week.22", "AI.week.23", "AI.week.24", "AI.week.25",
  "AI.week.26", "AI.week.27", "AI.week.28", "AI.week.29", "AI.week.30",
  "AI.week.31", "AI.week.32", "AI.week.33", "AI.week.34", "AI.week.35",
  "AI.week.36", "AI.week.37", "AI.week.38", "AI.week.39", "AI.week.40",
  "AI.week.41", "AI.week.42", "AI.week.43", "AI.week.44", "AI.week.45",
  "AI.week.46", "AI.week.47", "AI.week.48", "AI.week.49", "AI.week.50",
  "AI.week.51", "AI.week.52", "AI.week.53", "AI.month.1", "AI.month.2", "AI.month.3",
  "AI.month.4", "AI.month.5", "AI.month.6", "AI.month.7", "AI.month.8",
  "AI.month.9", "AI.month.10", "AI.month.11", "AI.month.12")

group5 <- c(
  "Total_autum_0_4", "Total_autum_5_8", "Total_autum_9_12", "Total_autum_13_16", 
  "Total_autum_17_20", "Total_autum_21_24", "Total_spring_0_4", "Total_spring_5_8",  
  "Total_spring_9_12", "Total_spring_13_16", "Total_spring_17_20", "Total_spring_21_24",
  "Total_summer_0_4", "Total_summer_5_8", "Total_summer_9_12", "Total_summer_13_16",
  "Total_summer_17_20", "Total_summer_21_24", "Total_winter_0_4", "Total_winter_5_8",  
  "Total_winter_9_12", "Total_winter_13_16", "Total_winter_17_20", "Total_winter_21_24",
  "Max_autum_0_4", "Max_autum_5_8", "Max_autum_9_12", "Max_autum_13_16",   
  "Max_autum_17_20", "Max_autum_21_24", "Max_spring_0_4", "Max_spring_5_8",    
  "Max_spring_9_12", "Max_spring_13_16", "Max_spring_17_20", "Max_spring_21_24",  
  "Max_summer_0_4", "Max_summer_5_8", "Max_summer_9_12", "Max_summer_13_16",  
  "Max_summer_17_20", "Max_summer_21_24", "Max_winter_0_4", "Max_winter_5_8",    
  "Max_winter_9_12", "Max_winter_13_16", "Max_winter_17_20", "Max_winter_21_24",  
  "Total_autum.finde", "Total_spring.finde", "Total_summer.finde", "Total_winter.finde",
  "Max_autum.finde", "Max_spring.finde", "Max_summer.finde", "Max_winter.finde"
)

group6 <- c(
  "mean.mape.week", "mean.mape.month", 
  "mape.AI.week.2_1", "mape.AI.week.3_2", "mape.AI.week.4_3",
  "mape.AI.week.5_4", "mape.AI.week.6_5", "mape.AI.week.7_6", "mape.AI.week.8_7",
  "mape.AI.week.9_8", "mape.AI.week.10_9", "mape.AI.week.11_10", "mape.AI.week.12_11",
  "mape.AI.week.13_12", "mape.AI.week.14_13", "mape.AI.week.15_14", "mape.AI.week.16_15",
  "mape.AI.week.17_16", "mape.AI.week.18_17", "mape.AI.week.19_18", "mape.AI.week.20_19",
  "mape.AI.week.21_20", "mape.AI.week.22_21", "mape.AI.week.23_22", "mape.AI.week.24_23",
  "mape.AI.week.25_24", "mape.AI.week.26_25", "mape.AI.week.27_26", "mape.AI.week.28_27",
  "mape.AI.week.29_28", "mape.AI.week.30_29", "mape.AI.week.31_30", "mape.AI.week.32_31",
  "mape.AI.week.33_32", "mape.AI.week.34_33", "mape.AI.week.35_34", "mape.AI.week.36_35",
  "mape.AI.week.37_36", "mape.AI.week.38_37", "mape.AI.week.39_38", "mape.AI.week.40_39",
  "mape.AI.week.41_40", "mape.AI.week.42_41", "mape.AI.week.43_42", "mape.AI.week.44_43",
  "mape.AI.week.45_44", "mape.AI.week.46_45", "mape.AI.week.47_46", "mape.AI.week.48_47",
  "mape.AI.week.49_48", "mape.AI.week.50_49", "mape.AI.week.51_50", "mape.AI.week.52_51",
  "mape.AI.week.53_52"
)


evaluar_modelo <- function(grupo_features, modelo, train, test) {
  # set.seed(123)
  # setDT(data)
  
  train_labels <- train$hasPV
  test_labels <- test$hasPV
  
  train <- train[, grupo_features]
  test <- test[, grupo_features]
  
  # print(names(train))
  
  # Usar directamente caret::train para evitar conflictos de nombres
  # fit <- caret::train(x = train, y = train_labels, method = modelo, trControl = trainControl(method="cv", number=10))
  fit <- caret::train(x = train, y = train_labels, method = modelo)
  
  pred <- predict(fit, test)
  # accuracy <- sum(pred == test_labels) / length(test_labels)
  accuracy <- accuracy(test_labels, pred)
  sensi <- sensitivity(test_labels, pred)
  speci <- specificity(test_labels, pred)
  # mape <- mape(as.numeric(as.character(test_labels)), as.numeric(as.character(pred)))
  return(c(accuracy, sensi, speci))
}

resultados <- data.frame()
modelos <- c("rf", "svmLinear", "glm")
modelos <- c("rf")

for(modelo in modelos) {
  for(i in 1:6) {
    grupo <- get(paste0("group", i))
  
    index_train <- createDataPartition(data_classif_imputed$hasPV, p=0.8, list=FALSE)
    train_set <- as.data.frame(data_classif_imputed[index_train, ])
    test_set <- as.data.frame(data_classif_imputed[-index_train, ])
    
    metrics <- evaluar_modelo(grupo, modelo, train_set, test_set)
    print(paste("Grupo ",i, ", Modelo: ",modelo, "DONE"))
    resultados <- rbind(resultados, c(modelo, paste0("group", i), metrics[1], metrics[2], metrics[3]))
  }
}
colnames(resultados) <- c("Modelo", "Grupo", "Accuracy", "Sensitivity", "Specificity")



fwrite(resultados, "resultados_modelos.csv", row.names = F)
