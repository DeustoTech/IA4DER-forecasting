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


#cargar archivos necesarios
feats_con_auto <- fread("SOLAR/features_con_autoconsumo_ConPV.CSV") 
feats_trampa <- fread("SOLAR/features_sin_autoconsumo_Trampa.csv") 
hasPV_data <- fread("SOLAR/Variation/HasPV.csv") 
features <- fread("SOLAR/features.csv") 

features <- features %>% filter(ASS == "CUPS" | ASS == "SOLAR") 

feats_totales <- rbind(feats_con_auto, feats_trampa, fill = T) 

# Ejecutar estas dos lineas para seleccionar de features (cruz) solo las que no tenemos nosotros
columns_to_select <- setdiff(names(feats_totales), names(features))
feats_totales <- feats_totales %>% select(ID, columns_to_select)

feats_totales <- merge(feats_totales, features, by = "ID")
data_classif <- merge(feats_totales, hasPV_data, by = "ID") 

# Quitamos todo lo de inyección, identificadores, fechas, repetidas y variables que no cambian
data_classif <- data_classif %>% select(-INSTALLATION_TIMESTAMP, -FEC_BAJA_PUN_SUM,
                                        -TIP_CONTRATO, -TIP_CUALIFICACION,
                                        -FirstInjection, -InstallationDate, -DIFF_HOURS,
                                        -COD_CNAE, -contains("AE."), -COD_PS, -COD_CLIENTE,
                                        -COD_CONTRATO, -ASS, -starts_with("FEC"), -SHARP, -ZERO, -COD_SOCIEDAD)

categorical_columns <- c("COD_POLIZA", "TIP_SUMINISTRO", "TARIF",
                         "CNAE", "COD_TARIF_IBDLA", "TIP_EST_POLIZA", "TIP_PUNTO_MEDIDA", "SUM")

data_classif$TarifCode <- data_classif$TARIF

for (col in categorical_columns) {
  data_classif[[col]] <- ifelse(is.na(data_classif[[col]]) | data_classif[[col]] == "", -1, as.factor(data_classif[[col]]))
  data_classif[[col]] <- (as.factor(data_classif[[col]]))
}


data_classif <- data_classif %>% 
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), NA)))

data_classif_imputed <- data_classif %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)))

data_classif_imputed$hasPV <- as.factor(data_classif_imputed$hasPV)


evaluar_modelo <- function(grupo_features, modelo, train, test, i) {

  train_feats <- train %>% select(all_of(grupo_features))
  train_labels <- train$hasPV

  fit <- caret::train(x = train_feats, y = train_labels, method = modelo)
  

  
  test_featsT2 <- test %>% filter(TarifCode != "96T1" & TarifCode != "97T2") %>% select(grupo_features)
  test_featsT6 <- test %>% filter(TarifCode == "96T1" | TarifCode == "97T2") %>% select(grupo_features)
  
  test_labelsT2 <- test %>% filter(TarifCode != "96T1" & TarifCode != "97T2") %>% select(hasPV)
  test_labelsT6 <- test %>% filter(TarifCode == "96T1" | TarifCode == "97T2") %>% select(hasPV)
  

  predT2 <- predict(fit, test_featsT2)
  predT6 <- predict(fit, test_featsT6)

  accuracyT2 <- accuracy(test_labelsT2$hasPV, predT2)
  sensiT2 <- sensitivity(test_labelsT2$hasPV, predT2)
  speciT2 <- specificity(test_labelsT2$hasPV, predT2)

  accuracyT6 <- accuracy(test_labelsT6$hasPV, predT6)
  sensiT6 <- sensitivity(test_labelsT6$hasPV, predT6)
  speciT6 <- specificity(test_labelsT6$hasPV, predT6)

  return(c(accuracyT2, sensiT2, speciT2, accuracyT6, sensiT6, speciT6))

}

resultados <- data.frame()
modelos <- c("rf", "svmLinear", "glm")
modelos <- c("glm")

for(modelo in modelos) {
  for(i in 1:2) {
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




fwrite(resultados, "SOLAR/resultados_clasificacion.csv", row.names = F)


##################### Prueba con grupo 1, todo trainset.
# 
group1 <- c("ZEROS","AVG","SD","MIN","Q1","MEDIAN","Q3",
            "ENERGY","ENTROPY")
resultadosT2 <- data.frame()
resultadosT6 <- data.frame()


# Normalize columns
for (col in group1){
  if (col != "ZEROS"){
    data_classif_imputed[[col]] <- data_classif_imputed[[col]] / data_classif_imputed$MAX
    data_classif_imputed[[col]] <- replace_na(data_classif_imputed[[col]], 0)
    
  }
}


##### PONER EL DE REGRESION SI SE HACE PARA REGRESION
c <- read.csv("SOLAR/Classif_Accuracy.csv") %>% arrange(desc(Accuracy)) %>% slice(1:20) %>% select(Grupo)
c_list <- lapply(c$Grupo, function(x) unlist(strsplit(x, ",\\s*")))



data_classif_imputed[which(is.infinite(data_classif_imputed$ENTROPY))]$ENTROPY <- 1

# group1 <- c("NAs", "ZEROS")
# modelos <- c("rf", "svmLinear", "glm")
modelos <- "rf"
permutations <- powerSet(group1, 9)
for(modelo in modelos) {
  
  for (i in 1:length(c_list)){ # permutations[[1]] is empty
    
  # grupo <- c(permutations[[i]]) USE PERMUTATIONS FOR ALL POSSIBLE COMBINATIONS
  grupo <- c_list[[i]]
  index_train <- createDataPartition(data_classif_imputed$hasPV, p=0.8, list=FALSE)
  train_set <- as.data.frame(data_classif_imputed[index_train, ]) %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
  test_set <- as.data.frame(data_classif_imputed[-index_train, ]) 
  
  metrics <- evaluar_modelo(grupo, modelo, train_set, test_set, i)
  print(paste("Feature set", i, "/512 completed"))
  print(paste("Grupo 1 ", ", Modelo: ",modelo, "DONE"))
  resultadosT2 <- rbind(resultadosT2, c(modelo, toString(grupo), metrics[1], metrics[2], metrics[3]))
  resultadosT6 <- rbind(resultadosT6, c(modelo, toString(grupo), metrics[4], metrics[5], metrics[6]))
  }
}
colnames(resultadosT2) <- c("Modelo", "Grupo", "Accuracy", "Sensitivity", "Specificity")
colnames(resultadosT6) <- c("Modelo", "Grupo", "Accuracy", "Sensitivity", "Specificity")
fwrite(resultadosT2, "SOLAR/Classif_Accuracy_T2.csv")
fwrite(resultadosT6, "SOLAR/Classif_Accuracy_T6.csv")








