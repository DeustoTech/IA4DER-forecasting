library(foreach)
library(doParallel)
library(doFuture)

registerDoFuture()
plan(multisession, workers = 4)  # Change the number of workers


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
feats_con_auto <- fread("SOLAR/features_con_autoconsumo_ConPV.CSV") #982 330
feats_trampa <- fread("SOLAR/features_sin_autoconsumo_Trampa.csv") #1451  312
hasPV_data <- fread("SOLAR/Variation/HasPV.csv") #2434    4
features <- fread("SOLAR/features.csv") #97028    22

## DATA CLEANING

#juntar todos los csvs
features <- features %>% filter(ASS == "CUPS" | ASS == "SOLAR") #71021 22
feats_totales <- rbind(feats_con_auto, feats_trampa, fill = T) #2433  330

# Ejecutar estas dos lineas para seleccionar de features (cruz) solo las que no tenemos nosotros
columns_to_select <- setdiff(names(feats_totales), names(features))
feats_totales <- feats_totales %>% select(ID, all_of(columns_to_select))

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

#quitar categorias que solo tienen un factor o 2
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

#renombrar algunas columnas para que funcione random forest
for (col in colnames(data_classif)) {
  new_col_name <- gsub("-", ".", col)
  names(data_classif)[names(data_classif) == col] <- new_col_name
}

#imputar valores
data_classif <- data_classif %>% 
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), NA)))
solar_data <- data_classif %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)))

#convertir a numerica y normalizar la variable a predecir
solar_data$POT_AUT <- as.numeric(solar_data$POT_AUT)

normalize_min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

solar_data$POT_AUT <- normalize_min_max(solar_data$POT_AUT)
solar_data <- solar_data %>% select(-ID) #quitar el ID

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

## PREDICCIONES
# 
# feats <- c("ZEROS","AVG","SD","MIN","Q1","MEDIAN","Q3",
#             "ENERGY","ENTROPY")


feats <- c("ZEROS","AVG","SD")

models <- c("lm", "rf", "gbm")

# Normalizar columnas
for (col in feats){
  if (col != "ZEROS"){
    solar_data[[col]] <- solar_data[[col]] / solar_data$MAX
    solar_data[[col]] <- replace_na(solar_data[[col]], 0)
    
  }
}

solar_data[which(is.infinite(solar_data$ENTROPY))]$ENTROPY <- 1

# T2: TarifCode != 96T1, 97T2
# T6: TarifCode == 96T1, 97T2


# TODO Train t2, test t2. Case 1
#      Train t6, test t6. Case 2
#      Train t2, test t6. Case 3
#      Train t6, test t2. Case 4
# Repeat each 100 times

cases <- c(1:4)

evaluar_modelo <- function(grupo_features, train, test) {
  
  train_labels <- train$POT_AUT
  train <- train %>% select(all_of(grupo_features), POT_AUT)
  test_labels <- test$POT_AUT
  
  
  
  #linear regression
  model_lm <- lm(POT_AUT ~ ., data = train)
  predictions_lm <- predict(model_lm, newdata = test)
  
  #random forest
  model_rf <- randomForest(POT_AUT ~ ., data = train, ntree = 100)
  predictions_rf <- predict(model_rf, newdata = test)
  
  #gbm
  
  # por alguna razon está con cv, de momento lo quito
  # model_gbm <- caret::train(POT_AUT ~ ., data = train, method = 'gbm', trControl = trainControl(method = "cv", number = 10), verbose = FALSE)
  model_gbm <- caret::train(POT_AUT ~ ., data = train, method = 'gbm', verbose = FALSE)
  predictions_gbm <- predict(model_gbm, newdata = test)

  #CALCULATE MAPES
  mape_lm <- mape(test_labels, predictions_lm)
  mape_rf <- mape(test_labels, predictions_rf)
  mape_gbm <- mape(test_labels, predictions_gbm)
  
  rmse_lm <- rmse(test_labels, predictions_lm)
  rmse_rf <- rmse(test_labels, predictions_rf)
  rmse_gbm <- rmse(test_labels, predictions_gbm)
  
  return(c(mape_lm, mape_rf, mape_gbm, rmse_lm, rmse_rf, rmse_gbm))
}

# USING DOFUTURE AND PARALLEL PROCESSING
cases <- c(1:4)
permutations <- powerSet(feats, 3)
results <- data.frame()

results <- foreach(case = cases, .combine = rbind, 
                   .options.future = list(globals = c("evaluar_modelo", "solar_data", "permutations"), .packages = librerias, seed = TRUE)) %dofuture% {
                     
         
         train <- data.frame()
         test <- data.frame()
         
         if(case == 1) {
           case1 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
           train_idx <- createDataPartition(case1$POT_AUT, p=0.8, list=FALSE)
           train <- as.data.frame(solar_data[train_idx, ])
           test <- as.data.frame(solar_data[-train_idx, ])
         }
         
         if(case == 2) {
           case2 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
           train_idx <- createDataPartition(case2$POT_AUT, p=0.8, list=FALSE)
           train <- as.data.frame(solar_data[train_idx, ])
           test <- as.data.frame(solar_data[-train_idx, ])
         }
         
         if(case == 3) {
           t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
           t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
           train_idx <- createDataPartition(t2$POT_AUT, p=0.8, list=FALSE)
           test_idx <- createDataPartition(t6$POT_AUT, p=0.2, list=FALSE)
           train <- as.data.frame(solar_data[train_idx, ])
           test <- as.data.frame(solar_data[test_idx, ])
         }
         
         if(case == 4) {
           t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
           t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
           train_idx <- createDataPartition(t6$POT_AUT, p=0.8, list=FALSE)
           test_idx <- createDataPartition(t2$POT_AUT, p=0.2, list=FALSE)
           train <- as.data.frame(solar_data[train_idx, ])
           test <- as.data.frame(solar_data[test_idx, ])
         }
         
         foreach(i = 2:length(permutations), .combine = rbind, .options.future = list(seed = TRUE)) %dofuture% {
           grupo <- c(permutations[[i]])
           metrics <- evaluar_modelo(grupo, train, test)
           print(paste("Case", case, ". Feature set", i-1,"/",length(permutations)-1, "completed"))
           data.frame(Grupo = toString(grupo),
                      MAPE_lm = metrics[1],
                      MAPE_rf = metrics[2],
                      MAPE_gbm = metrics[3],
                      RMSE_lm = metrics[4],
                      RMSE_rf = metrics[5],
                      RMSE_gbm = metrics[6],
                      Train_test_Case = case)
         }
       }

print("All cases completed")





# Without doFuture
{
results <- data.frame()
# permutations <- powerSet(feats, 9)
permutations <- powerSet(feats, 3)

for(case in cases){ 
  
  train <- data.frame()
  test <- data.frame()
 
  
  
  # Train t2, test t2
  if(case == 1) {
    case1 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
    train_idx <- createDataPartition(case1$POT_AUT, p=0.8, list=FALSE)
    train <- as.data.frame(solar_data[train_idx, ])
    test <- as.data.frame(solar_data[-train_idx, ])
    
  
  }
  

  # Train t6, test t6
  if(case == 2) {
    
    case2 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
    train_idx <- createDataPartition(case2$POT_AUT, p=0.8, list=FALSE)
    train <- as.data.frame(solar_data[train_idx, ])
    test <- as.data.frame(solar_data[-train_idx, ])

  }
  
  # Train t2, test t6
  if(case == 3) {
    
    t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
    t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
    train_idx <- createDataPartition(t2$POT_AUT, p=0.8, list=FALSE)
    test_idx <- createDataPartition(t6$POT_AUT, p=0.2, list=FALSE)
    
    train <- as.data.frame(solar_data[train_idx, ])
    test <- as.data.frame(solar_data[test_idx, ])
    
  }
 
  # Train t6, test t2
  if(case == 4) {
    t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
    t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
    train_idx <- createDataPartition(t6$POT_AUT, p=0.8, list=FALSE)
    test_idx <- createDataPartition(t2$POT_AUT, p=0.2, list=FALSE)
    
    train <- as.data.frame(solar_data[train_idx, ])
    test <- as.data.frame(solar_data[test_idx, ])
    
  }
  
    for(i in 2:length(permutations)){ # permutations[[1]] is empty
      grupo <- c(permutations[[i]])
      metrics <- evaluar_modelo(grupo, train, test)
      print(paste("Case", case, ". Feature set", i-1,"/",length(permutations)-1, "completed"))
      results <- rbind(results, c(toString(grupo), metrics[1], metrics[2], metrics[3], metrics[4], metrics[5], metrics[6], case))
      colnames(results) <- c("Grupo", "MAPE_lm", "MAPE_rf", "MAPE_gbm", "RMSE_lm", "RMSE_rf", "RMSE_gbm", "Train-test_Case")
      
    }
  print(paste("Case", case, "completed"))
}

}





# OLD ANE CODE
{
resultadosT2 <- data.frame()
resultadosT6 <- data.frame()

c <- read.csv("SOLAR/resultadosPerms_regrs.csv") %>% arrange(desc(RMSE_rf)) %>% slice(1:20) %>% select(Grupo)
c_list <- lapply(c$Grupo, function(x) unlist(strsplit(x, ",\\s*")))

# Normalizar columnas
for (col in group1){
  if (col != "ZEROS"){
    data_classif_imputed[[col]] <- data_classif_imputed[[col]] / data_classif_imputed$MAX
    data_classif_imputed[[col]] <- replace_na(data_classif_imputed[[col]], 0)
    
  }
}

data_classif_imputed[which(is.infinite(data_classif_imputed$ENTROPY))]$ENTROPY <- 1


evaluar_modelo <- function(grupo_features, train, test) {
  
  train_labels <- train$POT_AUT
  train <- train %>% select(all_of(grupo_features), POT_AUT)
  
  #test <- test %>% filter(TarifCode != "96T1" & TarifCode != "97T2") %>% select(all_of(grupo_features), POT_AUT)
  test <- test %>% filter(TarifCode == "96T1" | TarifCode == "97T2") %>% select(all_of(grupo_features), POT_AUT)
  
  test_labels <- test$POT_AUT
  
  #linear regression
  model_lm <- lm(POT_AUT ~ ., data = train)
  predictions_lm <- predict(model_lm, newdata = test)
  
  #random forest
  model_rf <- randomForest(POT_AUT ~ ., data = train, ntree = 100)
  predictions_rf <- predict(model_rf, newdata = test)
  
  #svm
  model_gbm <- caret::train(POT_AUT ~ ., data = train, method = 'gbm', trControl = trainControl(method = "cv", number = 10), verbose = FALSE)
  predictions_gbm <- predict(model_gbm, newdata = test)
  
  #CALCULATE MAPES
  mape_lm <- mape(test_labels, predictions_lm)
  mape_rf <- mape(test_labels, predictions_rf)
  mape_gbm <- mape(test_labels, predictions_gbm)
  
  rmse_lm <- rmse(test_labels, predictions_lm)
  rmse_rf <- rmse(test_labels, predictions_rf)
  rmse_gbm <- rmse(test_labels, predictions_gbm)
  
  return(c(mape_lm, mape_rf, mape_gbm, rmse_lm, rmse_rf, rmse_gbm))
}


permutations <- powerSet(group1, 9)
  
  for (i in 1:length(c_list)){
    
    grupo <- c_list[[i]]
    print(grupo)
    initial_train_indices <- ensure_levels_in_train(data_classif_imputed, categorical_columns)
    data_train_initial <- data_classif_imputed[initial_train_indices, ]
    remaining_data <- data_classif_imputed[-initial_train_indices, ]
    
    set.seed(123)
    remaining_train_size <- floor(0.8 * nrow(data_classif_imputed)) - nrow(data_train_initial)
    remaining_train_indices <- sample(seq_len(nrow(remaining_data)), size = remaining_train_size)
    data_train <- rbind(data_train_initial, remaining_data[remaining_train_indices, ]) %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
    data_test <- remaining_data[-remaining_train_indices, ] 
    metrics <- evaluar_modelo(grupo, data_train, data_test)
    print(paste("Feature set", i, "/20 completed"))
    resultadosT6 <- rbind(resultadosT6, c(toString(grupo), metrics[1], metrics[2], metrics[3], metrics[4], metrics[5], metrics[6]))
  }

colnames(resultadosT6) <- c("Grupo", "MAPE_lm", "MAPE_rf", "MAPE_gbm", "RMSE_lm", "RMSE_rf", "RMSE_gbm")
fwrite(resultadosT6, "SOLAR/resultadosPerms_regrs_t6.csv", row.names = T)


datat2 <- fread("SOLAR/resultadosPerms_regrs_t2.csv")
datat6 <- fread("SOLAR/resultadosPerms_regrs_t6.csv")

summary(datat2)

data <- data %>% arrange(RMSE_rf)

fwrite(data, "SOLAR/resultadosPerms_regrs_ord.csv", row.names = FALSE)
}