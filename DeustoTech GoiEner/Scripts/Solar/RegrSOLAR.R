library(foreach)
library(doParallel)
library(doFuture)

registerDoFuture()
plan(multisession, workers = 30)  # Change the number of workers


# añadir las librerias nuevas en este vector
librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "stringr", 
               "randomForest", "purrr", "matrixStats","glmnet", "recipes", "pROC", "rje", "dplyr", "progressr") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

#cargar archivos necesarios
feats_con_auto <- fread("SOLAR/features_con_autoconsumo_ConPV.csv") #982 330
feats_trampa <- fread("SOLAR/features_sin_autoconsumo_Trampa.csv") %>% slice(1:1000) #1000  312
hasPV_data <- fread("SOLAR/HasPV.csv") #2434    4
features <- fread("SOLAR/features.csv") #97028    22

## DATA CLEANING

#juntar todos los csvs con todas las columnas
features <- features %>% filter(ASS == "CUPS" | ASS == "SOLAR") #71021 22
feats_totales <- rbind(feats_con_auto, feats_trampa, fill = T) #1982  330
columns_to_select <- setdiff(names(feats_totales), names(features))
feats_totales <- feats_totales %>% select(ID, all_of(columns_to_select))
solar_data <- merge(feats_totales, features, by = "ID")
solar_data <- merge(solar_data, hasPV_data, by = "ID") #1982  346


# Quitamos todo lo solar_data inyección, identificadores, fechas, repetidas y variables que no cambian
solar_data <- solar_data %>% select(-INSTALLATION_TIMESTAMP, -FEC_BAJA_PUN_SUM,
                                        -TIP_CONTRATO, -TIP_CUALIFICACION,
                                        -FirstInjection, -InstallationDate, -DIFF_HOURS,
                                        -COD_CNAE, -contains("AE."), -COD_PS, -COD_CLIENTE,
                                        -COD_CONTRATO,  -starts_with("FEC"), -SHARP, -ZEROS, -COD_SOCIEDAD)

categorical_columns <- c( "TIP_SUMINISTRO", "COD_TARIF_IBDLA", "TIP_EST_POLIZA", "CNAE", "TARIF", "SUM", "TIP_PUNTO_MEDIDA")

solar_data$TarifCode <- solar_data$TARIF

# Transformar categoricas a factores
for (col in categorical_columns) {
  solar_data[[col]] <- ifelse(is.na(solar_data[[col]]) | solar_data[[col]] == "", -1, as.factor(solar_data[[col]]))
  solar_data[[col]] <- (as.factor(solar_data[[col]]))
}


#renombrar algunas columnas para que funcione random forest
for (col in colnames(solar_data)) {
  new_col_name <- gsub("-", ".", col)
  names(solar_data)[names(solar_data) == col] <- new_col_name
}

solar_data <- solar_data %>%
  mutate(POT_AUT = ifelse(ASS == "CUPS", 0, POT_AUT)) %>% mutate(POT_AUT = POT_AUT / MAX)

# take out incorrect cups and solars
solar_data <- solar_data %>% filter(!is.na(POT_AUT) & !is.infinite(POT_AUT)) 

summary(solar_data$POT_AUT)

feats <- c("ZERO","AVG","SD","MIN","Q1","MEDIAN","Q3",
           "ENERGY","ENTROPY")

# Normalizar columnas dividiendo entre MAX
for (col in feats){
  if (col != "ZERO" & col != "ENTROPY"){
    features[[col]] <- features[[col]] / features$MAX
    features[[col]] <- replace_na(features[[col]], 0)
    
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
  model_gbm <- caret::train(POT_AUT ~ ., data = train, method = 'gbm', verbose = FALSE)
  predictions_gbm <- predict(model_gbm, newdata = test)

  #CALCULATE MAPES
  rmse_lm <- rmse(test_labels, predictions_lm)
  rmse_rf <- rmse(test_labels, predictions_rf)
  rmse_gbm <- rmse(test_labels, predictions_gbm)
  

  return(c(rmse_lm, rmse_rf, rmse_gbm))
}

# USING DOFUTURE AND PARALLEL PROCESSING

permutations <- powerSet(feats, length(feats))
howMany <- length(permutations)
results <- data.frame() 
globalvars <- c("evaluar_modelo", "solar_data", "permutations", "librerias", "cases", "case_progress",  "perm_progress",
                "c1_list", "c2_list", "c3_list", "c4_list")


# Codigo para, a partir de un csv de resultados ya existente
# sacar la columna grupo en el mismo formato que el loop a usar
{
c1 <- read.csv("SOLAR/Regresion/Top/case1_top.csv") %>% arrange(desc(MAPE_rf))  %>% select(Grupo)
c1_list <- lapply(c1$Grupo, function(x) unlist(strsplit(x, ",\\s*")))

c2 <- read.csv("SOLAR/Regresion/Top/case2_top.csv") %>% arrange(desc(MAPE_rf))  %>% select(Grupo)
c2_list <- lapply(c2$Grupo, function(x) unlist(strsplit(x, ",\\s*")))

c3 <- read.csv("SOLAR/Regresion/Top/case3_top.csv") %>% arrange(desc(MAPE_rf))  %>% select(Grupo)
c3_list <- lapply(c3$Grupo, function(x) unlist(strsplit(x, ",\\s*")))

c4 <- read.csv("SOLAR/Regresion/Top/case4_top.csv") %>% arrange(desc(MAPE_rf))  %>% select(Grupo)
c4_list <- lapply(c4$Grupo, function(x) unlist(strsplit(x, ",\\s*")))
}



# 100 times the best 30 of each case
# IN THIS LOOP, PERMUTATIONS TAKES THE VALUE OF Cn_list depending on the case

{

final_results <- foreach(iteration = 1:100, .combine = rbind, 
                         .options.future = list(globals = globalvars,add = TRUE,
                                                packages = librerias, seed = TRUE)) %dofuture% {
        
  # outer_progress(message = sprintf("Starting iteration %d of 100", iteration))
                                                  
  foreach(case = cases, .combine = rbind, .options.future = list(seed = TRUE)) %dofuture% {
    
    train <- data.frame()
    test <- data.frame()
    
    
    if(case == 1) {
      case1 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      train_idx <- createDataPartition(case1$POT_AUT, p=0.8, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
      permutations <- c1_list # Remove or comment if all permutations are to be used
    }
    
    if(case == 2) {
      case2 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(case2$POT_AUT, p=0.8, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
      permutations <- c2_list # Remove or comment if all permutations are to be used
    }
    
    if(case == 3) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t2$POT_AUT, p=0.8, list=FALSE)
      test_idx <- createDataPartition(t6$POT_AUT, p=0.2, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
      permutations <- c3_list # Remove or comment if all permutations are to be used
    }
    
    if(case == 4) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t6$POT_AUT, p=0.8, list=FALSE)
      test_idx <- createDataPartition(t2$POT_AUT, p=0.2, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
      permutations <- c4_list # Remove or comment if all permutations are to be used
    }
    
    foreach(i = 2:length(permutations), .combine = rbind, .options.future = list(seed = TRUE)) %dofuture% {
      grupo <- c(permutations[[i]])
      metrics <- evaluar_modelo(grupo, train, test)
      
      data.frame(Grupo = toString(grupo),
                 MAPE_lm = metrics[1],
                 MAPE_rf = metrics[2],
                 MAPE_gbm = metrics[3],
                 Train_test_Case = case,
                 Iteration = iteration)
    }
  }
}
}
# TODO fwrite final results


# ONLY 1 time with all permutations
with_progress({
  # Initialize the outer progress for cases
  
  final_results <- foreach(case = cases, .combine = rbind, .options.future = list(seed = TRUE, add = TRUE, globals = globalvars, packages = librerias)) %dofuture% {
      case_progress <- progressr::progressor(steps = length(cases), message = "Processing cases")

    case_progress(sprintf("Processing case %d", case))  # Update case progress
    
    train <- data.frame()
    test <- data.frame()
    
    # Prepare training and testing data based on the case
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
    
    # Initialize the progress for permutations
    perm_progress <- progressr::progressor(steps = length(permutations) - 1, message = sprintf("Permutations for case %d", case))
    
    foreach(i = 2:length(permutations), .combine = rbind, .options.future = list(seed = TRUE)) %dofuture% {
      grupo <- c(permutations[[i]])
      metrics <- evaluar_modelo(grupo, train, test)
      
      perm_progress(sprintf("Permutation %d/%d for case %d", i-1, length(permutations)-1, case))  # Update permutation progress
      
      data.frame(Grupo = toString(grupo),
                 RMSE_lm = metrics[1],
                 RMSE_rf = metrics[2],
                 RMSE_gbm = metrics[3],
                 Train_test_Case = case)
    }
  }
})




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