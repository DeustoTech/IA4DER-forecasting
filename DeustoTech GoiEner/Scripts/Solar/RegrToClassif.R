library(foreach)
library(doParallel)
library(doFuture)

registerDoFuture()
plan(multisession)  # Change the number of workers


# añadir las librerias nuevas en este vector
librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "purrr", "matrixStats","glmnet", "recipes", "pROC", "rje", "dplyr", "progressr", "stringr") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


# for each case, select the feature combination wiht lowest mape

c1 <- read.csv("SOLAR/Regresion/Top/case1_top.csv") %>% arrange(RMSE_rf) %>% slice(1)
c2 <- read.csv("SOLAR/Regresion/Top/case2_top.csv") %>% arrange(RMSE_rf) %>% slice(1)
c3 <- read.csv("SOLAR/Regresion/Top/case3_top.csv") %>% arrange(RMSE_rf) %>% slice(1)
c4 <- read.csv("SOLAR/Regresion/Top/case4_top.csv") %>% arrange(RMSE_rf) %>% slice(1)

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
solar_data$hasPV <- factor(solar_data$hasPV, levels = c(0, 1))


cases <- c(1:4)

# Make the prediction of the POT AUT using a random forest
makePreds <- function(feats, train, test, threshold, case){
  
  feats_str <- paste(feats, collapse = ", ")
  
  pvList <- test %>% select(ID, hasPV, POT_AUT)
  pvList$Grupo <- feats_str

  pvList$predPV <- 0
  train <- train %>% select(all_of(feats), POT_AUT)
  test_labels <- test$POT_AUT
  test <- test %>% select(all_of(feats))
  
  model <- randomForest(POT_AUT ~ ., data = train, ntree = 100)
  preds <- predict(model, newdata = test)
  
  # With these predictions, try different thresholds to say if it has PV or not
  
  pvList$PredPot <- preds
  # print(sum(is.na(pvList$PredPot)))  # Should be 0
  pvList$RMSE <- rmse(test_labels, pvList$PredPot)
  
  pvList <- pvList %>%
    mutate(predPV = ifelse(pvList$PredPot < threshold, 0, 1))
  
  pvList$Threshold <- threshold
  pvList$Case <- case
  
  return(pvList)
}

results <- data.frame()
thresholds <- seq(1, 3, by = 0.05)

globalvars <- c("makePreds", "solar_data", "permutations", "librerias", "cases", "c1", "c2", "c3", "c4", "results", "thresholds")

final_results <- foreach(case = cases, .combine = rbind, .options.future = list(seed = TRUE, add = TRUE, 
  globals = globalvars, packages = librerias)) %dofuture% {

    threshold = 3
    
    foreach(threshold = thresholds, .combine = rbind, .options.future = list(seed = TRUE)) %dofuture% {
      
    
    # Prepare training and testing data based on the case
    if(case == 1) {
      case1 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      train_idx <- createDataPartition(case1$POT_AUT, p=0.8, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
      feats <- strsplit(c1$Grupo, ",\\s*")[[1]]
      
      
      # Make predictions, this is where the threshold should change
     makePreds(feats, train, test, threshold, case)
    }
    
    else if(case == 2) {
      case2 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(case2$POT_AUT, p=0.8, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
      feats <- strsplit(c2$Grupo, ",\\s*")[[1]]
      
      # Make predictions, this is where the threshold should change
      makePreds(feats, train, test, threshold, case)
      
    }
    
    else if(case == 3) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t2$POT_AUT, p=0.8, list=FALSE)
      test_idx <- createDataPartition(t6$POT_AUT, p=0.2, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
      feats <- strsplit(c3$Grupo, ",\\s*")[[1]]
      
      
      # Make predictions, this is where the threshold should change
      makePreds(feats, train, test, threshold, case)
      
    }
    
    else if(case == 4) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t6$POT_AUT, p=0.8, list=FALSE)
      test_idx <- createDataPartition(t2$POT_AUT, p=0.2, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
      feats <- strsplit(c4$Grupo, ",\\s*")[[1]]
      
      
      # Make predictions, this is where the threshold should change
      makePreds(feats, train, test, threshold, case)
      
    }
      
  }  
}

final_results$hasPV <- factor(final_results$hasPV, levels = c(0, 1))
final_results$predPV <- factor(final_results$predPV, levels = c(0, 1))

compute_metrics <- function(data){
  # Create confusion matrix based on predPV and actual hasPV
  confusion <- confusionMatrix(factor(data$predPV), factor(data$hasPV))
  
  # Extract the necessary values
  TP <- confusion$table[2,2]
  TN <- confusion$table[1,1]
  FP <- confusion$table[1,2]
  FN <- confusion$table[2,1]
  
  # Compute accuracy, sensitivity, and specificity
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))  # Handle division by zero for sensitivity
  specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))  # Handle division by zero for specificity
  
  
  return(data.frame(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
}



# Group the final_results by Case and threshold and apply the compute_metrics function
metrics_results <- final_results %>%
  group_by(Case, Threshold) %>%
  summarise(
    accuracy = mean(as.numeric(predPV == hasPV)),  # Alternatively, use compute_metrics() if it's applied row-wise
    sensitivity = sum(predPV == 1 & hasPV == 1) / sum(hasPV == 1, na.rm = TRUE),  # True Positive Rate
    specificity = sum(predPV == 0 & hasPV == 0) / sum(hasPV == 0, na.rm = TRUE),  # True Negative Rate
    .groups = 'drop'  # To avoid grouped output; modify if different behavior is desired
  )



# View the computed metrics for each case
print(metrics_results)


