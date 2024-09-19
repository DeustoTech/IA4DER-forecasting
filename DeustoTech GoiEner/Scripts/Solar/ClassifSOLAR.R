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
solar_data$hasPV <- factor(solar_data$hasPV, levels = c(0, 1))



cases <- c(1:4)

evaluar_modelo <- function(grupo_features, train, test) {
  
  train_labels <- train$hasPV
  train <- train %>% select(all_of(grupo_features), hasPV)
  test_labels <- test$hasPV
  
  model_lr <- glm(hasPV ~ ., data = train, family = binomial)
  predictions_lr <- predict(model_lr, newdata = test, type = "response")
  predictions_lr <- ifelse(predictions_lr > 0.5, 1, 0)  # Convert to binary classification
  
  model_rf <- randomForest(hasPV ~ ., data = train, ntree = 100)
  predictions_rf <- predict(model_rf, newdata = test)
  
  model_gbm <- caret::train(hasPV ~ ., data = train, method = 'gbm', verbose = FALSE)
  predictions_gbm <- predict(model_gbm, newdata = test)
  
  test_labels <- factor(test_labels, levels = c(0, 1))
  predictions_lr <- factor(predictions_lr, levels = c(0, 1))
  predictions_rf <- factor(predictions_rf, levels = c(0, 1))
  predictions_gbm <- factor(predictions_gbm, levels = c(0, 1))
  
  lr_accuracy <- accuracy(test_labels, predictions_lr)
  lr_sensitivity <- sensitivity(test_labels, predictions_lr)
  lr_specificity <- specificity(test_labels, predictions_lr)
  
  rf_accuracy <- accuracy(test_labels, predictions_rf)
  rf_sensitivity <- sensitivity(test_labels, predictions_rf)
  rf_specificity <- specificity(test_labels, predictions_rf)
  
  return(c(lr_accuracy, lr_sensitivity, lr_specificity, 
           rf_accuracy, rf_sensitivity, rf_specificity))
}



resultados <- data.frame()
modelos <- c("lr",  "rf", "gbm")


# Define global variables
permutations <- powerSet(feats, length(feats))
howMany <- length(permutations)


# Initialize progress handlers
handlers(global = TRUE)

# Start the progress reporting
with_progress({
  # Create a global progress bar for all cases
  global_progress <- progressor(steps = length(cases) * (howMany - 1), message = "Processing all cases and permutations")
  globalvars <- c("evaluar_modelo", "solar_data", "permutations", "librerias", "cases", "case_progress", "perm_progress", 
                "c1_list", "c2_list", "c3_list", "c4_list", "howMany", "global_progress")
  # Run the outer foreach loop for each case
  final_results <- foreach(case = cases, .combine = rbind, .options.future = list(seed = TRUE, add = TRUE, globals = globalvars, packages = librerias)) %dofuture% {
    # Create a progress bar for the current case
    case_progress <- progressr::progressor(steps = howMany - 1, message = sprintf("Processing Case %d", case))
    
    # Initialize training and testing datasets based on the case
    train <- data.frame()
    test <- data.frame()
    
    if (case == 1) {
      case1 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      train_idx <- createDataPartition(case1$hasPV, p = 0.8, list = FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
    } else if (case == 2) {
      case2 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(case2$hasPV, p = 0.8, list = FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
    } else if (case == 3) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t2$hasPV, p = 0.8, list = FALSE)
      test_idx <- createDataPartition(t6$hasPV, p = 0.2, list = FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
    } else if (case == 4) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t6$hasPV, p = 0.8, list = FALSE)
      test_idx <- createDataPartition(t2$hasPV, p = 0.2, list = FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
    }
    
    # Inner loop over permutations
    foreach(i = 2:howMany, .combine = rbind, .options.future = list(seed = TRUE)) %dofuture% {
      grupo <- c(permutations[[i]])
      metrics <- evaluar_modelo(grupo, train, test)
      
      # Update the progress bars
      case_progress(sprintf("Permutation %d/%d for Case %d", i - 1, howMany - 1, case))
      global_progress()
      
      # Return the result as a data frame
      data.frame(Grupo = toString(grupo),
                 lr_accuracy = metrics[1],
                 lr_sensitivity = metrics[2],
                 lr_specificity = metrics[3],
                 rf_accuracy = metrics[4],
                 rf_sensitivity = metrics[5],
                 rf_specificity = metrics[6],
                 Train_test_Case = case)
    }
  }
})

fwrite(final_results, "SOLAR/Classification/Models/allClassif.csv")

# DO PULL BEFORE PUSHING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

