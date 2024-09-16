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

c1 <- read.csv("SOLAR/Regresion/Top/case1_top.csv") %>% arrange(MAPE_rf) %>% slice(1)
c2 <- read.csv("SOLAR/Regresion/Top/case2_top.csv") %>% arrange(MAPE_rf) %>% slice(1)
c3 <- read.csv("SOLAR/Regresion/Top/case3_top.csv") %>% arrange(MAPE_rf) %>% slice(1)
c4 <- read.csv("SOLAR/Regresion/Top/case4_top.csv") %>% arrange(MAPE_rf) %>% slice(1)



#cargar archivos necesarios
feats_con_auto <- fread("SOLAR/features_con_autoconsumo_ConPV.csv") #982 330
feats_trampa <- fread("SOLAR/features_sin_autoconsumo_Trampa.csv") #1451  312
hasPV_data <- fread("SOLAR/HasPV.csv") #2434    4
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

# Transformar categoricas a factores
for (col in categorical_columns) {
  data_classif[[col]] <- ifelse(is.na(data_classif[[col]]) | data_classif[[col]] == "", -1, as.factor(data_classif[[col]]))
  data_classif[[col]] <- (as.factor(data_classif[[col]]))
}


for (col in colnames(data_classif)) {
  new_col_name <- gsub("-", ".", col)
  names(data_classif)[names(data_classif) == col] <- new_col_name
}

#imputar valores
data_classif <- data_classif %>% 
  mutate(across(where(is.numeric), ~replace(., !is.finite(.), NA)))
solar_data <- data_classif %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)))


solar_data$POT_AUT <- as.numeric(solar_data$POT_AUT)
solar_data <- solar_data %>% filter(POT_AUT > 0)


# solar_data <- solar_data %>% select(-ID) #quitar el ID

feats <- c("ZEROS","AVG","SD","MIN","Q1","MEDIAN","Q3",
           "ENERGY","ENTROPY")


# Normalizar columnas dividiendo entre MAX
for (col in feats){
  if (col != "ZEROS" & col != "ENTROPY"){
    solar_data[[col]] <- solar_data[[col]] / solar_data$MAX
    solar_data[[col]] <- replace_na(solar_data[[col]], 0)
    
  }
}
solar_data[which(is.infinite(solar_data$ENTROPY))]$ENTROPY <- 1

cases <- c(1:4)

# Make the prediction of the POT AUT using a random forest
makePreds <- function(feats, train, test, threshold){
  
  pvList <- test %>% select(ID, hasPV)
  pvList$predPV <- numeric()
  train <- train %>% select(feats)
  test <- test %>% select(feats)
  
  model <- randomForest(POT_AUT ~ ., data = train, ntree = 100)
  preds <- predict(model, newdata = test)
  
  # With these predictions, try different thresholds to say if it has PV or not
  
  pvList$PredPot <- preds
  
  pvList <- pvList %>%
    mutate(predPV = ifelse(pvList$PredPot < threshold, 0, 1))
  
  pvList$Grupo <- train$Grupo
  pvList$Threshold <- threshold
  
  return(pvList)
}

results <- data.frame()
globalvars <- c("makePreds", "solar_data", "permutations", "librerias", "caseFeats", "c1", "c2", "c3", "c4", "results")

final_results <- foreach(case = cases, .combine = rbind, .options.future = list(seed = TRUE, add = TRUE, 
  globals = globalvars, packages = librerias)) %dofuture% {

    threshold = 3
    
    results <- data.frame()
    train <- data.frame()
    test <- data.frame()
    
    # Prepare training and testing data based on the case
    if(case == 1) {
      case1 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      train_idx <- createDataPartition(case1$POT_AUT, p=0.8, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
      feats <- strsplit(c1$Grupo, ",\\s*")[[1]]
      
      
      # Make predictions, this is where the threshold should change
      results <- rbind(results, makePreds(feats, train, test, threshold))
    }
    
    if(case == 2) {
      case2 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(case2$POT_AUT, p=0.8, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[-train_idx, ])
      feats <- strsplit(c2$Grupo, ",\\s*")[[1]]
      
      # Make predictions, this is where the threshold should change
      results <- rbind(results, makePreds(feats, train, test, threshold))
      
    }
    
    if(case == 3) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t2$POT_AUT, p=0.8, list=FALSE)
      test_idx <- createDataPartition(t6$POT_AUT, p=0.2, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
      feats <- strsplit(c3$Grupo, ",\\s*")[[1]]
      
      
      # Make predictions, this is where the threshold should change
      results <- rbind(results, makePreds(feats, train, test, threshold))
      
    }
    
    if(case == 4) {
      t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
      t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
      train_idx <- createDataPartition(t6$POT_AUT, p=0.8, list=FALSE)
      test_idx <- createDataPartition(t2$POT_AUT, p=0.2, list=FALSE)
      train <- as.data.frame(solar_data[train_idx, ])
      test <- as.data.frame(solar_data[test_idx, ])
      feats <- strsplit(c4$Grupo, ",\\s*")[[1]]
      
      
      # Make predictions, this is where the threshold should change
      results <- rbind(results, makePreds(feats, train, test, threshold))
      
    }
  }




