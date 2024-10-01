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
c1 <- read.csv("SOLAR/Regresion/Top/case1_top.csv") %>% arrange(desc(RMSE_rf))  %>% select(Grupo)
c1_list <- lapply(c1$Grupo, function(x) unlist(strsplit(x, ",\\s*")))

c2 <- read.csv("SOLAR/Regresion/Top/case2_top.csv") %>% arrange(desc(RMSE_rf))  %>% select(Grupo)
c2_list <- lapply(c2$Grupo, function(x) unlist(strsplit(x, ",\\s*")))

c3 <- read.csv("SOLAR/Regresion/Top/case3_top.csv") %>% arrange(desc(RMSE_rf))  %>% select(Grupo)
c3_list <- lapply(c3$Grupo, function(x) unlist(strsplit(x, ",\\s*")))

c4 <- read.csv("SOLAR/Regresion/Top/case4_top.csv") %>% arrange(desc(RMSE_rf))  %>% select(Grupo)
c4_list <- lapply(c4$Grupo, function(x) unlist(strsplit(x, ",\\s*")))
}



# 100 times the best 30 of each case
# IN THIS LOOP, PERMUTATIONS TAKES THE VALUE OF Cn_list depending on the case
{
with_progress({

final_results_100_iters <- foreach(iteration = 1:100, .combine = rbind, 
                         .options.future = list(globals = globalvars,add = TRUE,
                                                packages = librerias, seed = TRUE)) %dofuture% {
    case_progress <- progressr::progressor(steps = length(cases), message = "Processing cases")
                                                  
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
                 RMSE_lm = metrics[1],
                 RMSE_rf = metrics[2],
                 RMSE_gbm = metrics[3],
                 Train_test_Case = case,
                 Iteration = iteration)
    }
  }
}
})
}
# TODO fwrite final results
fwrite(final_results_100_iters, "SOLAR/Regresion/Top/Top30.csv")


# ONLY CASE 3 BEST COMBINATION WITH RF

{
train <- data.frame()
test <- data.frame()

# Initialize an empty data frame to store results
results <- data.frame(Iteration = integer(),
                      Actual = numeric(),
                      Predictions = numeric(),
                      RMSE = numeric())

for(it in 1:100) {
  # Filter data
  t2 <- solar_data %>% filter(TarifCode != "96T1" & TarifCode != "97T2")
  t6 <- solar_data %>% filter(TarifCode == "96T1" | TarifCode == "97T2")
  
  # Train and test index creation
  train_idx <- createDataPartition(t2$POT_AUT, p = 0.8, list = FALSE)
  test_idx <- createDataPartition(t6$POT_AUT, p = 0.2, list = FALSE)
  
  # Training and testing sets
  train <- as.data.frame(solar_data[train_idx, ])
  test <- as.data.frame(solar_data[test_idx, ])
  
  # Select the features
  permutations <- c(c3_list[[28]])  # SD AND ENERGY
  
  # Prepare the train and test sets for modeling
  train_labels <- train$POT_AUT
  train <- train %>% select(all_of(permutations), POT_AUT)
  test_labels <- test$POT_AUT
  
  # Random Forest model
  model_rf <- randomForest(POT_AUT ~ ., data = train, ntree = 100)
  predictions_rf <- predict(model_rf, newdata = test)
  
  # Calculate RMSE for this iteration
  rmse_value <- sqrt(mean((test_labels - predictions_rf)^2))
  
  # Store predictions, actuals, and RMSE for each observation in the test set
  iter_results <- data.frame(
    Iteration = rep(it, length(test_labels)),  # Replicate the iteration number
    Actual = test_labels,                      # Actual values
    Predictions = predictions_rf,              # Predictions
    RMSE = rep(rmse_value, length(test_labels)) # RMSE for each observation
  )
  
  # Append the results of this iteration to the final results data frame
  results <- rbind(results, iter_results)
}

# View the first few rows of the results
head(results)

results$residuals <- results$Actual - results$Predictions

fwrite(results, "SOLAR/Regresion/case3_100times.csv")

ggplot(results, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linetype = "dashed") +  # Add a reference line
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()  # Use a clean theme



ggplot(results, aes(x = Predictions, y = Actual)) +
     geom_point(alpha = 0.5, color = "blue") +  # Scatter plot with some transparency
     geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 1:1 line
     labs(title = "Actual vs Predicted Plot",
                   x = "Predicted Values",
                   y = "Actual Values") +
     theme_minimal()  # Use a clean theme

# PP plot

results <- results %>%
  arrange(Predictions) %>%
  mutate(
    Prob_Actual = cumsum(Actual) / sum(Actual),  # Probabilidad acumulada de valores reales
    Prob_Predicted = cumsum(Predictions) / sum(Predictions)  # Probabilidad acumulada de predicciones
  )


ggplot(results, aes(x = Prob_Predicted, y = Prob_Actual)) +
  geom_line(color = "blue") +  # Línea de predicciones
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Línea de referencia
  labs(title = "PP Plot: Probabilidades Reales vs. Predicciones",
       x = "Probabilidades Predichas",
       y = "Probabilidades Reales") +
  theme_minimal()  # Usar un tema limpio

}

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

# GENERAL CASE: TRAIN AND TEST WITH ALL DATAPOINTS USING SD, ENERGY
{
  
  global_results <- data.frame(Actual = numeric(),
                        Predictions = numeric(),
                        RMSE = numeric())
  
  
    general_data <- solar_data %>% select(POT_AUT, SD, ENERGY)


    # Prepare the train and test sets for modeling
    labels <- general_data$POT_AUT
    
    # Random Forest model
    model_rf <- randomForest(POT_AUT ~ ., data = general_data, ntree = 100)
    predictions_rf <- predict(model_rf, newdata = general_data)
    
    # Calculate RMSE for this iteration
    rmse_values <- sqrt((labels - predictions_rf)^2)
    
    # Store predictions, actuals, and RMSE for each observation in the test set
    global_results <- data.frame(
      Actual = labels,                      # Actual values
      Predictions = predictions_rf,              # Predictions
      RMSE = rmse_values                    # RMSE por observación
    )
    fwrite(global_results, "SOLAR/Regresion/GENERAL_MODEL.csv")


    # Crear el gráfico con un filtro directo en ggplot
    ggplot(global_results, aes(x = Predictions, y = Actual)) +
      geom_point(data = subset(global_results,Actual < 10), 
                 alpha = 0.5, color = "blue") +  # Scatter plot con transparencia
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Línea 1:1
      labs(title = "Actual vs Predicted Plot",
           x = "Predicted Values",
           y = "Actual Values") +
      theme_minimal()  # Usar un tema limpio
    
    
}

