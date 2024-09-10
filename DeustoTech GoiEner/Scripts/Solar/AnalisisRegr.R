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

regr <- read.csv("SOLAR/Regresion/RegrSolar.csv")

# Create a new column 'numFeats' by counting commas + 1
regr$numFeats <- sapply(regr$Grupo, function(x) {
  num_commas <- str_count(x, ",")
  return(num_commas + 1)
})
regr_sorted <- regr %>% 
  arrange(numFeats, MAPE_rf)


# ALL COMBINATIONS
case1_all <- regr_sorted %>% filter(Train_test_Case == 1)
case2_all <- regr_sorted %>% filter(Train_test_Case == 2)
case3_all <- regr_sorted %>% filter(Train_test_Case == 3)
case4_all <- regr_sorted %>% filter(Train_test_Case == 4)

# PLOTS

# Case 1
{
ggplot(case1_all, aes(x = as.factor(numFeats), y = MAPE_rf)) +
  geom_boxplot() +
  labs(title = "Case 1: Train t2 test t2",
       x = "Number of Features used",
       y = "MAPE_rf")

summary_c1 <- case1_all %>%
  group_by(numFeats) %>%
  summarise(                      
    mean_MAPE_rf = mean(MAPE_rf, na.rm = TRUE),  # Mean of MAPE_rf
    median_MAPE_rf = median(MAPE_rf, na.rm = TRUE), # Median of MAPE_rf
    min_MAPE_rf = min(MAPE_rf, na.rm = TRUE),     # Minimum MAPE_rf
    max_MAPE_rf = max(MAPE_rf, na.rm = TRUE),     # Maximum MAPE_rf
    sd_MAPE_rf = sd(MAPE_rf, na.rm = TRUE)       # Standard deviation of MAPE_rf
  ) %>% as.data.frame()

 fwrite(summary_c1, "SOLAR/Regresion/All/Summary_case1.csv")
}

# Case 2
{
  ggplot(case2_all, aes(x = as.factor(numFeats), y = MAPE_rf)) +
    geom_boxplot() +
    labs(title = "Case 2: Train t6 test t6",
         x = "Number of Features used",
         y = "MAPE_rf")
  
  summary_c2 <- case2_all %>%
    group_by(numFeats) %>%
    summarise(                      
      mean_MAPE_rf = mean(MAPE_rf, na.rm = TRUE),  # Mean of MAPE_rf
      median_MAPE_rf = median(MAPE_rf, na.rm = TRUE), # Median of MAPE_rf
      min_MAPE_rf = min(MAPE_rf, na.rm = TRUE),     # Minimum MAPE_rf
      max_MAPE_rf = max(MAPE_rf, na.rm = TRUE),     # Maximum MAPE_rf
      sd_MAPE_rf = sd(MAPE_rf, na.rm = TRUE)       # Standard deviation of MAPE_rf
    ) %>% as.data.frame()
  
  fwrite(summary_c2, "SOLAR/Regresion/All/Summaries/Summary_case2.csv")
  
}

# Case 3
{
  ggplot(case3_all, aes(x = as.factor(numFeats), y = MAPE_rf)) +
    geom_boxplot() +
    labs(title = "Case 3: Train t2 test t6",
         x = "Number of Features used",
         y = "MAPE_rf")
  
  summary_c3 <- case3_all %>%
    group_by(numFeats) %>%
    summarise(                      
      mean_MAPE_rf = mean(MAPE_rf, na.rm = TRUE),  # Mean of MAPE_rf
      median_MAPE_rf = median(MAPE_rf, na.rm = TRUE), # Median of MAPE_rf
      min_MAPE_rf = min(MAPE_rf, na.rm = TRUE),     # Minimum MAPE_rf
      max_MAPE_rf = max(MAPE_rf, na.rm = TRUE),     # Maximum MAPE_rf
      sd_MAPE_rf = sd(MAPE_rf, na.rm = TRUE)       # Standard deviation of MAPE_rf
    ) %>% as.data.frame()
  
  fwrite(summary_c3, "SOLAR/Regresion/All/Summaries/Summary_case3.csv")
  
}

# Case 4
{
  ggplot(case4_all, aes(x = as.factor(numFeats), y = MAPE_rf)) +
    geom_boxplot() +
    labs(title = "Case 4: Train t6 test t2",
         x = "Number of Features used",
         y = "MAPE_rf")
  
  summary_c4 <- case4_all %>%
    group_by(numFeats) %>%
    summarise(                      
      mean_MAPE_rf = mean(MAPE_rf, na.rm = TRUE),  # Mean of MAPE_rf
      median_MAPE_rf = median(MAPE_rf, na.rm = TRUE), # Median of MAPE_rf
      min_MAPE_rf = min(MAPE_rf, na.rm = TRUE),     # Minimum MAPE_rf
      max_MAPE_rf = max(MAPE_rf, na.rm = TRUE),     # Maximum MAPE_rf
      sd_MAPE_rf = sd(MAPE_rf, na.rm = TRUE)       # Standard deviation of MAPE_rf
    ) %>% as.data.frame()
  
  fwrite(summary_c4, "SOLAR/Regresion/All/Summaries/Summary_case4.csv")
  
}





# ONLY THE BEST (FIRST OBSERVATION FOR EACH COMBINATION)

{
regr_first_obs <- regr %>%
  group_by(Train_test_Case, numFeats) %>%
  slice_min(order_by = MAPE_rf, with_ties = FALSE) %>% # Selects the row with the minimum MAPE_gbm
  ungroup()  # Remove grouping after selection

case1 <- regr_first_obs %>% filter(Train_test_Case == 1)
case2 <- regr_first_obs %>% filter(Train_test_Case == 2)
case3 <- regr_first_obs %>% filter(Train_test_Case == 3)
case4 <- regr_first_obs %>% filter(Train_test_Case == 4)
}


# BEST 30 COMBINATIONS OF EACH CASE TO THEN EXECUTE 100 TIMES 
{
case1_top <- case1_all %>% head(30)
case2_top <- case2_all %>% head(30)
case3_top <- case3_all %>% head(30)
case4_top <- case4_all %>% head(30)
}
