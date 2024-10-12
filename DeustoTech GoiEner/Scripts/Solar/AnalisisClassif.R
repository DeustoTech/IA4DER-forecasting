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

classif <- read.csv("SOLAR/Classification/Models/allClassif.csv")

# Create a new column 'numFeats' by counting commas + 1
classif$numFeats <- sapply(classif$Grupo, function(x) {
  num_commas <- str_count(x, ",")
  return(num_commas + 1)
})

# CLASSIFICATION
{
classif_sorted <- classif %>% 
  arrange(numFeats, desc(rf_accuracy))


# ALL COMBINATIONS
case1_all <- classif %>% filter(Train_test_Case == 1)
case2_all <- classif %>% filter(Train_test_Case == 2)
case3_all <- classif %>% filter(Train_test_Case == 3)
case4_all <- classif %>% filter(Train_test_Case == 4)


fwrite(case1_all, "SOLAR/Classification/Models/case1_all.csv")
fwrite(case2_all, "SOLAR/Classification/Models/case2_all.csv")
fwrite(case3_all, "SOLAR/Classification/Models/case3_all.csv")
fwrite(case4_all, "SOLAR/Classification/Models/case4_all.csv")


# MEJOR GRUPO EN GENERAL

# Combina los cuatro data frames en uno solo
all_cases <- bind_rows(case1_all, case2_all, case3_all, case4_all)

# Calcula el promedio de rf_accuracy para cada grupo en los cuatro casos
group_performance <- all_cases %>%
  group_by(Grupo) %>%
  summarise(avg_rf_accuracy = mean(rf_accuracy, na.rm = TRUE)) %>%
  arrange(desc(avg_rf_accuracy))

# Muestra el grupo con el promedio de rf_accuracy más alto
best_group <- group_performance %>% slice(1)

# Ver los resultados
print(group_performance)
print(best_group) 

# AUNQUE NO SEA EL MEJOR, VOY A USAR ZERO SD Y MIN PORQUE SON MENOS FEATURES

fwrite(group_performance, "SOLAR/Classification/Models/Group_performance.csv")


# PLOTS

max_yaxis <- max(classif$rf_accuracy, na.rm = TRUE)  # Use na.rm = TRUE to handle any NA values


acc_all <- ggplot(classif, aes(x = factor(Train_test_Case), y = rf_accuracy)) +
  geom_boxplot() +
  labs(title = "Accuracy (rf) for each Case",
       x = "Case",
       y = "RF Accuracy") + ylim(0, max_yaxis)

ggsave(
  filename = "SOLAR/Classification/Models/Plots/Accuracy all cases.png",  # File name
  plot = acc_all,                        # The plot object
  width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
  height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
  dpi = 200                            # Resolution in DPI (Dots Per Inch)
)



# Case 1
{
  p1 <- ggplot(case1_all, aes(x = as.factor(numFeats), y = rf_accuracy)) +
    geom_boxplot() +
    labs(title = "Case 1: Train t2 test t2",
         x = "Number of Features used",
         y = "Accuracy") + ylim(0, max_yaxis)
  ggsave(
    filename = "SOLAR/Classification/Models/Plots/Case1.png",  # File name
    plot = p1,                        # The plot object
    width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
    height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
    dpi = 200                            # Resolution in DPI (Dots Per Inch)
  )
  
  
  
  summary_c1 <- case1_all %>%
    group_by(numFeats) %>%
    summarise(                      
      mean_acc_rf = mean(rf_accuracy, na.rm = TRUE),  # Mean of rf_accuracy
      median_acc_rf = median(rf_accuracy, na.rm = TRUE), # Median of rf_accuracy
      min_acc_rf = min(rf_accuracy, na.rm = TRUE),     # Minimum rf_accuracy
      max_acc_rf = max(rf_accuracy, na.rm = TRUE),     # Maximum rf_accuracy
      sd_acc_rf = sd(rf_accuracy, na.rm = TRUE)       # Standard deviation of rf_accuracy
    ) %>% as.data.frame()
  
  fwrite(summary_c1, "SOLAR/Classification/Models/Summaries/Summary_case1.csv")
}

# Case 2
{
  p2 <- ggplot(case2_all, aes(x = as.factor(numFeats), y = rf_accuracy)) +
    geom_boxplot() +
    labs(title = "Case 2: Train t6 test t6",
         x = "Number of Features used",
         y = "Accuracy") + ylim(0, max_yaxis)
  ggsave(
    filename = "SOLAR/Classification/Models/Plots/Case2.png",  # File name
    plot = p2,                        # The plot object
    width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
    height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
    dpi = 200                            # Resolution in DPI (Dots Per Inch)
  )
  
  
  
  summary_c2 <- case2_all %>%
    group_by(numFeats) %>%
    summarise(                      
      mean_acc_rf = mean(rf_accuracy, na.rm = TRUE),  # Mean of rf_accuracy
      median_acc_rf = median(rf_accuracy, na.rm = TRUE), # Median of rf_accuracy
      min_acc_rf = min(rf_accuracy, na.rm = TRUE),     # Minimum rf_accuracy
      max_acc_rf = max(rf_accuracy, na.rm = TRUE),     # Maximum rf_accuracy
      sd_acc_rf = sd(rf_accuracy, na.rm = TRUE)       # Standard deviation of rf_accuracy
    ) %>% as.data.frame()
  
  fwrite(summary_c2, "SOLAR/Classification/Models/Summaries/Summary_case2.csv")
}

# Case 3
{
  p3 <- ggplot(case3_all, aes(x = as.factor(numFeats), y = rf_accuracy)) +
    geom_boxplot() +
    labs(title = "Case 3: Train t2 test t6",
         x = "Number of Features used",
         y = "Accuracy") + ylim(0, max_yaxis)
  ggsave(
    filename = "SOLAR/Classification/Models/Plots/Case3.png",  # File name
    plot = p3,                        # The plot object
    width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
    height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
    dpi = 200                            # Resolution in DPI (Dots Per Inch)
  )
  
  
  
  summary_c3 <- case3_all %>%
    group_by(numFeats) %>%
    summarise(                      
      mean_acc_rf = mean(rf_accuracy, na.rm = TRUE),  # Mean of rf_accuracy
      median_acc_rf = median(rf_accuracy, na.rm = TRUE), # Median of rf_accuracy
      min_acc_rf = min(rf_accuracy, na.rm = TRUE),     # Minimum rf_accuracy
      max_acc_rf = max(rf_accuracy, na.rm = TRUE),     # Maximum rf_accuracy
      sd_acc_rf = sd(rf_accuracy, na.rm = TRUE)       # Standard deviation of rf_accuracy
    ) %>% as.data.frame()
  
  fwrite(summary_c3, "SOLAR/Classification/Models/Summaries/Summary_case3.csv")
}

# Case 4
{
  {
    p4 <- ggplot(case4_all, aes(x = as.factor(numFeats), y = rf_accuracy)) +
      geom_boxplot() +
      labs(title = "Case 4: Train t6 test t2",
           x = "Number of Features used",
           y = "Accuracy") + ylim(0, max_yaxis)
    ggsave(
      filename = "SOLAR/Classification/Models/Plots/Case4.png",  # File name
      plot = p4,                        # The plot object
      width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
      height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
      dpi = 200                            # Resolution in DPI (Dots Per Inch)
    )
    
    
    
    summary_c4 <- case4_all %>%
      group_by(numFeats) %>%
      summarise(                      
        mean_acc_rf = mean(rf_accuracy, na.rm = TRUE),  # Mean of rf_accuracy
        median_acc_rf = median(rf_accuracy, na.rm = TRUE), # Median of rf_accuracy
        min_acc_rf = min(rf_accuracy, na.rm = TRUE),     # Minimum rf_accuracy
        max_acc_rf = max(rf_accuracy, na.rm = TRUE),     # Maximum rf_accuracy
        sd_acc_rf = sd(rf_accuracy, na.rm = TRUE)       # Standard deviation of rf_accuracy
      ) %>% as.data.frame()
    
    fwrite(summary_c4, "SOLAR/Classification/Models/Summaries/Summary_case4.csv")
  }
}

# AUC GRAPHS
{
  plot_auc_accuracy <- function(data, case_num, title) {
    data <- data %>% arrange(desc(rf_accuracy)) # Sort by accuracy
    
    ggplot(data, aes(x = seq_along(rf_accuracy), y = rf_accuracy)) +
      geom_line(color = "blue", size = 1) +               # Line plot for accuracy
      geom_area(fill = "skyblue", alpha = 0.4) +          # Shaded area under the curve
      labs(
        title = title,
        x = "Observations",
        y = "RF Accuracy"
      ) +
      theme_minimal() +                                   # Clean theme
      theme(plot.title = element_text(hjust = 0.5))       # Center the plot title
  }
  
  # Generate plots for each case
  p1 <- plot_auc_accuracy(case1_all, 1, "AUC-like Graph for Case 1: Train t2 test t2")
  p2 <- plot_auc_accuracy(case2_all, 2, "AUC-like Graph for Case 2: Train t6 test t6")
  p3 <- plot_auc_accuracy(case3_all, 3, "AUC-like Graph for Case 3: Train t2 test t6")
  p4 <- plot_auc_accuracy(case4_all, 4, "AUC-like Graph for Case 4: Train t6 test t2")
  
  # Display plots
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
  # Save plots as PNG
  ggsave(filename = "SOLAR/Classification/Models/Plots/AUC_Case1_Accuracy.png", plot = p1, width = 833 / 72, height = 761 / 72, dpi = 200)
  ggsave(filename = "SOLAR/Classification/Models/Plots/AUC_Case2_Accuracy.png", plot = p2, width = 833 / 72, height = 761 / 72, dpi = 200)
  ggsave(filename = "SOLAR/Classification/Models/Plots/AUC_Case3_Accuracy.png", plot = p3, width = 833 / 72, height = 761 / 72, dpi = 200)
  ggsave(filename = "SOLAR/Classification/Models/Plots/AUC_Case4_Accuracy.png", plot = p4, width = 833 / 72, height = 761 / 72, dpi = 200)
}




}

# SENSITIVITY

{
classif_sorted <- classif %>% 
  arrange(numFeats, desc(rf_sensitivity))


# ALL COMBINATIONS
case1_all <- classif %>% filter(Train_test_Case == 1)
case2_all <- classif %>% filter(Train_test_Case == 2)
case3_all <- classif %>% filter(Train_test_Case == 3)
case4_all <- classif %>% filter(Train_test_Case == 4)


# PLOTS

max_yaxis <- max(classif$rf_sensitivity, na.rm = TRUE)  # Use na.rm = TRUE to handle any NA values


sensi_all <- ggplot(classif, aes(x = factor(Train_test_Case), y = rf_sensitivity)) +
  geom_boxplot() +
  labs(title = "Sensitivity (rf) for each Case",
       x = "Case",
       y = "RF Sensitivity") + ylim(0, max_yaxis)

ggsave(
  filename = "SOLAR/Classification/Models/Plots/Sensitivity all cases.png",  # File name
  plot = sensi_all,                        # The plot object
  width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
  height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
  dpi = 200                            # Resolution in DPI (Dots Per Inch)
)



# Case 1
{
  p1 <- ggplot(case1_all, aes(x = as.factor(numFeats), y = rf_sensitivity)) +
    geom_boxplot() +
    labs(title = "Case 1: Train t2 test t2",
         x = "Number of Features used",
         y = "Sensitivity") + ylim(0, max_yaxis)
  ggsave(
    filename = "SOLAR/Classification/Models/Plots/Case1_sensi.png",  # File name
    plot = p1,                        # The plot object
    width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
    height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
    dpi = 200                            # Resolution in DPI (Dots Per Inch)
  )
  
  
}

# Case 2
{
  p2 <- ggplot(case2_all, aes(x = as.factor(numFeats), y = rf_sensitivity)) +
    geom_boxplot() +
    labs(title = "Case 2: Train t6 test t6",
         x = "Number of Features used",
         y = "Sensitivity") + ylim(0, max_yaxis)
  ggsave(
    filename = "SOLAR/Classification/Models/Plots/Case2_sensi.png",  # File name
    plot = p2,                        # The plot object
    width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
    height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
    dpi = 200                            # Resolution in DPI (Dots Per Inch)
  )
  
}

# Case 3
{
  p3 <- ggplot(case3_all, aes(x = as.factor(numFeats), y = rf_sensitivity)) +
    geom_boxplot() +
    labs(title = "Case 3: Train t2 test t6",
         x = "Number of Features used",
         y = "Sensitivity") + ylim(0, max_yaxis)
  ggsave(
    filename = "SOLAR/Classification/Models/Plots/Case3_sensi.png",  # File name
    plot = p3,                        # The plot object
    width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
    height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
    dpi = 200                            # Resolution in DPI (Dots Per Inch)
  )
  
}

# Case 4
  {
    p4 <- ggplot(case4_all, aes(x = as.factor(numFeats), y = rf_sensitivity)) +
      geom_boxplot() +
      labs(title = "Case 4: Train t6 test t2",
           x = "Number of Features used",
           y = "Sensitivity") + ylim(0, max_yaxis)
    ggsave(
      filename = "SOLAR/Classification/Models/Plots/Case4_sensi.png",  # File name
      plot = p4,                        # The plot object
      width = 833 / 72,                   # Width in inches (833 pixels / 72 DPI)
      height = 761 / 72,                  # Height in inches (761 pixels / 72 DPI)
      dpi = 200                            # Resolution in DPI (Dots Per Inch)
    )
  }
} 


# SPECIFICITY

{
  # Calculate the maximum y-axis value for rf_specificity
  max_yaxis_specificity <- max(classif$rf_specificity, na.rm = TRUE)  # Use na.rm = TRUE to handle any NA values
  
  # Plot: Sensitivity (rf) for each Case
  sensi_all_specificity <- ggplot(classif, aes(x = factor(Train_test_Case), y = rf_specificity)) +
    geom_boxplot() +
    labs(
      title = "Specificity (rf) for each Case",
      x = "Case",
      y = "RF Specificity"
    ) + ylim(0, max_yaxis_specificity)
  
  # Save the overall specificity plot
  ggsave(
    filename = "SOLAR/Classification/Models/Plots/Specificity_all_cases.png",  # File name
    plot = sensi_all_specificity,           # The plot object
    width = 833 / 72,                       # Width in inches (833 pixels / 72 DPI)
    height = 761 / 72,                      # Height in inches (761 pixels / 72 DPI)
    dpi = 200                               # Resolution in DPI (Dots Per Inch)
  )
  
  # Case 1
  {
    p1_specificity <- ggplot(case1_all, aes(x = as.factor(numFeats), y = rf_specificity)) +
      geom_boxplot() +
      labs(
        title = "Case 1: Train t2 test t2",
        x = "Number of Features used",
        y = "Specificity"
      ) + ylim(0, max_yaxis_specificity)
    
    ggsave(
      filename = "SOLAR/Classification/Models/Plots/Case1_specificity.png",  # File name
      plot = p1_specificity,                 # The plot object
      width = 833 / 72,                      # Width in inches (833 pixels / 72 DPI)
      height = 761 / 72,                     # Height in inches (761 pixels / 72 DPI)
      dpi = 200                              # Resolution in DPI (Dots Per Inch)
    )
  }
  
  # Case 2
  {
    p2_specificity <- ggplot(case2_all, aes(x = as.factor(numFeats), y = rf_specificity)) +
      geom_boxplot() +
      labs(
        title = "Case 2: Train t6 test t6",
        x = "Number of Features used",
        y = "Specificity"
      ) + ylim(0, max_yaxis_specificity)
    
    ggsave(
      filename = "SOLAR/Classification/Models/Plots/Case2_specificity.png",  # File name
      plot = p2_specificity,                 # The plot object
      width = 833 / 72,                      # Width in inches (833 pixels / 72 DPI)
      height = 761 / 72,                     # Height in inches (761 pixels / 72 DPI)
      dpi = 200                              # Resolution in DPI (Dots Per Inch)
    )
  }
  
  # Case 3
  {
    p3_specificity <- ggplot(case3_all, aes(x = as.factor(numFeats), y = rf_specificity)) +
      geom_boxplot() +
      labs(
        title = "Case 3: Train t2 test t6",
        x = "Number of Features used",
        y = "Specificity"
      ) + ylim(0, max_yaxis_specificity)
    
    ggsave(
      filename = "SOLAR/Classification/Models/Plots/Case3_specificity.png",  # File name
      plot = p3_specificity,                 # The plot object
      width = 833 / 72,                      # Width in inches (833 pixels / 72 DPI)
      height = 761 / 72,                     # Height in inches (761 pixels / 72 DPI)
      dpi = 200                              # Resolution in DPI (Dots Per Inch)
    )
  }
  
  # Case 4
  {
    p4_specificity <- ggplot(case4_all, aes(x = as.factor(numFeats), y = rf_specificity)) +
      geom_boxplot() +
      labs(
        title = "Case 4: Train t6 test t2",
        x = "Number of Features used",
        y = "Specificity"
      ) + ylim(0, max_yaxis_specificity)
    
    ggsave(
      filename = "SOLAR/Classification/Models/Plots/Case4_specificity.png",  # File name
      plot = p4_specificity,                 # The plot object
      width = 833 / 72,                      # Width in inches (833 pixels / 72 DPI)
      height = 761 / 72,                     # Height in inches (761 pixels / 72 DPI)
      dpi = 200                              # Resolution in DPI (Dots Per Inch)
    )
  }
  
}




# ONLY THE BEST (FIRST OBSERVATION FOR EACH COMBINATION)

{
  regr_first_obs <- regr %>%
    group_by(Train_test_Case, numFeats) %>%
    slice_min(order_by = RMSE_rf, with_ties = FALSE) %>% # Selects the row with the minimum RMSE_rf
    ungroup()  # Remove grouping after selection
  
  case1best <- regr_first_obs %>% filter(Train_test_Case == 1)
  case2best <- regr_first_obs %>% filter(Train_test_Case == 2)
  case3best <- regr_first_obs %>% filter(Train_test_Case == 3)
  case4best <- regr_first_obs %>% filter(Train_test_Case == 4)
  
  
  
  fwrite(case1best, "SOLAR/Regresion/Best/case1_best.csv")
  fwrite(case2best, "SOLAR/Regresion/Best/case2_best.csv")
  fwrite(case3best, "SOLAR/Regresion/Best/case3_best.csv")
  fwrite(case4best, "SOLAR/Regresion/Best/case4_best.csv")
  
  
}


# BEST 30 COMBINATIONS OF EACH CASE TO THEN EXECUTE 100 TIMES 
{
  case1_top <- case1_all %>% head(30)
  case2_top <- case2_all %>% head(30)
  case3_top <- case3_all %>% head(30)
  case4_top <- case4_all %>% head(30)
  
  
  
  fwrite(case1_top, "SOLAR/Regresion/Top/case1_top.csv")
  fwrite(case2_top, "SOLAR/Regresion/Top/case2_top.csv")
  fwrite(case3_top, "SOLAR/Regresion/Top/case3_top.csv")
  fwrite(case4_top, "SOLAR/Regresion/Top/case4_top.csv")
  
  
}


# RESULTS AFTER 100 EXECUTIONS OF EACH 30 BEST COMBINATIONS
{
  top <- read.csv("SOLAR/Regresion/Top/Top30.csv")
  top$numFeats <- sapply(top$Grupo, function(x) {
    num_commas <- str_count(x, ",")
    return(num_commas + 1)
  })
  
  c1 <- top %>% filter(Train_test_Case == 1)
  c2 <- top %>% filter(Train_test_Case == 2)
  c3 <- top %>% filter(Train_test_Case == 3)
  c4 <- top %>% filter(Train_test_Case == 4)
  
  # SUMMARIES 
  {
    c1_summary <- c1 %>%
      group_by(Grupo) %>%
      summarize(
        mean_MAPE_rf = mean(RMSE_rf, na.rm = TRUE),  # Mean of RMSE_rf
        median_MAPE_rf = median(RMSE_rf, na.rm = TRUE),  # Median of RMSE_rf
        sd_MAPE_rf = sd(RMSE_rf, na.rm = TRUE),  # Standard deviation of RMSE_rf
        min_MAPE_rf = min(RMSE_rf, na.rm = TRUE),  # Minimum RMSE_rf
        q1_MAPE_rf = quantile(RMSE_rf, 0.25, na.rm = TRUE),  # First quartile (Q1)
        q3_MAPE_rf = quantile(RMSE_rf, 0.75, na.rm = TRUE),  # Third quartile (Q3)
        max_MAPE_rf = max(RMSE_rf, na.rm = TRUE),  # Maximum RMSE_rf
        Train_test_Case = 1
      )
    
    c2_summary <- c2 %>%
      group_by(Grupo) %>%
      summarize(
        mean_RMSE_rf = mean(RMSE_rf, na.rm = TRUE),  # Mean of RMSE_rf
        median_RMSE_rf = median(RMSE_rf, na.rm = TRUE),  # Median of RMSE_rf
        sd_RMSE_rf = sd(RMSE_rf, na.rm = TRUE),  # Standard deviation of RMSE_rf
        min_RMSE_rf = min(RMSE_rf, na.rm = TRUE),  # Minimum RMSE_rf
        q1_RMSE_rf = quantile(RMSE_rf, 0.25, na.rm = TRUE),  # First quartile (Q1)
        q3_RMSE_rf = quantile(RMSE_rf, 0.75, na.rm = TRUE),  # Third quartile (Q3)
        max_RMSE_rf = max(RMSE_rf, na.rm = TRUE),  # Maximum RMSE_rf
        Train_test_Case = 2
      )
    
    c3_summary <- c3 %>%
      group_by(Grupo) %>%
      summarize(
        mean_RMSE_rf = mean(RMSE_rf, na.rm = TRUE),  # Mean of RMSE_rf
        median_RMSE_rf = median(RMSE_rf, na.rm = TRUE),  # Median of RMSE_rf
        sd_RMSE_rf = sd(RMSE_rf, na.rm = TRUE),  # Standard deviation of RMSE_rf
        min_RMSE_rf = min(RMSE_rf, na.rm = TRUE),  # Minimum RMSE_rf
        q1_RMSE_rf = quantile(RMSE_rf, 0.25, na.rm = TRUE),  # First quartile (Q1)
        q3_RMSE_rf = quantile(RMSE_rf, 0.75, na.rm = TRUE),  # Third quartile (Q3)
        max_RMSE_rf = max(RMSE_rf, na.rm = TRUE),  # Maximum RMSE_rf
        Train_test_Case = 3
      )
    
    c4_summary <- c4 %>%
      group_by(Grupo) %>%
      summarize(
        mean_RMSE_rf = mean(RMSE_rf, na.rm = TRUE),  # Mean of RMSE_rf
        median_RMSE_rf = median(RMSE_rf, na.rm = TRUE),  # Median of RMSE_rf
        sd_RMSE_rf = sd(RMSE_rf, na.rm = TRUE),  # Standard deviation of RMSE_rf
        min_RMSE_rf = min(RMSE_rf, na.rm = TRUE),  # Minimum RMSE_rf
        q1_RMSE_rf = quantile(RMSE_rf, 0.25, na.rm = TRUE),  # First quartile (Q1)
        q3_RMSE_rf = quantile(RMSE_rf, 0.75, na.rm = TRUE),  # Third quartile (Q3)
        max_RMSE_rf = max(RMSE_rf, na.rm = TRUE),  # Maximum RMSE_rf
        Train_test_Case = 4
      )
    
    fwrite(c1_summary, "SOLAR/Regresion/Top/Summaries/Summary_case1.csv")
    fwrite(c2_summary, "SOLAR/Regresion/Top/Summaries/Summary_case2.csv")
    fwrite(c3_summary, "SOLAR/Regresion/Top/Summaries/Summary_case3.csv")
    fwrite(c4_summary, "SOLAR/Regresion/Top/Summaries/Summary_case4.csv")
  }
  
  # PLOTS
  
  
  
  
  
  
}





