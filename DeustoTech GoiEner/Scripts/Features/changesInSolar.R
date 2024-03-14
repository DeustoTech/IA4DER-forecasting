# R script for calculating one feature of SOLAR each week, month and day

library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "mice", "tsfeatures",
               "catch22") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

roseta <- fread("SOLAR/roseta.csv")
archivos <- list.files("SOLAR/SOLAR", full.names = TRUE, pattern = "\\.csv$")

IDs <-  gsub("\\.csv$", "", basename(archivos)) # All the IDs in the solar folder


valPots <- roseta %>% filter(CUPS %in% IDs) %>% select(VAL_POT_AUTORIZADA, CUPS) %>% distinct()

summary_functions <- list(ENTROPY = entropy, 
                          AVG = mean, 
                          Q1 = function(x) quantile(x, probs = 0.25, na.rm = TRUE), 
                          MEDIAN = median, 
                          Q3 = function(x) quantile(x, probs = 0.75, na.rm = TRUE), 
                          VAR = var, 
                          SD = sd)




getWeeklyFeature <- function(serie, val_pot, summary_function){
  
  start_date <- min(serie$timestamp)
  
  # Week function repeats week TS is longer than a year, so we calculate the number of weeks by hand
  serie$week <- ceiling(as.numeric(difftime(serie$timestamp, start_date, units = "days")) / 7)
  
  serie$VAL_AI <- serie$VAL_AI / val_pot # Normalize the consumption

  
  weeklyValues <- serie %>% group_by(week) %>%
    summarise(Value = summary_function(VAL_AI))
 
  return(weeklyValues)
}


# Run this code to get the max number of weeks in the time series
{
max_weeks <- 0

for (archivo in archivos) {
  serie <- fread(archivo)
  num_weeks <- nweeks(serie$timestamp)
  
  if (num_weeks > max_weeks) {
    max_weeks <- num_weeks
  }
}

}



# Initizalize df with one column per ID and max_weeks rows (total unique weeks)


for (funcname in names(summary_functions)){
  
  func <- summary_functions[[funcname]]
  weeklyDf <- data.frame(matrix(NA, nrow = max_weeks, ncol = length(IDs)))
  colnames(weeklyDf) <- IDs
  
    for (archivo in archivos) {
    # Leer el archivo y obtener su ID
    serie <- fread(archivo)
    id_serie <- gsub("\\.csv$", "", basename(archivo))
    
    
    # Obtener el valor potencial autorizado para esa serie
    vs <- valPots$VAL_POT_AUTORIZADA[which(valPots$CUPS == id_serie)]
    
    # Calcular las características semanales
    weeklyValues <- getWeeklyFeature(serie, vs, summary_function = func)
  
    weeklyDf[1:nrow(weeklyValues), id_serie] <- weeklyValues$Value
    }
  fwrite(weeklyDf, paste("SOLAR/Variation/Weekly/",funcname, ".csv", sep =""))
}


