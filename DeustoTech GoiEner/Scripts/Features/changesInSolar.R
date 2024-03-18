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
archivos <- list.files("SOLAR/Iberdrola Limpio/SOLAR/", full.names = TRUE, pattern = "\\.csv$")

IDs <-  gsub("\\.csv$", "", basename(archivos)) # All the IDs in the solar folder


valPots <- roseta %>% filter(CUPS %in% IDs) %>% select(VAL_POT_AUTORIZADA, CUPS) %>% distinct()
horas <- data.frame(
  hora = 0:23,
  TARIFA_2.0 = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano", "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  ),
  TARIFA_SOLAR = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "solar pico", "solar pico", "solar pico", "solar pico",
    "solar llano", "solar llano",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  )
)

# Functions to compute the features we want to get
summary_functions <- list(ENTROPY = entropy, 
                          AVG = mean, 
                          Q1 = function(x) quantile(x, probs = 0.25, na.rm = TRUE), 
                          MEDIAN = median, 
                          Q3 = function(x) quantile(x, probs = 0.75, na.rm = TRUE), 
                          VAR = var, 
                          SD = sd,
                          TOTAL = sum,
                          T2.0_VALLE = function(x) sum(x[horas$TARIFA_2.0 == "valle"]),
                          T2.0_LLANO = function(x) sum(x[horas$TARIFA_2.0 == "llano"]),
                          T2.0_PICO = function(x) sum(x[horas$TARIFA_2.0 == "pico"]),
                          T_SOLAR_SLLANO = function(x) sum(x[horas$TARIFA_2.0 == "llano"]),
                          T_SOLAR_PICO = function(x) sum(x[horas$TARIFA_2.0 == "pico"]),
                          T_SOLAR_SPICO = function(x) sum(x[horas$TARIFA_2.0 == "solar pico"]),
                          T_SOLAR_SLLANO = function(x) sum(x[horas$TARIFA_2.0 == "solar llano"]))



# Weekly 
{

getWeeklyFeature <- function(serie, val_pot, summary_function){
  serie$hour <- hour(serie$timestamp)
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
  fwrite(weeklyDf, paste("SOLAR/Variation/Weekly/Weekly_",funcname, ".csv", sep =""))
}
}


# Daily
{
  getDailyFeature <- function(serie, val_pot, summary_function){
    serie$hour <- hour(serie$timestamp)
    start_date <- min(serie$timestamp)
    
    
    serie$day <- ceiling(as.numeric(difftime(serie$timestamp, start_date, units = "days")))
    
    serie$VAL_AI <- serie$VAL_AI / val_pot # Normalize the consumption
    
    
    dailyValues <- serie %>% group_by(day) %>%
      summarise(Value = summary_function(VAL_AI))
    
    return(dailyValues)
  }
  
  
  
  # Run this code to get the max number of weeks in the time series
  {
    max_days <- 0
    
    for (archivo in archivos) {
      serie <- fread(archivo)
      num_days <- ndays(serie$timestamp)
      
      if (num_days > max_days) {
        max_days <- num_days
      }
    }
    
  }
  
  
  
  for (funcname in names(summary_functions)){
    
    func <- summary_functions[[funcname]]
    dailyDf <- data.frame(matrix(NA, nrow = max_days, ncol = length(IDs)))
    colnames(dailyDf) <- IDs
    
    for (archivo in archivos) {
      # Leer el archivo y obtener su ID
      serie <- fread(archivo)
      id_serie <- gsub("\\.csv$", "", basename(archivo))
      
      
      # Obtener el valor potencial autorizado para esa serie
      vs <- valPots$VAL_POT_AUTORIZADA[which(valPots$CUPS == id_serie)]
      
      # Calcular las características semanales
      dailyValues <- getDailyFeature(serie, vs, summary_function = func)
      
      dailyDf[1:nrow(dailyValues), id_serie] <- dailyValues$Value
    }
    fwrite(dailyDf, paste("SOLAR/Variation/Daily/Daily_",funcname, ".csv", sep =""))
  }
}



# Monthly
{

getMonthlyFeature <- function(serie, val_pot, summary_function){
  serie$hour <- hour(serie$timestamp)
  start_date <- min(serie$timestamp)
  
  # Week function repeats week TS is longer than a year, so we calculate the number of weeks by hand
  serie$month <- ceiling(as.numeric(difftime(serie$timestamp, start_date, units = "days")) / 30)
  
  serie$VAL_AI <- serie$VAL_AI / val_pot # Normalize the consumption
  
  
  monthlyValues <- serie %>% group_by(month) %>%
    summarise(Value = summary_function(VAL_AI))
  
  return(monthlyValues)
}


# Run this code to get the max number of weeks in the time series
{
  max_months <- 0
  
  for (archivo in archivos) {
    serie <- fread(archivo)
    num_months <- nmonths(serie$timestamp)
    
    if (num_months > max_months) {
      max_months <- num_months
    }
  }
  
}





for (funcname in names(summary_functions)){
  
  func <- summary_functions[[funcname]]
  monthlyDf <- data.frame(matrix(NA, nrow = max_months, ncol = length(IDs)))
  colnames(monthlyDf) <- IDs
  
  for (archivo in archivos) {
    # Leer el archivo y obtener su ID
    serie <- fread(archivo)
    id_serie <- gsub("\\.csv$", "", basename(archivo))
    
    # Obtener el valor potencial autorizado para esa serie
    vs <- valPots$VAL_POT_AUTORIZADA[which(valPots$CUPS == id_serie)]
    
    # Calcular las características semanales
    monthlyValues <- getMonthlyFeature(serie, vs, summary_function = func)
    
    monthlyDf[1:nrow(monthlyValues), id_serie] <- monthlyValues$Value
  }
  fwrite(monthlyDf, paste("SOLAR/Variation/Monthly/Monthly_",funcname, ".csv", sep =""))
}

}
