# Script that takes the time series with PV and without PV and 
# generates a CSV with the columns ID, Installed, When and First installation 
# later we may add power

library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", "tidyverse",  "lubridate", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "mice", "tsfeatures") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

process_solar <- function(file_path) {
  data <- read.csv(file_path, fileEncoding = "UTF-8")
  
 
  ID <-  tools::file_path_sans_ext(basename(file_path))
  first_auto_1 <- data$timestamp[which(data$AUTO == 1)[1]]
  first_ae_nonzero <- data$timestamp[which(data$VAL_AE != 0)[1]]
  
  # Crear un dataframe con la información
  df <- tibble(
    ID = ID,
    InstallationDate = first_auto_1,
    FirstInjection = first_ae_nonzero,
    hasPV = 1
    # maybe add power
  )
  
  return(df)
}

process_solar_no_pv <- function(file_path) {
  ID <- tools::file_path_sans_ext(basename(file_path))
  
  df <- tibble(
    ID = ID,
    InstallationDate = NA,
    FirstInjection = NA,
    hasPV = 0
    # maybe add power
  )
  
  return(df)
}



solar_folder <- "SOLAR/SOLAR/"
solar_no_pv_folder <- "SOLAR/SOLAR_NO_PV/"

solar_data <- list.files(solar_folder, full.names = TRUE) %>%
  map_df(process_solar)
solar_no_pv_data <- list.files(solar_no_pv_folder, full.names = TRUE) %>%
  map_dfr(process_solar_no_pv)

final_df <- bind_rows(solar_data, solar_no_pv_data)
fwrite(final_df, "SOLAR/HasPV.csv")


