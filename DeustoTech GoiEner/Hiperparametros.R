library(foreach)
library(doParallel)

# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', "tidyverse") 
foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


path <- "dataset_red.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina

unzip(path, exdir = tempdir) # descomprime. Tarda un poco


# Lista de archivos CSV en la carpeta extraída

csv_files <- list.files(tempdir, pattern = ".csv$", recursive = T, full.names = F)



# Pruebas con un solo archivo

csv1 <- fread(csv_files[1])
ts1 <- csv1 %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
  select(-imputed) %>% 
  as_tsibble(key = kWh, index = timestamp) %>% arrange(timestamp) %>%
  mutate(Dia = weekdays(timestamp), Hora = hour(timestamp), 
      TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"), "Laborable", "Finde")
)

# Ya tenemos clasificado el tipo de día. Pruebas con una hora concreta

hora0 <- ts1 %>% filter(hour(timestamp) == 0)

# Para empezar, intentamos predecir un dia laborable. 

# Para empezar, usamos solo los ultimos. Cogemos los 6 días anteriores de ese tipo

ultimosLab <- hora0 %>% filter(TipoDia == "Laborable") %>% tail(n = 6)
trainSet <- xts(ultimosLab$kWh[-nrow(ultimosLab)], order.by = ultimosLab$timestamp[-nrow(ultimosLab)])
testSet <- xts(ultimosLab$kWh[nrow(ultimosLab)], order.by = ultimosLab$timestamp[nrow(ultimosLab)])




modelo <- nnetar(trainSet, size = 3)
pred <- forecast(modelo, h = 1)
smape(testSet, as.numeric(pred$mean))
mase(testSet, as.numeric(pred$mean))
mase(as.numeric(testSet), as.numeric(pred$mean))

neuronas <- c(1, seq(5, 100, by = 5)) # numero de neuronas a probar


resultado0lab <- tibble(
  nNeuronas = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric()
)

# El mase peta

foreach( numNeurona = neuronas, .packages = librerias) %dopar%{
  
  modelo <- nnetar(trainSet, size = numNeurona)
  pred <- forecast(modelo, h = 1)
  
  actual <- as.numeric(testSet)
  predicted <- as.numeric(pred$mean)
  
  smape <- smape(actual, predicted)
  rmse <- rmse(actual, predicted)
  # mase <- mase(actual, predicted)
  
  resultado0lab <<- resultado0lab %>% add_row(
    nNeuronas = numNeurona,
    sMAPE = smape,
    RMSE = rmse,
    MASE = mase
  )
  
}

# En estas pruebas tontas, sale que lo mejor son 10 neuronas


horas <- 0:23

foreach(hora = horas, .packages = librerias) %dopar% {
  
}