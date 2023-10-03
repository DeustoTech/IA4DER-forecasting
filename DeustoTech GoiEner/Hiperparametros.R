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

hora0 <- ts1 %>% filter(hour(timestamp) == 16)


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

# Ahora con CV 

lab <- hora0 %>% filter(TipoDia == "Laborable")
errors <- tsCV(lab$kWh, forecastNN, h = 1, window = 7, n = 4) %>% na.omit()
actual <- lab$kWh[1: length(errors)]


predicted <- actual + errors

# Calcular métricas
smape <- smape(actual, predicted)
rmse <- rmse(actual, predicted)


resultado0lab <- tibble(
  nNeuronas = numeric(),
  sMAPE = numeric(),
  RMSE = numeric()
  # MASE = numeric()
)

# El mase peta

forecastNN <- function(x, h, n){
  prediccion <- forecast(nnetar(x, size = n), h = h)
  return(prediccion)
}


foreach( numNeurona = 1:20, .packages = librerias) %dopar%{
  
  errors <- tsCV(lab$kWh, forecastNN, h = 1, window = 5, n = numNeurona) %>% na.omit()
  actual <- lab$kWh[1: length(errors)]
  
  predicted <- actual + errors
  
  
  smape <- smape(actual, predicted)
  rmse <- rmse(actual, predicted)
  # mase <- mase(actual, predicted)
  
  resultado0lab <<- resultado0lab %>% add_row(
    nNeuronas = numNeurona,
    sMAPE = smape,
    RMSE = rmse,
  )
  
}
 # IMPORTANTE, NO SE VEN LOS RESULTADOS HASTA QUE ACABE. NO SE PUEDE PARAR


# Ahora tenemos que intentar con todos los CSV (mejor usar aleatorios)

horas <- 0:23
neuronas <- c(1, seq(5, 100, by = 5)) 

resultadosLaborable <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  numNeuronas = numeric()
) 


# Prueba con los laborables

RedNeuronalLaborable <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp)) %>%
    mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%
    select(-imputed)
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias) %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable")
    # Crear un tsibble para la hora actual
    ts1 <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 

    foreach( numNeurona = neuronas, .packages = librerias) %dopar%{
      
    
    errors <- tsCV(ts1$kWh, forecastNN, h = 1, window = 3, n = numNeurona) %>% na.omit()
    actual <- ts1$kWh[1: length(errors)]
    
    # Calculamos la prediccion haciendo real + error
    predicted <- actual + errors
    
    # Calcular métricas
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    # mase <- mase(actual, predicted)
    
    # Almacenar los resultados en el tibble para ETS
    
    resultadosLaborable <<- resultadosLaborable %>% add_row(
      Hora = hora,
      Predicted = predicted,
      # MASE = mase,
      sMAPE = smape,
      RMSE = rmse,
      nNeuronas = numNeurona,
    )
    }
  }
}

foreach(csv_file = csv_files,
        .packages = librerias) %dopar% RedNeuronalLaborable(csv_file)


