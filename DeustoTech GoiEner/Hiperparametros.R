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


# BUCLE PARA PROCESAR EL NUMERO OPTIMO DE NEURONAS

resultadosDia <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  nNeuronas = numeric(),
  TipoDia = character()
)




horas <- 0:23
neuronas <- c(1:4, seq(5, 20, by = 5)) 


cl <- makeCluster(4) 
registerDoParallel(cl)


RedNeuronalDia <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp)) %>%
    mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%
    select(-imputed)
  
  
  datosLab <- csv_actual %>% filter(TipoDia == "Laborable")
  datosFinde <- csv_actual %>% filter(TipoDia == "Finde")
  
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias, .combine = 'c') %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% distinct()
    datosHoraFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% distinct()
    
    
    # Crear un tsibble para la hora actual - Laborable
    ts1Lab <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    # Crear un tsibble para el siguiente día - Finde
    ts1Finde <- datosHoraFinde %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    foreach(numNeurona = neuronas, .packages = librerias) %dopar% {
      # Entrenar el modelo neuronal para días laborables
      errorsLab <- tsCV(ts1Lab$kWh, forecastNN, h = 1, window = 5, n = numNeurona) %>% na.omit()
      actualLab <- ts1Lab$kWh[1: length(errorsLab)]
      predictedLab <- actualLab + errorsLab
      
      # Calcular métricas para días laborables
      smapeLab <- smape(actualLab, predictedLab)
      rmseLab <- rmse(actualLab, predictedLab)
      
      # Almacenar los resultados en el tibble para días laborables
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedLab,
        sMAPE = smapeLab,
        RMSE = rmseLab,
        # MASE = NA,
        nNeuronas = numNeurona,
        TipoDia = "Laborable"
      )
      
      # Entrenar el modelo neuronal para días de fin de semana
      errorsFinde <- tsCV(ts1Finde$kWh, forecastNN, h = 1, window = 3, n = numNeurona) %>% na.omit()
      actualFinde <- ts1Finde$kWh[1: length(errorsFinde)]
      predictedFinde <- actualFinde + errorsFinde
      
      # Calcular métricas para días de fin de semana
      smapeFinde <- smape(actualFinde, predictedFinde)
      rmseFinde <- rmse(actualFinde, predictedFinde)
      
      # Almacenar los resultados en el tibble para días de fin de semana
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedFinde,
        sMAPE = smapeFinde,
        RMSE = rmseFinde,
        # MASE = NA,
        nNeuronas = numNeurona,
        TipoDia = "Finde"
      )
    }
  }
}



resultados <- foreach(csv_file = csv_files, .combine='c', 
        .packages = librerias) %dopar% RedNeuronalDia(csv_file)

stopCluster(cl)




# Boxplot

filtradoRMSE <- resultadosDia %>%
  filter(!is.na({{ RMSE }}))


# Dividir los datos en una lista de data frames por Modelo
divididoRMSE <- split(filtradoRMSE$RMSE, filtradoRMSE$nNeuronas)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoRMSE))

# Crear un boxplot para cada modelo
boxplot(divididoRMSE, 
        main = "RMSE por Numero de neuronas", 
        ylab = "RMSE",
        col = colores,
        names = names(divididoRMSE),
        names.arg = filtradoRMSE$nNeuronas,
        las = 2)  # Etiquetas de los modelos en el eje X









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

#train <- ultimosLab[1:5, ] %>% select(-Dia, -Hora, -TipoDia)
#test <- ultimosLab[6, ] %>% select(-Dia, -Hora, -TipoDia)

modelo <- nnetar(trainSet, size = 1)
pred <- forecast(modelo, h = 1)
smape(testSet, as.numeric(pred$mean))
mase(testSet[1],pred$mean)

#modelo1 <- nnetar(as.xts(train), size = 3)
#pred1 <- forecast(modelo1, h = 1)
#smape(test$kWh ,pred1$mean)
#mase(test$kWh,pred1$mean, 1)

neuronas <- c(1, seq(5, 100, by = 5)) # numero de neuronas a probar

# Ahora con CV 
forecastNN1 <- function(x){
  prediccion <- forecast(nnetar(x))
  return(prediccion)
}

forecastNN1(lab$kWh)

lab <- hora0 %>% filter(TipoDia == "Laborable")
errors <- tsCV(lab$kWh, forecastNN1, h = 1, window = 7, n =1) %>% na.omit() #esto a ane: ERRORRR
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

horas <- 0:23
neuronas <- c(1, seq(5, 50, by = 5)) 

lab1Resultados <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  numNeuronas = numeric()
) 


# El mase peta

forecastNN <- function(x, h, n){
  prediccion <- forecast(nnetar(x, size = n), h = h)
  return(prediccion)
}

# Prueba con todas las horas y solo laborables

labHoras <- ts1 %>% filter(TipoDia == "Laborable")

foreach(hora = horas) %dopar%{

  foreach( numNeurona = neuronas, .packages = librerias) %dopar%{
    
    lab <- labHoras %>% filter(Hora == hora)
    
    errors <- tsCV(lab$kWh, forecastNN, h = 1, window = 5, n = numNeurona) %>% na.omit()
    actual <- lab$kWh[1: length(errors)]
    
    predicted <- actual + errors
    
    
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    # mase <- mase(actual, predicted)
    
    lab1Resultados <<- lab1Resultados %>% add_row(
      Hora = hora,
      Predicted = predicted,
      sMAPE = smape,
      RMSE = rmse,
      # MASE = numeric(),
      numNeuronas = numNeurona
    )
    
  }
}

filtradoRMSE <- lab1Resultados %>%
  filter(!is.na({{ RMSE }}))


# Dividir los datos en una lista de data frames por Modelo
divididoRMSE <- split(filtradoRMSE$RMSE, filtradoRMSE$numNeuronas)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoRMSE))

# Crear un boxplot para cada modelo
boxplot(divididoRMSE, 
        main = "RMSE por Numero de neuronas", 
        ylab = "RMSE",
        col = colores,
        names = names(divididoRMSE),
        names.arg = filtradoRMSE$numNeuronas,
        las = 2)  # Etiquetas de los modelos en el eje X







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

foreach(csv_file = csv_files[1:5],
        .packages = librerias) %dopar% RedNeuronalLaborable(csv_file)


##pruebas svm

csv1 <- csv_files[5]
csvSVM <- fread(csv1)
csv_SVM <- csvSVM %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(Dia = weekdays(timestamp)) %>%
  mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                          "Laborable", "Finde")) %>%
  select(-imputed)

datos_hora20 <- csv_SVM[hour(csv_SVM$timestamp) == 20, ] 

datosHoraLab20 <- datos_hora20 %>% filter(TipoDia == "Laborable") %>% distinct()
datosHoraFinde20 <- datos_hora20 %>% filter(TipoDia == "Finde") %>% distinct()


ts1Lab20 <- datosHoraLab20 %>%
  mutate(timestamp = as.Date(timestamp)) %>%
  as_tsibble(key = kWh, index = timestamp) %>%
  arrange(timestamp) 

n <- nrow(ts1Lab20)
propTrain <- 0.75
indexTrain <- floor(n * propTrain)
trainSet20 <- ts1Lab20[1:indexTrain, ]
testSet20 <- ts1Lab20[(indexTrain + 1):n, ]

# Definir rangos de valores para los hiperparámetros
kernel_values <- c("linear", "radial")
cost_values <- seq(0.01, 100, length.out = 10) 
gamma_values <- seq(0.01, 100, length.out = 10)  

# Crear todas las combinaciones de hiperparámetros
hyperparameters <- expand.grid(
  kernel = kernel_values,
  cost = cost_values,
  gamma = gamma_values
)

set.seed(123)
h <- hyperparameters[sample(nrow(hyperparameters), 20),] %>% arrange(cost)

forecastSVM <- function(x, hp, h){
  prediccion <- predict(svm(x$kWh ~ x$timestamp, kernel = hp$kernel, cost = hp$cost, gamma = hp$gamma), h = h)
  return(prediccion)
}

resultadosDia <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  nNeuronas = numeric(),
  TipoDia = character()
)

foreach(hp = h, .packages = librerias) %dopar% {
    # Entrenar el modelo neuronal para días laborables
  errorsLab <- tsCV(trainSet20$kWh, forecastSVM, h = 1, window = 5, hp = hp) %>% na.omit()
  actualLab <- trainSet20$kWh[1: length(errorsLab)]
  predictedLab <- actualLab + errorsLab
    
  # Calcular métricas para días laborables
  smapeLab <- smape(actualLab, predictedLab)
  rmseLab <- rmse(actualLab, predictedLab)
  
  # Almacenar los resultados en el tibble para días laborables
  resultadosDia <<- resultadosDia %>% add_row(
    Hora = hora,
    Predicted = predictedLab,
    sMAPE = smapeLab,
    RMSE = rmseLab,
    # MASE = NA,
    kernel = h$kernel,
    cost = h$cost,
    gamma = h$gamma,
    TipoDia = "Laborable"
  )
  
}







fit_svm_and_metrics <- function(train, test, kernel, cost, gamma) {
  model <- svm(train$kWh ~ train$timestamp, kernel = kernel, cost = cost, gamma = gamma)
  predicciones <- predict(model, test)
  rmse <- sqrt(mean((test$kWh - predicciones)^2))
  mase <- mean(abs(test$kWh - predicciones))
  return(data.frame(kernel, cost, gamma, rmse, mase))
}

resultados <- do.call(rbind, lapply(1:nrow(hyperparameters), function(i) {
  fit_svm_and_metrics(trainSet20, testSet20, hyperparameters[i, ])
}))


svm_model20_1 <- svm(trainSet20$kWh ~ trainSet20$timestamp, 
                    kernel = "linear", cost = 1)
svm_model20_2 <- svm(trainSet20$kWh ~ trainSet20$timestamp, 
                     kernel = "radial", cost = 1, gamma = 0.1)
svm_model20_3 <- svm(trainSet20$kWh ~ trainSet20$timestamp, 
                     kernel = "radial", cost = 10, gamma = 0.01)

predictions20_1 <- predict(svm_model20_1, newdata = testSet20)
predictions20_2 <- predict(svm_model20_2, newdata = testSet20)
predictions20_3 <- predict(svm_model20_3, newdata = testSet20)

#probar el bucle grande

resultadosDia <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
 # MASE = numeric(),
  kernel = character(),
  cost = numeric(),
  gamma = numeric(),
  TipoDia = character()
)


horas <- 0:23

# Definir rangos de valores para los hiperparámetros
kernel_values <- c("linear", "radial")
cost_values <- seq(0.01, 100, length.out = 10) 
gamma_values <- seq(0.01, 100, length.out = 10)  

# Crear todas las combinaciones de hiperparámetros
hyperparameters <- expand.grid(
  kernel = kernel_values,
  cost = cost_values,
  gamma = gamma_values
)

set.seed(123)
h <- hyperparameters[sample(nrow(hyperparameters), 20),] %>% arrange(cost)

forecastSVM <- function(x, hp, h){
  prediccion <- predict(svm(x$kWh ~ x$timestamp, kernel = hp$kernel, 
                            cost = hp$cost, gamma = hp$gamma), h = h)
  return(prediccion)
}

cl <- makeCluster(4) 
registerDoParallel(cl)


SvmDia <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp)) %>%
    mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%
    select(-imputed)
  
  
  datosLab <- csv_actual %>% filter(TipoDia == "Laborable")
  datosFinde <- csv_actual %>% filter(TipoDia == "Finde")
  
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias, .combine = 'c') %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% distinct()
    datosHoraFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% distinct()
    
    
    # Crear un tsibble para la hora actual - Laborable
    ts1Lab <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    # Crear un tsibble para el siguiente día - Finde
    ts1Finde <- datosHoraFinde %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    foreach(hyperparameters = h, .packages = librerias) %dopar% {
      # Entrenar el modelo neuronal para días laborables
      errorsLab <- tsCV(ts1Lab$kWh, forecastSVM, h = 1, window = 5, hyperparameters) %>% na.omit()
      actualLab <- ts1Lab$kWh[1: length(errorsLab)]
      predictedLab <- actualLab + errorsLab
      
      # Calcular métricas para días laborables
      smapeLab <- smape(actualLab, predictedLab)
      rmseLab <- rmse(actualLab, predictedLab)
      
      # Almacenar los resultados en el tibble para días laborables
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedLab,
        sMAPE = smapeLab,
        RMSE = rmseLab,
        # MASE = NA,
        nNeuronas = numNeurona,
        TipoDia = "Laborable"
      )
      
      # Entrenar el modelo neuronal para días de fin de semana
      errorsFinde <- tsCV(ts1Finde$kWh, forecastNN, h = 1, window = 3, n = numNeurona) %>% na.omit()
      actualFinde <- ts1Finde$kWh[1: length(errorsFinde)]
      predictedFinde <- actualFinde + errorsFinde
      
      # Calcular métricas para días de fin de semana
      smapeFinde <- smape(actualFinde, predictedFinde)
      rmseFinde <- rmse(actualFinde, predictedFinde)
      
      # Almacenar los resultados en el tibble para días de fin de semana
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedFinde,
        sMAPE = smapeFinde,
        RMSE = rmseFinde,
        # MASE = NA,
        kernel = hyperparameters$kernel,
        cost = hyperparameters$cost,
        gamma = hyperparameters$gamma,
        TipoDia = "Finde"
      )
    }
  }
}



resultados <- foreach(csv_file = csv_files,  
                      .packages = librerias) %dopar% SvmDia(csv_file)

stopCluster(cl)


