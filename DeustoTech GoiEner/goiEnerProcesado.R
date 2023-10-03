library(foreach)
library(doParallel)

#install.packages("e1071")

# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071') 
foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


#install.packages("future")

# codigo y pruebas

path <- "dataset_red.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina

unzip(path, exdir = tempdir) # descomprime. Tarda un poco


# Lista de archivos CSV en la carpeta extraída

csv_files <- list.files(tempdir, pattern = ".csv$", recursive = T, full.names = F)

# funciones y proporcion para las predicciones
csv_files
# SOLO CAMBIAR PARA REDUCIR O AUMENTAR EL NÚMERO DE CSV-S QUE COGEMOS

# Establecer una semilla para reproducibilidad
set.seed(123)


# Seleccionar 200 archivos CSV aleatorios
files_to_copy <- sample(csv_files, 200)

# Crear la carpeta Dataset_red si no existe
if (!file.exists("Dataset_red")) {
  dir.create("Dataset_red")
}

# Copiar los archivos seleccionados a la carpeta Dataset_red
for (file in files_to_copy) {
  file.copy(file, file.path("Dataset_red", basename(file)))
}


resultadosMedia <- tibble( # tibble con los resultados de la media
  Hora = character(),
  rango = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  MASE = numeric(),
  ARIMA = numeric(),
  media_entrenamiento = numeric()
)

# Recorre los archivos CSV

for (csv_file in csv_files) { 
  
  file <- read.csv(csv_file, sep = ",")
  
  fileTs <- file %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
    filter(imputed == 0) %>% select(-imputed) %>% 
    as_tsibble(key = kWh, index = timestamp) %>% arrange(timestamp) # crear la Tsibble
  
  # aqui ya tenemos el csv como time series ordenado con, con la fecha en el formato Y-m-d H:m:s
  
  
  fileTShora <- split(fileTs, f = hour(fileTs$timestamp))
  # fileTShora contiene 24 tsibbles, una por cada hora (0:23) con sus observarciones durante todo el año
  

  for (i in 1:length(fileTShora)) {
    # Dividir cada tsibble en un conjunto de entrenamiento y un conjunto de prueba
    tsibble_actual <- fileTShora[[i]]
    n <- nrow(tsibble_actual)
    propTrain <- 0.75
    indexTrain <- floor(n * propTrain)
    trainSet <- tsibble_actual[1:indexTrain, ]
    testSet <- tsibble_actual[(indexTrain + 1):n, ]
    
    # Calcular la media del consumo en el conjunto de entrenamiento
    media_entrenamiento <- round(mean(trainSet$kWh), 4)
    
    # Calcular MAE y RMSE en el conjunto de prueba
    MAE_actual <- round(mae(testSet$kWh, media_entrenamiento), 4)
    RMSE_actual <- round(rmse(testSet$kWh, media_entrenamiento), 4)
    MAPE_actual <- round(mape(testSet$kWh, media_entrenamiento), 4)
    MASE_actual <- round(mase(testSet$kWh, media_entrenamiento, 1), 4)
    # 0 es que no hay estacionalidad
    
    # Calcular rango (máximo y mínimo) en el conjunto de prueba
    max_valor <- max(testSet$kWh)
    min_valor <- min(testSet$kWh)
    
    
    # Agregar los resultados a la tibble resultados por la media
    resultadosMedia <- resultadosMedia %>%
      add_row(
        Hora = paste("Hora", i - 1),  # Asumiendo que quieres etiquetar cada resultado con "Hora X"
        rango = paste("Máximo:", max_valor, "Mínimo:", min_valor),
        MAE = MAE_actual,
        RMSE = RMSE_actual,
        MAPE = MAPE_actual,
        MASE = MASE_actual,
        media_entrenamiento = media_entrenamiento
      )
  }
    
resultadosMedia <- resultadosMedia %>% na.omit()
    
}
  
# Cambiar la ruta si es necesario
write.csv(resultadosMedia, file = "resultadosMedia2.csv")

# usando naive

resultadosNaiveDia <- tibble(
  Hora = character(),
  Rango = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  entrenamiento = numeric()
)


# Recorre los archivos CSV

for (csv_file in csv_files) { 
  
  file <- read.csv(csv_file, sep = ",")
  
  fileTs = file %>% 
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    filter(imputed==0) %>% 
    select(-imputed) %>%
    as_tsibble(key = kWh, index = timestamp) %>%
    arrange(timestamp) 
  # aqui ya tenemos el csv como time series ordenado con, con la fecha en el formato Y-m-d H:m:s
  
  fileTShora <- split(fileTs, f = hour(fileTs$timestamp))
  
  for (i in 1:length(fileTShora)) {
    
    tsibble_actual <- fileTShora[[i]]
    n <- nrow(tsibble_actual)
    propTrain <- 0.75
    indexTrain <- floor(n * propTrain)
    entrenamiento <- tsibble_actual[1:indexTrain, ]
    prueba <- tsibble_actual[(indexTrain + 1):n, ]
    
    entrenamiento1 = entrenamiento %>% as.data.frame() %>% select(-timestamp)
    
    naive_method = naive(entrenamiento1, h = 1)
    
    prueba$naive = naive_method$mean
    MAE_actual = round(mae(prueba$kWh, prueba$naive),4)
    RMSE_actual = round(rmse(prueba$kWh, prueba$naive),4)
    MAPE_actual <- round(mape(prueba$kWh, prueba$naive), 4)
    
    max_valor = max(prueba$kWh)
    min_valor = min(prueba$kWh)
    
    
    resultadosNaiveDia = resultadosNaiveDia %>%
      add_row(
        Hora = paste("Hora", i - 1),
        Rango = paste("Máximo:", max_valor, "Mínimo:", min_valor),
        MAE = MAE_actual,
        RMSE = RMSE_actual,
        MAPE = MAPE_actual,
        entrenamiento = prueba$naive
      ) %>% na.omit()
  }
  
}

write.csv(resultadosNaiveDia, file = "resultadosNaiveDia2.csv")


# usando seasonal naive 
resultadosSNaive <- tibble(
  DiaDeLaSemana = character(),
  Hora = numeric(),
  Prediccion = numeric(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  MASE = numeric(),
  Rango = character()
)

# Días de la semana
dias_semana <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")


for (csv_file in csv_files){
  file <- read.csv(csv_file, sep = ",")
  
  tsCurrentFile <- file %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
     select(-imputed) %>% 
    as_tsibble(key = kWh, index = timestamp) %>% arrange(timestamp) %>%
    mutate(Dia = weekdays(timestamp), Hora = hour(timestamp))
  
  ts_horas <- split(tsCurrentFile, f = hour(tsCurrentFile$timestamp))
  # tenemos una tsibble para cada hora
  
  for (tsHoraActual in ts_horas){
    tsHoraActual <- tsHoraActual %>% arrange(timestamp)
    
    n <- nrow(tsHoraActual)
    propTrain <- 0.75
    indexTrain <- floor(n * propTrain)
    trainSet <- tsHoraActual[1:indexTrain, ]
    testSet <- tsHoraActual[(indexTrain + 1):n, ]
    
    
    for (dia in dias_semana) {
      # Filtra tus datos de entrenamiento para incluir solo el día de la semana actual
      datos_dia <- trainSet[weekdays(trainSet$timestamp) == dia, ]
      
      # Crea una serie temporal con frecuencia 7 (semanal)
      serie_temporal <- ts(datos_dia$kWh, frequency = 7)
      
      # Utiliza SNAIVE para hacer la predicción
      prediccion <- snaive(serie_temporal, h = 1)  # Predice el próximo día de la semana
      
      # Filtra los datos de prueba para incluir solo el día de la semana actual
      actual_dia <- testSet[weekdays(testSet$timestamp) == dia, ]
      
      # Obtiene el valor predicho
      predValor <- prediccion$mean[1]
      
      # Calcula el MAE y RMSE
      MAE_actual <- round(mae(actual_dia$kWh, predValor), 4)
      RMSE_actual <- round(rmse(actual_dia$kWh, predValor), 4)
      MAPE_actual <- round(mape(actual_dia$kWh, predValor), 4)
      MASE_actual <- round(mase(actual_dia$kWh, predValor, 7), 4)
      # seasonality es 7 porque es semanal
      
      # Calcula el rango (mínimo y máximo) de los valores reales
      min_valor <- round(min(actual_dia$kWh), 4)
      max_valor <- round(max(actual_dia$kWh), 4)
      
      # Crea una cadena de texto con el rango
      rango_str <- paste("Mínimo:", min_valor, "Máximo:", max_valor)
      
      # Agrega los resultados a la tibble resultadosSn
      resultadosSNaive <- resultadosSNaive %>%
        add_row(DiaDeLaSemana = dia, Hora = actual_dia$Hora, 
                Prediccion = predValor, MAE = MAE_actual, RMSE = RMSE_actual, 
                MAPE = MAPE_actual, MASE = MASE_actual,
                Rango = rango_str) %>% unique() %>% na.omit()
    }
  }
}

write.csv(resultadosSNaive, file = "resultadosSnaive2.csv")



# ARIMA Y EXP SMOOTHING

# ARIMA Y EXPONENTIAL SMOOTHING CON CROSS-VALIDATION

# La siguiente linea es solo para muchos datos. Ajustar en funcion del numero de nucleos
# registerDoParallel(cores = 4)  # Puedes ajustar el número de núcleos según tu CPU

# Inicializar el tibble para los resultados

resultadosTotales <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  Modelo = character()
)

horas <- 0:23

# Funcion para CV con Exponential smoothing
forecastETS <- function(x, h) {
  prediccion <- forecast(ets(x), h = h)
  return(prediccion)
}

# Funcion para CV con Arima
forecastARIMA <- function(x, h) {
  prediccion <- forecast(auto.arima(x), h = h)
  return(prediccion)
}

# Funcion para Redes Neuronales
forecastNN <- function(x, h){
  prediccion <- forecast(nnetar(x), h = h)
  return(prediccion)
}

procesarCsvHoras <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    select(-imputed)
# Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias) %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    # Crear un tsibble para la hora actual
    ts1 <- datos_hora %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    # el ultimo valor siempre es NA. quitarlo

    errors <- tsCV(ts1$kWh, forecastETS, h = 1, window = 1) %>% na.omit()
    actual <- ts1$kWh[1: length(errors)]

    # Calculamos la prediccion haciendo real + error
    predicted <- actual + errors

    # Calcular métricas
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    mase <- mase(actual, predicted)

    # Almacenar los resultados en el tibble para ETS

    resultadosTotales <<- resultadosTotales %>% add_row(
      Hora = hora,
      Predicted = predicted,
      MASE = mase,
      sMAPE = smape,
      RMSE = rmse,
      Modelo = "ETS"
    )

    # AHORA LO MISMO PERO CON ARIMA

    errors <- tsCV(ts1$kWh, forecastARIMA, h = 1, window = 1) %>% na.omit()
    actual <- ts1$kWh[1: length(errors)]

    # Calculamos la prediccion haciendo real + error
    predicted <- actual + errors

    # Calcular métricas
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    mase <- mase(actual, predicted)

    # Almacenar los resultados en el tibble
    resultadosTotales <<- resultadosTotales %>% add_row(
      Hora = hora,
      Predicted = predicted,
      MASE = mase,
      sMAPE = smape,
      RMSE = rmse,
      Modelo = "ARIMA"
    )

    
    # Mismo para Red Neuronal
    
    errors <- tsCV(ts1$kWh, forecastNN, h = 1, window = 3) 
    omitir <- which(is.na(errors)) 
    errors <- errors[-omitir]
    actual <- ts1$kWh[-omitir]

    # quitamos los valores NA de errores y de los valores reales.
    
    # Calculamos la prediccion haciendo real + error
    
    predicted <- actual + errors 

    
    # Calcular métricas
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    mase <- mase(actual, predicted)
    rmse <- rmse(actual, predicted)
    
    # Almacenar los resultados en el tibble
    resultadosTotales <<- resultadosTotales %>% add_row(
      Hora = hora,
      Predicted = predicted,
      MASE = mase,
      sMAPE = smape,
      RMSE = rmse,
      Modelo = "Red Neuronal"
    )
  }
}


foreach(csv_file = csv_files,
        .packages = librerias) %dopar% procesarCsvHoras(csv_file)

# Detén el backend después de usarlo. Solo si se usa paralelo
stopImplicitCluster()

write.csv(resultadosTotales, file = "resultadosTotales.csv")



#SUPPORT VECTOR MACHINE

# Bucle con validacion normal

resultadosSVM <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  Modelo = character()
)


horas <- 0:23



procesarSVM <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    select(-imputed)
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias) %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    # Crear un tsibble para la hora actual
    ts1 <- datos_hora %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    

    n <- nrow(ts1)
    propTrain <- 0.75
    indexTrain <- floor(n * propTrain)
    trainSet <- ts1[1:indexTrain, ]
    testSet <- ts1[(indexTrain + 1):n, ]
  
    
    #mismo pero para svm
    modelo <- svm(trainSet$kWh ~ trainSet$timestamp, type = "eps-regression", kernel = "radial", cost = 1)
    prediccion <- predict(modelo, newdata = testSet)
    largoTest <- nrow(testSet)
    
    # quitamos los valores NA de errores y de los valores reales.
    
    # Calculamos la prediccion haciendo real + error
   
    predictions <- as.numeric(prediccion)
    
    predictions2 <- predictions[1:largoTest]
    
    # Calcular métricas
    smape <- smape(testSet$kWh, predictions2)
    rmse <- rmse(testSet$kWh, predictions2)
    mase <- mase(testSet$kWh, predictions2)
    
    
    # Almacenar los resultados en el tibble
    resultadosSVM <<- resultadosSVM %>% add_row(
      Hora = hora,
      Predicted = predictions2,
      MASE = mase,
      sMAPE = smape,
      RMSE = rmse,
      Modelo = "SVM"
    )
  }
}


foreach(csv_file = csv_files,
        .packages = librerias) %dopar% procesarSVM(csv_file)


write.csv(resultadosSVM, file = "resultadosSVM.csv")



#########MEJORAR EL CODIGO DEL PRINCIPIO#################


# Definir el archivo ZIP que contiene los CSV
path <- "dataset_red.zip"

# Crear una función para cargar y dividir los CSV en series temporales
process_csv <- function(csv_file) {

  data <- fread(csv_file)
  
  if (!inherits(data$timestamp, "POSIXct")) {
    data$fecha <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%S")
  }
  
  data <- data[order(data$timestamp), ]
  

  n <- nrow(data)
  propTrain <- 0.75
  indexTrain <- floor(n * propTrain)
  
  trainSet <- data[1:indexTrain, ]
  testSet <- data[(indexTrain + 1):n, ]
  
  trainTimeSeries <- xts(trainSet$kWh, order.by = trainSet$timestamp)
  testTimeSeries <- xts(testSet$kWh, order.by = testSet$timestamp)
  
  return(list(trainSet = trainTimeSeries, testSet = testTimeSeries))
}

# Obtener la lista de archivos CSV en el ZIP
csv_files <- unzip(path, list = TRUE)$Name

# Crear listas para almacenar las series temporales de entrenamiento y prueba
train_series_list <- list()
test_series_list <- list()

# Procesar los archivos CSV en paralelo
plan(multisession)
for (csv_file in csv_files) {
  time_series_data <- process_csv(csv_file)
  train_series_list[[csv_file]] <- time_series_data$trainSet
  test_series_list[[csv_file]] <- time_series_data$testSet
}

#Verificar las dimensiones de las listas de series temporales
print(length(train_series_list))
print(length(test_series_list))

head(train_series_list)

#PREDICCION POR MEDIA

resultadosMedia <- tibble( # tibble con los resultados de la media
  Hora = character(),
  rango = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  MASE = numeric(),
  media_entrenamiento = numeric()
)

PredecirPorMedia <- function(datosTrain, datosTest) {
  
  
  trainHora <- split(datosTrain, f = hour(datosTrain$timestamp))
  testHora <- split(datosTest, f = hour(datosTest$timestamp))

for (i in 1:length(trainHora)) {
  
  trainActual <- trainHora[[i]]
  testActual <- testHora[[i]]
  
  # Calcular la media del consumo en el conjunto de entrenamiento
  media_entrenamiento <- round(MEAN(trainActual), 4)
  
  # Calcular ERRORES
  MAE_actual <- round(mae(testActual$kWh, media_entrenamiento), 4)
  RMSE_actual <- round(rmse(testActual$kWh, media_entrenamiento), 4)
  MAPE_actual <- round(mape(testActual$kWh, media_entrenamiento), 4)
  MASE_actual <- round(mase(testActual$kWh, media_entrenamiento, 1), 4)
  
  # Calcular rango (máximo y mínimo) en el conjunto de prueba
  max_valor <- max(testActual$kWh)
  min_valor <- min(testActual$kWh)
  
  
  # Agregar los resultados a la tibble resultados por la media
  resultadosMedia <- resultadosMedia %>%
    add_row(
      Hora = paste("Hora", i - 1),  # Asumiendo que quieres etiquetar cada resultado con "Hora X"
      rango = paste("Máximo:", max_valor, "Mínimo:", min_valor),
      MAE = MAE_actual,
      RMSE = RMSE_actual,
      MAPE = MAPE_actual,
      MASE = MASE_actual,
      media_entrenamiento = media_entrenamiento
    )
}

resultadosMedia <- resultadosMedia %>% na.omit()
}

data_pairs <- list(train_series_list, test_series_list)

apply_function <- function(data_pair) {
  PredecirPorMedia(data_pair$datosTrain, data_pair$datosTest)
}

#future.apply::future_lapply(c(train_series_list, test_series_list), PredecirPorMedia)



###########MODELOS SIN CV####################

resultadosTotales <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  Modelo = character()
)

# Función para calcular las métricas de pronóstico por hora con ETS y ARIMA
procesarCsvHoras <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    select(-imputed)
  
  # Dividir los datos por horas
  horas <- c(0:23)
  
  foreach(hora = horas, .packages = librerias) %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ]
    
    ts1 <- xts(datos_hora$kWh, order.by = datos_hora$timestamp)
    
    n <- nrow(ts1)
    propTrain <- 0.75
    indexTrain <- floor(n * propTrain)
    trainSet <- ts1[1:indexTrain, ]
    testSet <- ts1[(indexTrain + 1):n, ]
    
    # Ajustar el modelo ETS
    ex <- ets(trainSet)
    result <- forecast(ex, h = nrow(testSet))
    
    predicted <- as.numeric(result$mean)
    actual <- drop(coredata(testSet))
    
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    
    options(digits = 4) #en vez de round
    resultadosTotales <<- resultadosTotales %>% add_row(
      Hora = hora,
      Predicted = predicted,
      sMAPE = smape,
      RMSE = rmse,
      Modelo = "ETS"
    )
    
    # Ajustar el modelo ARIMA
    arim <- auto.arima(trainSet)
    p <- forecast(arim, h = nrow(testSet))
    
    predicted_arima <- as.numeric(p$mean)
    
    smape_arima <- smape(actual, predicted_arima)
    rmse_arima <- rmse(actual, predicted_arima)
    
    options(digits = 4) #en vez de round
    resultadosTotales <<- resultadosTotales %>% add_row(
      Hora = hora,
      Predicted = predicted_arima,
      sMAPE = smape_arima,
      RMSE = rmse_arima,
      Modelo = "ARIMA"
    ) %>% unique() # para eliminar duplicados
  }
}


# Luego puedes llamar a la función para procesar múltiples archivos CSV en paralelo
foreach(csv_file = csv_files,
        .packages = librerias) %dopar% procesarCsvHoras(csv_file)

# Detén el backend después de usarlo. Solo si se usa paralelo
stopImplicitCluster()


write.csv(resultadosTotales, file = "resultadosArimaETS.csv")


#############PRUEBAS REDES NEURONALES###############

csv1 <- fread(csv_files[10])

csv1 <- csv1 %>%  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  select(-imputed) %>% arrange(timestamp)

hora0 <- csv1 %>% filter(hour(timestamp) == 0)
hora0ts <- xts(hora0$kWh, order.by = hora0$timestamp)

csv1_ts <- xts(csv1$kWh, order.by = csv1$timestamp)


propTrain <- 0.75
n <- nrow(hora0ts)
indexTrain <- floor(n * propTrain)
trainSet <- hora0ts[1:indexTrain, ]
testSet <- hora0ts[(indexTrain + 1):n, ]

modelo <- nnetar(trainSet)

predicciones <- forecast(modelo, h = length(testSet))

predicciones


horas <- 0:23

for (hora in horas) {
  
  datos_hora <- csv1[hour(csv1$timestamp) == rep(i, nrow(csv1)), ]
  datos_hora1 <- datos_hora %>%
    mutate(timestamp = as.Date(timestamp)) %>%
    select(-imputed) %>% arrange(timestamp)
  
  datos_hora_ts <- xts(datos_hora1$kWh, order.by = datos_hora1$timestamp)
  
  n <- nrow(datos_hora_ts)
  propTrain <- 0.75
  indexTrain <- floor(n * propTrain)
  trainSet <- datos_hora_ts[1:indexTrain, ]
  testSet <- datos_hora_ts[(indexTrain + 1):n, ]
  
  modelo <- nnetar(trainSet)
  predicciones <- forecast(modelo, h = length(testSet))
  
}


datosPrueba <- tsibbles_por_hora[[1]] 
datosPrueba <- xts(datosPrueba$kWh, order.by = datosPrueba$timestamp)
datosPrueba

modelo = nnetar(datosPrueba)
modelo$x
predicted = forecast(modelo, h = 1)
predicted

autoplot(predicted)


#############REDES NEURONALES con CV###################

forecastNN <- function(x, h){
  prediccion <- forecast(nnetar(x), h = h)
  return(prediccion)
}

errores <- tsCV(hora0ts, forecastNN, h = 1, window = 3) 
omitir <- which(is.na(errores)) 
errores <- errores[-omitir]
actual <- drop(coredata(hora0ts))[-omitir]

predichos <- actual + errores

# algunos errores son NA, hay que saber cuales para no tenerlos en cuenta 
# para luego calcular los valores y las predicciones


#manera 1
csv1 = fread(csv_files[10])

fileTs <- csv1 %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
  select(-imputed) %>% 
  as_tsibble(key = kWh, index = timestamp) %>% 
  arrange(timestamp) 

fileTShora <- split(fileTs, f = hour(fileTs$timestamp))

pruebaNN <- tibble(
  Predicted = numeric(),
  sMAPE = numeric(),
  MASE = numeric(),
  RMSE = numeric(),
)

result <- forecast(ex, h = nrow(testSet))

predicted <- as.numeric(result$mean)
actual <- drop(coredata(testSet))



for (i in 1:length(fileTShora)) {
  # Dividir cada tsibble en un conjunto de entrenamiento y un conjunto de prueba
  tsibble_actual <- fileTShora[[i]] %>% mutate(timestamp = ymd(timestamp))
  
  datosPrueba <- xts(tsibble_actual$kWh, order.by = tsibble_actual$timestamp)
  
  modelo = nnetar(datosPrueba)
  predicted = forecast(modelo, h = 1)
  
  prediccion <- as.numeric(predicted$mean)
  actual <- 
    
    smape <- smape(as.numeric(datosPrueba), predicted)
  # mase <- mase(as.numeric(datosPrueba), predicted$mean)
  # rmse <- rmse(as.numeric(datosPrueba), predicted$mean)
  
  pruebaNN <- pruebaNN %>% add_row(
    Predicted = prediccion,
    sMAPE = mape,
    # MASE = mase,
    # RMSE = rmse
  )
  
  
}

#manera 2
tsibbles_por_hora <- lapply(0:23, function(hora) {
  # Filtrar los datos para la hora actual
  datos_hora <- csv1 %>% mutate(h = as.numeric(format(timestamp, format = "%H"))) %>% filter(hour(h) == hora)
  
  # Crear un tsibble para la hora actual
  tsibble_hora <- datos_hora %>%
    mutate(timestamp = as.Date(timestamp)) %>%
    as_tsibble(key = kWh, index = timestamp) %>%
    select(-imputed) %>%
    arrange(timestamp)
  
  return(tsibble_hora)
})


# Definir la función que deseas aplicar a cada tsibble
mi_funcion <- function(tsibble) {
  
  datosPrueba <- xts(tsibble$kWh, order.by = tsibble$timestamp)
  
  modelo = nnetar(datosPrueba)
  predicted = forecast(modelo, h = 1)
  return(predicted)
}

# Crear una lista para almacenar los resultados
resultados <- list()

# Aplicar la función a cada tsibble en tsibbles_por_hora
for (i in 1:length(tsibbles_por_hora)) {
  resultado <- mi_funcion(tsibbles_por_hora[[i]])
  resultados[[i]] <- resultado
}



###############3SUPPORT VECTOR MACHINE PRUEBAS################3
csv1 = fread(csv_files[10])
head(csv1)

fileTs <- csv1 %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
  select(-imputed) %>% 
  as_tsibble(key = kWh, index = timestamp) %>% 
  arrange(timestamp)

n <- nrow(fileTs)
propTrain <- 0.75
indexTrain <- floor(n * propTrain)
trainSet <- fileTs[1:indexTrain, ]
testSet <- fileTs[(indexTrain + 1):n, ]

nrow(trainSet) #6678
nrow(testSet) #2227

svm_model <- svm(trainSet$kWh ~ trainSet$timestamp, type = "eps-regression", kernel = "radial", cost = 1)

predictions <- predict(svm_model, newdata = testSet)
largo <- nrow(testSet)
p <- predictions[1: largo]
mase(testSet$kWh, as.numeric(p))
rmse(testSet$kWh, as.numeric(p))
smape(testSet$kWh, as.numeric(p))
head(testSet)
length(predictions) #6678
