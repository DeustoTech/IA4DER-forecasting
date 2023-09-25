library(ggplot2)
library(lattice)
library(caret)
library(fpp3)
library(lattice)
library(forecast)


# codigo y pruebas

path <- "dataset_red.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina

unzip(path, exdir = tempdir) # descomprime. Tarda un poco


# Lista de archivos CSV en la carpeta extraída

csv_files <- list.files(tempdir, pattern = ".csv$", recursive = TRUE, full.names = TRUE)

# funciones y proporcion para las predicciones

# reducir el tamaño del dataset a 200 csv

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




propTrain <- 0.8 # usamos el 80% de las observaciones como set de entrenamiento

calculateMAE <- function(actual, predicted) {
  return(mean(abs(predicted - actual)))
}

# Función para calcular RMSE
calculateRMSE <- function(actual, predicted) {
  return(sqrt(mean((predicted - actual)^2)))
}


resultadosMedia <- tibble( # tibble con los resultados de la media
  Hora = character(),
  rango = character(),
  MAE = numeric(),
  RMSE = numeric(),
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
    propTrain <- 0.8
    indexTrain <- floor(n * propTrain)
    trainSet <- tsibble_actual[1:indexTrain, ]
    testSet <- tsibble_actual[(indexTrain + 1):n, ]
    
    # Calcular la media del consumo en el conjunto de entrenamiento
    media_entrenamiento <- round(mean(trainSet$kWh), 4)
    
    # Calcular MAE y RMSE en el conjunto de prueba
    MAE_actual <- round(calculateMAE(testSet$kWh, media_entrenamiento), 4)
    RMSE_actual <- round(calculateRMSE(testSet$kWh, media_entrenamiento), 4)
    
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
        media_entrenamiento = media_entrenamiento
      )
  }
    
    
    
    
}
  
# Cambiar la ruta si es necesario
write.csv(resultadosMedia, file = "resultadosMedia.csv")

# usando naive

resultadosNaiveDia <- tibble(
  Hora = character(),
  Rango = character(),
  MAE = numeric(),
  RMSE = numeric(),
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
    propTrain <- 0.8
    indexTrain <- floor(n * propTrain)
    entrenamiento <- tsibble_actual[1:indexTrain, ]
    prueba <- tsibble_actual[(indexTrain + 1):n, ]
    
    entrenamiento1 = entrenamiento %>% as.data.frame() %>% select(-timestamp)
    
    naive_method = naive(entrenamiento1, h = 1)
    
    prueba$naive = naive_method$mean
    MAE_actual = round(calculateMAE(prueba$kWh, prueba$naive),4)
    RMSE_actual = round(calculateRMSE(prueba$kWh, prueba$naive),4)
    
    max_valor = max(prueba$kWh)
    min_valor = min(prueba$kWh)
    
    
    resultadosNaiveDia = resultadosNaiveDia %>%
      add_row(
        Hora = paste("Hora", i - 1),
        Rango = paste("Máximo:", max_valor, "Mínimo:", min_valor),
        MAE = MAE_actual,
        RMSE = RMSE_actual,
        entrenamiento = prueba$naive
      )
  }
  
  
  
  
}

write.csv(resultadosNaiveDia, file = "resultadosNaiveDia.csv")


# usando seasonal naive 
resultadosSNaive <- tibble(
  DiaDeLaSemana = character(),
  Hora = numeric(),
  Prediccion = numeric(),
  MAE = numeric(),
  RMSE = numeric(),
  Rango = character()
)

# Días de la semana
dias_semana <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")


for (csv_file in csv_files){
  file <- read.csv(csv_file, sep = ",")
  
  tsCurrentFile <- file %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
    filter(imputed == 0) %>% select(-imputed) %>% 
    as_tsibble(key = kWh, index = timestamp) %>% arrange(timestamp) %>%
    mutate(Dia = weekdays(timestamp), Hora = hour(timestamp))
  
  ts_horas <- split(tsCurrentFile, f = hour(tsCurrentFile$timestamp))
  # tenemos una tsibble para cada hora
  
  for (tsHoraActual in ts_horas){
    tsHoraActual <- tsHoraActual %>% arrange(timestamp)
    
    n <- nrow(tsHoraActual)
    propTrain <- 0.8
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
      MAE_actual <- round(calculateMAE(actual_dia$kWh, predValor), 4)
      RMSE_actual <- round(calculateRMSE(actual_dia$kWh, predValor), 4)
      
      # Calcula el rango (mínimo y máximo) de los valores reales
      min_valor <- round(min(actual_dia$kWh), 4)
      max_valor <- round(max(actual_dia$kWh), 4)
      
      # Crea una cadena de texto con el rango
      rango_str <- paste("Mínimo:", min_valor, "Máximo:", max_valor)
      
      # Agrega los resultados a la tibble resultadosSn
      resultadosSNaive <- resultadosSNaive %>%
        add_row(DiaDeLaSemana = dia, Hora = actual_dia$Hora, 
                Prediccion = predValor, MAE = MAE_actual, RMSE = RMSE_actual, 
                Rango = rango_str) %>% unique()
    }
  }
}

write.csv(resultadosSNaive, file = "DeustoTech GoiEner/resultadosSnaive.csv")


# Campo de pruebas usando solo el primer csv de la carpeta


csv1 <- read.csv(csv_files[1], sep = ",") 
ts1 <- csv1 %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
  filter(imputed == 0) %>% select(-imputed) %>% 
  as_tsibble(key = kWh, index = timestamp) %>% mutate(hora = hour(timestamp)) %>% arrange(timestamp) 
# tenemos la columna de las horas. 


ts_horas <- split(ts1, f = hour(ts1$timestamp)) # tenemos una tsibble para cada hora

propTrain <- 0.8 # usamos el 80% de las observaciones como set de entrenamiento

calculateMAE <- function(actual, predicted) {
  return(mean(abs(predicted - actual)))
}

# Función para calcular RMSE
calculateRMSE <- function(actual, predicted) {
  return(sqrt(mean((predicted - actual)^2)))
}


# Crear una tibble para almacenar los resultados
resultados <- tibble(
  Hora = character(),
  rango = character(),
  MAE = numeric(),
  RMSE = numeric(),
  media_entrenamiento = numeric()
)

# Supongamos que tienes una lista llamada ts_horas con las 24 tsibbles

for (i in 1:length(ts_horas)) {
  # Dividir cada tsibble en un conjunto de entrenamiento y un conjunto de prueba
  tsibble_actual <- ts_horas[[i]]
  n <- nrow(tsibble_actual)
  propTrain <- 0.8
  indexTrain <- floor(n * propTrain)
  trainSet <- tsibble_actual[1:indexTrain, ]
  testSet <- tsibble_actual[(indexTrain + 1):n, ]
  
  # Calcular la media del consumo en el conjunto de entrenamiento
  media_entrenamiento <- round(mean(trainSet$kWh), 4)
  
  # Calcular MAE y RMSE en el conjunto de prueba
  MAE_actual <- round(calculateMAE(testSet$kWh, media_entrenamiento), 4)
  RMSE_actual <- round(calculateRMSE(testSet$kWh, media_entrenamiento), 4)
  
  # Calcular rango (máximo y mínimo) en el conjunto de prueba
  max_valor <- max(testSet$kWh)
  min_valor <- min(testSet$kWh)
  
  
  # Agregar los resultados a la tibble resultados
  resultados <- resultados %>%
    add_row(
      Hora = paste("Hora", i - 1),  # Asumiendo que quieres etiquetar cada resultado con "Hora X"
      rango = paste("Máximo:", max_valor, "Mínimo:", min_valor),
      MAE = MAE_actual,
      RMSE = RMSE_actual,
      media_entrenamiento = media_entrenamiento
    )
}

# Visualizar la tibble resultados
print(resultados)


tshora0 <- ts_horas[[1]] %>% select(-hora)# así accedemos a la hora 0.

tshora0 %>% ggplot(aes(x = timestamp, y = kWh)) + geom_line() +
  geom_hline(yintercept = mean(tshora0$kWh), color = "blue") +
  labs(title = "Consumo de electricidad",
       x = "Hora",
       y = "kWh") # ploteamos. la linea es la media


#intento de predecir por el ultimo valor by ane :/0
n1 <- nrow(ts1)
propTrain1 <- 0.8
indexTrain1 <- floor(n * propTrain)
trainSet1 <- ts1[1:indexTrain, ]
testSet1 <- ts1[(indexTrain + 1):n, ]

serie_temporal <- ts(trainSet1$kWh, frequency = 1)
prediccion <- naive(serie_temporal, h = 1)
print(prediccion)

ts1 = ts1 %>% select(-hora)

ggplot(data = ts1, aes(x = timestamp, y = kWh)) + geom_line(color = "black")


# pruebas lunes 18


prueba <- read.csv("imp_csv/0001b3b2f18c01c62ed9b2a87de7b4e33e7836f786f7904471d8866978405c1b.csv")

pruebaTs <- prueba %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS"
    )) %>% filter(imputed == 0) %>% select(-imputed)  

pruebaTsBien <- pruebaTs %>% as_tsibble(index = timestamp, key = kWh)

# predecir por la media. De prueba, intentar predecir el consumo a una
# hora basandonos en la media de los anteriores dias a esa hora

pruebaTsBien %>% autoplot(kWh)

prueba9 %>% ggplot(aes(x = timestamp, y = kWh)) + geom_line()

prueba9b <- prueba9 %>% as_tibble() %>% mutate(timestamp = date(timestamp))
prueba9b <- prueba9b %>% as_tsibble()

gg_season(prueba9b)
prueba9b %>% gg_subseries(kWh, period = "day")
# todos los dias a las 9 am


prueba9[1:365, ] %>% gg_subseries(kWh)

prueba9b <- prueba9 %>% mutate(dia = as.POSIXct(timestamp)) %>% select(-timestamp)




prueba9train <- prueba9[90:100, ] 

diaSiguienteVerdad <- prueba9[101, ]
diaSiguientePred <- mean(prueba9train$kWh)





