library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


#descropmir carpeta de 200 csv
path <- "dataset_red.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina
unzip(path, exdir = tempdir) # descomprime. Tarda un poco

# Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(tempdir, pattern = ".csv$", recursive = T, full.names = F)

resultadosSVM <- tibble(
  Hora = numeric(),
  TipoDia = character(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  Modelo = character()
)

COMPLETE <- 0.10
horas <- 0:23

mediaF <- function(x, h) {
  prediccion <- predict(mean(x), h = h)
  return(prediccion)
}

ultimoValorF <- function(x, h) {
  prediccion <- predict(tail(x, 1), h = h)
  return(prediccion)
}


#funcion grande con todos los modelos
predict_models <- function(csv_file) {
  
  csv_actual <- fread(csv_file)
  
  a <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%  select(-imputed)
  
  LENGTH  <- length(csv_actual$kWh)
  ZEROS   <- sum(csv_actual$kWh==0)/LENGTH
  IMPUTED <- sum(csv_actual$imputed == 1)/LENGTH

  propTrain <- 0.75
  indexTrain <- floor(LENGTH * propTrain)
  trainSet <- a[1:indexTrain, ]
  testSet <- a[(indexTrain + 1):LENGTH, ]
  
  
  if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)) {
    
    foreach(hora = horas, .packages = librerias) %dopar% {
      
      train_hora <- trainSet[hour(trainSet$timestamp) == hora,]
      test_hora <- testSet[hour(testSet$timestamp) == hora,]
      
      mean <- tsCV(train_hora$kWh , mediaF , h = 1)
      naive <- tsCV(train_hora$kWh , ultimoValorF , h = 1)
      #seasonal naive no se como hacerlo :)
      
      
     
    }
    
  }
  
  
}

#ejecutar funcion para todos los csv
foreach(csv_file = csv_files,
        .packages = librerias) %dopar% predict_models(csv_file)





prueba <- fread(csv_files[1])
prueba <- prueba %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                          "Laborable", "Finde")) 

LENGTH  <- length(prueba$kWh)
ZEROS   <- sum(prueba$kWh==0)/LENGTH
IMPUTED <- sum(prueba$imputed == 1)/LENGTH

prueba <- prueba %>% select(- imputed)

propPrueba <- 0.75
indexPrueba <- floor(LENGTH * propPrueba)
trainPrueba <- prueba[1:indexPrueba, ]
testPrueba <- prueba[(indexPrueba + 1):LENGTH, ]

if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)) {
  
  foreach(hora = horas, .packages = librerias) %dopar% {
    
    datos_hora <- prueba[hour(prueba$timestamp) == hora, ] 
    datos_naive <- datos_hora %>% as.data.frame() %>% select(-timestamp)
    
    mean <- as.numeric(mean(datos_hora$kWh))
    naive <- as.numeric(naive(datos_naive$kWh, h = 1))
    
    datosLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% unique()
    datosFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% unique()
  }
  
}
