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

resultadosModelos <- tibble(
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
dias_semana <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

mediaF <- function(x, h) {
  prediccion <- predict(mean(x), h = h)
  return(prediccion)
}


naiveF <- function(x, h) {
  prediccion <- predict(naive(x, h = 1)$mean, h = h)
}

snaiveF <- function(x, h) {
  prediccion <- predict(snaive(x, h = 1)$mean, h = h)
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
  
  
  if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)) {
    
    foreach(hora = horas, .packages = librerias) %dopar% {
      
      datos_hora <- a[hour(a$timestamp) == hora,]
      datosLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% unique()
      datosFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% unique()
      
      #MEDIA L
      errors <- tsCV(datosLab$kWh, mediaF, h = 1, window = 5) %>% na.omit()
      actual <- datosLab$kWh[1: length(errors)]
      predicted <- actual + errors
      
      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      mase <- mase(actual, predicted)
      
      resultadosModelos <- resultadosModelos %>% add_row(
        Hora = hora,
        TipoDia = "Laborable",
        Predicted = predicted,
        sMAPE = smape,
        RMSE = rmse,
        MASE = mase,
        Modelo = "Media"
      ) 
      
      #media F
      errors <- tsCV(datosFinde$kWh, mediaF, h = 1, window = 3) %>% na.omit()
      actual <- datosFinde$kWh[1: length(errors)]
      predicted <- actual + errors
      
      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      mase <- mase(actual, predicted)
      
      resultadosModelos <- resultadosModelos %>% add_row(
        Hora = hora,
        TipoDia = "Finde",
        Predicted = predicted,
        sMAPE = smape,
        RMSE = rmse,
        MASE = mase,
        Modelo = "Media"
      ) 
      
      #NAIVE L
      errors <- tsCV(datosLab$kWh, naiveF, h = 1, window = 5) %>% na.omit()
      actual <- datosLab$kWh[1: length(errors)]
      predicted <- actual + errors
      
      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      mase <- mase(actual, predicted)
      
      resultadosModelos <- resultadosModelos %>% add_row(
        Hora = hora,
        TipoDia = "Laborable",
        Predicted = predicted,
        sMAPE = smape,
        RMSE = rmse,
        MASE = mase,
        Modelo = "Naive"
      ) 
      
      #NAIVE F
      errors <- tsCV(datosFinde$kWh, naiveF, h = 1, window = 3) %>% na.omit()
      actual <- datosFinde$kWh[1: length(errors)]
      predicted <- actual + errors
      
      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      mase <- mase(actual, predicted)
      
      resultadosModelos <- resultadosModelos %>% add_row(
        Hora = hora,
        TipoDia = "Finde",
        Predicted = predicted,
        sMAPE = smape,
        RMSE = rmse,
        MASE = mase,
        Modelo = "Naive"
      ) 
      
      #SNAIVE L
      foreach(dia = dias_semana, .packages = librerias) %dopar% {
        
        dia_semana <- datos_hora[weekdays(datos_hora$timestamp) == dia, ]
        
        errors <- tsCV(dia_semana$kWh, snaiveF, h = 1, window = 5) %>% na.omit()
        actual <- dia_semana$kWh[1: length(errors)]
        predicted <- actual + errors
        
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = dia,
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          Modelo = "sNaive"
        ) 
        
      }
      
      #ARIMA L
      
    }
    
  }
  
  
}

#ejecutar funcion para todos los csv
foreach(csv_file = csv_files,
        .packages = librerias) %dopar% predict_models(csv_file)



#PARA HACER PRUEBAS
resultadosModelos <- tibble(
  Hora = numeric(),
  TipoDia = character(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  Modelo = character()
)


prueba <- fread(csv_files[1])
prueba <- prueba %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                          "Laborable", "Finde")) 

LENGTH  <- length(prueba$kWh)
ZEROS   <- sum(prueba$kWh==0)/LENGTH
IMPUTED <- sum(prueba$imputed == 1)/LENGTH

prueba <- prueba %>% select(- imputed)


if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)) {
  
  prueba_hora <- prueba[hour(prueba$timestamp) == 20,]
  
  datosLabP <- prueba_hora %>% filter(TipoDia == "Laborable") %>% unique()
  datosFindeP <- prueba_hora %>% filter(TipoDia == "Finde") %>% unique()

 
 
}

