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


arimaF <- function(x, h) {
  prediccion <- forecast(auto.arima(x), h = h)
  return(prediccion)
}

etsF <- function(x, h) {
  prediccion <- forecast(ets(x), h = h)
  return(prediccion)
}

nnF <- function(x, h, n){
  prediccion <- forecast(nnetar(x, size = n), h = h)
  return(prediccion)
}

NEURON_RANGE <- c(1:4, seq(5, 30, by = 5))

# Constantes de SVM

kernel_values <- c("linear", "radial")
cost_values <- seq(0.01, 100, length.out = 20) 
gamma_values <- seq(0.01, 100, length.out = 20)  

# Crear todas las combinaciones de hiperparámetros
svmHP <- expand.grid(
  kernel = kernel_values,
  cost = cost_values,
  gamma = gamma_values
)


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
  
  
  RESULT_FILE <- "ResultadosTotales.csv"
  
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
      
      if (!file.exists(RESULT_FILE)) {
        # Si el archivo no existe, crea el archivo y agrega los nombres de las columnas
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = TRUE)
      } else {
        # Si el archivo ya existe, solo appendea los resultados
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      }
      
      #SNAIVE 
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
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      }
      
      #ARIMA L
      
      errors <- tsCV(datosLab$kWh, arimaF, h = 1, window = 5) %>% na.omit()
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
        Modelo = "Arima"
      ) 
      fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      
      # ARIMA F
      
      errors <- tsCV(datosFinde$kWh, arimaF, h = 1, window = 3) %>% na.omit()
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
        Modelo = "Arima"
      ) 
      fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      
      # EXP SMOOTHING L
      
      errors <- tsCV(datosLab$kWh, etsF, h = 1, window = 5) %>% na.omit()
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
        Modelo = "ETS"
      ) 
      fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      
      # EXP SMOOTHING F
      
      errors <- tsCV(datosFinde$kWh, etsF, h = 1, window = 3) %>% na.omit()
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
        Modelo = "ETS"
      ) 
      fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      
      # RED NEURONAL L
      
      tuned_nn <- tune.nnet(kWh ~ timestamp, data = datosLab, size = NEURON_RANGE)
      best_size <- tuned_nn$best.model$size
      
      errors <- tsCV(datosLab$kWh, nnF, h = 1, window = 5, n = best_size) %>% na.omit()
      actual <- datosLab$kWh[1: length(errors)]
      predicted <- actual + errors
      
      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      
      # para que no pete mase
      if (is.numeric(mase(actual, predicted))){  mase <- mase(actual, predicted)      }
      
      resultadosModelos <- resultadosModelos %>% add_row(
        Hora = hora,
        TipoDia = "Laborable",
        Predicted = predicted,
        sMAPE = smape,
        RMSE = rmse,
        MASE = mase,
        Modelo = "NN"
      ) 
      
      fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      
      # RED NEURONAL F
      
      tuned_nn <- tune.nnet(kWh ~ timestamp, data = datosFinde, size = NEURON_RANGE)
      best_size <- tuned_nn$best.model$size
      
      errors <- tsCV(datosFinde$kWh, nnF, h = 1, window = 3, n = best_size) %>% na.omit()
      actual <- datosFinde$kWh[1: length(errors)]
      predicted <- actual + errors
      
      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      
      # para que no pete mase
      if (is.numeric(mase(actual, predicted))){  mase <- mase(actual, predicted)      }
      
      resultadosModelos <- resultadosModelos %>% add_row(
        Hora = hora,
        TipoDia = "Finde",
        Predicted = predicted,
        sMAPE = smape,
        RMSE = rmse,
        MASE = mase,
        Modelo = "NN"
      ) 
      
      fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      
      
      # SVM 
      
      # para svm, no nos sirve la misma forma de hacer CV. Lo hacemos de otra manera
      
      slicesLab <- createTimeSlices(datos_hora$timestamp, initialWindow = 5, horizon = 1, skip = 0, fixedWindow = F)
      slicesFinde <- createTimeSlices(datos_hora$timestamp, initialWindow = 3, horizon = 1, skip = 0, fixedWindow = F)
      
      # LABORABLE
      
      for (j in 1:length(slicesLab$train)) {
        
        train_index <- slicesLab$train[[j]]
        test_index <- slicesLab$test[[j]]
        
        trainSet <- datosLab[train_index, ] %>% na.omit()
        testSet <- datosLab[test_index, ] %>% na.omit()
        
        tunned_model <- tune.svm(kWh ~ timestamp, data = trainSet, gamma = svmHP$gamma,
                                 cost = svmHP$cost)
        
        bestCost <- tunned_model$best.model$cost
        
        # PROBLEMA, TUNE SVM NO SACA LA MEJOR GAMMA 
        
        model <- e1071::svm(kWh ~ timestamp, data = trainSet, kernel = hp$kernel,
                            cost = hp$cost, gamma = hp$gamma, type = "eps-regression")
        fit <- predict(model, h = 1)
        
        smape = smape(testSet$kWh, fit)
        rmse = rmse(testSet$kWh, fit)
        
        
        resultadosSVM <<- resultadosSVM %>% 
          add_row(
            Hora = hora,
            # MASE = test_metrics["MASE"],
            sMAPE = smape,
            RMSE = rmse,
            kernel = hp$kernel, # Ajusta el índice de la columna según corresponda
            cost = hp$cost,  # Ajusta el índice de la columna según corresponda
            gamma = hp$gamma,   # Ajusta el índice de la columna según corresponda
            TipoDia = "Laborable"
          )
        write.csv(resultadosSVM, file = fileIteracion, append = T, col.names = F)
      }
      
      
      
      
      
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

