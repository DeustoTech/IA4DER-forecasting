library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


plan(multisession)


folder <- "TransformersV2/"
# Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)

# csv_files <- csv_files[201:length(csv_files)]

N <- csv_files[!grepl("-CT\\.csv$", csv_files) & !grepl("-L\\.csv$", csv_files)]
CT <- csv_files[grepl("-CT\\.csv$", csv_files)]
L <- csv_files[grepl("-L\\.csv$", csv_files)]

# Agregar el prefijo 'folder' a las rutas en N, CT y L
N <- paste(folder, N, sep = "")
CT <- paste(folder, CT, sep = "")
L <- paste(folder, L, sep = "")


RESULT_FILE <- "PruebasAsier.csv"


resultadosModelos <- tibble(
  Hora = numeric(),
  TipoDia = character(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  MAPE1 = numeric(),
  MAPEBien = numeric(),
  Modelo = character()
)


fwrite(resultadosModelos, file = RESULT_FILE, col.names = T)  # SOLO SI SE QUIERE HACER UNO NUEVO

COMPLETE <- 0.10
horas <- 0:23
dias_semana <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
F_DAYS <- 7 # Días que vamos a predecir con SVM
T_DAYS <- 60 # Días con los que vamos a entrenar en cada trozo



svmHP <- list( # Posibles valores para tunear SVM
  cost = 10^(-4:4),
  gamma = 10^(-3:2)
)


predict_models <- function(csv_file){
  
  csv_actual <- fread(csv_file)
  
  if ("time" %in% colnames(csv_actual)) {
    # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
    colnames(csv_actual)[colnames(csv_actual) == "time"] <- "timestamp"
    csv_actual$imputed <- 0
    csv_actual <- csv_actual %>% select(timestamp, kWh)
  }
  
  
  a <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%  select(-imputed)
  
  LENGTH  <- length(csv_actual$kWh)
  ZEROS   <- sum(csv_actual$kWh==0)/LENGTH
  IMPUTED <- sum(csv_actual$imputed == 1)/LENGTH
  
  
  
  if ( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)){
    
    foreach(hora = horas, .packages = librerias) %dopar% {
      
      datos_hora <- a[hour(a$timestamp) == hora,]
      datosLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% unique()
      datosFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% unique()
      
      # PARTIMOS LA SERIE TEMPORAL N VECES: 60 DIAS ENTRENAMIENTO, 7 PREDICCION
      
      # primero con los laborables
      
      for (i in 1:(nrow(datosLab) - T_DAYS - F_DAYS + 1)){
        
        train_start <- i
        train_end <- i + T_DAYS - 1
        test_start <- i + T_DAYS
        test_end <- i + T_DAYS + F_DAYS - 1
        
        trainSetTs <- datosLab[train_start : train_end]
        testSetTs <- datosLab[test_start : test_end]
        
        trainSet <- zoo(trainSetTs$kWh, order.by = trainSetTs$timestamp)
        actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) # testSet
        
        # MEDIA
        
        predicted <- predict(mean(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "Media"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # NAIVE
        
        predicted <- predict(naive(trainSet))
        predicted <- predicted$mean

        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))

        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "Naive"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        #SNAIVE
        
        foreach(dia = dias_semana, .packages = librerias) %dopar% {
        
        snaive_trainsetTs <- trainSetTs[weekdays(trainSetTs$timestamp) == dia,]
        snaive_testsetTs <- testSet[weekdays(testSet$timestamp) == dia,]
        
        sn_trainSet <- zoo(snaive_trainsetTs$kWh, order.by = snaive_trainsetTs$timestamp)
        actual <- zoo(snaive_testsetTs$kWh, order.by = snaive_testsetTs$timestamp)
        
        predicted <- predict(snaive(sn_trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "SNaive"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        }
        
        # ARIMA 
        
        predicted <- forecast(auto.arima(trainSet))
        predicted <- predicted$mean

        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "Arima"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # ETS
        
        predicted <- forecast(ets(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "ETS"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # NN
        
        predicted <- forecast(nnetar(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "NN"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # SVM. HACER PRUEBAS PARA VER SI VA BIEN
        
        lagged <- merge(trainSet, lag(trainSet, 1))
        SVM_TRAINSET <- window(retrasados,start=index(trainSet)[length(trainSet) - T_DAYS]) # Es el trainset. Coge los días marcado por T_DAYS
        PREDICT <- data.frame(past = as.numeric(window(trainSet,
              start = index(trainSet)[length(trainSet - F_DAYS)])))
        names(SVM_TRAINSET) <- c("actual", "past")
        
        
        SVM <- tune(svm, actual ~ past, data = SVM_TRAINSET, ranges(list(gamma = 10^(-3:2), cost = 10^(-4:4))))
        
        predicted <- predict(SVM$best.model, PREDICT)
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "SVM"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      }
      
      
      # AHORA LO MISMO PERO CON FINDE
      
      for (i in 1:(nrow(datosFinde) - T_DAYS - F_DAYS + 1)){
        
        train_start <- i
        train_end <- i + T_DAYS - 1
        test_start <- i + T_DAYS
        test_end <- i + T_DAYS + F_DAYS - 1
        
        trainSetTs <- datosFinde[train_start : train_end]
        testSetTs <- datosFinde[test_start : test_end]
        
        trainSet <- zoo(trainSetTs$kWh, order.by = trainSetTs$timestamp)
        actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) # testSet
        
        # MEDIA
        
        predicted <- predict(mean(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Finde",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "Media"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # NAIVE
        
        predicted <- predict(naive(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Finde",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "Naive"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        #SNAIVE
        
        foreach(dia = dias_semana, .packages = librerias) %dopar% {
          
          snaive_trainsetTs <- trainSetTs[weekdays(trainSetTs$timestamp) == dia,]
          snaive_testsetTs <- testSet[weekdays(testSet$timestamp) == dia,]
          
          sn_trainSet <- zoo(snaive_trainsetTs$kWh, order.by = snaive_trainsetTs$timestamp)
          actual <- zoo(snaive_testsetTs$kWh, order.by = snaive_testsetTs$timestamp)
          
          predicted <- predict(snaive(sn_trainSet))
          predicted <- predicted$mean
          
          aux  <- actual != 0
          smape <- smape(actual, predicted)
          rmse <- rmse(actual, predicted)
          mase <- mase(actual, predicted)
          if (!is.finite(mase)) { mase <- NA}
          mape <- mape(actual[aux], predicted[aux])
          mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
          
          resultadosModelos <- resultadosModelos %>% add_row(
            Hora = hora,
            TipoDia = "Finde",
            Predicted = predicted,
            sMAPE = smape,
            RMSE = rmse,
            MASE = mase,
            MAPE1 = mape,
            MAPEBien = mape2,
            Modelo = "SNaive"
          )
          
          fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
          
        }
        
        # ARIMA 
        
        predicted <- forecast(auto.arima(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Finde",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "Arima"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # ETS
        
        predicted <- forecast(ets(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Finde",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "ETS"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # NN
        
        predicted <- forecast(nnetar(trainSet))
        predicted <- predicted$mean
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Finde",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "NN"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        # SVM. HACER PRUEBAS PARA VER SI VA BIEN
        
        lagged <- merge(trainSet, lag(trainSet, 1))
        SVM_TRAINSET <- window(retrasados,start=index(trainSet)[length(trainSet) - T_DAYS]) # Es el trainset. Coge los días marcado por T_DAYS
        PREDICT <- data.frame(past = as.numeric(window(trainSet,
                                                       start = index(trainSet)[length(trainSet - F_DAYS)])))
        names(SVM_TRAINSET) <- c("actual", "past")
        
        
        SVM <- tune(svm, actual ~ past, data = SVM_TRAINSET, ranges(list(gamma = 10^(-3:2), cost = 10^(-4:4))))
        
        predicted <- predict(SVM$best.model, PREDICT)
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- mape(actual[aux], predicted[aux])
        mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Finde",
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE1 = mape,
          MAPEBien = mape2,
          Modelo = "SVM"
        )
        
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      }
      
      
     
    }
    
  }
  
}

foreach(csv_file = CT, 
        .packages = librerias) %dopar% predict_models(csv_file)



# pruebas


csvPrueba <- fread(CT[1])

resultadosModelos <- tibble(
  Hora = numeric(),
  TipoDia = character(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  MAPE1 = numeric(),
  MAPEBien = numeric(),
  Modelo = character()
)


if ("time" %in% colnames(csvPrueba)) {
  # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
  colnames(csvPrueba)[colnames(csvPrueba) == "time"] <- "timestamp"
  csvPrueba$imputed <- 0
  csvPrueba <- csvPrueba %>% select(timestamp, kWh, imputed)
}

a <- csvPrueba %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                          "Laborable", "Finde")) %>%  select(-imputed)




datos_hora20 <- a[hour(a$timestamp) == 20,]
datosLab <- datos_hora20 %>% filter(TipoDia == "Laborable") %>% unique()
datosFinde <- datos_hora20 %>% filter(TipoDia == "Finde") %>% unique()


for (i in 1:(nrow(datosLab) - T_DAYS - F_DAYS + 1)){
  
  train_start <- i
  train_end <- i + T_DAYS - 1
  test_start <- i + T_DAYS
  test_end <- i + T_DAYS + F_DAYS - 1
  
  trainSet <- datosLab[train_start : train_end]
  testSet <- datosLab[test_start : test_end]
  
  trainSet <- zoo(trainSet$kWh, order.by = trainSet$timestamp)
  actual <- zoo(testSet$kWh, order.by = testSet$timestamp) # testSet
  
  # arima
  
  predicted <- forecast(auto.arima(trainSet))
  predicted <- predicted$mean
  
  aux  <- actual != 0
  smape <- smape(actual, predicted)
  rmse <- rmse(actual, predicted)
  mase <- mase(actual, predicted)
  if (!is.finite(mase)) { mase <- NA}
  mape <- mape(actual[aux], predicted[aux])
  mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
  
  resultadosModelos <- resultadosModelos %>% add_row(
    Hora = 20,
    TipoDia = "Laborable",
    Predicted = predicted,
    sMAPE = smape,
    RMSE = rmse,
    MASE = mase,
    MAPE1 = mape,
    MAPEBien = mape2,
    Modelo = "Arima"
  )
}
























