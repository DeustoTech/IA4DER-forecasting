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


#descropmir carpeta de 200 csv
path <- "Transformers.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina
unzip(path, exdir = tempdir) # descomprime. Tarda un poco
plan(multisession)


folder <- "Transformers/"
# Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)

csv_files <- csv_files[201:length(csv_files)]

N <- csv_files[!grepl("-CT\\.csv$", csv_files) & !grepl("-L\\.csv$", csv_files)]
CT <- csv_files[grepl("-CT\\.csv$", csv_files)]
L <- csv_files[grepl("-L\\.csv$", csv_files)]

# Agregar el prefijo 'folder' a las rutas en N, CT y L
N <- paste(folder, N, sep = "")
CT <- paste(folder, CT, sep = "")
L <- paste(folder, L, sep = "")


RESULT_FILE <- "ResultadosCT.csv"


resultadosModelos <- tibble(
  Hora = numeric(),
  TipoDia = character(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  Modelo = character()
)

fwrite(resultadosModelos, file = RESULT_FILE, col.names = T)  # SOLO SI SE QUIERE HACER UNO NUEVO

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

# set.seed(123)


# Crear todas las combinaciones de hiperparámetros
svmHP <- list(
  cost = 10^(-4:4),
  gamma = 10^(-3:2)
)


#funcion grande con todos los modelos
predict_models <- function(csv_file) {
  
  csv_actual <- fread(csv_file)
  
  if ("time" %in% colnames(csv_actual)) {
    # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
    colnames(csv_actual)[colnames(csv_actual) == "time"] <- "timestamp"
    csv_actual$imputed <- 0
  }
  
  
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
      
      # #MEDIA L
      
      # USAR EL INITIAL CON MAS O MENOS LA MITAD DE LA SERIE TEMPORAL. MIRARLO MAS
      errors <- tsCV(datosLab$kWh, mediaF, h = 1, window = 5, initial = ) %>% na.omit()
      actual <- datosLab$kWh[1: length(errors)]
      predicted <- actual + errors
      
      aux  <- actual != 0
      # mape a mano
      
      mape2 <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))


      smape <- smape(actual, predicted)
      mape <- mape(actual[aux], predicted[aux])
      rmse <- rmse(actual, predicted)
      mase <- mase(actual, predicted)
      
      

      resultadosModelos <- resultadosModelos %>% add_row(
        Hora = hora,
        TipoDia = "Laborable",
        Predicted = predicted,
        sMAPE = smape,
        RMSE = rmse,
        MASE = mase,
        MAPE1 = mape,
        MAPEBien = mape2,
        Modelo = "Media" # AÑADIR MAPE ARRIBA
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

      # tuned_nn <- tune.nnet(kWh ~ timestamp, data = datosLab, size = NEURON_RANGE)
      # best_size <- as.numeric(tuned_nn$best.parameters)
      
      errors <- tsCV(datosLab$kWh, nnF, h = 1, window = 5, n = best_size) %>% na.omit()
      # QUITAR BEST SIZE. LA WINDOW PEQUEÑA = MUELTE. PONER EL WINDOW AL MISMO TAMAÑO Q INITIAL
      actual <- datosLab$kWh[1: length(errors)]
      predicted <- actual + errors

      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      mase <- mase(actual, predicted)

      # mase peta. Creemos que es por como have CV, pero no lo tenemos claro
      # lo dejamos en NA
      # if (is.numeric(mase(actual, predicted))){  mase <- mase(actual, predicted)      }


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
      best_size <- as.numeric(tuned_nn$best.parameters)
      
      errors <- tsCV(datosFinde$kWh, nnF, h = 1, window = 3, n = best_size) %>% na.omit()
      actual <- datosFinde$kWh[1: length(errors)]
      predicted <- actual + errors

      smape <- smape(actual, predicted)
      rmse <- rmse(actual, predicted)
      mase <- mase(actual, predicted)

      # mase peta. Creemos que es por como have CV, pero no lo tenemos claro
      # lo dejamos en NA
      # if (is.numeric(mase(actual, predicted))){  mase <- mase(actual, predicted)      }

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
      
      # antes de hacer CV, sacamos los hiperparámetros para toda la serie temporal
      
      tunned_model <- tune.svm(kWh ~ timestamp, data = datosLab, kernel = "linear",
                               gamma = svmHP$gamma, cost = svmHP$gamma)
      
      bestGamma <- tunned_model$best.parameters$gamma
      bestCost <- tunned_model$best.parameters$cost

      
      rr              <- merge(r,lag(r,-1))
      SVM    <- tune(svm,real~past,data=TRAINSET,ranges=list(elsilon=seq(0,1,0.2), cost=seq(1,100,10)))
      TRAINSET        <- window(rr,start=index(r)[length(r)-24*T_DAYS-23])
      PREDICT         <- data.frame(past=as.numeric(window(r,start=index(r)[length(r)-24*F_DAYS+1])))
      names(TRAINSET) <- c("real","past")

      f["ens"]   <- rowMedians(as.matrix(f),na.rm=T)
      

      # Ahora, usando estos parámetros, hacemos CV
            
      for (j in 1:length(slicesLab$train)) {
        
        train_index <- slicesLab$train[[j]]
        test_index <- slicesLab$test[[j]]
        
        trainSet <- datosLab[train_index, ] %>% na.omit()
        testSet <- datosLab[test_index, ] %>% na.omit()
        
        model <- e1071::svm(kWh ~ timestamp, data = trainSet, kernel = "linear",
                            cost = bestCost, gamma = bestGamma, type = "eps-regression")
        fit <- predict(model, h = 1)
        
        smape = smape(testSet$kWh, fit)
        rmse = rmse(testSet$kWh, fit)
        mase <- mase(testSet$kWh, fit)
        
        # if (is.numeric(mase(actual, fit))){  mase <- mase(actual, fit)      }
        
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Laborable",
          Predicted = fit,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          Modelo = "SVM"
        ) 
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      }
      
      # LO MISMO PERO CON FINDE
      
      tunned_model <- tune.svm(kWh ~ timestamp, data = datosFinde, kernel = "linear",
                               gamma = svmHP$gamma, cost = svmHP$gamma)
      
      bestGamma <- tunned_model$best.parameters$gamma
      bestCost <- tunned_model$best.parameters$cost


      # Ahora, usando estos parámetros, hacemos CV
            
      for (j in 1:length(slicesFinde$train)) {
        
        train_index <- slicesFinde$train[[j]]
        test_index <- slicesFinde$test[[j]]
        
        trainSet <- datosFinde[train_index, ] %>% na.omit()
        testSet <- datosFinde[test_index, ] %>% na.omit()
        
        model <- e1071::svm(kWh ~ timestamp, data = trainSet, kernel = "linear",
                            cost = bestCost, gamma = bestGamma, type = "eps-regression")
        fit <- predict(model, h = 1)
        
        smape <- smape(testSet$kWh, fit)
        rmse <- rmse(testSet$kWh, fit)
        mase <- mase(testSet$kWh, fit)
        # if (is.numeric(mase(actual, fit))){  mase <- mase(actual, fit)      }
      
        
        resultadosModelos <- resultadosModelos %>% add_row(
          Hora = hora,
          TipoDia = "Finde",
          Predicted = fit,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          Modelo = "SVM"
        ) 
        fwrite(resultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
      }
  
      
    }
    
  }
  
  
}


# num_cores <- 4  # Ajusta según la cantidad de núcleos de tu CPU
# 
# # Configurar el clúster paralelo
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)


#ejecutar funcion para todos los csv
foreach(csv_file = CT, 
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


prueba <- fread(paste("Transformers/",csv_files[1], sep = ""))
prueba <- prueba %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                          "Laborable", "Finde")) 

LENGTH  <- length(prueba$kWh)
ZEROS   <- sum(prueba$kWh==0)/LENGTH
IMPUTED <- sum(prueba$imputed == 1)/LENGTH

prueba <- prueba %>% select(- imputed)


prueba_hora <- prueba[hour(prueba$timestamp) == 20,]

datosLabP <- prueba_hora %>% filter(TipoDia == "Laborable") %>% unique()




tuned_nn <- tune.nnet(kWh ~ timestamp, data = datosLabP, size = NEURON_RANGE)
best_size <- as.numeric(tuned_nn$best.parameters)

errors <- tsCV(datosLabP$kWh, nnF, h = 1, window = 5, n = best_size) %>% na.omit()
actual <- datosLabP$kWh[1: length(errors)]
predicted <- actual + errors

smape <- smape(actual, predicted)
rmse <- rmse(actual, predicted)

# mase peta. Creemos que es por como have CV, pero no lo tenemos claro
# lo dejamos en NA
# if (is.numeric(mase(actual, predicted))){  mase <- mase(actual, predicted)      }


resultadosModelos <- resultadosModelos %>% add_row(
  Hora = hora,
  TipoDia = "Laborable",
  Predicted = predicted,
  sMAPE = smape,
  RMSE = rmse,
  MASE = NA,
  Modelo = "NN"
)


kernel_valuesP <- c("linear", "radial")
cost_valuesP <- seq(0.01, 20, length.out = 5) 
gamma_valuesP <- seq(0.01,20, length.out = 5)  

# Crear todas las combinaciones de hiperparámetros
svmHP_P <- expand.grid(
  kernel = kernel_valuesP,
  cost = cost_valuesP,
  gamma = gamma_valuesP
)

svmHP_P <- list(
  cost = 10^(-3:3),
  gamma = 10^(-2:2),
  kernel = c("radial", "linear")
)


prueba_hora <- prueba[hour(prueba$timestamp) == 20,]

datosLabP <- prueba_hora %>% filter(TipoDia == "Laborable") %>% unique()
datosFindeP <- prueba_hora %>% filter(TipoDia == "Finde") %>% unique()

tuned_model <- tune.svm(kWh ~ timestamp, data = datosLabP[1:130], kernel = "linear",
                        gamma = svmHP_P$gamma, cost = svmHP_P$cost)




if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)) {
  
  prueba_hora <- prueba[hour(prueba$timestamp) == 20,]
  
  datosLabP <- prueba_hora %>% filter(TipoDia == "Laborable") %>% unique()
  datosFindeP <- prueba_hora %>% filter(TipoDia == "Finde") %>% unique()

  tuned_model <- tune.svm(kWh ~ timestamp, data = datosLabP[1:50], kernel = "radial", cost = svmHP_P$cost, gamma = svmHP_P$gamma)
  
  cost <- tuned_model$best.parameters$cost
  gamma <- tuned_model$best.parameters$gamma
  
  model <- svm(kWh ~ timestamp, data = datosLabP, kernel = "radial", cost = cost, gamma = gamma, epsilon = 0.1)

  ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
  cv_model <- train(model, data = datosLabP, method = "svm", trControl = ctrl)
  
  slicesLab <- createTimeSlices(prueba_hora$timestamp, initialWindow = 5, horizon = 1, skip = 0, fixedWindow = F)
  slicesFinde <- createTimeSlices(prueba_hora$timestamp, initialWindow = 3, horizon = 1, skip = 0, fixedWindow = F)
  
  for (j in 1:length(slicesLab$train)) {
    
    train_index <- slicesLab$train[[j]]
    test_index <- slicesLab$test[[j]]
    
    trainSet <- datosLabP[train_index, ] %>% na.omit()
    testSet <- datosLabP[test_index, ] %>% na.omit()
    
    svmGrid <- expand.grid(C = c(0.1, 1, 10), gamma = c(0.1, 1, 10))
    svm_params <- e1071::tune.svm(kWh ~ timestamp, data = trainSet, kernel = "radial", 
                           tuneGrid = svmHP_P)
  
    svm_params$best.parameters
    
    model <- svm(kWh ~ timestamp, data = trainSet, )
    
    
      resultadosModelos <<- resultadosModelos %>% 
      add_row(
        Hora = hora,
        TipoDia = "Laborable",
        Predicted = fit,
        sMAPE = smape,
        RMSE = rmse,
        MASE = NA,
        Modelo = "SVM"
      )
    write.csv(resultadosModelos, file = fileIteracion, append = T, col.names = F)
  }
}

