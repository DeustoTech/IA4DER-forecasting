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


RESULT_FILE <- "ResultadosHiperNuevos.csv"


ResultadosModelos <- tibble(
  ID = character(),
  Hora = numeric(),
  TipoDia = character(),
  Real = numeric(),
  
  Media_pred = numeric(),
  Naive_pred = numeric(),
  SNaive_pred = numeric(),
  Arima_pred = numeric(),
  ETS_pred = numeric(),
  NN_pred = numeric(),
  SMV_pred = numeric(),
  Ensenmble_pred = numeric(),
  
  Media_mape = numeric(),
  Naive_mape = numeric(),
  SNaive_mape = numeric(),
  Arima_mape = numeric(),
  ETS_mape = numeric(),
  NN_mape = numeric(),
  SMV_mape = numeric(),
  Ensenmble_mape = numeric()
  
  
)
#lo pongo aqui por si tenemos que volver a usarlo
#mape <- mape(actual[aux], predicted[aux])

fwrite(ResultadosModelos, file = RESULT_FILE, col.names = T)  # SOLO SI SE QUIERE HACER UNO NUEVO

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
  
  nombre     <- tools::file_path_sans_ext(csv_file)
  ID <-  sub("TransformersV2/", "", nombre)
  
  if ("time" %in% colnames(csv_actual)) {
    # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
    colnames(csv_actual)[colnames(csv_actual) == "time"] <- "timestamp"
    csv_actual$imputed <- 0
    csv_actual <- csv_actual %>% select(timestamp, kWh, imputed)
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
      iteracionesLab <- floor(nrow(datosLab) / (F_DAYS + T_DAYS))
      iteracionesFinde <- floor(nrow(datosFinde) / (F_DAYS + T_DAYS))
      
      for (i in 1:iteracionesLab){
        
        mape_media <- c()
        mape_naive <- c()
        mape_snaive <- c()
        mape_arima <- c()
        mape_ets <- c()
        mape_nn <- c()
        mape_svm <- c()
        mape_ensemble <- c()
        
        
        train_start <- (i - 1) * (F_DAYS + T_DAYS) + 1
        train_end <- train_start + T_DAYS - 1
        test_start <- train_end + 1
        test_end <- test_start + F_DAYS - 1
        
        trainSetTs <- datosLab[train_start : train_end]
        testSetTs <- datosLab[test_start : test_end]
        
        trainSet <- zoo(trainSetTs$kWh, order.by = trainSetTs$timestamp)
        actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) # testSet
        aux  <- actual != 0
        
        # MEDIA
        
        media <- predict(mean(trainSet, h = F_DAYS))
        media <- media$mean
        media <- coredata(media)
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_media[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - media[aux_dia]) / actual[aux_dia], NA))
        }
        
        # NAIVE
        
        naive <- predict(naive(trainSet, h = F_DAYS))
        naive <- naive$mean
        naive <- coredata(naive)
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_naive[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - naive[aux_dia]) / actual[aux_dia], NA))
        }
        
        # SNAIVE
        
        snaive <- as.numeric(tail(trainSet, F_DAYS)) # Repetir los valores de la ultima semana
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_snaive[j] <- ifelse(sum(aux_dia) != 0, 100 * median(abs(actual[aux_dia] - snaive[aux_dia]) / actual[aux_dia], na.rm = TRUE), NA)
        }
        
        # ARIMA
        
        arima <- forecast(auto.arima(trainSet), h = F_DAYS)
        arima <- arima$mean
        arima <- coredata(arima)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_arima[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - arima[aux_dia]) / actual[aux_dia], NA))
        }
        
        # ETS
        trainSetTsETS <- trainSetTs$kWh
        ets <- forecast(ets(trainSetTsETS), h = F_DAYS)
        ets <- ets$mean
        ets <- coredata(ets)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_ets[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - ets[aux_dia]) / actual[aux_dia], NA))
        }
        
        
        # NN
        
        nn <- forecast(nnetar(trainSetTs$kWh), h = F_DAYS)
        nn <- nn$mean
        nn <- coredata(nn)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_nn[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - nn[aux_dia]) / actual[aux_dia], NA))
        }
        
        # SVM
        
        lagged <- merge(trainSet, shift(trainSet, -F_DAYS))
        SVM_TRAINSET <- window(lagged,start=index(trainSet)[length(trainSet) - T_DAYS + 1]) # Es el trainset. Coge los días marcado por T_DAYS
        PREDICT <- data.frame(past = as.numeric(tail(trainSet, F_DAYS)))
        names(SVM_TRAINSET) <- c("actual", "past")
        
        modelo_svm <- tune(e1071::svm, actual ~ past, data = SVM_TRAINSET, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
        
        svm <- predict(modelo_svm$best.model, newdata = PREDICT)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_svm[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - svm[aux_dia]) / actual[aux_dia], NA))
        }
        
        # ENSEMBLE
        
        ensemble <- c()
        
        for (j in 1:F_DAYS){
          
          ensemble[j] <- median(media[j], naive[j], snaive[j], arima[j], ets[j], nn[j], svm[j])
          
          aux_dia = aux[j]
          mape_ensemble[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - ensemble[aux_dia]) / actual[aux_dia], NA))
        }
        
        for (j in 1:F_DAYS) {
          # ... (resto del código)
          
          # Escribir directamente en el archivo CSV usando fwrite
          fwrite(
            data.table(
              ID = ID,
              Hora = hora,
              TipoDia = "Laborable",
              Real = actual[j],
              Media_pred = media[j],
              Naive_pred = naive[j],
              SNaive_pred = snaive[j],
              Arima_pred = arima[j],
              ETS_pred = ets[j],
              NN_pred = nn[j],
              SMV_pred = svm[j],
              Ensemble_pred = ensemble[j],
              Media_mape = mape_media[j],
              Naive_mape = mape_naive[j],
              SNaive_mape = mape_snaive[j],
              Arima_mape = mape_arima[j],
              ETS_mape = mape_ets[j],
              NN_mape = mape_nn[j],
              SMV_mape = mape_svm[j],
              Ensemble_mape = mape_ensemble[j]
            ),
            file = RESULT_FILE,
            append = TRUE,
            col.names = !file.exists(RESULT_FILE)  # Agregar encabezados solo si el archivo no existe
          )
        }
        
      
      # AHORA LO MISMO PERO CON FINDE
      
      for (i in 1:iteracionesFinde){
        
        mape_media <- c()
        mape_naive <- c()
        mape_snaive <- c()
        mape_arima <- c()
        mape_ets <- c()
        mape_nn <- c()
        mape_svm <- c()
        mape_ensemble <- c()
        
        train_start <- (i - 1) * (F_DAYS + T_DAYS) + 1
        train_end <- train_start + T_DAYS - 1
        test_start <- train_end + 1
        test_end <- test_start + F_DAYS - 1
        
        trainSetTs <- datosLab[train_start : train_end]
        testSetTs <- datosLab[test_start : test_end]
        
        trainSet <- zoo(trainSetTs$kWh, order.by = trainSetTs$timestamp)
        actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) # testSet
        aux <- actual != 0
        
        # MEDIA
        
        media <- predict(mean(trainSet, h = F_DAYS))
        media <- media$mean
        media <- coredata(media)
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_media[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - media[aux_dia]) / actual[aux_dia], NA))
        }
        
        # NAIVE
        
        naive <- predict(naive(trainSet, h = F_DAYS))
        naive <- naive$mean
        naive <- coredata(naive)
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_naive[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - naive[aux_dia]) / actual[aux_dia], NA))
        }
        
        # SNAIVE
        
        snaive <- as.numeric(tail(trainSet, F_DAYS)) # Repetir los valores de la ultima semana
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_snaive[j] <- ifelse(sum(aux_dia) != 0, 100 * median(abs(actual[aux_dia] - snaive[aux_dia]) / actual[aux_dia], na.rm = TRUE), NA)
        }
        
        # ARIMA
        
        arima <- forecast(auto.arima(trainSet), h = F_DAYS)
        arima <- arima$mean
        arima <- coredata(arima)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_arima[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - arima[aux_dia]) / actual[aux_dia], NA))
        }
        
        # ETS
        trainSetTsETS <- trainSetTs$kWh
        ets <- forecast(ets(trainSetTsETS), h = F_DAYS)
        ets <- ets$mean
        ets <- coredata(ets)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_ets[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - ets[aux_dia]) / actual[aux_dia], NA))
        }
        
        
        # NN
        
        nn <- forecast(nnetar(trainSetTs$kWh), h = F_DAYS)
        nn <- nn$mean
        nn <- coredata(nn)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_nn[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - nn[aux_dia]) / actual[aux_dia], NA))
        }
        
        # SVM
        
        lagged <- merge(trainSet, shift(trainSet, -F_DAYS))
        SVM_TRAINSET <- window(lagged,start=index(trainSet)[length(trainSet) - T_DAYS + 1]) # Es el trainset. Coge los días marcado por T_DAYS
        PREDICT <- data.frame(past = as.numeric(tail(trainSet, F_DAYS)))
        names(SVM_TRAINSET) <- c("actual", "past")
        
        modelo_svm <- tune(e1071::svm, actual ~ past, data = SVM_TRAINSET, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
        
        svm <- predict(modelo_svm$best.model, newdata = PREDICT)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_svm[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - svm[aux_dia]) / actual[aux_dia], NA))
        }
        
        # ENSEMBLE
        
        ensemble <- c()
        
        for (j in 1:F_DAYS){
          
          ensemble[j] <- median(media[j], naive[j], snaive[j], arima[j], ets[j], nn[j], svm[j])
          
          aux_dia = aux[j]
          mape_ensemble[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - ensemble[aux_dia]) / actual[aux_dia], NA))
        }
        
        for (j in 1:F_DAYS) {
          # ... (resto del código)
          
          # Escribir directamente en el archivo CSV usando fwrite
          fwrite(
            data.table(
              ID = ID,
              Hora = hora,
              TipoDia = "Laborable",
              Real = actual[j],
              Media_pred = media[j],
              Naive_pred = naive[j],
              SNaive_pred = snaive[j],
              Arima_pred = arima[j],
              ETS_pred = ets[j],
              NN_pred = nn[j],
              SMV_pred = svm[j],
              Ensemble_pred = ensemble[j],
              Media_mape = mape_media[j],
              Naive_mape = mape_naive[j],
              SNaive_mape = mape_snaive[j],
              Arima_mape = mape_arima[j],
              ETS_mape = mape_ets[j],
              NN_mape = mape_nn[j],
              SMV_mape = mape_svm[j],
              Ensemble_mape = mape_ensemble[j]
            ),
            file = RESULT_FILE,
            append = TRUE,
            col.names = !file.exists(RESULT_FILE)  # Agregar encabezados solo si el archivo no existe
          )
        }
        
        
        
      }
    }
  }
}
num_cores <- 4  # Ajusta según la cantidad de núcleos de tu CPU

# Configurar el clúster paralelo
cl <- makeCluster(num_cores)
registerDoParallel(cl)

foreach(csv_file = N, 
        .packages = librerias) %do% predict_models(csv_file)

for (csv_file in N) {
  predict_models(csv_file)
}

print(ResultadosModelos)























# Pruebas

COMPLETE <- 0.10
horas <- 0:23
dias_semana <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
F_DAYS <- 7 # Días que vamos a predecir con SVM
T_DAYS <- 60 # Días con los que vamos a entrenar en cada trozo

svmHP <- list( # Posibles valores para tunear SVM
  cost = 10^(-4:4),
  gamma = 10^(-3:2)
)

csvPrueba <- fread(N[1])
csvPrueba <- csvPrueba[0:5000,]



ResultadosModelos <- tibble(
  ID = character(),
  Hora = numeric(),
  TipoDia = character(),
  Real = numeric(),
  
  Media_pred = numeric(),
  Naive_pred = numeric(),
  SNaive_pred = numeric(),
  Arima_pred = numeric(),
  ETS_pred = numeric(),
  NN_pred = numeric(),
  SMV_pred = numeric(),
  Ensenmble_pred = numeric(),
  
  Media_mape = numeric(),
  Naive_mape = numeric(),
  SNaive_mape = numeric(),
  Arima_mape = numeric(),
  ETS_mape = numeric(),
  NN_mape = numeric(),
  SMV_mape = numeric(),
  Ensenmble_mape = numeric()
  
  
)

nombre     <- tools::file_path_sans_ext(N[1])
ID <-  sub("TransformersV2/", "", nombre)


if ("time" %in% colnames(csvPrueba)) {
  # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
  colnames(csvPrueba)[colnames(csvPrueba) == "time"] <- "timestamp"
  colnames(csvPrueba)[colnames(csvPrueba) == "issue"] <- "imputed"
  csvPrueba <- csvPrueba %>% select(timestamp, kWh, imputed)
}

a <- csvPrueba %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                          "Laborable", "Finde")) %>%  select(-imputed)


datos_hora20 <- a[hour(a$timestamp) == 20,]
datosLab <- datos_hora20 %>% filter(TipoDia == "Laborable") %>% unique()
datosFinde <- datos_hora20 %>% filter(TipoDia == "Finde") %>% unique()

iteracionesLab <- floor(nrow(datosLab) / (F_DAYS + T_DAYS))
iteracionesFinde <- floor(nrow(datosFinde) / (F_DAYS + T_DAYS))

mape_media <- c()
mape_naive <- c()
mape_snaive <- c()
mape_arima <- c()
mape_ets <- c()
mape_nn <- c()
mape_svm <- c()
mape_ensemble <- c()


train_start <- (i - 1) * (F_DAYS + T_DAYS) + 1
train_end <- train_start + T_DAYS - 1
test_start <- train_end + 1
test_end <- test_start + F_DAYS - 1

trainSetTs <- datosLab[train_start : train_end]
testSetTs <- datosLab[test_start : test_end]

trainSet <- zoo(trainSetTs$kWh, order.by = trainSetTs$timestamp)
actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) # testSet
aux  <- actual != 0

# MEDIA

media <- predict(mean(trainSet, h = F_DAYS))
media <- media$mean
media <- coredata(media)

for (j in 1:F_DAYS){
  aux_dia = aux[j]
  mape_media[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - media[aux_dia]) / actual[aux_dia], NA))
}

# NAIVE

naive <- predict(naive(trainSet, h = F_DAYS))
naive <- naive$mean
naive <- coredata(naive)

for (j in 1:F_DAYS){
  aux_dia = aux[j]
  mape_naive[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - naive[aux_dia]) / actual[aux_dia], NA))
}

# SNAIVE

snaive <- as.numeric(tail(trainSet, F_DAYS)) # Repetir los valores de la ultima semana
for (j in 1:F_DAYS){
  aux_dia = aux[j]
  mape_snaive[j] <- ifelse(sum(aux_dia) != 0, 100 * median(abs(actual[aux_dia] - snaive[aux_dia]) / actual[aux_dia], na.rm = TRUE), NA)
}

# ARIMA

arima <- forecast(auto.arima(trainSet), h = F_DAYS)
arima <- arima$mean
arima <- coredata(arima)


for (j in 1:F_DAYS){
  aux_dia = aux[j]
  mape_arima[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - arima[aux_dia]) / actual[aux_dia], NA))
}

# ETS
trainSetTsETS <- trainSetTs$kWh
ets <- forecast(ets(trainSetTsETS), h = F_DAYS)
ets <- ets$mean
ets <- coredata(ets)


for (j in 1:F_DAYS){
  aux_dia = aux[j]
  mape_ets[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - ets[aux_dia]) / actual[aux_dia], NA))
}


# NN

nn <- forecast(nnetar(trainSetTs$kWh), h = F_DAYS)
nn <- nn$mean
nn <- coredata(nn)


for (j in 1:F_DAYS){
  aux_dia = aux[j]
  mape_nn[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - nn[aux_dia]) / actual[aux_dia], NA))
}

# SVM

lagged <- merge(trainSet, shift(trainSet, -F_DAYS))
SVM_TRAINSET <- window(lagged,start=index(trainSet)[length(trainSet) - T_DAYS + 1]) # Es el trainset. Coge los días marcado por T_DAYS
PREDICT <- data.frame(past = as.numeric(tail(trainSet, F_DAYS)))
names(SVM_TRAINSET) <- c("actual", "past")

modelo_svm <- tune(e1071::svm, actual ~ past, data = SVM_TRAINSET, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))

svm <- predict(modelo_svm$best.model, newdata = PREDICT)


for (j in 1:F_DAYS){
  aux_dia = aux[j]
  mape_svm[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - svm[aux_dia]) / actual[aux_dia], NA))
}

# ENSEMBLE

ensemble <- c()

for (j in 1:F_DAYS){
  
  ensemble[j] <- median(media[j], naive[j], snaive[j], arima[j], ets[j], nn[j], svm[j])
  
  aux_dia = aux[j]
  mape_ensemble[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - ensemble[aux_dia]) / actual[aux_dia], NA))
  }
}


