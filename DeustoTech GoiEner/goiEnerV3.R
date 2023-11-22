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


RESULT_FILE <- "ResultadosNuevosCT_Media.csv"


ResultadosModelos <- tibble(
  ID = character(),
  Hora = numeric(),
  TipoDia = character(),
  Real = numeric(),
  
  Media_pred = numeric(),
  Naive_pred = numeric(),
  SNaive_pred = numeric,
  Arima_pred = numeric(),
  ETS_pred = numeric(),
  NN_pred = numeric(),
  SMV_pred = numeric(),
  Ensenmble_pred = numeric(),
  
  Media_mape = numeric(),
  Naive_mape = numeric(),
  SNaive_mape = numeric,
  Arima_mape = numeric(),
  ETS_mape = numeric(),
  NN_mape = numeric(),
  SMV_mape = numeric(),
  Ensenmble_mape = numeric(),
  
  Media_rmse = numeric(),
  Naive_rmse = numeric(),
  SNaive_rmse = numeric,
  Arima_rmse = numeric(),
  ETS_rmse = numeric(),
  NN_rmse = numeric(),
  SMV_rmse = numeric(),
  Ensenmble_rmse = numeric()
  
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
        
        train_start <- (i - 1) * (F_DAYS + T_DAYS) + 1
        train_end <- train_start + T_DAYS - 1
        test_start <- train_end + 1
        test_end <- test_start + F_DAYS - 1
        
        trainSetTs <- datosLab[train_start : train_end]
        testSetTs <- datosLab[test_start : test_end]
        
        trainSet <- zoo(trainSetTs$kWh, order.by = trainSetTs$timestamp)
        actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) # testSet
        
        # MEDIA
        
        predicted <- predict(mean(trainSet))
        predicted <- predicted$mean
        predicted <- coredata(predicted)[1]
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        
        
        ResultadosModelos <- ResultadosModelos %>% add_row(
          ID = ID,
          Hora = hora,
          TipoDia = "Laborable",
          Real = as.numeric(actual),
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE = mape,
          Modelo = "Media"
        )
        
        fwrite(ResultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
        
      # AHORA LO MISMO PERO CON FINDE
      
      for (i in 1:iteracionesFinde){
        
        
        train_start <- (i - 1) * (F_DAYS + T_DAYS) + 1
        train_end <- train_start + T_DAYS - 1
        test_start <- train_end + 1
        test_end <- test_start + F_DAYS - 1
        
        trainSetTs <- datosLab[train_start : train_end]
        testSetTs <- datosLab[test_start : test_end]
        
        trainSet <- zoo(trainSetTs$kWh, order.by = trainSetTs$timestamp)
        actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) # testSet
        
        # MEDIA
        
        predicted <- predict(mean(trainSet))
        predicted <- predicted$mean
        predicted <- coredata(predicted)[1]
        
        aux  <- actual != 0
        smape <- smape(actual, predicted)
        rmse <- rmse(actual, predicted)
        mase <- mase(actual, predicted)
        if (!is.finite(mase)) { mase <- NA}
        mape <- 100*median(ifelse(sum(aux)!=0,abs(actual[aux]-predicted[aux])/actual[aux],NA))
        
        ResultadosModelos <- ResultadosModelos %>% add_row(
          ID = ID,
          Hora = hora,
          TipoDia = "Finde",
          Real = as.numeric(actual),
          Predicted = predicted,
          sMAPE = smape,
          RMSE = rmse,
          MASE = mase,
          MAPE = mape,
          Modelo = "Media"
        )
        
        fwrite(ResultadosModelos, file = RESULT_FILE, col.names = FALSE, append = TRUE)
        
      }
    }
  }
  }
}
num_cores <- 4  # Ajusta según la cantidad de núcleos de tu CPU

# Configurar el clúster paralelo
cl <- makeCluster(num_cores)
registerDoParallel(cl)

foreach(csv_file = CT, 
        .packages = librerias) %dopar% predict_models(csv_file)

print(ResultadosModelos)
