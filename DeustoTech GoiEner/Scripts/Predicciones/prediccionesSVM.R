# Este fichero es como goiEnerV3 pero solo para SVM.

library(foreach)
library(doParallel)
librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

folder <- "TransformersV2/"

## Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)

N <- csv_files[!grepl("-CT\\.csv$", csv_files) & !grepl("-L\\.csv$", csv_files)]
#CT <- csv_files[grepl("-CT\\.csv$", csv_files)]
#L <- csv_files[grepl("-L\\.csv$", csv_files)]

### Agregar el prefijo 'folder' a las rutas en N, CT y L
N <- paste(folder, N, sep = "")
#CT <- paste(folder, CT, sep = "")
#L <- paste(folder, L, sep = "")

## crear documento para los resultados
RESULT_FILE <- "ResultadosCUPS_SVM_2"

ResultadosModelos <- tibble(
  ID = character(),
  Hora = numeric(),
  TipoDia = character(),
  Real = numeric(),
  
  SMV_pred = numeric(),
  SMV_mape = numeric(),

)

fwrite(ResultadosModelos, file = RESULT_FILE, col.names = T)


## definir variable necesarias para la funcion
COMPLETE <- 0.10
horas <- 0:23
dias_semana <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
F_DAYS <- 7 
T_DAYS <- 60

### Posibles valores para tunear SVM
svmHP <- list( 
  cost = 10^(-4:4),
  gamma = 10^(-3:2)
)

predict_models <- function(csv_file){
  
  #leer csv y limpiar
  csv_actual <- fread(csv_file)
  
  nombre     <- tools::file_path_sans_ext(csv_file)
  ID <-  sub("TransformersV2/", "", nombre)
  
  #Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
  if ("time" %in% colnames(csv_actual)) {
    colnames(csv_actual)[colnames(csv_actual) == "time"] <- "timestamp"
    csv_actual$imputed <- 0
    csv_actual <- csv_actual %>% select(timestamp, kWh, imputed)
  }
  
  
  a <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(TipoDia = ifelse(weekdays(timestamp) %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%  select(-imputed)
  
  #extraer variables necesarias
  LENGTH  <- length(csv_actual$kWh)
  ZEROS   <- sum(csv_actual$kWh==0)/LENGTH
  IMPUTED <- sum(csv_actual$imputed == 1)/LENGTH
  
  
  #predicciones
  if ( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)){
    
    foreach(hora = horas, .packages = librerias) %dopar% {
      
      datos_hora <- a[hour(a$timestamp) == hora,]
      datosLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% unique()
      datosFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% unique()
      
      #PARTIMOS LA SERIE TEMPORAL N VECES: 60 DIAS ENTRENAMIENTO, 7 PREDICCION
      
      #primero con los laborables
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
        actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) 
        aux  <- actual != 0
        ### SVM
        
        lagged <- merge(trainSet, shift(trainSet, -F_DAYS))
        SVM_TRAINSET <- window(lagged,start=index(trainSet)[length(trainSet) - T_DAYS + 1]) 
        PREDICT <- data.frame(past = as.numeric(tail(trainSet, F_DAYS)))
        names(SVM_TRAINSET) <- c("actual", "past")
        
        modelo_svm <- tune(e1071::svm, actual ~ past, data = SVM_TRAINSET, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
        
        svm <- predict(modelo_svm$best.model, newdata = PREDICT)
        
        
        for (j in 1:F_DAYS){
          aux_dia = aux[j]
          mape_svm[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - svm[aux_dia]) / actual[aux_dia], NA))
        }
        
        
        
        for (j in 1:F_DAYS) {
          #... (resto del código)
          
          #Escribir directamente en el archivo CSV usando fwrite
          fwrite(
            data.table(
              ID = ID,
              Hora = hora,
              TipoDia = "Laborable",
              Real = actual[j],
              SMV_pred = svm[j],
              SMV_mape = mape_svm[j],
            ),
            file = RESULT_FILE,
            append = TRUE,
            col.names = !file.exists(RESULT_FILE)  #Agregar encabezados solo si el archivo no existe
          )
        }
        
        
        ### AHORA LO MISMO PERO CON FINDE
        
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
          actual <- zoo(testSetTs$kWh, order.by = testSetTs$timestamp) 
          aux <- actual != 0
          
          lagged <- merge(trainSet, shift(trainSet, -F_DAYS))
          #Es el trainset. Coge los días marcado por T_DAYS 
          SVM_TRAINSET <- window(lagged,start=index(trainSet)[length(trainSet) - T_DAYS + 1])
          PREDICT <- data.frame(past = as.numeric(tail(trainSet, F_DAYS)))
          names(SVM_TRAINSET) <- c("actual", "past")
          
          modelo_svm <- tune(e1071::svm, actual ~ past, data = SVM_TRAINSET, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
          
          svm <- predict(modelo_svm$best.model, newdata = PREDICT)
          
          for (j in 1:F_DAYS){
            aux_dia = aux[j]
            mape_svm[j] <- 100 * median(ifelse(sum(aux_dia) != 0, abs(actual[aux_dia] - svm[aux_dia]) / actual[aux_dia], NA))
          }
          
          for (j in 1:F_DAYS) {
            #... (resto del código)
            
            #Escribir directamente en el archivo CSV usando fwrite
            fwrite(
              data.table(
                ID = ID,
                Hora = hora,
                TipoDia = "Finde",
                Real = actual[j],
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
              col.names = !file.exists(RESULT_FILE)  #Agregar encabezados solo si el archivo no existe
            )
          }
          
          
          
        }
      }
    }
  }
}


foreach(csv_file = N, 
        .packages = librerias) %do% predict_models(csv_file)

