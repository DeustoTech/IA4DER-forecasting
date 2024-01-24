library(foreach)
library(doParallel)

# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', "tidyverse") 
foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

path <- "dataset_red.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina

unzip(path, exdir = tempdir) # descomprime. Tarda un poco


# Lista de archivos CSV en la carpeta extraída

csv_files <- list.files(tempdir, pattern = ".csv$", recursive = T, full.names = F)


# BUCLE PARA PROCESAR EL NUMERO OPTIMO DE NEURONAS

resultadosDia <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  nNeuronas = numeric(),
  TipoDia = character()
)




horas <- 0:23
neuronas <- c(1:4, seq(5, 20, by = 5))
# neuronas <- c(1:10)


cl <- makeCluster(4) 
registerDoParallel(cl)


RedNeuronalDia <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp)) %>%
    mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%
    select(-imputed)
  if (any(is.na(csv_actual$timestamp))) { next }
  
  
  datosLab <- csv_actual %>% filter(TipoDia == "Laborable")
  datosFinde <- csv_actual %>% filter(TipoDia == "Finde")
  
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias, .combine = 'c') %dopar% {
    # Filtrar los datos para la hora actual
    if (any(is.na(csv_actual[hour(csv_actual$timestamp) == hora, ]))){ next }
    
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% distinct()
    datosHoraFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% distinct()
    
    
    # Crear un tsibble para la hora actual - Laborable
    ts1Lab <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    # Crear un tsibble para el siguiente día - Finde
    ts1Finde <- datosHoraFinde %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    foreach(numNeurona = neuronas, .packages = librerias) %dopar% {
      if (any(is.na(csv_actual[hour(csv_actual$timestamp) == hora, ]))){ next }
      
      
      # Entrenar el modelo neuronal para días laborables
      errorsLab <- tsCV(ts1Lab$kWh, forecastNN, h = 1, window = 5, n = numNeurona) %>% na.omit()
      actualLab <- ts1Lab$kWh[1: length(errorsLab)]
      predictedLab <- actualLab + errorsLab
      
      # Calcular métricas para días laborables
      smapeLab <- smape(actualLab, predictedLab)
      rmseLab <- rmse(actualLab, predictedLab)
      
      # Almacenar los resultados en el tibble para días laborables
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedLab,
        sMAPE = smapeLab,
        RMSE = rmseLab,
        # MASE = NA,
        nNeuronas = numNeurona,
        TipoDia = "Laborable"
      )
      
      # Entrenar el modelo neuronal para días de fin de semana
      errorsFinde <- tsCV(ts1Finde$kWh, forecastNN, h = 1, window = 3, n = numNeurona) %>% na.omit()
      actualFinde <- ts1Finde$kWh[1: length(errorsFinde)]
      predictedFinde <- actualFinde + errorsFinde
      
      # Calcular métricas para días de fin de semana
      smapeFinde <- smape(actualFinde, predictedFinde)
      rmseFinde <- rmse(actualFinde, predictedFinde)
      
      # Almacenar los resultados en el tibble para días de fin de semana
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedFinde,
        sMAPE = smapeFinde,
        RMSE = rmseFinde,
        # MASE = NA,
        nNeuronas = numNeurona,
        TipoDia = "Finde"
      )
    }
  }
}



foreach(csv_file = csv_files, .combine='c', 
        .packages = librerias) %dopar% RedNeuronalDia(csv_file)

stopCluster(cl)
resultadosDia <- fread("numNeuronas.csv")

# Boxplot

filtradoRMSE <- resultadosDia %>%
  filter(!is.na({{ RMSE }}))


# Dividir los datos en una lista de data frames por Modelo
divididoRMSE <- split(filtradoRMSE$RMSE, filtradoRMSE$nNeuronas)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoRMSE))

# Crear un boxplot para cada modelo
boxplot(divididoRMSE, 
        main = "RMSE por Numero de neuronas", 
        ylab = "RMSE",
        col = colores,
        names = names(divididoRMSE),
        names.arg = filtradoRMSE$nNeuronas,
        las = 2)  # Etiquetas de los modelos en el eje X


filtradoSMAPE <- resultadosDia %>%
  filter(!is.na(sMAPE))


smape1 <- resultadosDia %>% select(sMAPE, nNeuronas)


# Dividir los datos en una lista de data frames por Modelo
divididoSMAPE <- split(smape1$sMAPE, smape1$nNeuronas)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoSMAPE))

# Crear un boxplot para cada modelo
boxplot(divididoSMAPE, 
        main = "sMAPE por Numero de neuronas", 
        ylab = "sMAPE",
        col = colores,
        names = names(divididoSMAPE),
        names.arg = divididoSMAPE$nNeuronas,
        las = 2)  # Etiquetas de los modelos en el eje X







# Pruebas con un solo archivo

csv1 <- fread(csv_files[1])
ts1 <- csv1 %>% mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>% 
  select(-imputed) %>% 
  as_tsibble(key = kWh, index = timestamp) %>% arrange(timestamp) %>%
  mutate(Dia = weekdays(timestamp), Hora = hour(timestamp), 
      TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"), "Laborable", "Finde")
)


# Ya tenemos clasificado el tipo de día. Pruebas con una hora concreta

hora0 <- ts1 %>% filter(hour(timestamp) == 16)


# Para empezar, intentamos predecir un dia laborable. 

# Para empezar, usamos solo los ultimos. Cogemos los 6 días anteriores de ese tipo

ultimosLab <- hora0 %>% filter(TipoDia == "Laborable") %>% tail(n = 6)
trainSet <- xts(ultimosLab$kWh[-nrow(ultimosLab)], order.by = ultimosLab$timestamp[-nrow(ultimosLab)])
testSet <- xts(ultimosLab$kWh[nrow(ultimosLab)], order.by = ultimosLab$timestamp[nrow(ultimosLab)])

#train <- ultimosLab[1:5, ] %>% select(-Dia, -Hora, -TipoDia)
#test <- ultimosLab[6, ] %>% select(-Dia, -Hora, -TipoDia)

modelo <- nnetar(trainSet, size = 1)
pred <- forecast(modelo, h = 1)
smape(testSet, as.numeric(pred$mean))
mase(testSet[1],pred$mean)

#modelo1 <- nnetar(as.xts(train), size = 3)
#pred1 <- forecast(modelo1, h = 1)
#smape(test$kWh ,pred1$mean)
#mase(test$kWh,pred1$mean, 1)

neuronas <- c(1, seq(5, 100, by = 5)) # numero de neuronas a probar

# Ahora con CV 
forecastNN1 <- function(x){
  prediccion <- forecast(nnetar(x))
  return(prediccion)
}

forecastNN1(lab$kWh)

lab <- hora0 %>% filter(TipoDia == "Laborable")
errors <- tsCV(lab$kWh, forecastNN1, h = 1, window = 7, n =1) %>% na.omit() #esto a ane: ERRORRR
actual <- lab$kWh[1: length(errors)]


predicted <- actual + errors

# Calcular métricas
smape <- smape(actual, predicted)
rmse <- rmse(actual, predicted)


resultado0lab <- tibble(
  nNeuronas = numeric(),
  sMAPE = numeric(),
  RMSE = numeric()
  # MASE = numeric()
)

horas <- 0:23
neuronas <- c(1, seq(5, 50, by = 5)) 

lab1Resultados <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  numNeuronas = numeric()
) 


# El mase peta

forecastNN <- function(x, h, n){
  prediccion <- forecast(nnetar(x, size = n), h = h)
  return(prediccion)
}

# Prueba con todas las horas y solo laborables

labHoras <- ts1 %>% filter(TipoDia == "Laborable")

foreach(hora = horas) %dopar%{

  foreach( numNeurona = neuronas, .packages = librerias) %dopar%{
    
    lab <- labHoras %>% filter(Hora == hora)
    
    errors <- tsCV(lab$kWh, forecastNN, h = 1, window = 5, n = numNeurona) %>% na.omit()
    actual <- lab$kWh[1: length(errors)]
    
    predicted <- actual + errors
    
    
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    # mase <- mase(actual, predicted)
    
    lab1Resultados <<- lab1Resultados %>% add_row(
      Hora = hora,
      Predicted = predicted,
      sMAPE = smape,
      RMSE = rmse,
      # MASE = numeric(),
      numNeuronas = numNeurona
    )
    
  }
}

filtradoRMSE <- lab1Resultados %>%
  filter(!is.na({{ RMSE }}))


# Dividir los datos en una lista de data frames por Modelo
divididoRMSE <- split(filtradoRMSE$RMSE, filtradoRMSE$numNeuronas)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoRMSE))

# Crear un boxplot para cada modelo
boxplot(divididoRMSE, 
        main = "RMSE por Numero de neuronas", 
        ylab = "RMSE",
        col = colores,
        names = names(divididoRMSE),
        names.arg = filtradoRMSE$numNeuronas,
        las = 2)  # Etiquetas de los modelos en el eje X







# Ahora tenemos que intentar con todos los CSV (mejor usar aleatorios)

horas <- 0:23
neuronas <- c(1, seq(5, 100, by = 5)) 

resultadosLaborable <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  numNeuronas = numeric()
) 


# Prueba con los laborables

RedNeuronalLaborable <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp)) %>%
    mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%
    select(-imputed)
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias) %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable")
    # Crear un tsibble para la hora actual
    ts1 <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 

    foreach( numNeurona = neuronas, .packages = librerias) %dopar%{
      
    
    errors <- tsCV(ts1$kWh, forecastNN, h = 1, window = 3, n = numNeurona) %>% na.omit()
    actual <- ts1$kWh[1: length(errors)]
    
    # Calculamos la prediccion haciendo real + error
    predicted <- actual + errors
    
    # Calcular métricas
    smape <- smape(actual, predicted)
    rmse <- rmse(actual, predicted)
    # mase <- mase(actual, predicted)
    
    # Almacenar los resultados en el tibble para ETS
    
    resultadosLaborable <<- resultadosLaborable %>% add_row(
      Hora = hora,
      Predicted = predicted,
      # MASE = mase,
      sMAPE = smape,
      RMSE = rmse,
      nNeuronas = numNeurona,
    )
    }
  }
}

foreach(csv_file = csv_files[1:5],
        .packages = librerias) %dopar% RedNeuronalLaborable(csv_file)

# SVM

horas <- 0:23
set.seed(123)
# Definir rangos de valores para los hiperparámetros
kernel_values <- c("linear", "radial")
cost_values <- seq(0.01, 100, length.out = 20) 
gamma_values <- seq(0.01, 100, length.out = 20)  

# Crear todas las combinaciones de hiperparámetros
hyperparameters <- expand.grid(
  kernel = kernel_values,
  cost = cost_values,
  gamma = gamma_values
)


h <- hyperparameters[sample(nrow(hyperparameters), 20),] %>% arrange(cost)

resultadosSVM <- tibble(
  Hora = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  kernel = character(),
  cost = numeric(),
  gamma = numeric(),
  TipoDia = character()
)

fileIteracion <- "SVM_finde.csv"
fwrite(resultadosSVM, file = fileIteracion, col.names = T)



tunearSVM <- function(csv_file){
  
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp)) %>%
    mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%
    select(-imputed)
  
  
  datosLab <- csv_actual %>% filter(TipoDia == "Laborable")
  datosFinde <- csv_actual %>% filter(TipoDia == "Finde")
  
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias, .combine = 'c') %dopar% {
    # Filtrar los datos para la hora actual
    
    if (any(is.na(csv_actual[hour(csv_actual$timestamp) == hora, ]))){ next }
    
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% distinct()
    datosHoraFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% distinct()
    
    # Crear un tsibble para la hora actual - Laborable
    ts1Lab <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    # Crear un tsibble para el siguiente día - Finde
    ts1Finde <- datosHoraFinde %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp)   
    
    
    slicesLab <- createTimeSlices(ts1Lab$timestamp, initialWindow = 5, horizon = 1, skip = 0, fixedWindow = F)
    slicesFinde <- createTimeSlices(ts1Finde$timestamp, initialWindow = 3, horizon = 1, skip = 0, fixedWindow = F)
    
    for (i in 1:nrow(h)){
      
      hp <- h[i, ]
      
      # Primero con los dias laborables
      
      for (j in 1:length(slicesLab$train)) {
        
        train_index <- slicesLab$train[[j]]
        test_index <- slicesLab$test[[j]]
        
        trainSet <- ts1Lab[train_index, ] %>% na.omit()
        testSet <- ts1Lab[test_index, ] %>% na.omit()
        
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
      
      # Ahora con finde

      for (k in 1:length(slicesFinde$train)) {

        train_index <- slicesFinde$train[[k]]
        test_index <- slicesFinde$test[[k]]

        trainSet <- ts1Finde[train_index, ] %>% na.omit()
        testSet <- ts1Finde[test_index, ] %>% na.omit()

        model <- e1071::svm(kWh ~ timestamp, data = trainSet, kernel = hp$kernel,
                            cost = hp$cost, gamma = hp$gamma, type = "eps-regression")
        fit <- predict(model, h = 1)

        smape = smape(testSet$kWh, fit)
        rmse = rmse(testSet$kWh, fit)


        resultadosSVM <<- resultadosSVM %>%
          add_row(
            Hora = hora,
            #MASE = test_metrics["MASE"],
            sMAPE = smape,
            RMSE = rmse,
            kernel = hp$kernel, # Ajusta el índice de la columna según corresponda
            cost = hp$cost,  # Ajusta el índice de la columna según corresponda
            gamma = hp$gamma,   # Ajusta el índice de la columna según corresponda
            TipoDia = "Finde"
          )
        write.csv(resultadosSVM, file = fileIteracion, append = T, col.names = F)
      }

    }
    
  }
}



foreach(csv_file = csv_files,
        .packages = librerias, .combine = 'c') %dopar% tunearSVM(csv_file)


svm_dataset <- fread("SVM_finde.csv")
svm_datasetFinde <- svm_dataset %>% filter(TipoDia == "Finde")
svm_datasetLab <- svm_dataset %>% filter(TipoDia == "Laborable")

which.min(svm_datasetLab$sMAPE)
which.min(svm_datasetLab$RMSE)

which.min(svm_datasetFinde$sMAPE)
which.min(svm_datasetFinde$RMSE)

svm_datasetLab[2606,]
svm_datasetFinde[68,]

ggplot(svm_datasetFinde, aes(x = svm_datasetFinde$cost, y = svm_datasetFinde$gamma, fill = svm_datasetFinde$RMSE)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "log10(cost)", y = "log10(gamma)", fill = "RMSE") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggtitle("Heart Scale Plot for SVM")# Supongamos que tienes un data frame 'datos' que contiene los datos con las columnas 'gamma', 'cost' y 'rmse'

# Crea el gráfico utilizando ggplot2



##pruebas svm

csv1 <- csv_files[5]
csvSVM <- fread(csv1)
csv_SVM <- csvSVM %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(Dia = weekdays(timestamp)) %>%
  mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                          "Laborable", "Finde")) %>%
  select(-imputed)

datos_hora20 <- csv_SVM[hour(csv_SVM$timestamp) == 20, ] 

datosHoraLab20 <- datos_hora20 %>% filter(TipoDia == "Laborable") %>% distinct()
datosHoraFinde20 <- datos_hora20 %>% filter(TipoDia == "Finde") %>% distinct()


ts1Lab20 <- datosHoraLab20 %>%
  mutate(timestamp = as.Date(timestamp)) %>%
  as_tsibble(key = kWh, index = timestamp) %>%
  arrange(timestamp) 

n <- nrow(ts1Lab20)
propTrain <- 0.75
indexTrain <- floor(n * propTrain)
trainSet20 <- ts1Lab20[1:indexTrain, ]
testSet20 <- ts1Lab20[(indexTrain + 1):n, ]


# Definir rangos de valores para los hiperparámetros
kernel_values <- c("linear", "radial")
cost_values <- seq(0.01, 100, length.out = 10) 
gamma_values <- seq(0.01, 100, length.out = 10)  

# Crear todas las combinaciones de hiperparámetros
hyperparameters <- expand.grid(
  kernel = kernel_values,
  cost = cost_values,
  gamma = gamma_values
)

set.seed(123)
h <- hyperparameters[sample(nrow(hyperparameters), 20),] %>% arrange(cost)


forecastSVM <- function(x, hp, h){
  prediccion <- predict(svm(kWh ~ timestamp, data = x, kernel = hp$kernel, cost = hp$cost, gamma = hp$gamma), h = h)
  return(prediccion)
}

resultadosSVM0 <- tibble(
  Hora = numeric(),
  # Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  MASE = numeric(),
  kernel = character(),
  cost = numeric(),
  gamma = numeric(),
  TipoDia = character()
)

slices0 <- createTimeSlices(ts1Lab20$timestamp, initialWindow = 5, horizon = 1, skip = 0, fixedWindow = F)

for (i in 1:nrow(h)){
  
  hp <- h[i, ]
  
  kernel <- hp$kernel
  cost <- hp$cost
  gamma <- hp$gamma

  for (j in 1:lengths(slices0)) {
    
    train_index <- slices0$train[[j]]
    test_index <- slices0$test[[j]]
    
    trainSet <- ts1Lab20[train_index, ] %>% na.omit()
    testSet <- ts1Lab20[test_index, ] %>% na.omit()
    
    model <- e1071::svm(kWh ~ timestamp, data = trainSet, kernel = hp$kernel,
                        cost = hp$cost, gamma = hp$gamma, type = "eps-regression")
    fit <- predict(model, h = 1)
    
    smape = smape(testSet$kWh, fit)
    rmse = rmse(testSet$kWh, fit)

    
    resultadosSVM0 <- resultadosSVM0 %>% 
      add_row(
        # MASE = test_metrics["MASE"],
        sMAPE = smape,
        RMSE = rmse,
        kernel = hp$kernel,  # Ajusta el índice de la columna según corresponda
        cost = hp$cost,  # Ajusta el índice de la columna según corresponda
        gamma = hp$gamma,   # Ajusta el índice de la columna según corresponda
        TipoDia = "Laborable"
       )
  
    }
  }


hp1 <- h[1, ]


train_index <- slices$train[[1]]
test_index <- slices$test[[1]]

trainSetb <- ts1Lab20[train_index, ] %>% na.omit()
testSetb <- ts1Lab20[test_index, ] %>% na.omit()
  
model <- e1071::svm(kWh ~ timestamp, data = trainSetb, kernel = hp1$kernel,
                    cost = hp1$cost, gamma = hp1$gamma, type = "eps-regression")
fit <- predict(model, h = 1)



obj <- tune(svm, kWh ~ timestamp + tipoDia, data = trainSetb, gamma = gamma_values, cost = cost_values,
            tunecontrol = tune.)


custom_control <- trainControl(
  method = "timeslice", 
  initialWindow = 5,  # Especifica la longitud de la ventana de entrenamiento inicial
  horizon = 1,         # Especifica la longitud de la ventana de prueba (1 para una observación)
  fixedWindow = TRUE,  # Utiliza una ventana fija
  summaryFunction = defaultSummary
)



# Acceder al primer conjunto de entrenamiento
train_data <- slices$train[[1]]

# Calcular la longitud del conjunto de entrenamiento
train_length <- length(train_data)

# Luego, puedes usar train_length en tune.svm
params <- tune.svm(kWh ~ timestamp, data = train_data, gamma = gamma_values, cost = cost_values,
                   tunecontrol = tune.control(sampling = "fix", fix = train_length))


params <- tune.svm(kWh ~ timestamp, data = slices$train[1], gamma = gamma_values,
                   cost = cost_values, tunecontrol = tune.control(sampling = "fix",
                                                                  fix = length(slices$train[[1]])))


crtl <- caret::trainControl(method = "timeslice", savePredictions = T, )
model <- caret::train(kWh ~ timestamp, data = trainSet20, method = "svmLinear2", trControl = custom_control)


caret::train



train_indices <- slices$train
test_indices <- slices$test
train_data <- ts1Lab20 %>% filter(row_number() %in% train_indices)

train_data <- ts1Lab20[train_indices, ]
test_data <- ts1Lab20[test_indices, ]

# Entrena el modelo SVM en la ventana de tiempo actual
svm_model <- svm(kWh ~ timestamp, data = train_data, kernel = kernel_type, cost = cost, gamma = gamma)

# Realiza predicciones en el conjunto de prueba
predictions <- predict(svm_model, newdata = test_data)







results <- lapply(slices, function(slice) {
  # Divide los datos en conjunto de entrenamiento y prueba
  train_indices <- slice$train
  test_indices <- slice$test
  train_data <- trainSet20[train_indices, ]
  test_data <- trainSet20[test_indices, ]
  
  # Entrena el modelo SVM en la ventana de tiempo actual
  svm_model <- svm(kWh ~ timestamp, data = train_data, kernel = kernel_type, cost = cost, gamma = gamma)
  
  # Realiza predicciones en el conjunto de prueba
  predictions <- predict(svm_model, newdata = test_data)
  
  # Calcula las métricas de rendimiento (por ejemplo, RMSE, sMAPE) según tus necesidades
  
  # Devuelve las métricas de rendimiento
  return(metrics)
})





trainSet20b <- trainSet20 %>% select(timestamp, kWh)
hp1 <- h[1, ]

ts20 <- xts(trainSet20$kWh, order.by = trainSet20$timestamp)

    # Entrenar el modelo neuronal para días laborables
  errorsLab <- tsCV(ts20, forecastSVM, h = 1, window = 1, hp = h[1, ])# %>% na.omit()
  actualLab <- trainSet20$kWh[1: length(errorsLab)]
  predictedLab <- actualLab + errorsLab
    
  # Calcular métricas para días laborables
  smapeLab <- smape(actualLab, predictedLab)
  rmseLab <- rmse(actualLab, predictedLab)
  
  # Almacenar los resultados en el tibble para días laborables
  resultadosDia <<- resultadosDia %>% add_row(
    Hora = hora,
    Predicted = predictedLab,
    sMAPE = smapeLab,
    RMSE = rmseLab,
    # MASE = NA,
    kernel = h$kernel,
    cost = h$cost,
    gamma = h$gamma,
    TipoDia = "Laborable"
  )
  
# }







fit_svm_and_metrics <- function(train, test, kernel, cost, gamma) {
  model <- svm(train$kWh ~ train$timestamp, kernel = kernel, cost = cost, gamma = gamma)
  predicciones <- predict(model, test)
  rmse <- sqrt(mean((test$kWh - predicciones)^2))
  mase <- mean(abs(test$kWh - predicciones))
  return(data.frame(kernel, cost, gamma, rmse, mase))
}

resultados <- do.call(rbind, lapply(1:nrow(hyperparameters), function(i) {
  fit_svm_and_metrics(trainSet20, testSet20, hyperparameters[i, ])
}))


svm_model20_1 <- svm(trainSet20$kWh ~ trainSet20$timestamp, 
                    kernel = "linear", cost = 1)
svm_model20_2 <- svm(trainSet20$kWh ~ trainSet20$timestamp, 
                     kernel = "radial", cost = 1, gamma = 0.1)
svm_model20_3 <- svm(trainSet20$kWh ~ trainSet20$timestamp, 
                     kernel = "radial", cost = 10, gamma = 0.01)

predictions20_1 <- predict(svm_model20_1, newdata = testSet20)
predictions20_2 <- predict(svm_model20_2, newdata = testSet20)
predictions20_3 <- predict(svm_model20_3, newdata = testSet20)

#probar el bucle grande

resultadosDia <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
 # MASE = numeric(),
  kernel = character(),
  cost = numeric(),
  gamma = numeric(),
  TipoDia = character()
)


horas <- 0:23

# Definir rangos de valores para los hiperparámetros
kernel_values <- c("linear", "radial")
cost_values <- seq(0.01, 100, length.out = 10) 
gamma_values <- seq(0.01, 100, length.out = 10)  

# Crear todas las combinaciones de hiperparámetros
hyperparameters <- expand.grid(
  kernel = kernel_values,
  cost = cost_values,
  gamma = gamma_values
)

set.seed(123)
h <- hyperparameters[sample(nrow(hyperparameters), 20),] %>% arrange(cost)

forecastSVM <- function(x, hp, h){
  prediccion <- predict(svm(x$kWh ~ x$timestamp, kernel = hp$kernel, 
                            cost = hp$cost, gamma = hp$gamma), h = h)
  return(prediccion)
}

cl <- makeCluster(4) 
registerDoParallel(cl)


SvmDia <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp)) %>%
    mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                            "Laborable", "Finde")) %>%
    select(-imputed)
  
  
  datosLab <- csv_actual %>% filter(TipoDia == "Laborable")
  datosFinde <- csv_actual %>% filter(TipoDia == "Finde")
  
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias, .combine = 'c') %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% distinct()
    datosHoraFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% distinct()
    
    
    # Crear un tsibble para la hora actual - Laborable
    ts1Lab <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    
    # Crear un tsibble para el siguiente día - Finde
    ts1Finde <- datosHoraFinde %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
   
    
    foreach(hyperparameters = h, .packages = librerias) %dopar% {
      # Entrenar el modelo neuronal para días laborables
      errorsLab <- tsCV(ts1Lab, forecastSVM, h = 1, window = 5, hyperparameters) %>% na.omit()
      actualLab <- ts1Lab$kWh[1: length(errorsLab)]
      predictedLab <- actualLab + errorsLab
      
      # Calcular métricas para días laborables
      smapeLab <- smape(actualLab, predictedLab)
      rmseLab <- rmse(actualLab, predictedLab)
      
      # Almacenar los resultados en el tibble para días laborables
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedLab,
        sMAPE = smapeLab,
        RMSE = rmseLab,
        # MASE = NA,
        kernel = hyperparameters$kernel,
        cost = hyperparameters$cost,
        gamma = hyperparameters$gamma,
        TipoDia = "Laborable"
      )
      
      # Entrenar el modelo neuronal para días de fin de semana
      errorsFinde <- tsCV(ts1Finde, forecastNN, h = 1, window = 3, hyperparameters) %>% na.omit()
      actualFinde <- ts1Finde$kWh[1: length(errorsFinde)]
      predictedFinde <- actualFinde + errorsFinde
      
      # Calcular métricas para días de fin de semana
      smapeFinde <- smape(actualFinde, predictedFinde)
      rmseFinde <- rmse(actualFinde, predictedFinde)
      
      # Almacenar los resultados en el tibble para días de fin de semana
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedFinde,
        sMAPE = smapeFinde,
        RMSE = rmseFinde,
        # MASE = NA,
        kernel = hyperparameters$kernel,
        cost = hyperparameters$cost,
        gamma = hyperparameters$gamma,
        TipoDia = "Finde"
      )
    }
  }
}



resultados <- foreach(csv_file = csv_files,  
                      .packages = librerias) %dopar% SvmDia(csv_file)

stopCluster(cl)

csv_file <- fread(csv_files[10])
csv_actual <- csv_file %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
  mutate(Dia = weekdays(timestamp)) %>% mutate(TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                                                                "Laborable", "Finde")) %>%
  select(-imputed)

# Define una cuadrícula de hiperparámetros para buscar
param_grid <- expand.grid(
  kernel = c("linear", "radial", "polynomial"),
  cost = c(0.1, 1, 10),
  gamma = c(0.01, 0.1, 1)
)

# Función para realizar la validación cruzada y ajustar el modelo SVM
fit_svm <- function(kernel, cost, gamma) {
  svm_model <- svm(target_variable ~ ., data = train_data, kernel = kernel, cost = cost, gamma = gamma)
  return(svm_model)
}

# Realiza la búsqueda de hiperparámetros utilizando validación cruzada
tune_results <- tune(svm, target_variable ~ ., data = train_data, kernel = kernel, ranges = param_grid)

# Muestra los resultados
print(tune_results)

### BUCLE GRANDE DE OTRA MANERA

resultadosDia <- tibble(
  Hora = numeric(),
  Predicted = numeric(),
  sMAPE = numeric(),
  RMSE = numeric(),
  # MASE = numeric(),
  kernel = character(),
  cost = numeric(),
  gamma = numeric(),
  TipoDia = character()
)


horas <- 0:23
iteraciones <- 1:10
kernel_values <- c("radial", "radial", "radial", "radial", "radial", "lineal", "lineal", "lineal", "lineal", "lineal")
cost_values <- c(0.01, 0.1, 1, 10, 100, 0.01, 0.1, 1, 10, 100)
gamma_values <- c(0.001, 0.1, 1, 10, 100, 0.01, 0.1, 1, 10, 100)

forecastSVM <- function(x, h, kernel, cost, gamma){
  prediccion <- predict(svm(x$kWh ~ x$timestamp, kernel = kernel, 
                            cost = cost, gamma = gamma), h = h)
  return(prediccion)
}

cl <- makeCluster(4) 
registerDoParallel(cl)


SvmDia <- function(csv_file) {
  csv_actual <- fread(csv_file)
  
  csv_actual <- csv_actual %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(Dia = weekdays(timestamp), 
           TipoDia = ifelse(Dia %in% c("lunes", "martes", "miércoles", "jueves", "viernes"),
                                                       "Laborable", "Finde")) %>%
    select(-imputed)
  
  # Bucle para procesar cada hora
  foreach(hora = horas, .packages = librerias, .combine = 'c') %dopar% {
    # Filtrar los datos para la hora actual
    datos_hora <- csv_actual[hour(csv_actual$timestamp) == hora, ] 
    
    datosHoraLab <- datos_hora %>% filter(TipoDia == "Laborable") %>% distinct()
    datosHoraFinde <- datos_hora %>% filter(TipoDia == "Finde") %>% distinct()
    
    
    # Crear un tsibble para la hora actual - Laborable
    ts1Lab <- datosHoraLab %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    
    # Crear un tsibble para el siguiente día - Finde
    ts1Finde <- datosHoraFinde %>%
      mutate(timestamp = as.Date(timestamp)) %>%
      as_tsibble(key = kWh, index = timestamp) %>%
      arrange(timestamp) 
    
    
    foreach(i = iteraciones, .packages = librerias) %dopar% {
      # Entrenar el modelo neuronal para días laborables
      errorsLab <- tsCV(ts1Lab, forecastSVM, h = 1, window = 5, kernel = kernel_values[i], cost = cost_values[i], gamma = gamma_values[i]) %>% na.omit()
      actualLab <- ts1Lab$kWh[1: length(errorsLab)]
      predictedLab <- actualLab + errorsLab
      
      # Calcular métricas para días laborables
      smapeLab <- smape(actualLab, predictedLab)
      rmseLab <- rmse(actualLab, predictedLab)
      
      # Almacenar los resultados en el tibble para días laborables
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedLab,
        sMAPE = smapeLab,
        RMSE = rmseLab,
        # MASE = NA,
        kernel = kernel_values[i],
        cost = cost_values[i],
        gamma = gamma_values[i],
        TipoDia = "Laborable"
      )
      
      # Entrenar el modelo neuronal para días de fin de semana
      errorsFinde <- tsCV(ts1Finde, forecastNN, h = 1, window = 3,kernel = kernel_values[i], cost = cost_values[i], gamma = gamma_values[i]) %>% na.omit()
      actualFinde <- ts1Finde$kWh[1: length(errorsFinde)]
      predictedFinde <- actualFinde + errorsFinde
      
      # Calcular métricas para días de fin de semana
      smapeFinde <- smape(actualFinde, predictedFinde)
      rmseFinde <- rmse(actualFinde, predictedFinde)
      
      # Almacenar los resultados en el tibble para días de fin de semana
      resultadosDia <<- resultadosDia %>% add_row(
        Hora = hora,
        Predicted = predictedFinde,
        sMAPE = smapeFinde,
        RMSE = rmseFinde,
        # MASE = NA,
        kernel = kernel_values[i],
        cost = cost_values[i],
        gamma = gamma_values[i],
        TipoDia = "Finde"
      )
    }
  }
}



resultados <- foreach(csv_file = csv_files,  
                      .packages = librerias) %dopar% SvmDia(csv_file)

stopCluster(cl)
