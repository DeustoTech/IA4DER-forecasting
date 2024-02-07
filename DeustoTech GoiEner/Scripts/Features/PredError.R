# Script para predecir el error que comete cada modelo 
# a partir de las features 

library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


# Cargar ficheros y constantes
setwd('C:/GIT HUB ANE/IA4DER-forecasting/DeustoTech GoiEner')

folder <- "TransformersV2/"
# Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)
metadata_file <- fread("metadata.csv")

# csv_files <- csv_files[201:length(csv_files)]
{
N <- csv_files[!grepl("-CT\\.csv$", csv_files) & !grepl("-L\\.csv$", csv_files)]
CT <- csv_files[grepl("-CT\\.csv$", csv_files)]
L <- csv_files[grepl("-L\\.csv$", csv_files)]

summaryPreds_CUPS <- fread("Resultados/CUPS/SummaryPreds.csv")
summaryPreds_CUPS$ID <- basename(summaryPreds_CUPS$ID)

summaryMedia_CUPS <- fread("Resultados/CUPS/SummaryMedia.csv")
summaryMedia_CUPS$ID <- basename(summaryMedia_CUPS$ID)

summaryNaive_CUPS <- fread("Resultados/CUPS/SummaryNaive.csv")
summaryNaive_CUPS$ID <- basename(summaryNaive_CUPS$ID)

summarysNaive_CUPS <- fread("Resultados/CUPS/SummarySNaive.csv") 
summarysNaive_CUPS$ID <- basename(summarysNaive_CUPS$ID)


summaryArima_CUPS <- fread("Resultados/CUPS/SummaryArima.csv")
summaryArima_CUPS$ID <- basename(summaryArima_CUPS$ID)

summaryETS_CUPS <- fread("Resultados/CUPS/SummaryETS.csv")
summaryETS_CUPS$ID <- basename(summaryETS_CUPS$ID)

summaryNN_CUPS <- fread("Resultados/CUPS/SummaryNN.csv")
summaryNN_CUPS$ID <- basename(summaryNN_CUPS$ID)

summarySVM_CUPS <- fread("Resultados/CUPS/SummarySVM.csv")
summarySVM_CUPS$ID <- basename(summarySVM_CUPS$ID)

#summaryEnsemble_CUPS <- fread("Resultados/CUPS/SummaryEnsemble.csv")
#summaryEnsemble_CUPS$ID <- basename(summaryEnsemble_CUPS$ID)

# Agregar el prefijo 'folder' a las rutas en N, CT y L
N <- paste(folder, N, sep = "")
CT <- paste(folder, CT, sep = "")
L <- paste(folder, L, sep = "")

horas <- data.frame(
  hora = 0:23,
  TARIFA_2.0 = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano", "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  ),
  TARIFA_SOLAR = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "solar pico", "solar pico", "solar pico", "solar pico",
    "solar llano", "solar llano",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  )
)
MC  <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error
#p
# Precios de electricidad

# TD : Peajes €/kWh
# CS : Cargos Sistema €/kWh
# CG : Coste Gestión €/MWh
# L : Precio energía libre €/kWh

TD_p1 <- 0.029098
TD_p2 <- 0.019794
TD_p3 <- 0.00098

CS_p1 <- 0.043893
CS_p2 <- 0.008779
CS_p3 <- 0.002195

CG_p1 <- 36.115695
CG_p2 <- 36.115695
CG_p3 <- 36.115695

L_p1 <- 0.204321
L_p2 <- 0.200549
L_p3 <- 0.185471
}
model_names <- c("Media", "Naive", "SNaive", "Arima", "ETS", "SVM", "NN", "Ensemble")


# Carga fichero con todas las features

feats3 <- read.csv("featuresPredicciones_3.csv")
feats_complete <- fread("feats-complete.csv") #los ID estan en la variable file

summary(cols_cuest)

#columnas cuestionario
cols_cuest <- feats_complete %>% select(matches("^Q\\d"))
cols_cuest$ID <- feats_complete$file
sum(is.na(cols_cuest[, Q1_1_X1...Cul.]))


# Solo las que han respondido al cuestionario entero
cuest <- cols_cuest[complete.cases(cols_cuest) & rowSums(!is.na(cols_cuest)) > 1]



cuest <- left_join(cuest, feats3[, c("ID", target)], by = "ID")

# Eliminar duplicados en feats3 basándote en la columna ID
feats3_unique <- feats3 %>% distinct(ID, .keep_all = TRUE)

# Realizar la unión sin duplicados
cuest <- left_join(cuest, feats3_unique[, c("ID", target)], by = "ID")

# Columnas y limipiar cuestionario
{
#descripcion socieconomica
descSE <- c("Q6_2_X2...Es.la", "Q6_15_X15...Cu", "Q6_16_X16...Cu", # Categoricas
            "Q6_17_X17...Cu", "Q6_18_X18...Cu", "Q6_19_X19...Cu","Q6_8_X8...Cunt" , 
            "Q6_9_X9...Cul.", "Q6_10_X10...Cu","Q6_11_X11...Cu", 
            "Q6_12_X12...Cu", "Q6_13_X13...Cu", "Q6_14_X14..Nive", "Q6_20_X20...Cu", 
            "Q6_21_X21...Has", "Q6_22_X22...Cm", "Q6_24_X24...Con",  "Q6_25_X25...Sab", 
            "Q6_99_X.Conside", "Q6_27_X27...En.") # numericas

#descripcion del edificio
descEd <- c("Q3_1_X1..El.sum", "Q3_2_X2...Cul." , "Q6_1_X1...En.qu", "Q6_3_X3...Cul.", 
            "Q6_4_X4...En.qu", "Q6_5_X5...Dispo", "Q6_6_X6...En.qu", "Q6_7_X7..Tamao") # Todas categoricas

#las costumbres de la gente
descCG <- c("Q1_1_X1...Cul.", # rango 0-10
            "Q1_2_Aumentar.l", "Q1_2_Incentivar","Q1_2_Mejorar.la",  "Q1_2_Reducir.la", "Q1_2_Todas.las." , # Binarias
            "Q1_5_Ajustar.el",  "Q1_5_Buscar.inf", "Q1_5_Cambiar.la", "Q1_5_Cambiar.lo", "Q1_5_Cambiar.mi", # Binarias
            "Q1_5_Cambiar.mi.1","Q1_5_Ninguna","Q1_5_Otro", "Q1_5_Reducir.el","Q1_5_Usar.la.fu", # Binarias
            "Q1_3_X3...Qu.o",  # Categorica
            # "Q1_99_X.Han.afe", # Son varias respuestas separadas por ; para cada CUP
            "Q1_6_Desconoca", "Q1_6_La.rutina.","Q1_6_Mi.horario" , "Q1_6_Ninguno" ,"Q1_6_No.quiero." ,    
            "Q1_6_Otro", "Q1_6_Priorizo.e",

            "Q1_7_X7...Han.a", # Categorica
            # "Q1_98_X8...Ha.b",  # Son varias respuestas separadas por ; para cada CUP
            "Q2_1_X1...Le.ha", # Categorica
            "Q2_2_X2...Le.ha",  "Q2_3_X3...Le.ha",  # rango 0-10
            "Q3_99_X.Hay.alg", # Categorica
            "Q3_98_X.Est.la" , # Categorica
            "Q4_1_X1..Elija.", "Q5_1_X1..Elija." # categoricas
            )

# Vector con las variables categóricas (incluyendo binarias)
categoricas <- c("Q6_2_X2...Es.la", "Q6_15_X15...Cu", "Q6_16_X16...Cu", 
                 "Q6_17_X17...Cu", "Q6_18_X18...Cu", "Q6_19_X19...Cu","Q6_8_X8...Cunt" , 
                 "Q6_9_X9...Cul.", "Q6_10_X10...Cu","Q6_11_X11...Cu", 
                 "Q6_12_X12...Cu", "Q6_13_X13...Cu", "Q6_14_X14..Nive", "Q6_20_X20...Cu", 
                 "Q6_21_X21...Has", "Q6_22_X22...Cm", "Q6_24_X24...Con",  "Q6_25_X25...Sab", 
                 "Q6_99_X.Conside", "Q6_27_X27...En.",
                 "Q3_1_X1..El.sum", "Q3_2_X2...Cul." , "Q6_1_X1...En.qu", "Q6_3_X3...Cul.", 
                 "Q6_4_X4...En.qu", "Q6_5_X5...Dispo", "Q6_6_X6...En.qu", "Q6_7_X7..Tamao",
                 "Q1_1_X1...Cul.", "Q1_2_Aumentar.l", "Q1_2_Incentivar","Q1_2_Mejorar.la",  
                 "Q1_2_Reducir.la", "Q1_2_Todas.las.", "Q1_5_Ajustar.el",  "Q1_5_Buscar.inf", 
                 "Q1_5_Cambiar.la", "Q1_5_Cambiar.lo", "Q1_5_Cambiar.mi", "Q1_5_Cambiar.mi.1",
                 "Q1_5_Ninguna","Q1_5_Otro", "Q1_5_Reducir.el","Q1_5_Usar.la.fu", "Q1_3_X3...Qu.o",
                 "Q1_6_Desconoca", "Q1_6_La.rutina.","Q1_6_Mi.horario" , "Q1_6_Ninguno" ,"Q1_6_No.quiero." ,    
                 "Q1_6_Otro", "Q1_6_Priorizo.e", "Q1_7_X7...Han.a", "Q2_1_X1...Le.ha", 
                 "Q2_2_X2...Le.ha",  "Q2_3_X3...Le.ha", "Q3_99_X.Hay.alg", "Q3_98_X.Est.la", 
                 "Q4_1_X1..Elija.", "Q5_1_X1..Elija.")

# Vector con las variables numéricas y de rango
numericas <- c("Q1_99_X.Han.afe", "Q1_98_X8...Ha.b", "Q2_1_X1...Le.ha", 
                       "Q2_2_X2...Le.ha", "Q2_3_X3...Le.ha")


# Definir las columnas categóricas
columnas <- c("descSE", "descEd", "descCG")

# Crear el conjunto de entrenamiento y prueba
set.seed(0)  # Establecer semilla para reproducibilidad
index <- 0.7
trainIndexCuest <- sample(1:cuest_nrow, index * cuest_nrow)
trainSetCuest <- cuest[trainIndexCuest, ]
testSetCuest <- cuest[-trainIndexCuest, ]

verificar_y_eliminar_niveles <- function(train, test, col) {
  # Verificar si la columna es categórica
  if (col %in% categoricas) {
    # Obtener niveles únicos en el conjunto de entrenamiento y prueba
    niveles_train <- unique(train[[col]])
    niveles_test <- unique(test[[col]])
    
    # Verificar si todos los niveles están presentes en el conjunto de entrenamiento
    niveles_faltantes <- setdiff(niveles_test, niveles_train)
    
    # Verificar si la columna tiene menos de dos niveles
    if (length(niveles_train) < 2 || length(niveles_test) < 2) {
      cat(paste("Eliminando la columna", col, "debido a menos de dos niveles.\n"))
      train[[col]] <- NULL
      test[[col]] <- NULL
    } else if (length(niveles_faltantes) > 0) {
      cat(paste("Eliminando la columna", col, "debido a niveles faltantes en el conjunto de entrenamiento.\n"))
      train[[col]] <- NULL
      test[[col]] <- NULL
    }
  }
  
  return(list(train = train, test = test))
}

# Verificar y eliminar niveles solo para columnas categóricas
for (col in categoricas) {
  result <- verificar_y_eliminar_niveles(trainSetCuest, testSetCuest, col)
  trainSetCuest <- result$train
  testSetCuest <- result$test
}





}



# Target: columna que vamos a predecir: error mediano de cada modelo
target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana",
            "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana", "mapeEnsemble_mediana")
{
allFeatures <- c( # Lista de todas las columnas
  "ID", "LENGTH", "ZERO", "IMPUTED", "AVG",
  "SD", "MIN", "Q1", "MEDIAN", "Q3",
  "MAX", "TOTAL", "VAR", "POT_1", "POT_2",
  "POT_3", "POT_4", "POT_5", "POT_6", "POT_NOM",
  "MC25", "MC50", "MC80", "MC90", "MC95",
  "P_T2.0_VALLE", "P_T2.0_LLANO", "P_T2.0_PICO", "P_T_SOLAR_PICO", "P_T_SOLAR_LLANO",
  "P_T_SOLAR_SPICO", "P_T_SOLAR_SLLANO", "zip_code", "cnae", "municipality",
  "contracted_tariff", "self_consumption_type", "mapeMedia_mediana", "mapeNaive_mediana",
  "mapeSN_mediana", "mapeArima_mediana", "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana",
  "mapeEnsemble_mediana", "mapeMedia_q1", "mapeNaive_q1", "mapeSN_q1", "mapeArima_q1", "mapeETS_q1",
  "mapeSVM_q1", "mapeNN_q1", "mapeEnsemble_q1", "mapeMedia_q3", "mapeNaive_q3", "mapeSN_q3", "mapeArima_q3",
  "mapeETS_q3", "mapeSVM_q3", "mapeNN_q3", "mapeEnsemble_q3", "P1_PICO_PRECIO", "P2_LLANO_PRECIO",
  "P3_VALLE_PRECIO", "kWhTotal_autum_0.4", "kWhTotal_autum_5.8", "kWhTotal_autum_9.12", "kWhTotal_autum_13.16",
  "kWhTotal_autum_17.20", "kWhTotal_autum_21.24", "kWhTotal_spring_0.4", "kWhTotal_spring_5.8", "kWhTotal_spring_9.12",
  "kWhTotal_spring_13.16", "kWhTotal_spring_17.20", "kWhTotal_spring_21.24", "kWhTotal_summer_0.4", "kWhTotal_summer_5.8",
  "kWhTotal_summer_9.12", "kWhTotal_summer_13.16", "kWhTotal_summer_17.20", "kWhTotal_summer_21.24", "kWhTotal_winter_0.4",
  "kWhTotal_winter_5.8", "kWhTotal_winter_9.12", "kWhTotal_winter_13.16", "kWhTotal_winter_17.20", "kWhTotal_winter_21.24",
  "kWhMax_autum_0.4", "kWhMax_autum_5.8", "kWhMax_autum_9.12", "kWhMax_autum_13.16", "kWhMax_autum_17.20", "kWhMax_autum_21.24",
  "kWhMax_spring_0.4", "kWhMax_spring_5.8", "kWhMax_spring_9.12", "kWhMax_spring_13.16", "kWhMax_spring_17.20", "kWhMax_spring_21.24",
  "kWhMax_summer_0.4", "kWhMax_summer_5.8", "kWhMax_summer_9.12", "kWhMax_summer_13.16", "kWhMax_summer_17.20", "kWhMax_summer_21.24",
  "kWhMax_winter_0.4", "kWhMax_winter_5.8", "kWhMax_winter_9.12", "kWhMax_winter_13.16", "kWhMax_winter_17.20", "kWhMax_winter_21.24",
  "kWhTotal_autum_finde", "kWhTotal_spring_finde", "kWhTotal_summer_finde", "kWhTotal_winter_finde", "kWhMax_autum_finde",
  "kWhMax_spring_finde", "kWhMax_summer_finde", "kWhMax_winter_finde"
)



s1 <- c( # All stational feats
  "kWhTotal_autum_0.4", "kWhTotal_autum_5.8", "kWhTotal_autum_9.12", "kWhTotal_autum_13.16",
  "kWhTotal_autum_17.20", "kWhTotal_autum_21.24", "kWhTotal_spring_0.4", "kWhTotal_spring_5.8", "kWhTotal_spring_9.12",
  "kWhTotal_spring_13.16", "kWhTotal_spring_17.20", "kWhTotal_spring_21.24", "kWhTotal_summer_0.4", "kWhTotal_summer_5.8",
  "kWhTotal_summer_9.12", "kWhTotal_summer_13.16", "kWhTotal_summer_17.20", "kWhTotal_summer_21.24", "kWhTotal_winter_0.4",
  "kWhTotal_winter_5.8", "kWhTotal_winter_9.12", "kWhTotal_winter_13.16", "kWhTotal_winter_17.20", "kWhTotal_winter_21.24",
  "kWhMax_autum_0.4", "kWhMax_autum_5.8", "kWhMax_autum_9.12", "kWhMax_autum_13.16", "kWhMax_autum_17.20", "kWhMax_autum_21.24",
  "kWhMax_spring_0.4", "kWhMax_spring_5.8", "kWhMax_spring_9.12", "kWhMax_spring_13.16", "kWhMax_spring_17.20", "kWhMax_spring_21.24",
  "kWhMax_summer_0.4", "kWhMax_summer_5.8", "kWhMax_summer_9.12", "kWhMax_summer_13.16", "kWhMax_summer_17.20", "kWhMax_summer_21.24",
  "kWhMax_winter_0.4", "kWhMax_winter_5.8", "kWhMax_winter_9.12", "kWhMax_winter_13.16", "kWhMax_winter_17.20", "kWhMax_winter_21.24"
  
)

s2 <- c("AVG", "SD", "MIN", "Q1", "MEDIAN", "Q3", "MAX", "TOTAL", "VAR")

s3 <- c("POT_1", "POT_2",  
        "MC25", "MC50", "MC80", "MC90", "MC95","P_T2.0_VALLE", "P_T2.0_LLANO",
        "P_T2.0_PICO", "P_T_SOLAR_PICO", "P_T_SOLAR_LLANO")

}

# Regresion lineal pruebas 
{
# Para evitar predicciones negativas (el error no puede ser negativo)
# usamos logaritmo y luego lo "deshacemos" 
set.seed(0)
index <- 0.75
columns <- append(s3, target[5])
media <- feats[columns] 
media$ID <- feats$ID
media <- media %>% filter(!is.na(mapeETS_mediana))
trainIndex <- sample(1:nrow(media), index * nrow(media))
testIndex <- setdiff(1:nrow(media), trainIndex)
target_variable <- "mapeETS_mediana"

# Transformación logarítmica en conjunto de entrenamiento
trainSet <- media[trainIndex, ] %>% select(-ID)
trainSet$log_mapeETS_mediana <- log(trainSet$mapeETS_mediana + 1)

log_variable <- paste("log", target_variable, sep = "_")
trainSet[[log_variable]] <- log(trainSet[[target_variable]] +1)


testSet <- media[-trainIndex, ] %>% select(-ID)
testSet[[log_variable]] <- log(testSet[[target_variable]] + 1)


nn <-  neuralnet(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet, hidden = 5)

predicciones_log <- (compute(nn, testSet))



# Ajustar el modelo lineal a la variable transformada en el conjunto de entrenamiento
mediaLM_log <- lm(log_mapeMedia_mediana ~ . - mapeMedia_mediana, data = trainSet)

# Transformación logarítmica en conjunto de prueba
testSet <- media[-trainIndex, ] %>% select(-ID)
testSet$log_mapeMedia_mediana <- log(testSet$mapeMedia_mediana + 1)
resultados <- data.frame(ID = media$ID[testIndex], Real = testSet$mapeMedia_mediana)
# Realizar predicciones en el conjunto de prueba

# S1

predicciones_log <- exp(predict(mediaLM_log, newdata = testSet)) - 1
resultados$Predicted_S1 <- predicciones_log
resultados$MAE_S1 <- abs(resultados$Real - resultados$Predicted_S1)

#S2

predicciones_log <- exp(predict(mediaLM_log, newdata = testSet)) - 1
resultados$Predicted_S2 <- predicciones_log
resultados$MAE_S2 <- abs(resultados$Real - resultados$Predicted_S2)

#S3

predicciones_log <- exp(predict(mediaLM_log, newdata = testSet)) - 1
resultados$Predicted_S3 <- predicciones_log
resultados$MAE_S3 <- abs(resultados$Real - resultados$Predicted_S3)

fwrite(resultados, file = "Resultados/PrediccionError/PredMediaLM.csv", col.names = T, row.names = F)

}


# Función para realizar regresión y generar resultados
regresion_model <- function(model_type, target_variable, s1_columns, s2_columns, s3_columns, descSE_columns, descEd_columns, descCG_columns, trainIndex) {
  
  modelo <- gsub("^mape|_mediana$", "", target_variable)
  
  columns_s1 <- append(s1_columns, target_variable)
  columns_s2 <- append(s2_columns, target_variable)
  columns_s3 <- append(s3_columns, target_variable)
  columns_descSE <- append(descSE_columns, target_variable)
  columns_descEd <- append(descEd_columns, target_variable)
  columns_descCG <- append(descCG_columns, target_variable)
  
  columns <- list(columns_s1, columns_s2, columns_s3)
  columnsDesc <- list(descSE_columns, descEd_columns, descCG_columns)
  results_list <- list()
  
  for (i in seq_along(columns)) {

    col <- columns[[i]]
    col_name <- paste("s", i, sep = "")

    datos <- feats3[col]
    datos$ID <- feats3$ID
    datos <- datos %>% filter(!is.na(!!sym(target_variable)))

    trainSet <- datos[trainIndex, ] %>% select(-ID)
    log_variable <- paste("log", target_variable, sep = "_")
    trainSet[[log_variable]] <- log(trainSet[[target_variable]] + 1)

    testSet <- datos[-trainIndex, ] %>% select(-ID)
    testSet[[log_variable]] <- log(testSet[[target_variable]] + 1)


    if (model_type == "lm") {
      # Regresión Lineal
      model <- lm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = testSet)) - 1
    } else if (model_type == "rf") {
      # Random Forest
      model <- randomForest(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = testSet)) - 1
    } else if (model_type == "gbm") {
      # Gradient Boosting
      model <- gbm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = testSet, n.trees = 100)) - 1
    } else if (model_type == "svm"){
      # SVM
      model <- tune(e1071::svm, as.formula(paste(log_variable, "~ . - ", target_variable)),
                                                data = trainSet, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
      predicciones_log <- exp(predict(model$best.model, newdata = testSet)) - 1

    } else if (model_type == "nn"){
      # Neural Network
      model <- neuralnet(
          as.formula(paste(log_variable, "~ . - ", target_variable)),
          data = trainSet,
          hidden = 3
        )
      pred <- compute(model, testSet)
      predicciones_log <- exp(pred$net.result) - 1


    }

    namePred <- paste("Predicted", modelo, col_name, model_type, sep = "_")
    nameMAE <- paste("MAE", modelo, col_name, sep = "_")

    results_list[[namePred]] <- predicciones_log
    results_list[[nameMAE]] <- abs(predicciones_log - testSet[[target_variable]])

  }
  
  names(columnsDesc) <- c("descSE", "descEd", "descCG")
  resultados_list <- list()
  
  for (colsDesc in names(columnsDesc)) {
    
    col_names <- colsDesc
    colsD <- columnsDesc[[colsDesc]]
    datosDesc <- cuest %>%
      select(!!colsD, all_of(target_variable))
    datosDesc <- datosDesc %>% filter(!is.na(!!sym(target_variable)))
    
    # Verificar y eliminar niveles ausentes
    for (col in colsD) {
      result <- verificar_y_eliminar_niveles(trainSetCuest, datosDesc, col)
      trainSetCuest <- result$train
      datosDesc <- result$test
    }
    
    log_variable <- paste("log", target_variable, sep = "_")
    trainSetCuest[[log_variable]] <- log(trainSetCuest[[target_variable]] + 1)
    datosDesc[[log_variable]] <- log(datosDesc[[target_variable]] + 1)
    
    if (model_type == "lm") {
      # Regresión Lineal
      model <- lm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSetCuest)
      predicciones_log <- exp(predict(model, newdata = datosDesc)) - 1
    } else if (model_type == "rf") {
      # Random Forest
      model <- randomForest(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSetCuest)
      predicciones_log <- exp(predict(model, newdata = datosDesc)) - 1
    } else if (model_type == "gbm") {
      # Gradient Boosting
      model <- gbm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSetCuest)
      predicciones_log <- exp(predict(model, newdata = datosDesc, n.trees = 100)) - 1
    } else if (model_type == "svm"){
      # SVM
      model <- tune(e1071::svm, as.formula(paste(log_variable, "~ . - ", target_variable)),
                    data = trainSetCuest, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
      predicciones_log <- exp(predict(model$best.model, newdata = datosDesc)) - 1
    } else if (model_type == "nn"){
      # Neural Network
      model <- neuralnet(
        as.formula(paste(log_variable, "~ . - ", target_variable)),
        data = trainSetCuest,
        hidden = 3
      )
      pred <- compute(model, datosDesc)
      predicciones_log <- exp(pred$net.result) - 1
    }
    
    namePred <- paste("Predicted", modelo, colsDesc, model_type, sep = "_")
    nameMAE <- paste("MAE", modelo, colsDesc, sep = "_")
    
    results_list[[namePred]] <- predicciones_log
    results_list[[nameMAE]] <- abs(predicciones_log - datosDesc[[target_variable]])
  }
  
  resultados <- data.frame(
    ID = datosDesc$ID,  # Puedes necesitar ajustar esto si la columna ID se eliminó
    Real = datosDesc[[target_variable]],
    results_list
  )
  
  # Escribir el CSV final
  write.csv(resultados, file = paste("Resultados/PrediccionError/Cuest/Pred_", modelo, "_", model_type, ".csv", sep = ""))
  
  
  return(resultados)
}

# Lista de modelos
modelos <- c("lm", "rf", "gbm", "svm", "nn")

# Lista de variables objetivo
target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana", 
            "mapeETS_mediana", "mapeNN_mediana", "mapeSVM_mediana", "mapeEnsemble_mediana")

cuest <- feats3 %>%
  inner_join(cuest, by = "ID") %>%
  filter(!is.na(mapeEnsemble_mediana)) 
# Cuest son todos los que han respondido al cuestionario y procesadas
cuest
set.seed(0)
index <- 0.75
cuest_nrow <- nrow(cuest)
feats_nrow <- nrow(feats3 %>% filter(!is.na(mapeEnsemble_mediana)))
trainIndex <- sample(1:feats_nrow, index * feats_nrow) # 214 porque son las que no son NA
trainIndexCuest <- sample(1:cuest_nrow, index * cuest_nrow)



# Aplicar la función para cada modelo y variable objetivo con selección de columnas
for (modelo in modelos) {
  for (variable in target) {
    regresion_model(modelo, variable, s1, s2, s3, descSE, descEd, descCG, trainIndex)
  }
}



# CLASIFICACION

clasificacion_model <- function(model_type, s1, s2, s3) {
  
  # seleccionamos solo las filas que tengan variable de respuesta (best model)
  datos_clasificacion <- feats[which(!is.na(feats$best_model)), ]
  set.seed(0)
  index <- 0.75
  target <- datos_clasificacion$best_model
  names(columns) <- c("s1", "s2", "s3")
  
  columns_s1 <- append(s1, "best_model")
  columns_s2 <- append(s2, "best_model")
  columns_s3 <- append(s3, "best_model")
  
  columns <- list(columns_s1, columns_s2, columns_s3)
  results_list <- list()
  
  for (cols in names(columns)) {
    
    col_name <- cols
    col <- columns[[cols]]
    datos <- datos_clasificacion[, col]
    datos$ID <- datos_clasificacion$ID
    
    trainIndex <- sample(1:nrow(datos), index * nrow(datos))
    trainset <- datos[trainIndex, ] %>% select(-ID)
    testset <- datos[-trainIndex, ] %>% select(-ID)
    
    
    if (model_type == "rf") {
      # Random Forest para clasificación
      modelo_clasificacion <- randomForest(as.factor(best_model) ~ ., data = trainset, ntree = 100, replace = T)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "response")
    } else if (model_type == "gbm") {
      # Gradient Boosting para clasificación
      modelo_clasificacion <- gbm(as.factor(best_model) ~ ., data = trainset, distribution = "multinomial", n.trees = 100)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "response")
    }
    else if (model_type == "logistic") {
      # Regresión Logística para clasificación
      modelo_clasificacion <- multinom(best_model ~ ., data = trainset)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "probs")
    }
    
    namePred <- paste("Predicted", model_type, col_name, sep = "_")
    nameMAE <- paste("MAE", model_type, col_name, sep = "_")
    results_list[[namePred]] <- predicciones_clasificacion
    results_list[[nameMAE]] <- abs(predicciones_clasificacion - as.numeric(testset$best_model))
    
  }
  
  # Crear un nuevo conjunto de resultados
  resultados_clasificacion <- data.frame(
    ID = datos$ID[-trainIndex],
    Real = testset$best_model,
    results_list
  )
  
  # Escribir el CSV final
  fwrite(resultados_clasificacion, file = paste("Resultados/PrediccionClasificacion/Clasif_", model_type, ".csv", sep = ""))
  
  return(resultados_clasificacion)
}


# Definir modelos
modelos_clasificacion <- c("rf", "gbm", "logistic")

# Aplicar la función para clasificación y la variable objetivo "best_model"
for (modelo_clasificacion in modelos_clasificacion) {
  clasificacion_model(modelo_clasificacion, s1, s2, s3)
}



library(randomForest)
library(gbm)
library(nnet)  # Necesario para la regresión logística

clasificacion_model <- function(model_type, s1, s2, s3) {
  
  # seleccionamos solo las filas que tengan variable de respuesta  (best model)
  datos_clasificacion <- feats[which(!is.na(feats$best_model)), ]
  set.seed(0)
  index <- 0.75
  target <- datos_clasificacion$best_model
  
  columns_s1 <- append(s1, "best_model")
  columns_s2 <- append(s2, "best_model")
  columns_s3 <- append(s3, "best_model")
  
  columns <- list(columns_s1, columns_s2, columns_s3)
  names(columns) <- c("s1", "s2", "s3")
  results_list <- list()
  
  for (cols in names(columns)) {
    
    col_name <- cols
    col <- columns[[cols]]
    datos <- datos_clasificacion[, col]

    datos$ID <- datos_clasificacion$ID
    
    trainIndex <- sample(1:nrow(datos), index * nrow(datos))
    trainset <- datos[trainIndex, ] %>% select(-ID)
    testset <- datos[-trainIndex, ] %>% select(-ID)
    
    
    if (model_type == "rf") {
      # Random Forest para clasificación
      modelo_clasificacion <- randomForest(as.factor(best_model) ~ ., data = trainset, ntree = 100, replace = T)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "response")
    } else if (model_type == "gbm") {
      # Gradient Boosting para clasificación
      modelo_clasificacion <- gbm(as.factor(best_model) ~ ., data = trainset, distribution = "multinomial", n.trees = 100)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "response")
    } else if (model_type == "logistic") {
      # Regresión Logística para clasificación
      modelo_clasificacion <- multinom(best_model ~ ., data = trainset)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "probs")
    }
    
    namePred <- paste("Predicted", model_type, col_name, sep = "_")
    nameMAE <- paste("MAE", model_type, col_name, sep = "_")
    
    results_list[[namePred]] <- predicciones_clasificacion
    results_list[[nameMAE]] <- abs(predicciones_clasificacion - as.numeric(testset$best_model))
  }
  
  # Crear un nuevo conjunto de resultados
  resultados_clasificacion <- data.frame(
    ID = datos$ID[-trainIndex],
    Real = testset$best_model,
    results_list
  )
  
  # Escribir el CSV final
  fwrite(resultados_clasificacion, file = paste("Resultados/PrediccionClasificacion/Clasif_", model_type, ".csv", sep = ""))
  
  return(resultados_clasificacion)
}

# Definir modelos
modelos_clasificacion <- c( "gbm", "logistic")

for (modelo_clasificacion in modelos_clasificacion) {
  clasificacion_model(modelo_clasificacion, s1, s2, s3)
}   






