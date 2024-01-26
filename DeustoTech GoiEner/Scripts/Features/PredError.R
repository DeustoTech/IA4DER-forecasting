# Script para predecir el error que comete cada modelo 
# a partir de las features 

library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


# Cargar ficheros y constantes

folder <- "TransformersV2/"
# Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)
metadata_file <- fread("metadata.csv")

# csv_files <- csv_files[201:length(csv_files)]

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

model_names <- c("Media", "Naive", "SNaive", "Arima", "ETS", "SVM", "NN", "Ensemble")


# Carga fichero con todas las features

feats <- read.csv("featuresPredicciones.csv")
colnames(feats)


# Estos son errores de los q1 y q3. De momento no los usamos

# "mapeMedia_q1", "mapeNaive_q1", "mapeSN_q1", "mapeArima_q1", "mapeETS_q1",
#   "mapeSVM_q1", "mapeNN_q1", "mapeEnsemble_q1", "mapeMedia_q3", "mapeNaive_q3", "mapeSN_q3", "mapeArima_q3",
#   "mapeETS_q3", "mapeSVM_q3", "mapeNN_q3", "mapeEnsemble_q3", 


# Target: columna que vamos a predecir: error mediano de cada modelo

target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana", 
            "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana", "mapeEnsemble_mediana")



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



features <- c(
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


############################### Regresion lineal 


# Para evitar predicciones negativas (el error no puede ser negativo)
# usamos logaritmo y luego lo "deshacemos" 
set.seed(0)

index <- 0.75
trainIndex <- sample(1:nrow(media), index * nrow(media))



columns <- append(features, target[1])
media <- feats[columns] 
media$ID <- feats$ID
media <- media %>% filter(!is.na(mapeMedia_mediana))


# Transformación logarítmica en conjunto de entrenamiento
trainSet <- media[trainIndex, ] %>% select(-ID)
trainSet$log_mapeMedia_mediana <- log(trainSet$mapeMedia_mediana + 1)

# Ajustar el modelo lineal a la variable transformada en el conjunto de entrenamiento
mediaLM_log <- lm(log_mapeMedia_mediana ~ .-ID - mapeMedia_mediana, data = trainSet)

# Transformación logarítmica en conjunto de prueba
testSet <- media[-trainIndex, ] %>% select(-ID)
testSet$log_mapeMedia_mediana <- log(testSet$mapeMedia_mediana + 1)

# Realizar predicciones en el conjunto de prueba
predicciones_log <- exp(predict(mediaLM_log, newdata = testSet)) - 1

# Comparar predicciones con los valores reales en el conjunto de prueba
resultados <- data.frame(Real = testSet$mapeMedia_mediana, Prediccion = predicciones_log)
print(resultados)



# Otros modelos: gradient boosting, random walk, random forest, svr










