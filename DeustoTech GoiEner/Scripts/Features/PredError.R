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
setwd('C:/GIT HUB ANE/IA4DER-forecasting/DeustoTech GoiEner')

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

feats <- read.csv("featuresPredicciones_2.csv")

summary(feats)
colnames(feats)

# Estos son errores de los q1 y q3. De momento no los usamos

# "mapeMedia_q1", "mapeNaive_q1", "mapeSN_q1", "mapeArima_q1", "mapeETS_q1",
#   "mapeSVM_q1", "mapeNN_q1", "mapeEnsemble_q1", "mapeMedia_q3", "mapeNaive_q3", "mapeSN_q3", "mapeArima_q3",
#   "mapeETS_q3", "mapeSVM_q3", "mapeNN_q3", "mapeEnsemble_q3", 


# Target: columna que vamos a predecir: error mediano de cada modelo
target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana",
            "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana", "mapeEnsemble_mediana")

# target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana", 
#             "mapeETS_mediana", "mapeNN_mediana")



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



# Regresion lineal 
# Para evitar predicciones negativas (el error no puede ser negativo)
# usamos logaritmo y luego lo "deshacemos" 
set.seed(0)
index <- 0.75
columns <- append(s3, target[1])
media <- feats[columns] 
media$ID <- feats$ID
media <- media %>% filter(!is.na(mapeMedia_mediana))
trainIndex <- sample(1:nrow(media), index * nrow(media))
testIndex <- setdiff(1:nrow(media), trainIndex)


# Transformación logarítmica en conjunto de entrenamiento
trainSet <- media[trainIndex, ] %>% select(-ID)
trainSet$log_mapeMedia_mediana <- log(trainSet$mapeMedia_mediana + 1)

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




#BOXPLOT DEL ERROR
predMediaLM <- fread("Resultados/PrediccionError/PredMediaLM.csv")
predMediaLM_PCA <- fread("Resultados/PrediccionError/PredPCA.csv")
data_filtered <- predMediaLM[predMediaLM$MAE_S1 <= quantile(predMediaLM$MAE_S1, 0.75) &
                               predMediaLM$MAE_S2 <= quantile(predMediaLM$MAE_S2, 0.75) &
                               predMediaLM$MAE_S3 <= quantile(predMediaLM$MAE_S3, 0.75), ]
data_filtered_pca <- predMediaLM_PCA[predMediaLM_PCA$MAE_PCA <= quantile(predMediaLM_PCA$MAE_PCA, 0.75, na.rm = T)]

data_filtered <- data_filtered %>% select(MAE_S1, MAE_S2, MAE_S3)
data_filtered_pca <- data_filtered_pca %>% select(MAE_PCA)

data_filtered_all <- cbind(data_filtered, data_filtered_pca)

boxplot(data_filtered_all, col = rainbow(ncol(data_filtered_all)))


# Función para realizar la regresión lineal y generar resultados
regresion_lineal <- function(target_variable, s1_columns, s2_columns, s3_columns) {
  
  modelo <- gsub("^mape|_mediana$", "", target_variable)
  
  set.seed(0)
  index <- 0.75
  
  columns_s1 <- append(s1_columns, target_variable)
  columns_s2 <- append(s2_columns, target_variable)
  columns_s3 <- append(s3_columns, target_variable)
  
  columns <- list(columns_s1, columns_s2, columns_s3)
  results_list <- list()
  
  for (i in seq_along(columns)) {
    
    col <- columns[[i]]
    col_name <- paste("s", i, sep = "")
    
    datos <- feats[col]
    datos$ID <- feats$ID
    datos <- datos %>% filter(!is.na(!!sym(target_variable)))
    
    trainIndex <- sample(1:nrow(datos), index * nrow(datos))
    testIndex <- setdiff(1:nrow(datos), trainIndex)
    
    trainSet <- datos[trainIndex, ] %>% select(-ID)
    log_variable <- paste("log", target_variable, sep = "_")
    trainSet[[log_variable]] <- log(trainSet[[target_variable]] +1)
    
    lm_formula <- as.formula(paste(log_variable, "~ . - ", target_variable))
    datosLM_log <- lm(lm_formula, data = trainSet)
    
    testSet <- datos[-trainIndex, ] %>% select(-ID)
    testSet[[log_variable]] <- log(testSet[[target_variable]] + 1)
    
    predicciones_log <- exp(predict(datosLM_log, newdata = testSet)) - 1
    
    namePred <- paste("Predicted", modelo, col_name, sep = "_")
    nameMAE <- paste("MAE", modelo, col_name, sep = "_")
    
    results_list[[namePred]] <- predicciones_log
    results_list[[nameMAE]] <- abs(testSet[[target_variable]] - predicciones_log)
  }
  
  resultados <- data.frame(
    ID = datos$ID[testIndex],
    Real = testSet[[target_variable]],
    results_list
  )
  
  # Escribir el CSV final
  fwrite(resultados, file = paste("Resultados/PrediccionError/Pred_", modelo, ".csv", sep = ""))
  
  return(resultados)
}

# Lista de variables objetivo
target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana", 
            "mapeETS_mediana", "mapeNN_mediana")
target2 <- c("mapeSVM_mediana", "mapeEnsemble_mediana")

# Aplicar la función para cada variable objetivo y selección de columnas
for (variable in target2) {
  regresion_lineal(variable, s1, s2, s3)
}



# Instalar paquetes si no están instalados
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

if (!requireNamespace("gbm", quietly = TRUE)) {
  install.packages("gbm")
}

library(randomForest)
library(gbm)

# Función para realizar regresión y generar resultados
regresion_model <- function(model_type, target_variable, s1_columns, s2_columns, s3_columns) {
  
  modelo <- gsub("^mape|_mediana$", "", target_variable)
  
  set.seed(0)
  index <- 0.75
  
  columns_s1 <- append(s1_columns, target_variable)
  columns_s2 <- append(s2_columns, target_variable)
  columns_s3 <- append(s3_columns, target_variable)
  
  columns <- list(columns_s1, columns_s2, columns_s3)
  results_list <- list()
  
  for (i in seq_along(columns)) {
    
    col <- columns[[i]]
    col_name <- paste("s", i, sep = "")
    
    datos <- feats[col]
    datos$ID <- feats$ID
    datos <- datos %>% filter(!is.na(!!sym(target_variable)))
    
    trainIndex <- sample(1:nrow(datos), index * nrow(datos))
    testIndex <- setdiff(1:nrow(datos), trainIndex)
    
    trainSet <- datos[trainIndex, ] %>% select(-ID)
    log_variable <- paste("log", target_variable, sep = "_")
    trainSet[[log_variable]] <- log(trainSet[[target_variable]] + 1)
    
    if (model_type == "lm") {
      # Regresión Lineal
      model <- lm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = trainSet)) - 1
    } else if (model_type == "rf") {
      # Random Forest
      model <- randomForest(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = trainSet)) - 1
    } else if (model_type == "gbm") {
      # Gradient Boosting
      model <- gbm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
      predicciones_log <- exp(predict(model, newdata = trainSet, n.trees = 100)) - 1
    }
    
    namePred <- paste("Predicted", modelo, col_name, sep = "_")
    nameMAE <- paste("MAE", modelo, col_name, sep = "_")
    
    results_list[[namePred]] <- predicciones_log
    results_list[[nameMAE]] <- abs(trainSet[[target_variable]] - predicciones_log)
  }
  
  resultados <- data.frame(
    ID = datos$ID[trainIndex],
    Real = trainSet[[target_variable]],
    results_list
  )
  
  # Escribir el CSV final
  fwrite(resultados, file = paste("Resultados/PrediccionError/Pred_", modelo, "_", model_type, ".csv", sep = ""))
  
  return(resultados)
}

# Lista de modelos
modelos <- c("lm", "rf", "gbm")

# Lista de variables objetivo
target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana", 
            "mapeETS_mediana", "mapeNN_mediana", "mapeSVM_mediana", "mapeEnsemble_mediana")

# Aplicar la función para cada modelo y variable objetivo con selección de columnas
for (modelo in modelos) {
  for (variable in target) {
    regresion_model(modelo, variable, s1, s2, s3)
  }
}


#### PCA ####


#preparar datos para pca
feats <- read.csv("featuresPredicciones_2.csv")

featsPCA <- feats %>% select(- c("ID", "municipality", "LENGTH", "ZERO", "IMPUTED"))

best_model <- c('Media' = 0, 'Naive' = 1, 'Arima' = 2, 'NN' = 3, 'ETS' = 4)
contracted_tariff <- c("2.0TD" = 0, "3.0TD" = 1, "6.1TD" = 2, "6.2TD" = 3)
self_consumption_type <- c("00" = 0, "0" = 1, "41" = 2, "42" = 3, "43" = 4, "2B" = 5)
featsPCA$best_model <- match(featsPCA$best_model, names(best_model))
featsPCA$contracted_tariff <- match(featsPCA$contracted_tariff, names(contracted_tariff))
featsPCA$self_consumption_type <- match(featsPCA$self_consumption_type, names(self_consumption_type))

featsPCA <- impute(featsPCA, "mean")
featsPCA <- scale(featsPCA)
pca_result <- prcomp(featsPCA)
summary(pca_result)


regresion_pca_model <- function(model_type, target_variable, pca_result) {
  
  modelo <- gsub("^mape|_mediana$", "", target_variable)
  
  set.seed(0)
  index <- 0.75
  
  # Seleccionar las columnas correspondientes a los primeros dos componentes principales
  columnas_pca <- c("PC1", "PC2") 
  featsPCA <- as.data.frame(predict(pca_result, newdata = featsPCA)[, columnas_pca])
  
  # Crear un nuevo conjunto de datos con los componentes principales y la variable objetivo
  datos_pca <- cbind(featsPCA, new_variable = feats[[target_variable]])
  
  set.seed(0)
  index <- sample(1:nrow(datos_pca), 0.75 * nrow(datos_pca))
  trainset <- datos_pca[index, ]
  testset <- datos_pca[-index, ]
  
  
  if (model_type == "lm") {
    # Regresión Lineal
    modelo_pca <- lm(new_variable ~ ., data = trainset)
    predicciones_pca <- predict(modelo_pca, newdata = testset)
  } else if (model_type == "rf") {
    # Random Forest
    modelo_pca <- randomForest(new_variable ~ ., data = trainset)
    predicciones_pca <- predict(modelo_pca, newdata = testset)
  } else if (model_type == "gbm") {
    # Gradient Boosting
    modelo_pca <- gbm(new_variable ~ ., data = trainset)
    predicciones_pca <- predict(modelo_pca, newdata = testset, n.trees = 100)
  }
  
  # Crear un nuevo conjunto de resultados solo para PCA
  resultados_pca <- data.frame(
    ID = feats$ID[-index],
    Real = testset$new_variable,
    Predicted_PCA = rep(NA, length(feats$ID[-index])),  # Inicializar con NA
    MAE_PCA = rep(NA, length(feats$ID[-index]))  # Inicializar con NA
  )
  
 
  # Asignar las predicciones PCA a las ubicaciones correspondientes
  resultados_pca$Predicted_PCA[match(resultados_pca$ID, feats$ID[-index])] <- predicciones_pca
  resultados_pca$MAE_PCA[match(resultados_pca$ID, feats$ID[-index])] <- abs(predicciones_pca - testset$new_variable)
  
  # Escribir el CSV final
  fwrite(resultados_pca, file = paste("Resultados/PrediccionError/PredPCA_", modelo, "_", model_type, ".csv", sep = ""))
  
  return(resultados_pca)
}

# Definir modelos
modelos <- c("lm", "rf", "gbm")

# Aplicar la función para cada modelo y variable objetivo
for (modelo in modelos) {
  for (variable in target) {
    regresion_pca_model(modelo, variable, pca_result)
  }
}





# Crear un nuevo conjunto de datos con los componentes principales y la variable objetivo
datos_pca <- cbind(featsPCA, mapeMedia_mediana = feats$mapeMedia_mediana)

set.seed(0)
index <- sample(1:nrow(datos_pca), 0.75 * nrow(datos_pca))
trainset <- datos_pca[index, ]
testset <- datos_pca[-index, ]

modelo_pca <- lm([[target_variable]] ~ ., data = trainset_pca)
predicciones_pca <- predict(modelo_pca, newdata = testset_pca)


# Crear un nuevo conjunto de resultados solo para PCA
resultados_pca <- data.frame(
  ID = feats$ID[-index],
  Real = testset$mapeMedia_mediana,
  Predicted_PCA = rep(NA, length(feats$ID[-index])),  # Inicializar con NA
  MAE_PCA = rep(NA, length(feats$ID[-index]))  # Inicializar con NA
)

# Asignar las predicciones PCA a las ubicaciones correspondientes
resultados$Predicted_PCA[match(resultados_pca$ID, feats$ID[-index])] <- predicciones_pca
resultados$MAE_PCA[match(resultados_pca$ID, feats$ID[-index])] <- abs(predicciones_pca - testset$mapeMedia_mediana)


fwrite(resultados_pca, file = "Resultados/PrediccionError/PredPCA.csv", col.names = TRUE, row.names = FALSE)



unique(feats$best_model)



library(randomForest)
library(gbm)

library(randomForest)
library(gbm)

clasificacion_model <- function(model_type) {
  
  set.seed(0)
  index <- 0.75
  
  # Crear un nuevo conjunto de datos con las variables predictoras y la variable objetivo
  datos_clasificacion <- feats
  datos_clasificacion <- na.omit(datos_clasificacion)  
  print(datos_clasificacion)
  set.seed(0)
  index <- sample(1:nrow(datos_clasificacion), index * nrow(datos_clasificacion))
  trainset <- datos_clasificacion[index, ]
  testset <- datos_clasificacion[-index, ]
  
  if (model_type == "rf") {
    # Random Forest para clasificación
    modelo_clasificacion <- randomForest(as.factor(best_model) ~ ., data = trainset)
    predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "response")
  } else if (model_type == "gbm") {
    # Gradient Boosting para clasificación
    modelo_clasificacion <- gbm(as.factor(best_model) ~ ., data = trainset, distribution = "multinomial", n.trees = 100)
    predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "response")
  }
  
  # Crear un nuevo conjunto de resultados
  resultados_clasificacion <- data.frame(
    ID = testset$ID,
    Real = testset$best_model,
    Predicted = predicciones_clasificacion,
    Correct_Predictions = ifelse(predicciones_clasificacion == testset$best_model, 1, 0)
  )
  
  # Escribir el CSV final
  fwrite(resultados_clasificacion, file = paste("Resultados/PrediccionClasificacion/Clasif_", model_type, ".csv", sep = ""))
  
  return(resultados_clasificacion)
}

# Definir modelos
modelos_clasificacion <- c("rf", "gbm")

# Aplicar la función para clasificación y la variable objetivo "best_model"
for (modelo_clasificacion in modelos_clasificacion) {
  clasificacion_model(modelo_clasificacion)
}


