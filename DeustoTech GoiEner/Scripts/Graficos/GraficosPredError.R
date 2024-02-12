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

# LEER archivos
{
folder <- "Resultados/PrediccionError/"
lm <- list.files(folder, pattern = "_lm.csv$", recursive = T, full.names = F)
rf <- list.files(folder, pattern = "_rf.csv$", recursive = T, full.names = F)
gbm <- list.files(folder, pattern = "_gbm.csv$", recursive = T, full.names = F)
svm <- list.files(folder, pattern = "_svm.csv$", recursive = T, full.names = F)
nn <- list.files(folder, pattern = "_nn.csv$", recursive = T, full.names = F)

Arima <- list.files(folder, pattern = "Pred_Arima_.*\\.csv$", recursive = T, full.names = F)
Arima <- paste(folder, Arima, sep = "")

Ensemble <- list.files(folder, pattern = "Pred_Ensemble_.*\\.csv$", recursive = T, full.names = F)
Ensemble <- paste(folder, Ensemble, sep = "")

ETS <- list.files(folder, pattern = "Pred_ETS_.*\\.csv$", recursive = T, full.names = F)
ETS <- paste(folder, ETS, sep = "")

Media <- list.files(folder, pattern = "Pred_Media_.*\\.csv$", recursive = T, full.names = F)
Media <- paste(folder, Media, sep = "")

Naive <- list.files(folder, pattern = "Pred_Naive_.*\\.csv$", recursive = T, full.names = F)
Naive <- paste(folder, Naive, sep = "")

NN <- list.files(folder, pattern = "Pred_NN_.*\\.csv$", recursive = T, full.names = F)
NN <- paste(folder, NN, sep = "")

SN <- list.files(folder, pattern = "Pred_SN_.*\\.csv$", recursive = T, full.names = F)
SN <- paste(folder, SN, sep = "")

SVM <- list.files(folder, pattern = "Pred_SVM_.*\\.csv$", recursive = T, full.names = F)
SVM <- paste(folder, SVM, sep = "")
}

# ARIMA
{
data_list <- lapply(Arima, fread)

model_names <- gsub("Resultados/PrediccionError/Pred_Arima_(.*?)\\.csv", "\\1", Arima)
# Cambiar el nombre de las columnas en cada dataframe
for (i in seq_along(data_list)) {
  names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
}

# Combinar los dataframes en uno solo, haciendo join por ID
combined_arima <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)


# Seleccionar las columnas relevantes (MAE_s1) para cada modelo
columns_to_plot <- grep("MAE_.*_s*", names(combined_arima), value = TRUE)

# Extraer el modelo y el número de serie de cada columna
# Modificar las etiquetas del modelo
model_series <- gsub("MAE_Arima_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)

# Eliminar "MAE_Arima_" y "s" del nombre del modelo
model_series <- gsub("MAE_Arima_", "", model_series)

boxplot(combined_arima[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_arima[, ..columns_to_plot], 0.9, na.rm = TRUE)),
        at = seq_along(columns_to_plot) * 2, names = F)

axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)

# Añadir un título
title(main = "MAE arima por modelo y conjunto de features")


# Añadir etiquetas a los ejes
xlabel <- "Modelo"
ylabel <- "MAE"
title(xlab = xlabel, ylab = ylabel)
}


# ENSEMBLE
{
data_list <- lapply(Ensemble, fread)

model_names <- gsub("Resultados/PrediccionError/Pred_Ensemble_(.*?)\\.csv", "\\1", Ensemble)
# Cambiar el nombre de las columnas en cada dataframe
for (i in seq_along(data_list)) {
  names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
}

# Combinar los dataframes en uno solo, haciendo join por ID
combined_ensemble <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)


# Seleccionar las columnas relevantes (MAE_s1) para cada modelo
columns_to_plot <- grep("MAE_.*_s*", names(combined_ensemble), value = TRUE)

# Extraer el modelo y el número de serie de cada columna
# Modificar las etiquetas del modelo
model_series <- gsub("MAE_Ensemble_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)

# Eliminar "MAE_Arima_" y "s" del nombre del modelo
model_series <- gsub("MAE_Ensemble_", "", model_series)

boxplot(combined_ensemble[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_ensemble[, ..columns_to_plot], 0.9, na.rm = TRUE)),
        at = seq_along(columns_to_plot) * 2, names = F)

axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)

# Añadir un título
title(main = "MAE Ensemble por modelo y conjunto de features")


# Añadir etiquetas a los ejes
xlabel <- "Modelo"
ylabel <- "MAE"
title(xlab = xlabel, ylab = ylabel)
}


# ETS
{
data_list <- lapply(ETS, fread)

model_names <- gsub("Resultados/PrediccionError/Pred_ETS_(.*?)\\.csv", "\\1", ETS)
# Cambiar el nombre de las columnas en cada dataframe
for (i in seq_along(data_list)) {
  names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
}

# Combinar los dataframes en uno solo, haciendo join por ID
combined_ets <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)


# Seleccionar las columnas relevantes (MAE_s1) para cada modelo
columns_to_plot <- grep("MAE_.*_s*", names(combined_ets), value = TRUE)

# Extraer el modelo y el número de serie de cada columna
# Modificar las etiquetas del modelo
model_series <- gsub("MAE_ETS_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)

# Eliminar "MAE_Arima_" y "s" del nombre del modelo
model_series <- gsub("MAE_ETS_", "", model_series)

boxplot(combined_ets[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_ets[, ..columns_to_plot], 0.9, na.rm = TRUE)),
        at = seq_along(columns_to_plot) * 2, names = F)

axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)

# Añadir un título
title(main = "MAE ETS por modelo y conjunto de features")


# Añadir etiquetas a los ejes
xlabel <- "Modelo"
ylabel <- "MAE"
title(xlab = xlabel, ylab = ylabel)

}

# MEDIA
{
  data_list <- lapply(Media, fread)
  
  model_names <- gsub("Resultados/PrediccionError/Pred_Media_(.*?)\\.csv", "\\1", Media)
  # Cambiar el nombre de las columnas en cada dataframe
  for (i in seq_along(data_list)) {
    names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
  }
  
  # Combinar los dataframes en uno solo, haciendo join por ID
  combined_media <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)
  
  
  # Seleccionar las columnas relevantes (MAE_s1) para cada modelo
  columns_to_plot <- grep("MAE_.*_s*", names(combined_media), value = TRUE)
  
  # Extraer el modelo y el número de serie de cada columna
  # Modificar las etiquetas del modelo
  model_series <- gsub("MAE_Media_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)
  
  # Eliminar "MAE_Arima_" y "s" del nombre del modelo
  model_series <- gsub("MAE_Media_", "", model_series)
  
  boxplot(combined_media[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_media[, ..columns_to_plot], 0.9, na.rm = TRUE)),
          at = seq_along(columns_to_plot) * 2, names = F)
  
  axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)
  
  # Añadir un título
  title(main = "MAE Media por modelo y conjunto de features")
  
  
  # Añadir etiquetas a los ejes
  xlabel <- "Modelo"
  ylabel <- "MAE"
  title(xlab = xlabel, ylab = ylabel)
}

# NAIVE
{
  data_list <- lapply(Naive, fread)
  
  model_names <- gsub("Resultados/PrediccionError/Pred_Naive_(.*?)\\.csv", "\\1", Naive)
  # Cambiar el nombre de las columnas en cada dataframe
  for (i in seq_along(data_list)) {
    names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
  }
  
  # Combinar los dataframes en uno solo, haciendo join por ID
  combined_naive <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)
  
  
  # Seleccionar las columnas relevantes (MAE_s1) para cada modelo
  columns_to_plot <- grep("MAE_.*_s*", names(combined_naive), value = TRUE)
  
  # Extraer el modelo y el número de serie de cada columna
  # Modificar las etiquetas del modelo
  model_series <- gsub("MAE_Naive_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)
  
  # Eliminar "MAE_Arima_" y "s" del nombre del modelo
  model_series <- gsub("MAE_Naive_", "", model_series)
  
  boxplot(combined_naive[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_naive[, ..columns_to_plot], 0.9, na.rm = TRUE)),
          at = seq_along(columns_to_plot) * 2, names = F)
  
  axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)
  
  # Añadir un título
  title(main = "MAE Naive por modelo y conjunto de features")
  
  
  # Añadir etiquetas a los ejes
  xlabel <- "Modelo"
  ylabel <- "MAE"
  title(xlab = xlabel, ylab = ylabel)
}

# NN
{
  data_list <- lapply(NN, fread)
  
  model_names <- gsub("Resultados/PrediccionError/Pred_NN_(.*?)\\.csv", "\\1", NN)
  # Cambiar el nombre de las columnas en cada dataframe
  for (i in seq_along(data_list)) {
    names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
  }
  
  # Combinar los dataframes en uno solo, haciendo join por ID
  combined_nn <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)
  
  
  # Seleccionar las columnas relevantes (MAE_s1) para cada modelo
  columns_to_plot <- grep("MAE_.*_s*", names(combined_nn), value = TRUE)
  
  # Extraer el modelo y el número de serie de cada columna
  # Modificar las etiquetas del modelo
  model_series <- gsub("MAE_NN_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)
  
  # Eliminar "MAE_Arima_" y "s" del nombre del modelo
  model_series <- gsub("MAE_NN_", "", model_series)
  
  boxplot(combined_nn[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_nn[, ..columns_to_plot], 0.9, na.rm = TRUE)),
          at = seq_along(columns_to_plot) * 2, names = F)
  
  axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)
  
  # Añadir un título
  title(main = "MAE NN por modelo y conjunto de features")
  
  
  # Añadir etiquetas a los ejes
  xlabel <- "Modelo"
  ylabel <- "MAE"
  title(xlab = xlabel, ylab = ylabel)
}

# SN
{
  data_list <- lapply(SN, fread)
  
  model_names <- gsub("Resultados/PrediccionError/Pred_SN_(.*?)\\.csv", "\\1", SN)
  # Cambiar el nombre de las columnas en cada dataframe
  for (i in seq_along(data_list)) {
    names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
  }
  
  # Combinar los dataframes en uno solo, haciendo join por ID
  combined_sn <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)
  
  
  # Seleccionar las columnas relevantes (MAE_s1) para cada modelo
  columns_to_plot <- grep("MAE_.*_s*", names(combined_sn), value = TRUE)
  
  # Extraer el modelo y el número de serie de cada columna
  # Modificar las etiquetas del modelo
  model_series <- gsub("MAE_SN_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)
  
  # Eliminar "MAE_Arima_" y "s" del nombre del modelo
  model_series <- gsub("MAE_SN_", "", model_series)
  
  boxplot(combined_sn[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_sn[, ..columns_to_plot], 0.9, na.rm = TRUE)),
          at = seq_along(columns_to_plot) * 2, names = F)
  
  axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)
  
  # Añadir un título
  title(main = "MAE SN por modelo y conjunto de features")
  
  
  # Añadir etiquetas a los ejes
  xlabel <- "Modelo"
  ylabel <- "MAE"
  title(xlab = xlabel, ylab = ylabel)
}

# SVM
{
  data_list <- lapply(SVM, fread)
  
  model_names <- gsub("Resultados/PrediccionError/Pred_SVM_(.*?)\\.csv", "\\1", SVM)
  # Cambiar el nombre de las columnas en cada dataframe
  for (i in seq_along(data_list)) {
    names(data_list[[i]])[names(data_list[[i]]) != "ID"] <- paste0(model_names[i], "_", names(data_list[[i]])[names(data_list[[i]]) != "ID"])
  }
  
  # Combinar los dataframes en uno solo, haciendo join por ID
  combined_svm <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), data_list)
  
  
  # Seleccionar las columnas relevantes (MAE_s1) para cada modelo
  columns_to_plot <- grep("MAE_.*_s*", names(combined_svm), value = TRUE)
  
  # Extraer el modelo y el número de serie de cada columna
  # Modificar las etiquetas del modelo
  model_series <- gsub("MAE_SVM_(.*)_(\\d+)", "\\1_\\2", columns_to_plot)
  
  # Eliminar "MAE_Arima_" y "s" del nombre del modelo
  model_series <- gsub("MAE_SVM_", "", model_series)
  
  boxplot(combined_svm[, ..columns_to_plot], outline = FALSE, ylim = c(0, quantile(combined_svm[, ..columns_to_plot], 0.9, na.rm = TRUE)),
          at = seq_along(columns_to_plot) * 2, names = F)
  
  axis(1, at = seq_along(columns_to_plot) * 2, labels = model_series, las = 2, cex.axis = 0.8)
  
  # Añadir un título
  title(main = "MAE SVM por modelo y conjunto de features")
  
  
  # Añadir etiquetas a los ejes
  xlabel <- "Modelo"
  ylabel <- "MAE"
  title(xlab = xlabel, ylab = ylabel)
}



##GRÁFICOS PREDICCIONES 3
{
  feats3 <- fread("featuresPredicciones_3.csv")
  
  summary(feats3)
  
  barplot_data <- table(feats3$best_model)
  barplot_data <- as.data.frame(barplot_data)
  barplot_data <- barplot_data %>% filter(Freq != 17307)
  
  
  barplot(barplot_data$Freq, 
          main = "Distribución del modelo con menor error mediano",
          names.arg = barplot_data$Var1,
          xlab = "Modelo",
          ylab = "Frecuencia",
          col = "skyblue",
          ylim = c(0, max(barplot_data$Freq) + 10),  # Ajusta el límite del eje y para mayor claridad
          las = 2)  # Rotar etiquetas del eje x si son largas
}


##prediccion clasificacion
{
  clasif_logistic <- fread("Resultados/PrediccionClasificacion/Clasif_logistic.csv")
  clasif_gbm <- fread("Resultados/PrediccionClasificacion/Clasif_gbm.csv")
  clasif_rf <- fread("Resultados/PrediccionClasificacion/Clasif_rf.csv") #con este no podemos hacer nada, esta mal
  clasif_gbm <- sub("\\.100$", "", clasif_gbm)
}



#Arima
{
  mae_arima_log <- grep("^MAE.*Arima$", names(clasif_logistic), value = TRUE)
  ma_arima_gbm <- grep("^MAE.*Arima.100$", names(clasif_logistic), value = TRUE)
}


#CLASIFICACION
{
  folder <- "Resultados/PrediccionClasificacion/"
  gbm <- list.files(folder, pattern = "_gbm.csv$", recursive = T, full.names = F)
  knn <- list.files(folder, pattern = "_knn.csv$", recursive = T, full.names = F)
  logistic <- list.files(folder, pattern = "_logistic.csv$", recursive = T, full.names = F)
  rf <- list.files(folder, pattern = "_rf.csv$", recursive = T, full.names = F)
  svm <- list.files(folder, pattern = "_svm.csv$", recursive = T, full.names = F)
  
  Arima <- list.files(folder, pattern = "Pred_Arima_.*\\.csv$", recursive = T, full.names = F)
  Arima <- paste(folder, Arima, sep = "")
  
  Ensemble <- list.files(folder, pattern = "Pred_Ensemble_.*\\.csv$", recursive = T, full.names = F)
  Ensemble <- paste(folder, Ensemble, sep = "")
  
  ETS <- list.files(folder, pattern = "Pred_ETS_.*\\.csv$", recursive = T, full.names = F)
  ETS <- paste(folder, ETS, sep = "")
  
  Media <- list.files(folder, pattern = "Pred_Media_.*\\.csv$", recursive = T, full.names = F)
  Media <- paste(folder, Media, sep = "")
  
  Naive <- list.files(folder, pattern = "Pred_Naive_.*\\.csv$", recursive = T, full.names = F)
  Naive <- paste(folder, Naive, sep = "")
  
  NN <- list.files(folder, pattern = "Pred_NN_.*\\.csv$", recursive = T, full.names = F)
  NN <- paste(folder, NN, sep = "")
  
  
  SN <- list.files(folder, pattern = "Pred_SN_.*\\.csv$", recursive = T, full.names = F)
  SN <- paste(folder, SN, sep = "")
  
  SVM <- list.files(folder, pattern = "Pred_SVM_.*\\.csv$", recursive = T, full.names = F)
  SVM <- paste(folder, SVM, sep = "")
}