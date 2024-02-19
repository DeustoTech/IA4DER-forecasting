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
folder <- "Resultados/PrediccionError/AllFeats"
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


#LEER NUEVOS ARCHIVOS
{
  folder <- "Resultados/PrediccionError/AllFeats/"
  
  # Definir una función para buscar archivos por patrón en todas las carpetas de modelos
  buscar_archivos_por_modelo <- function(folder, pattern) {
    # Obtener una lista de todas las subcarpetas dentro de la carpeta principal
    subcarpetas <- list.dirs(folder, recursive = FALSE)
    
    # Inicializar una lista para guardar los resultados
    archivos_modelo <- character()
    
    # Iterar sobre cada subcarpeta para buscar archivos que coincidan con el patrón
    for (subcarpeta in subcarpetas) {
      archivos_encontrados <- list.files(subcarpeta, pattern = pattern, recursive = TRUE, full.names = TRUE)
      archivos_modelo <- c(archivos_modelo, archivos_encontrados)
    }
    
    return(archivos_modelo)
  }
  
  # Buscar archivos por cada tipo de modelo
  archivos_lm <- buscar_archivos_por_modelo(folder, "_lm_.*\\.csv$")
  archivos_rf <- buscar_archivos_por_modelo(folder, "_rf_.*\\.csv$")
  archivos_gbm <- buscar_archivos_por_modelo(folder, "_gbm_.*\\.csv$")
  archivos_svm <- buscar_archivos_por_modelo(folder, "_svm_.*\\.csv$")
  archivos_nn <- buscar_archivos_por_modelo(folder, "_nn_.*\\.csv$")
  
  
  
  # Para modelos con prefijo específico en el nombre del archivo
  Arima <- list.files(paste0(folder, "Arima/"), pattern = "Pred_Arima_.*\\.csv$", recursive = T, full.names = F)
  Arima <- paste0(folder, "Arima/", Arima)
  
  Ensemble <- list.files(paste0(folder, "Ensemble/"), pattern = "Pred_Ensemble_.*\\.csv$", recursive = T, full.names = F)
  Ensemble <- paste0(folder, "Ensemble/", Ensemble)
  
  ETS <- list.files(paste0(folder, "ETS/"), pattern = "Pred_ETS_.*\\.csv$", recursive = T, full.names = F)
  ETS <- paste0(folder, "ETS/", ETS)
  
  Media <- list.files(paste0(folder, "Media/"), pattern = "Pred_Media_.*\\.csv$", recursive = T, full.names = F)
  Media <- paste0(folder, "Media/", Media)
  
  Naive <- list.files(paste0(folder, "Naive/"), pattern = "Pred_Naive_.*\\.csv$", recursive = T, full.names = F)
  Naive <- paste0(folder, "Naive/", Naive)
  
  NN <- list.files(paste0(folder, "NN/"), pattern = "Pred_NN_.*\\.csv$", recursive = T, full.names = F)
  NN <- paste0(folder, "NN/", NN)
  
  SN <- list.files(paste0(folder, "SN/"), pattern = "Pred_SN_.*\\.csv$", recursive = T, full.names = F)
  SN <- paste0(folder, "SN/", SN)
  
  SVM <- list.files(paste0(folder, "SVM/"), pattern = "Pred_SVM_.*\\.csv$", recursive = T, full.names = F)
  SVM <- paste0(folder, "SVM/", SVM)
  
  
}


#LEE ARCHVOS, HACE GRAFICOS Y LOS GUARDA EN FORMATO PNG
{
  
  # Definir el folder principal
  folder <- "Resultados/PrediccionError/AllFeats/"
  
  # Lista de los nombres de los submodelos para búsqueda
  modelos <- c("Arima", "Ensemble", "ETS", "Media", "Naive", "NN", "SN", "SVM")
  
  # Lista de métodos de predicción
  metodos_prediccion <- c("gbm", "lm", "nn", "rf", "svm")
  
  # Lista de conjuntos de features
  features_sets <- c("cluster", "consumo", "edificio", "habitos", "socio")
  
  # Función para leer y combinar archivos de un modelo específico
  read_and_combine <- function(modelo, metodo, feature) {
    pattern <- paste0("Pred_", modelo, "_", metodo, "_", feature, ".csv$")
    files <- list.files(paste0(folder, modelo), pattern = pattern, full.names = TRUE)
    
    # Lista para almacenar data frames
    df_list <- list()
    
    # Leer archivos y extraer métrica MAPE
    for (file in files) {
      df <- fread(file) # Leer el archivo con data.table por eficiencia
      # Crear una columna con el valor de MAPE
      mape_colname <- grep("MAPE", names(df), value = TRUE)
      df <- df[, .(ID, Real, Predicted = df[[mape_colname]], MAPE = df[[mape_colname]])]
      df$feature_set <- feature # Añadir el conjunto de características como una nueva columna
      df$metodo <- metodo # Añadir el método de predicción como una nueva columna
      df$modelo <- modelo # Añadir el modelo como una nueva columna
      df_list[[length(df_list) + 1]] <- df
    }
    
    # Combinar todos los data frames en uno solo
    combined_df <- rbindlist(df_list, fill = TRUE)
    return(combined_df)
  }
  
  # Lista para almacenar los datos combinados de todos los modelos y métodos
  combined_data <- list()
  
  # Leer y combinar los datos de todos los modelos y métodos
  for (modelo in modelos) {
    for (metodo in metodos_prediccion) {
      for (feature in features_sets) {
        combined_data[[paste(modelo, metodo, feature, sep = "_")]] <- read_and_combine(modelo, metodo, feature)
      }
    }
  }
  
  # Función para generar gráfico de caja
  generate_boxplot <- function(data, title) {
    # Calcular el cuartil 0.75 para cada conjunto de features y método
    limits <- data[, .(Q75 = quantile(MAPE, 0.75, na.rm = TRUE)), by = .(feature_set, metodo)]
    
    # Unir los límites con el conjunto de datos original para filtrar los outliers
    data <- merge(data, limits, by = c("feature_set", "metodo"))
    data <- data[MAPE <= Q75]
    
    # Generar el gráfico de caja sin los outliers
    p <- ggplot(data, aes(x = feature_set, y = MAPE, fill = metodo)) +
      geom_boxplot(outlier.shape = NA) + # Excluir puntos atípicos
      labs(title = title, x = "Conjunto de Features", y = "MAPE") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    return(p)
  }
  
  # Generar y guardar gráficos para cada modelo
  for (modelo in modelos) {
    # Filtrar los datos para el modelo actual
    datos_modelo <- rbindlist(combined_data[grep(modelo, names(combined_data))], fill = TRUE)
    
    # Generar el gráfico
    p <- generate_boxplot(datos_modelo, paste("MAPE para modelo", modelo))
    
    # Guardar el gráfico
    ggsave(paste0("MAPE_", modelo, "_por_conjunto_de_features.png"), plot = p, width = 11, height = 8, dpi = 300)
  }

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



#CLASIFICACION
{
  gbm <- fread("Resultados/PrediccionClasificacion/Clasif_gbm.csv")
  logistic <- fread("Resultados/PrediccionClasificacion/Clasif_logistic.csv")
  rf <- fread("Resultados/PrediccionClasificacion/Clasif_rf.csv")
  svm <- fread("Resultados/PrediccionClasificacion/Clasif_svm.csv")
  
  gbmCuest <- fread("Resultados/PrediccionClasificacion/Cuest/Cuest_Clasif_gbm.csv")
  logisticCuest <- fread("Resultados/PrediccionClasificacion/Cuest/Cuest_Clasif_logistic.csv")
  rfCuest <- fread("Resultados/PrediccionClasificacion/Cuest/Cuest_Clasif_rf.csv")
  svmCuest <- fread("Resultados/PrediccionClasificacion/Cuest/Cuest_Clasif_svm.csv")
}

#EJEMPLO CON UN SOLO DATASET
# Seleccionar solo las columnas de Error Rates
indices <- logisticCuest[, (grep("ErrorRate_", names(logisticCuest)))]
error_rates <- logisticCuest[,..indices]
# Convertir de formato ancho a largo para facilitar el gráfico con ggplot
error_rates_long <- reshape2::melt(error_rates)

# Generar el gráfico de bigotes
ggplot(error_rates_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.shape = NA) + # Excluir outliers con outlier.shape = NA si es necesario
  labs(title = "Boxplot de Error Rates para Clasificación Logística",
       x = "Método de Clasificación",
       y = "Error Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

