library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


# Leer los datos desde el archivo CSV
datos <- fread("Resultados/pBarras.csv")

# Función para generar gráficos de bigotes
generar_grafico_pBarra <- function(data, tipo_modelo) {
  # Definimos las columnas basadas en el tipo de modelo
  columnas <- switch(tipo_modelo,
                     "lm" = c("PBarra_lm_habitos", "PBarra_lm_cluster", "PBarra_lm_edificio", "PBarra_lm_socio", "PBarra_lm_consumo", "PBarra_lm_tarifa"),
                     "rf" = c("PBarra_rf_habitos", "PBarra_rf_cluster", "PBarra_rf_edificio", "PBarra_rf_socio", "PBarra_rf_consumo", "PBarra_rf_tarifa"),
                     "gbm" = c("PBarra_gbm_habitos", "PBarra_gbm_cluster", "PBarra_gbm_edificio", "PBarra_gbm_socio", "PBarra_gbm_consumo", "PBarra_gbm_tarifa"),
                     "nn" = c("PBarra_nn_habitos", "PBarra_nn_cluster", "PBarra_nn_edificio", "PBarra_nn_socio", "PBarra_nn_consumo", "PBarra_nn_tarifa"),
                     "svm" = c("PBarra_svm_habitos", "PBarra_svm_cluster", "PBarra_svm_edificio", "PBarra_svm_socio", "PBarra_svm_consumo", "PBarra_svm_tarifa"),
                     stop("Tipo de modelo no reconocido"))
  
  #data_filtrado <- data %>%
  #  mutate(across(all_of(columnas), ~ifelse(. > quantile(., 0.75, na.rm = TRUE), NA, .)))
  
  # Preparar los datos para el gráfico
  print(data$pBarra_habitos_lm)
  data_melt <- reshape2::melt(data, measure.vars = columnas, na.rm = T)
  
  # Generar el gráfico
  ggplot(data_melt, aes(x = variable, y = value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("PBarra del modelo", tipo_modelo),
         x = "Variable",
         y = "Valor")
}

generar_grafico_pBarra(datos, "lm")
generar_grafico_pBarra(datos, "rf")
generar_grafico_pBarra(datos, "gbm")
generar_grafico_pBarra(datos, "nn")
generar_grafico_pBarra(datos, "svm")

#DATOS MAPE
datosMAPE <- fread("Resultados/pBarrasMAPE.csv")

# Función para generar gráficos de bigotes
generar_grafico_pBarra <- function(data, tipo_modelo) {
  # Definimos las columnas basadas en el tipo de modelo
  columnas <- switch(tipo_modelo,
                     "lm" = c("PBarra_lm_habitos_MAPE", "PBarra_lm_cluster_MAPE", "PBarra_lm_edificio_MAPE", "PBarra_lm_socio_MAPE", "PBarra_lm_consumo_MAPE", "PBarra_lm_tarifa_MAPE"),
                     "rf" = c("PBarra_rf_habitos_MAPE", "PBarra_rf_cluster_MAPE", "PBarra_rf_edificio_MAPE", "PBarra_rf_socio_MAPE", "PBarra_rf_consumo_MAPE", "PBarra_rf_tarifa_MAPE"),
                     "gbm" = c("PBarra_gbm_habitos_MAPE", "PBarra_gbm_cluster_MAPE", "PBarra_gbm_edificio_MAPE", "PBarra_gbm_socio_MAPE", "PBarra_gbm_consumo_MAPE", "PBarra_gbm_tarifa_MAPE"),
                     "nn" = c("PBarra_nn_habitos_MAPE", "PBarra_nn_cluster_MAPE", "PBarra_nn_edificio_MAPE", "PBarra_nn_socio_MAPE", "PBarra_nn_consumo_MAPE", "PBarra_nn_tarifa_MAPE"),
                     "svm" = c("PBarra_svm_habitos_MAPE", "PBarra_svm_cluster_MAPE", "PBarra_svm_edificio_MAPE", "PBarra_svm_socio_MAPE", "PBarra_svm_consumo_MAPE", "PBarra_svm_tarifa_MAPE"),
                     "Ensemble" = c("PBarra_Ensemble_habitos_MAPE", "PBarra_Ensemble_cluster_MAPE", "PBarra_Ensemble_edificio_MAPE", "PBarra_Ensemble_socio_MAPE", "PBarra_Ensemble_consumo_MAPE", "PBarra_Ensemble_tarifa_MAPE"),
                     stop("Tipo de modelo no reconocido"))
  
  data_filtrado <- data %>%
    mutate(across(all_of(columnas), ~ifelse(. > quantile(., 0.75, na.rm = TRUE), NA, .)))
  
  # Preparar los datos para el gráfico
  #print(data_filtrado$pBarra_habitos_lm)
  data_melt <- reshape2::melt(data_filtrado, measure.vars = columnas, na.rm = T)
  
  # Generar el gráfico
  ggplot(data_melt, aes(x = variable, y = value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("PBarra del modelo", tipo_modelo),
         x = "Variable",
         y = "Valor")
}


generar_grafico_pBarra(datosMAPE, "lm")
generar_grafico_pBarra(datosMAPE, "rf")
generar_grafico_pBarra(datosMAPE, "gbm")
generar_grafico_pBarra(datosMAPE, "nn")
generar_grafico_pBarra(datosMAPE, "svm")
generar_grafico_pBarra(datosMAPE, "Ensemble")



filtrarDataFrameMAPE <- function(data, modelo) {

  features <- c("habitos", "cluster", "edificio", "socio", "consumo", "tarifa")
  metodos <- c("lm", "rf", "gbm", "nn", "svm")
  # Inicializar un vector vacío para almacenar los nombres de las columnas seleccionadas
  columnasSeleccionadas <- c()
  
    # Recorrer cada palabra clave
    for (feature in features) {
      for (metodo in metodos) {
        columna <- paste0("mapePbarra_", feature, "_", metodo, "_", modelo)
        columnasSeleccionadas <- c(columnasSeleccionadas, columna)
      }
  }
  
  
  return(data[, columnasSeleccionadas])
}

data_Media <- filtrarDataFrameMAPE(datos, "Media")
data_Naive <- filtrarDataFrameMAPE(datos, "Naive")
data_SN <- filtrarDataFrameMAPE(datos, "SN")
data_Arima <- filtrarDataFrameMAPE(datos, "Arima")
data_ETS <- filtrarDataFrameMAPE(datos, "ETS")
data_NN <- filtrarDataFrameMAPE(datos, "NN")
data_SVM <- filtrarDataFrameMAPE(datos, "SVM")
data_Ensemble <- filtrarDataFrameMAPE(datos, "Ensemble")


library(ggplot2)
library(reshape2)

generarGraficosPorMetodo <- function(data, metodo, modelo) {

  columnas_metodo <- grep(paste0(".*_", metodo, "_.*"), names(data), value = TRUE)
  
  data_largo <- melt(data, measure.vars = columnas_metodo)
  
  data_filtrado <- data_largo %>%
    group_by(variable) %>%
    filter(value <= quantile(value, 0.75, na.rm = T))
  
  # Generar gráfico de bigotes
  p <- ggplot(data_filtrado, aes(x = variable, y = value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + # Ajustar etiquetas del eje X
    labs(x = "Variable", y = "Valor", title = paste("MAPE del pBarra para el método", metodo, "del modelo", modelo)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) # Evitar superposición de etiquetas si son muchas
  
  # Guardar el gráfico como imagen
  # El nombre del archivo incluye el método para evitar sobrescrituras
  nombre_archivo <- paste("grafico_bigotes_", metodo, "_",modelo ,".png", sep = "")
  ggsave(nombre_archivo, plot = p, width = 10, height = 8, dpi = 300)
}

metodos <- c("lm", "rf", "gbm", "nn", "svm")
for (metodo in metodos) {
  generarGraficosPorMetodo(data_Media, metodo, "Media")
}

for (metodo in metodos) {
  generarGraficosPorMetodo(data_Naive, metodo, "Naive")
}

for (metodo in metodos) {
  generarGraficosPorMetodo(data_SN, metodo, "SN")
}

for (metodo in metodos) {
  generarGraficosPorMetodo(data_Arima, metodo, "Arima")
}

for (metodo in metodos) {
  generarGraficosPorMetodo(data_ETS, metodo, "ETS")
}

for (metodo in metodos) {
  generarGraficosPorMetodo(data_NN, metodo, "NN")
}


for (metodo in metodos) {
  generarGraficosPorMetodo(data_SVM, metodo, "SVM")
}

for (metodo in metodos) {
  generarGraficosPorMetodo(data_Ensemble, metodo, "Ensemble")
}


