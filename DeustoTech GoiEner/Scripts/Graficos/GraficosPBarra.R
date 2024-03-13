library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable") 

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
generar_grafico_pBarraMAPE <- function(data, tipo_modelo) {
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


generar_grafico_pBarraMAPE(datosMAPE, "lm")
generar_grafico_pBarraMAPE(datosMAPE, "rf")
generar_grafico_pBarraMAPE(datosMAPE, "gbm")
generar_grafico_pBarraMAPE(datosMAPE, "nn")
generar_grafico_pBarraMAPE(datosMAPE, "svm")
generar_grafico_pBarraMAPE(datosMAPE, "Ensemble")


#GRAFICOS CON MAPES NORMALES Y MAPES DEL PBARRA
allFeats <- fread("allFeats.csv")

datosMAPE_long <- datosMAPE %>%
  select(contains("MAPE")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE")

allFeats_long <- allFeats %>%
  select(contains("mape") & ends_with("mediana")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE")

combined_long <- bind_rows(datosMAPE_long, allFeats_long)

combined_long_filtered <- combined_long %>%
  group_by(Variable) %>%
  mutate(Q3 = quantile(MAPE, 0.75, na.rm = T)) %>%
  filter(MAPE <= Q3) %>%
  ungroup()

total_vars <- n_distinct(combined_long_filtered$Variable)
plots_needed <- ceiling(total_vars / 6)

# Iniciar el archivo PDF
pdf("MAPE_PBarra_Boxplots.pdf", width = 11, height = 8.5)

for (i in 1:plots_needed) {
  vars_subset <- combined_long_filtered %>%
    filter(Variable %in% unique(combined_long_filtered$Variable)[(6*(i-1) + 1):(6*i)])
  
  # Generar y mostrar boxplot
  p <- ggplot(vars_subset, aes(x = Variable, y = MAPE)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Distribución de MAPE hasta el 3er Cuartil - Grupo", i), x = "", y = "MAPE")
  
  print(p)
  
  # Asegura una nueva página en el PDF para la tabla
  grid.newpage()
  
  # Calcular y mostrar tabla de resumen
  summary_table <- vars_subset %>%
    group_by(Variable) %>%
    summarise(Min = min(MAPE), Q1 = quantile(MAPE, 0.25), Median = median(MAPE), Mean = mean(MAPE), 
              Q3 = quantile(MAPE, 0.75), Max = max(MAPE), .groups = 'drop')
  
  # Mostrar tabla de resumen
  grid.table(summary_table)
}

# Cerrar el archivo PDF
dev.off()




combined_long <- bind_rows(
  datosMAPE %>%
    select(contains("MAPE")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE"),
  allFeats %>%
    select(contains("mape") & contains("_mediana")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE")
)


# Iniciar el archivo PDF para los gráficos y tablas
pdf("MAPE_PBarra_Boxplots.pdf", width = 11, height = 8.5)

# Definir los grupos de modelos
model_groups <- list(
  lm = grep("lm", combined_long$Variable, value = TRUE),
  rf = grep("rf", combined_long$Variable, value = TRUE),
  gbm = grep("gbm", combined_long$Variable, value = TRUE),
  nn = grep("nn", combined_long$Variable, value = TRUE),
  svm = grep("svm", combined_long$Variable, value = TRUE),
  ensemble = grep("Ensemble", combined_long$Variable, value = TRUE),
  mediana = grep("mape.*(Media|Naive|SN|Arima|ETS|NN|SVM|Ensemble)_mediana", combined_long$Variable, value = TRUE)
)

for (group_name in names(model_groups)) {
  vars_subset_for_plot <- combined_long %>%
    filter(Variable %in% model_groups[[group_name]]) %>%
    mutate(Q3 = quantile(MAPE, 0.75, na.rm = TRUE)) %>%
    filter(MAPE <= Q3)
  
  vars_subset_for_summary <- vars_subset_for_plot %>%
    group_by(Variable) %>%
    summarise(Min = min(MAPE, na.rm = TRUE), Q1 = quantile(MAPE, 0.25, na.rm = TRUE), Median = median(MAPE, na.rm = TRUE), Mean = mean(MAPE, na.rm = TRUE), 
              Q3 = quantile(MAPE, 0.75, na.rm = TRUE), Max = max(MAPE, na.rm = TRUE), .groups = 'drop')
  
  # Identificar la fila con la mediana más baja
  lowest_median_row <- which.min(vars_subset_for_summary$Median)
  
  # Generar tabla con tableGrob
  table_grob <- tableGrob(vars_subset_for_summary, rows = NULL)
  
  # Colorear la fila con la mediana más baja
  table_grob <- gtable_add_grob(table_grob, 
                                list(rectGrob(gp = gpar(fill = "red", alpha = 0.5))), 
                                t = lowest_median_row + 1, # +1 porque la cabecera es la primera fila
                                l = 1,
                                b = lowest_median_row + 1,
                                r = ncol(vars_subset_for_summary))
  
  # Dibujar el boxplot
  if (nrow(vars_subset_for_plot) > 0) {
    p <- ggplot(vars_subset_for_plot, aes(x = Variable, y = MAPE)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("MAPE -", group_name), x = "", y = "MAPE")
    print(p)
  }
  
  # Nueva página para la tabla
  grid.newpage()
  grid.draw(table_grob)
  
  # Añadir una nueva página para el siguiente grupo si no es el último
  if (group_name != tail(names(model_groups), n = 1)) {
    grid.newpage()
  }
}

# Cerrar el archivo PDF
dev.off()






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


