library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS",  
               'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


############ GRAFICOS INDIVIDUALES #############

generar_grafico_pBarraMAPE <- function(data, tipo_modelo) {
  # Definimos las columnas basadas en el tipo de modelo
  columnas <- switch(tipo_modelo,
                     "lm" = c("PBarra_lm_tarifa_MAPE"),
                     "rf" = c("PBarra_rf_tarifa_MAPE"),
                     "gbm" = c("PBarra_gbm_tarifa_MAPE"),
                     "nn" = c("PBarra_nn_tarifa_MAPE"),
                     "svm" = c("PBarra_svm_tarifa_MAPE"),
                     "Ensemble" = c("PBarra_Ensemble_tarifa_MAPE"),
                     "RealError" = c("PBarra_errorMape_MAPE"),
                     stop("Tipo de modelo no reconocido"))
  
  # Filtrar los datos utilizando el criterio Q3 + 1.5 * IQR para los outliers
  data_filtrado <- data %>%
    mutate(across(all_of(columnas), function(x) {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      upper_limit <- Q3 + 1.5 * IQR
      lower_limit <- Q1 - 1.5 * IQR
      ifelse(x > upper_limit | x < lower_limit, NA, x)
    }))
  
  # Preparar los datos para el gráfico
  data_melt <- reshape2::melt(data_filtrado, measure.vars = columnas, na.rm = TRUE)
  
  # Generar el gráfico
  ggplot(data_melt, aes(x = variable, y = value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("PBarra del modelo", tipo_modelo),
         x = "Variable",
         y = "Valor")
}


datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv")
# Llamada a la función con diferentes tipos de modelo
generar_grafico_pBarraMAPE(datosMAPE, "lm")
generar_grafico_pBarraMAPE(datosMAPE, "rf")
generar_grafico_pBarraMAPE(datosMAPE, "gbm")
generar_grafico_pBarraMAPE(datosMAPE, "nn")
generar_grafico_pBarraMAPE(datosMAPE, "svm")
generar_grafico_pBarraMAPE(datosMAPE, "Ensemble")
generar_grafico_pBarraMAPE(datosMAPE, "RealError")


######################################


# Leer los datos
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv")
allFeats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE_reducido.csv")

# Combinar y transformar los datos a formato largo
combined_long <- bind_rows(
  datosMAPE %>%
    select(contains("MAPE")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE"),
  allFeats %>%
    select(contains("_mape")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE")
)

# Filtrar variables innecesarias
combined_long <- filter(combined_long, !Variable %in% c( "PBarra_errorMape"))

# Calcular Q1, Q3 y IQR para cada variable y filtrar outliers
combined_long <- combined_long %>%
  group_by(Variable) %>%
  mutate(
    Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_limit = Q3 + 1.5 * IQR,
    lower_limit = Q1 - 1.5 * IQR
  ) %>%
  filter(MAPE <= upper_limit & MAPE >= lower_limit) %>%
  ungroup()

# Calcular las medianas
medianas <- combined_long %>%
  group_by(Variable) %>%
  summarize(Mediana = median(MAPE, na.rm = TRUE)) %>%
  arrange(Mediana)

# Seleccionar las variables con la mediana más baja
variables_baja_mediana <- head(medianas$Variable, 5)

# Asignar colores en función de las variables con mediana más baja
combined_long$Color <- ifelse(combined_long$Variable %in% variables_baja_mediana, "Baja Mediana", "Otro")

# Generar el gráfico
ggplot(combined_long, aes(x = Variable, y = MAPE, fill = Color)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Baja Mediana" = "#5387E3", "Otro" = "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "MAPE - Análisis de Variables", x = "", y = "MAPE") +
  guides(fill = FALSE) # Para no mostrar la leyenda


################################################

# Leer los datos
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/FFORMA_MAPE.csv")
allFeats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

# Seleccionar las columnas relevantes
datosMAPE <- datosMAPE %>% select(contains("_MAPE"), contains("_mape"))


# Combinar y transformar los datos a formato largo
combined_long <- bind_rows(
  datosMAPE %>%
    select(contains("_MAPE"), contains("_mape")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE")
)


# Calcular Q1, Q3, IQR y filtrar outliers
combined_long <- combined_long %>%
  group_by(Variable) %>%
  mutate(
    Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_limit = Q3 + 1.5 * IQR,
    lower_limit = Q1 - 1.5 * IQR
  ) %>%
  filter(MAPE <= upper_limit & MAPE >= lower_limit) %>%
  ungroup()

# Calcular las medianas
medianas <- combined_long %>%
  group_by(Variable) %>%
  summarize(Mediana = median(MAPE, na.rm = TRUE)) %>%
  arrange(Mediana)

# Seleccionar la variable con la mediana más baja
variables_baja_mediana <- head(medianas$Variable, 1)

# Asignar colores en función de la variable con la mediana más baja
combined_long$Color <- ifelse(combined_long$Variable %in% variables_baja_mediana, "Baja Mediana", "Otro")

# Generar el gráfico
ggplot(combined_long, aes(x = Variable, y = MAPE, fill = Color)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)), 
               vjust = -0.5, color = "black", size = 3.5) +  # Etiquetas con la mediana
  scale_fill_manual(values = c("Baja Mediana" = "#5387E3", "Otro" = "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "MAPE - Análisis de Variables Ensemble", x = "", y = "MAPE") +
  guides(fill = none()) 


#Q3 + 1.5*(Q3-Q1)
############################################

# Leer los datos
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv")
allFeats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

# Combinar y transformar los datos a formato largo
combined_long <- bind_rows(
  datosMAPE %>%
    select(contains("tarifa_MAPE")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE"),
  allFeats %>%
    select(contains("_mape")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE")
)

# Filtrar variables innecesarias
combined_long <- filter(combined_long, Variable != "V1_error")
combined_long <- filter(combined_long, Variable != "PBarra_errorMape")

# Verificar que tenemos datos
print(paste("Total de filas en combined_long:", nrow(combined_long)))
print(paste("Variables únicas:", paste(unique(combined_long$Variable), collapse=", ")))

# Iniciar el archivo PDF para los gráficos y tablas
pdf("NuevosResultados/FFORMA/MAPE_PBarra_Boxplots.pdf", width = 11, height = 8.5)

# Definir los grupos de modelos - asegurarse de que los patrones coincidan con tus datos
model_groups <- list(
  tarifa = grep("tarifa_MAPE", combined_long$Variable, value = TRUE),
  mediana = grep("_mape", combined_long$Variable, value = TRUE)
)

# Verificar los grupos
print("Grupos identificados:")
print(model_groups)

# Iterar sobre cada grupo
for (group_name in names(model_groups)) {
  print(paste("Procesando grupo:", group_name))
  
  # Filtrar y calcular cuartiles para eliminar outliers
  vars_subset_for_plot <- combined_long %>%
    filter(Variable %in% model_groups[[group_name]]) %>%
    group_by(Variable) %>%
    mutate(
      Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
      Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      upper_limit = Q3 + 1.5 * IQR,
      lower_limit = Q1 - 1.5 * IQR
    ) %>%
    filter(MAPE <= upper_limit & MAPE >= lower_limit) %>%
    ungroup()
  
  print(paste("Filas después de filtrar outliers:", nrow(vars_subset_for_plot)))
  
  # Calcular resumen estadístico
  vars_subset_for_summary <- vars_subset_for_plot %>%
    group_by(Variable) %>%
    summarise(
      Min = min(MAPE, na.rm = TRUE), 
      Q1 = quantile(MAPE, 0.25, na.rm = TRUE), 
      Median = median(MAPE, na.rm = TRUE), 
      Mean = mean(MAPE, na.rm = TRUE), 
      Q3 = quantile(MAPE, 0.75, na.rm = TRUE), 
      Max = max(MAPE, na.rm = TRUE), 
      .groups = 'drop'
    )
  
  # Identificar la fila con la mediana más baja
  lowest_median_row <- which.min(vars_subset_for_summary$Median)
  print(paste("Fila con mediana más baja:", lowest_median_row))
  
  # Generar gráfico de boxplot si hay datos
  if (nrow(vars_subset_for_plot) > 0) {
    p <- ggplot(vars_subset_for_plot, aes(x = Variable, y = MAPE)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("MAPE -", group_name), x = "", y = "MAPE")
    
    # Imprimir explícitamente en el dispositivo PDF
    print(p)
  } else {
    print(paste("No hay datos para el grupo", group_name, "después de filtrar outliers"))
  }
  
  # Preparar los datos de la tabla para imprimir
  # Convertir a dataframe normal para mejor manipulación
  summary_df <- as.data.frame(vars_subset_for_summary)
  
  # Formatea los números para mejor visualización
  formatted_summary <- summary_df
  numeric_cols <- sapply(formatted_summary, is.numeric)
  formatted_summary[, numeric_cols] <- round(formatted_summary[, numeric_cols], 4)
  
  # Generar tabla con grid.table (más estable que tableGrob)
  grid.newpage()
  
  # Agregar título a la tabla
  grid.text(paste("Resumen estadístico -", group_name), 
            x = 0.5, y = 0.9, 
            gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Crear tabla usando grid.table
  grid.table(formatted_summary, 
             rows = NULL,
             theme = ttheme_minimal(
               core = list(
                 bg_params = list(
                   fill = c(rep("white", lowest_median_row - 1), "pink", 
                            rep("white", nrow(formatted_summary) - lowest_median_row))
                 ),
                 fg_params = list(fontface = "plain")
               ),
               colhead = list(fg_params = list(fontface = "bold"))
             ))
  
  # Añadir una nueva página para el siguiente grupo si no es el último
  if (group_name != tail(names(model_groups), n = 1)) {
    grid.newpage()
  }
}

# Cerrar el archivo PDF
dev.off()
print("PDF generado correctamente")

### CREAR DATOS FINALES ###
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv")
allFeats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

datosMAPE <- datosMAPE %>% select(ID, Real, contains("PBarra_"))
allFeats$ID <- allFeats$id
allFeats$Real <- allFeats$real
allFeats <- allFeats %>% select(ID, dia, hora, Real, contains("_pred"), contains("_mape"))

datosMAPE[, index := 1:.N, by = .(ID, Real)]
allFeats[, index := 1:.N, by = .(ID, Real)]
df_merged <- merge(datosMAPE, allFeats, by = c("ID", "Real", "index"), all = FALSE)
df_merged <- df_merged %>% select(-index)
fwrite(df_merged, "NuevosResultados/FFORMA/modelosFINALES.csv")

combined_long <- bind_rows(
  df_merged %>%
    select(contains("MAPE"), contains("_mape")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MAPE")
)

combined_long <- combined_long %>%
  group_by(Variable) %>%
  mutate(
    Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_limit = Q3 + 1.5 * IQR,
    lower_limit = Q1 - 1.5 * IQR
  ) %>%
  filter(MAPE <= upper_limit & MAPE >= lower_limit) %>%
  ungroup()

medianas <- combined_long %>%
  group_by(Variable) %>%
  summarize(Mediana = median(MAPE, na.rm = TRUE)) %>%
  arrange(Mediana)

variables_baja_mediana <- head(medianas$Variable, 5)
combined_long$Color <- ifelse(combined_long$Variable %in% variables_baja_mediana, "Baja Mediana", "Otro")

ggplot(combined_long, aes(x = Variable, y = MAPE, fill = Color)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Baja Mediana" = "#5387E3", "Otro" = "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "MAPE - Análisis de Variables", x = "", y = "MAPE") +
  guides(fill = FALSE) 


### GRAFICO DE LINEAS ###
df_merged[, tiempo := paste0(dia, ":", hora)]
df_long <- melt(df_merged, id.vars = c("tiempo"), 
                measure.vars = c("Real", "PBarra_lm_tarifa", "PBarra_rf_tarifa", 
                                 "PBarra_gbm_tarifa", "PBarra_nn_tarifa", 
                                 "PBarra_svm_tarifa", "PBarra_Ensemble_tarifa",
                                 "mean_pred", "rw_pred", "naive_pred", 
                                 "simple_pred", "lr_pred", "ann_pred", 
                                 "svm_pred", "arima_pred", "ses_pred", "ens_pred"),
                variable.name = "Modelo", value.name = "Valor")
ggplot(df_long, aes(x = tiempo, y = Valor, color = Modelo, group = Modelo)) +
  geom_line() +  
  labs(title = "Comparación de Modelos vs Valor Real",
       x = "Día:Hora",
       y = "Predicción / Real",
       color = "Modelos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


df_merged <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/FFORMA_MAPE.csv")

datos_media <- df_merged %>%
  group_by(dia, hora) %>%
  summarise(
    Real = mean(Real, na.rm = TRUE),
    FFORMA_lm = mean(FFORMA_lm_tarifa, na.rm = TRUE),  
    FFORMA_rf = mean(FFORMA_rf_tarifa, na.rm = TRUE),  
    FFORMA_gbm = mean(FFORMA_gbm_tarifa, na.rm = TRUE), 
    FFORMA_nn = mean(FFORMA_nn_tarifa, na.rm = TRUE),  
    FFORMA_svm = mean(FFORMA_svm_tarifa, na.rm = TRUE), 
    FFORMA_Ensemble = mean(FFORMA_Ensemble_tarifa, na.rm = TRUE),  
    mean_pred = mean(mean_pred, na.rm = TRUE),
    rw_pred = mean(rw_pred, na.rm = TRUE),
    naive_pred = mean(naive_pred, na.rm = TRUE),
    simple_pred = mean(simple_pred, na.rm = TRUE),
    lr_pred = mean(lr_pred, na.rm = TRUE),
    ann_pred = mean(ann_pred, na.rm = TRUE),
    svm_pred = mean(svm_pred, na.rm = TRUE),
    arima_pred = mean(arima_pred, na.rm = TRUE),
    ses_pred = mean(ses_pred, na.rm = TRUE),
    ens_pred = mean(ens_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_media %>%
  pivot_longer(cols = c(ends_with("_pred"), contains("FFORMA")), 
               names_to = "Modelo", 
               values_to = "Prediccion")

pdf("NuevosResultados/FFORMA/modelos_finales_prediccion_media.pdf", width = 10, height = 6)

for (d in unique(datos_grafico$dia)) {
  
  datos_dia <- datos_grafico %>% filter(dia == d)
  
  p <- ggplot(datos_dia, aes(x = hora, y = Prediccion, color = Modelo, group = Modelo)) +
    geom_line() +
    geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
    labs(title = paste("Predicciones Promedio vs Real - Día", d),
         x = "Hora",
         y = "Consumo Promedio (media) (kWh)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Mejor visibilidad del eje X
  
  print(p)
}

dev.off()

datos_mediana <- df_merged %>%
  group_by(dia, hora) %>%
  summarise(
    Real = median(Real, na.rm = TRUE),
    FFORMA_lm = median(FFORMA_lm_tarifa, na.rm = TRUE),  
    FFORMA_rf = median(FFORMA_rf_tarifa, na.rm = TRUE),  
    FFORMA_gbm = median(FFORMA_gbm_tarifa, na.rm = TRUE), 
    FFORMA_nn = median(FFORMA_nn_tarifa, na.rm = TRUE),  
    FFORMA_svm = median(FFORMA_svm_tarifa, na.rm = TRUE), 
    FFORMA_Ensemble = median(FFORMA_Ensemble_tarifa, na.rm = TRUE),  
    mean_pred = median(mean_pred, na.rm = TRUE),
    rw_pred = median(rw_pred, na.rm = TRUE),
    naive_pred = median(naive_pred, na.rm = TRUE),
    simple_pred = median(simple_pred, na.rm = TRUE),
    lr_pred = median(lr_pred, na.rm = TRUE),
    ann_pred = median(ann_pred, na.rm = TRUE),
    svm_pred = median(svm_pred, na.rm = TRUE),
    arima_pred = median(arima_pred, na.rm = TRUE),
    ses_pred = median(ses_pred, na.rm = TRUE),
    ens_pred = median(ens_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_mediana %>%
  pivot_longer(cols = c(ends_with("_pred"), contains("FFORMA")), 
               names_to = "Modelo", 
               values_to = "Prediccion")

pdf("NuevosResultados/FFORMA/modelos_finales_prediccion_mediana.pdf", width = 10, height = 6)

for (d in unique(datos_grafico$dia)) {
  
  datos_dia <- datos_grafico %>% filter(dia == d)
  
  p <- ggplot(datos_dia, aes(x = hora, y = Prediccion, color = Modelo, group = Modelo)) +
    geom_line() +
    geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
    labs(title = paste("Predicciones Mediana vs Real - Día", d),
         x = "Hora",
         y = "Consumo Promedio (mediana) (kWh)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Mejor visibilidad del eje X
  
  print(p)
}

dev.off()


########## tabla resultados ############
cols_mape <- grep("(_mape|_MAPE)$", names(df_merged), value = TRUE)
resumen_list <- lapply(cols_mape, function(col) {
  valores <- df_merged[[col]]
  q1 <- quantile(valores, 0.25, na.rm = TRUE)
  q3 <- quantile(valores, 0.75, na.rm = TRUE)
  limite_sup <- q3 + 1.5 * (q3 - q1)
  valores_filtrados <- valores[valores <= limite_sup]
  data.table(
    variable = col,
    mean = mean(valores_filtrados, na.rm = TRUE),
    sd = sd(valores_filtrados, na.rm = TRUE),
    p0 = quantile(valores_filtrados, 0, na.rm = TRUE),
    p25 = quantile(valores_filtrados, 0.25, na.rm = TRUE),
    median = quantile(valores_filtrados, 0.5, na.rm = TRUE),
    p75 = quantile(valores_filtrados, 0.75, na.rm = TRUE),
    p100 = quantile(valores_filtrados, 1, na.rm = TRUE),
    outliers_eliminados = sum(valores > limite_sup, na.rm = TRUE)
  )
})
summary_modelosFINALES <- rbindlist(resumen_list)
fwrite(summary_modelosFINALES, "NuevosResultados/FFORMA/summary_modelosFINALES.csv")
print(summary_modelosFINALES)


#######

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


