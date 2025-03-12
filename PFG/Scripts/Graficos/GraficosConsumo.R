library(foreach)
library(doParallel)
librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

modelosBaseRed <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE_reducido.csv")
modelosFforma <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv")
modelosBaseRed$ID <- modelosBaseRed$id
modelosBaseRed$Real <- modelosBaseRed$real
modelosBaseRed <- modelosBaseRed %>% select(ID, Real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred)
modelosFforma <- modelosFforma %>% select(ID, Real, PBarra_lm_tarifa, PBarra_rf_tarifa, PBarra_gbm_tarifa, PBarra_nn_tarifa, PBarra_svm_tarifa, PBarra_Ensemble_tarifa, PBarra_errorMape)



modelosFforma_clean <- modelosFforma %>%
  distinct(ID, .keep_all = TRUE)
modelosBaseRed_clean <- modelosBaseRed %>%
  filter(ID %in% modelosFforma_clean$ID)
all(modelosBaseRed_clean$ID == modelosFforma_clean$ID)
all(modelosBaseRed_clean$Real == modelosFforma_clean$Real)

datos_grafico <- modelosBaseRed_clean %>%
  select(ID, Real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred) %>%
  pivot_longer(cols = -c(ID), names_to = "Modelo", values_to = "Prediccion") %>%
  bind_rows(
    modelosFforma_clean %>%
      select(ID, Real, PBarra_lm_tarifa, PBarra_rf_tarifa, PBarra_gbm_tarifa, PBarra_nn_tarifa, PBarra_svm_tarifa, PBarra_Ensemble_tarifa) %>%
      pivot_longer(cols = -c(ID, Real), names_to = "Modelo", values_to = "Prediccion")
  )

primeros_ids <- head(modelosBaseRed_clean$ID, 2)
datos_filtrados <- datos_grafico %>%
  filter(ID %in% primeros_ids)

ggplot(datos_filtrados, aes(x = ID, y = Prediccion, color = Modelo, group = Modelo)) +
  geom_line() +
  geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +
  labs(title = "Comparación de Predicciones vs Real (Primeros 5 IDs)",
       x = "ID",
       y = "Consumo (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


modelosBase <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

id_seleccionado <- "002ed9a37fb8cf35ccd34afffb48add82354efd8a61c615b3c0f08b33c3404d8"

# Preparar los datos combinando día y hora como texto
datos_filtrados <- modelosBase %>%
  filter(id == id_seleccionado, dia == 1) %>%
  arrange(hora) %>%  # Ordenar por hora
  mutate(
    dia_hora = paste0(dia, ".", hora),
    dia_hora = factor(dia_hora, levels = unique(dia_hora)) # Mantener el orden en el gráfico
  )

# Convertir a formato largo para graficar múltiples modelos
datos_grafico <- datos_filtrados %>%
  pivot_longer(cols = ends_with("_pred"), names_to = "Modelo", values_to = "Prediccion")

# Crear el gráfico
ggplot(datos_grafico, aes(x = dia_hora, y = Prediccion, color = Modelo, group = Modelo)) +
  geom_line() +
  geom_line(aes(y = real, group = 1), color = "black", linetype = "dashed") + # Línea de datos reales
  labs(title = paste("Predicciones vs Real para ID:", id_seleccionado),
       x = "Día,Hora",
       y = "Consumo (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Gira el texto para mejor visibilidad





datos_media <- modelosBase %>%
  group_by(dia, hora) %>%
  summarise(
    real = mean(real, na.rm = TRUE),
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
  mutate(
    dia_hora = paste0(dia, ".", hora),
    dia_hora = factor(dia_hora, levels = unique(dia_hora))
  )


datos_grafico <- datos_media %>%
  pivot_longer(cols = ends_with("_pred"), names_to = "Modelo", values_to = "Prediccion")


ggplot(datos_grafico, aes(x = dia_hora, y = Prediccion, color = Modelo, group = Modelo)) +
  geom_line() +
  geom_line(aes(y = real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
  labs(title = "Predicciones Promedio vs Real para Todos los IDs",
       x = "Día, Hora",
       y = "Consumo Promedio (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 





datos_media <- modelosBase %>%
  group_by(dia, hora) %>%
  summarise(
    real = mean(real, na.rm = TRUE),
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
  mutate(hora = factor(hora, levels = 0:23))  # Asegurar el orden de las horas

datos_grafico <- datos_media %>%
  pivot_longer(cols = ends_with("_pred"), names_to = "Modelo", values_to = "Prediccion")

pdf("NuevosResultados/grafico_predicciones_por_dia.pdf", width = 10, height = 6)

for (d in unique(datos_grafico$dia)) {
  
  datos_dia <- datos_grafico %>% filter(dia == d)
  
  p <- ggplot(datos_dia, aes(x = hora, y = Prediccion, color = Modelo, group = Modelo)) +
    geom_line() +
    geom_line(aes(y = real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
    labs(title = paste("Predicciones Promedio vs Real - Día", d),
         x = "Hora",
         y = "Consumo Promedio (kWh)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Mejor visibilidad del eje X

  print(p)
}

dev.off()

