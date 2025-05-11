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

df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

datos_media <- df %>%
  group_by(dia, hora) %>%
  summarise(
    Real = mean(real, na.rm = TRUE),
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
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosEnsemble/modelos_baseyEns_prediccion_media.pdf", width = 10, height = 6)

for (d in unique(datos_grafico$dia)) {
  
  datos_dia <- datos_grafico %>% filter(dia == d)
  
  p <- ggplot(datos_dia, aes(x = hora, y = Prediction, color = Model, group = Model)) +
    geom_line() +
    geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
    labs(title = paste("Predictions (mean) vs Real - ", dias_semana[d]),
         x = "Hour",
         y = "Consumption in kWh (mean)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Mejor visibilidad del eje X
  
  print(p)
}

dev.off()


df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

datos_mediana <- df %>%
  group_by(dia, hora) %>%
  summarise(
    Real = median(real, na.rm = TRUE),
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
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosEnsemble/modelos_baseyEns_prediccion_mediana.pdf", width = 10, height = 6)

for (d in unique(datos_grafico$dia)) {
  
  datos_dia <- datos_grafico %>% filter(dia == d)
  
  p <- ggplot(datos_dia, aes(x = hora, y = Prediction, color = Model, group = Model)) +
    geom_line() +
    geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
    labs(title = paste("Predictions (median) vs Real - ", dias_semana[d]),
         x = "Hour",
         y = "Consumption in kWh (median) (kWh)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Mejor visibilidad del eje X
  
  print(p)
}

dev.off()


#boxplot del ensemble
df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

Q1 <- quantile(df$ens_mape, 0.25, na.rm = TRUE)
Q3 <- quantile(df$ens_mape, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

# Filtrar valores dentro del rango permitido
df_filtrado <- df %>%
  filter(ens_mape >= (Q1 - 1.5 * IQR_val) & ens_mape <= (Q3 + 1.5 * IQR_val)) %>% select(ens_mape)

mediana_val <- median(df_filtrado$ens_mape, na.rm = TRUE)


ggplot(df_filtrado, aes(x = "", y = ens_mape)) +
  geom_boxplot(fill = "skyblue") +
  geom_text(aes(label = round(mediana_val, 2), y = mediana_val), vjust = -0.5, size = 7) +
  labs(title = "MAPE error of simple ensemble method",
       y = "ens_mape",
       x = "") +
  theme_minimal()




#### BOXPLOT DE LOS FFORMA ###
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")

# Seleccionar las columnas relevantes
datosMAPE <- datosMAPE %>% select(matches("^FFORMA_.*_MAPE$"))


# Combinar y transformar los datos a formato largo
combined_long <- bind_rows(
  datosMAPE %>%
    select(contains("_MAPE")) %>%
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
  labs(title = "FFORMA models comparison", x = "", y = "MAPE") +
  guides(fill = "none") 



######## graficos fforma nuevos (git de pablo) #########
nuevos_fforma <- fread("Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")

mape_nuevos <- nuevos_fforma %>%
  select(id, starts_with("MAPE_"))

mape_nuevos_long <- mape_nuevos %>%
  pivot_longer(cols = -id, names_to = "Variable", values_to = "MAPE") %>%
  mutate(Origen = "Modelos FFORMA nuevos")

combined_long <- mape_nuevos_long %>%
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

orden_variables <- combined_long %>%
  distinct(Variable) %>%
  left_join(medianas, by = "Variable") %>%
  arrange(Mediana) %>%
  pull(Variable)

combined_long$Variable <- factor(combined_long$Variable, levels = orden_variables)

# Redefinir color según nueva variable de menor mediana
variables_baja_mediana <- head(medianas$Variable, 1)
combined_long$Color <- ifelse(combined_long$Variable %in% variables_baja_mediana, "Baja Mediana", "Otro")

# Gráfico final actualizado
ggplot(combined_long, aes(x = Variable, y = MAPE, fill = Color)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)),
               vjust = -0.5, color = "black", size = 3.5) +
  scale_fill_manual(values = c("Baja Mediana" = "#5387E3", "Otro" = "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "FFORMA models comparison", x = "", y = "MAPE") +
  guides(fill = "none")


