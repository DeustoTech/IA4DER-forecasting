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

### GRAFICO DE LINEAS ###
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
    ses_pred = mean(ses_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_media %>%
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosBase/modelos_base_prediccion_media.pdf", width = 10, height = 6)

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
    ses_pred = median(ses_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_mediana %>%
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosBase/modelos_base_prediccion_mediana.pdf", width = 10, height = 6)

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


#grafico de barras MAPE
df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

# Seleccionar solo las columnas _mape
datos_mape <- df %>%
  select(ends_with("_mape"))

datos_mape <- datos_mape %>% select(- ens_mape)

datos_long <- datos_mape %>%
  pivot_longer(cols = ends_with("_mape"), names_to = "Modelo", values_to = "MAPE")

df_filtrado <- datos_long %>%
  group_by(Modelo) %>%
  mutate(Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
         Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         Lower = Q1 - 1.5 * IQR,
         Upper = Q3 + 1.5 * IQR) %>%
  filter(MAPE >= Lower & MAPE <= Upper) %>%
  ungroup()

colores <- c(
  "mean_mape" = "green3",
  "rw_mape" = "blue",
  "naive_mape" = "cyan3",
  "simple_mape" = "magenta",
  "lr_mape" = "orange",
  "ann_mape" = "red",
  "svm_mape" = "hotpink",
  "arima_mape" = "goldenrod",
  "ses_mape" = "turquoise4"
)

# Crear el boxplot
ggplot(df_filtrado, aes(x = Modelo, y = MAPE, fill = Modelo)) +
  geom_boxplot() +
  scale_fill_manual(values = colores) +
  labs(title = "MAPE error distribution",
       y = "MAPE error", x = "Model", fill =  "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#grafico de barras RMSE
df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

# Seleccionar solo las columnas _mape
datos_rmse <- df %>%
  select(ends_with("_rmse"))

datos_rmse <- datos_rmse %>% select(- ens_rmse)

datos_long <- datos_rmse %>%
  pivot_longer(cols = ends_with("_rmse"), names_to = "Modelo", values_to = "RMSE")

df_filtrado <- datos_long %>%
  group_by(Modelo) %>%
  mutate(Q1 = quantile(RMSE, 0.25, na.rm = TRUE),
         Q3 = quantile(RMSE, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         Lower = Q1 - 1.5 * IQR,
         Upper = Q3 + 1.5 * IQR) %>%
  filter(RMSE >= Lower & RMSE <= Upper) %>%
  ungroup()

colores <- c(
  "mean_rmse" = "green3",
  "rw_rmse" = "blue",
  "naive_rmse" = "cyan3",
  "simple_rmse" = "magenta",
  "lr_rmse" = "orange",
  "ann_rmse" = "red",
  "svm_rmse" = "hotpink",
  "arima_rmse" = "goldenrod",
  "ses_rmse" = "turquoise4"
)

# Crear el boxplot
ggplot(df_filtrado, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_boxplot() +
  scale_fill_manual(values = colores) +
  labs(title = "RMSE error distribution",
       y = "RMSE error", x = "Model", fill =  "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#hacer tabla summary
mape_df <- df %>% select(ends_with("_mape")) %>% select(- ens_mape)

mape_long <- mape_df %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "Model", values_to = "MAPE") %>%
  drop_na()

# Filtrar outliers según IQR por modelo
mape_filtrado <- mape_long %>%
  group_by(Model) %>%
  mutate(Q1 = quantile(MAPE, 0.25),
         Q3 = quantile(MAPE, 0.75),
         IQR = Q3 - Q1,
         Lower = Q1 - 1.5 * IQR,
         Upper = Q3 + 1.5 * IQR) %>%
  filter(MAPE >= Lower & MAPE <= Upper) %>%
  ungroup()

# Calcular tabla resumen sin outliers
summary_table <- mape_filtrado %>%
  group_by(Model) %>%
  summarise(
    Min = min(MAPE),
    Q1 = quantile(MAPE, 0.25),
    Median = median(MAPE),
    Mean = mean(MAPE),
    Q3 = quantile(MAPE, 0.75),
    Max = max(MAPE)
  ) %>%
  arrange(Median)

# Mostrar como tabla
print(summary_table)



df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
df <- df %>% select(- ends_with("_rmse"))

df_mape <- df %>%
  select(id, dia, mean_mape, rw_mape, naive_mape, simple_mape, lr_mape, ann_mape, svm_mape,
         arima_mape, ses_mape) %>%  filter(if_all(ends_with("mape"), is.finite)) %>%
  pivot_longer(-id, names_to = "modelo", values_to = "mape")

rankings <- df_mape %>%
  group_by(id) %>%
  mutate(rank = rank(mape, ties.method = "average")) %>%
  ungroup()

# Ranking promedio por modelo
avg_ranks <- rankings %>%
  group_by(modelo) %>%
  summarise(avg_rank = mean(rank)) %>%
  arrange(avg_rank)

install.packages("pairwiseComparisons")  # si no lo tienes
library(pairwiseComparisons)

# Comparaciones múltiples usando Friedman + Nemenyi
comp <- pairwise_comparisons(
  data = rankings,
  dependent = "mape",
  subject = "id",
  between = "modelo",
  p_adjust_method = "holm",  # "holm" o "bonferroni" si quieres más conservador
  type = "nonparametric",
  paired = TRUE
)

sig_pairs <- comp %>% filter(p.value.adjusted > 0.05)



######### CON NUEVO FORMATO ##########
d <- data.table::fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

d <- d %>% select(ends_with("mape"))
d <- d[,-c("ens_mape")]

graficar_boxplot <- function(data) {
  matrix_data <- as.matrix(data)
  matrix_data <- matrix_data[complete.cases(matrix_data) & rowSums(is.finite(matrix_data)) == ncol(data), ]
  medianas <- apply(matrix_data, 2, median, na.rm = TRUE)
  orden <- order(medianas)
  matrix_data_ordenado <- matrix_data[, orden]
  nombres_ordenados <- colnames(data)[orden]
  medianas_ordenadas <- medianas[orden]
  par(mar = c(10.5, 4, 4, 2))
  b <- boxplot(matrix_data_ordenado, outline = FALSE, ylab = "MAPE", las = 2, names = nombres_ordenados)
  text(
    x = 1:length(medianas_ordenadas),
    y = medianas_ordenadas + 10,
    labels = round(medianas_ordenadas, 1),
    cex = 1.3
  )
}

graficar_boxplot(d)
