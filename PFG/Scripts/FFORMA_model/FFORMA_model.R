
resultados <- readRDS("Scripts/FFORMA_model/fforma_for.rds")

lista_errores <- lapply(resultados$dataset, function(x) {
  if (is.null(x$errors)) return(NULL)
  x$errors
})
lista_errores <- Filter(Negate(is.null), lista_errores)

df_errores <- do.call(rbind, lapply(lista_errores, function(x) as.data.frame(t(x))))
df_errores$serie <- seq_len(nrow(df_errores))

library(tidyr)
df_errores_long <- pivot_longer(df_errores, 
                                cols = -serie, 
                                names_to = "modelo", 
                                values_to = "error")

library(ggplot2)

ggplot(df_errores_long, aes(x = modelo, y = error)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de errores por modelo",
       x = "Modelo",
       y = "Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ahora lo vuelvo hacer pero con smape explicitamente
library(tidyverse)
library(dplyr)

# Extraer sMAPE para todos los modelos de todos los datasets
df_smape <- purrr::map_dfr(resultados$dataset, function(x) {
  smape <- as.data.frame(t(x$smape_err))  # Transponer: filas = pronósticos
  colnames(smape) <- rownames(x$smape_err)
  smape
}, .id = "serie") %>%
  pivot_longer(-serie, names_to = "modelo", values_to = "smape")

# Filtrar outliers por modelo con IQR
df_smape_filtrado <- df_smape %>%
  group_by(modelo) %>%
  mutate(
    Q1 = quantile(smape, 0.25),
    Q3 = quantile(smape, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) %>%
  filter(smape >= lower, smape <= upper) %>%
  select(-Q1, -Q3, -IQR, -lower, -upper) %>%
  ungroup()

# Calcular medianas
medianas <- df_smape_filtrado %>%
  group_by(modelo) %>%
  summarise(mediana = median(smape), .groups = "drop")

# Crear boxplot con medianas
ggplot(df_smape_filtrado, aes(x = modelo, y = smape)) +
  geom_boxplot() +
  geom_text(data = medianas, aes(x = modelo, y = mediana, label = round(mediana, 1)),
            vjust = -0.5, size = 3.5, color = "blue") +
  theme_minimal() +
  labs(
    title = "Distribución de errores sMAPE por modelo (sin outliers)",
    x = "Modelo",
    y = "sMAPE (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### intentar calcular yo el mape ###
library(purrr)

mape_data <- map2(resultados$dataset, seq_along(resultados$dataset), function(x, i) {
  forecast <- x$ff
  real <- matrix(rep(x$xx[1:x$h], each = nrow(forecast)), nrow = nrow(forecast))
  mape <- rowMeans(abs((forecast - real) / real)) * 100
  tibble(
    modelo = names(mape),
    mape = as.numeric(mape),
    serie = i
  )
})

# Unir todo en un solo data frame
mape_df <- bind_rows(mape_data)

# Filtrar outliers por IQR
mape_filtered <- mape_df %>%
  group_by(modelo) %>%
  filter(mape > quantile(mape, 0.25, na.rm = T) - 1.5 * IQR(mape, na.rm = T) &
           mape < quantile(mape, 0.75, na.rm = T) + 1.5 * IQR(mape, na.rm = T)) %>%
  ungroup()

# Calcular medianas para anotarlas
medians <- mape_filtered %>%
  group_by(modelo) %>%
  summarize(median_mape = median(mape))

# Crear boxplot
ggplot(mape_filtered, aes(x = modelo, y = mape)) +
  geom_boxplot() +
  stat_summary(
    fun = median, geom = "text",
    aes(label = round(after_stat(y), 2)),
    color = "blue", size = 4, vjust = -0.5
  ) +
  theme_minimal() +
  labs(
    title = "Boxplot del MAPE por modelo (outliers filtrados)",
    x = "Modelo",
    y = "MAPE"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




