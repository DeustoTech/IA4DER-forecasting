
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


## COMPROBACION DE DATOS

datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")

resultados <- readRDS("Scripts/FFORMA_model/fforma_for.rds")

nrow(datosMAPE)
length(resultados$dataset)

ids_datosMAPE <- unique(datosMAPE$ID)
ids_resultados <- names(resultados$dataset)

length(ids_datosMAPE)
length(ids_resultados)

ids_comunes <- intersect(ids_datosMAPE, ids_resultados)
length(ids_comunes)

setdiff(ids_datosMAPE, ids_resultados)      # IDs que están en datosMAPE pero no en resultados
setdiff(ids_resultados, ids_datosMAPE) 
