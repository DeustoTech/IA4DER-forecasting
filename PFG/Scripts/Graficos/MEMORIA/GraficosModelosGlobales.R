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

# Leer archivos adicionales
archivos <- c(
  "bolt_mini_fixed_errors.csv", "bolt_tiny_fixed_errors.csv", 
  "chronos_t5_small_fixed_errors.csv", "timesfm_fixed_errors.csv"
)

ruta_archivos <- "NuevosResultados/TimesFM/errores/"


df_total <- data.frame()
# Añadir datos de archivos
for (archivo in archivos) {
  df <- fread(file.path(ruta_archivos, archivo))
  nombre_variable <- gsub("_errors.csv", "", archivo)
  df_largo <- df  %>%
    select(mape) %>%
    mutate(Variable = nombre_variable, MAPE = mape) %>%
    select(Variable, MAPE)
  df_total <- bind_rows(df_total, df_largo)
}

# Eliminar outliers
combined_long <- df_total %>%
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
  labs(title = "MAPE of Global Models", x = "", y = "MAPE") +
  guides(fill = "none")


######## OTRA VEZ CON NUEVO FORMATO
archivos <- c(
  "bolt_mini_fixed_errors.csv", "bolt_tiny_fixed_errors.csv", 
  "chronos_t5_small_fixed_errors.csv", "timesfm_fixed_errors.csv"
)
ruta_archivos <- "NuevosResultados/TimesFM/errores/"
lista_mapes <- list()
for (archivo in archivos) {
  df <- fread(file.path(ruta_archivos, archivo))
  nombre_variable <- gsub("_fixed_errors.csv", "", archivo)
  lista_mapes[[nombre_variable]] <- df$mape  # Guardar vector en lista
}
max_len <- max(sapply(lista_mapes, length))
lista_con_NA <- lapply(lista_mapes, function(x) {
  length(x) <- max_len  # Rellena con NA automáticamente
  x
})
mape_globales <- as.data.frame(lista_con_NA)
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
    y = medianas_ordenadas + 7,
    labels = round(medianas_ordenadas, 1),
    cex = 1.5
  )
}
graficar_boxplot(mape_globales)


