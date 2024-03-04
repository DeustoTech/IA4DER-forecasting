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


feats_totales <- fread("SOLAR/features_totales.csv")
feats_conAuto <- fread("SOLAR/features_con_autoconsumo.csv")
feats_sinAuto <-fread("SOLAR/features_sin_autoconsumo.csv")


# Seleccionar solo las columnas que contienen "mape"
datos_mape <- feats_totales %>% select(contains("mape"))

# Convertir datos a formato largo para el gráfico de bigotes
datos_long <- pivot_longer(datos_mape, cols = everything(), names_to = "Variable", values_to = "Valor")
q75 <- quantile(datos_long$Valor, 0.75, na.rm = TRUE)

# Número total de variables "mape"
num_variables <- length(unique(datos_long$Variable))

# Número de variables por grupo, ajusta este número según sea necesario
variables_por_grupo <- 10

# Número total de grupos
total_grupos <- ceiling(num_variables / variables_por_grupo)

# Crear y guardar un gráfico por cada grupo de variables "mape"
for (i in 1:total_grupos) {
  # Filtrar el grupo de variables actual
  inicio <- (i - 1) * variables_por_grupo + 1
  fin <- min(i * variables_por_grupo, num_variables)
  variables_grupo <- unique(datos_long$Variable)[inicio:fin]
  
  datos_grupo <- datos_long %>% filter(Variable %in% variables_grupo)
  
  # Crear el gráfico de bigotes para el grupo actual
  p <- ggplot(datos_grupo, aes(x = Variable, y = Valor)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(NA, quantile(datos_grupo$Valor, 0.75, na.rm = TRUE))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = paste("Gráfico de Bigotes para Variables MAPE - Grupo", i)) +
    theme_minimal()
  
  # Guardar el gráfico
  ggsave(paste("grafico_mape_grupo_", i, ".png", sep = ""), plot = p, width = 12, height = 8, dpi = 300)
}
