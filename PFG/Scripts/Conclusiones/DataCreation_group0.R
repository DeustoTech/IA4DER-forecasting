library(dplyr)
library(ggplot2)
library(data.table)

# Parámetros
set.seed(123)
n_series <- 100
n_values <- 150
factor_ruido <- 3  # Amplifica la varianza del ruido

# Tiempo
t <- 1:n_values

# Base aleatoria uniforme entre 0 y 1
base_series <- matrix(runif(n_series * n_values, 0, 1), nrow = n_values)

#varianza teórica
var_grupos <- list(
  #1. onda
  grupo1 = 0.05 + 0.05 * sin(2 * pi * t / 25),
  
  #2. ruido decreciente 
  grupo2 = rev(seq(0.01, 0.1, length.out = n_values)),
  
  #3. ruido creciente 
  grupo3 = seq(0.01, 0.1, length.out = n_values),
  
  #4. campana
  grupo4 = {
    bell <- dnorm(seq(-3, 3, length.out = n_values))
    bell / max(bell) * 0.1
  }
)


series_list <- list()

# Generar las 100 series
for (i in 1:n_series) {
  grupo <- ceiling(i / 25)
  var <- var_grupos[[grupo]]
  
  ruido <- rnorm(n_values, mean = 0, sd = sqrt(var) * factor_ruido)
  base <- base_series[, i]
  serie <- base + ruido
  
  
  df_serie <- data.frame(
    serie_id = i,
    grupo = grupo,
    tiempo = t,
    valor = serie
  )
  
  series_list[[i]] <- df_serie
}

df <- bind_rows(series_list)
set.seed(999) 
df$random <- sample(1:10, size = nrow(df), replace = TRUE)
df$tipo <- 0
fwrite(df, "Scripts/Conclusiones/series0.csv")




dir.create("Scripts/Conclusiones/graficos_series0", showWarnings = FALSE)

# Gráfico 1: VALORES observados (base + ruido) por grupo
for (g in 1:4) {
  datos <- df %>% filter(grupo == g)
  
  p <- ggplot(datos, aes(x = tiempo, y = valor, group = serie_id)) +
    geom_line(alpha = 0.5, color = "darkgreen") +
    labs(title = paste("Grupo", g, "- Series completas (base + ruido)"),
         x = "Tiempo", y = "Valor observado") +
    theme_minimal(base_size = 14)
  
  ggsave(
    filename = paste0("Scripts/Conclusiones/graficos_series0/series_grupo_", g, ".png"),
    plot = p, width = 8, height = 5
  )
}

# Gráfico 2: SOLO EL RUIDO por grupo
for (g in 1:4) {
  datos <- df %>% filter(grupo == g)
  
  p <- ggplot(datos, aes(x = tiempo, y = ruido, group = serie_id)) +
    geom_line(alpha = 0.5, color = "red") +
    labs(title = paste("Grupo", g, "- Solo el ruido"),
         x = "Tiempo", y = "Ruido añadido") +
    theme_minimal(base_size = 14)
  
  ggsave(
    filename = paste0("Scripts/Conclusiones/graficos_series0/ruido_grupo_", g, ".png"),
    plot = p, width = 8, height = 5
  )
}

