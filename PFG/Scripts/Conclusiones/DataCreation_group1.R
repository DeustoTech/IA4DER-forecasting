library(ggplot2)

set.seed(123)

n_series <- 100
n_values <- 150
t <- 1:n_values

series_list <- list()

for (i in 1:n_series) {
  alpha <- 0  
  r <- runif(1, 0, 0.1)  
  ruido <- rnorm(n_values, mean = 0, sd = 0.05)  # Ruido gaussiano
  
  y <- alpha + r * t + ruido
  
  df <- data.frame(
    serie_id = i,
    tiempo = t,
    valor = y
  )
  
  series_list[[i]] <- df
}

df_lineal <- do.call(rbind, series_list)

head(df_lineal)

ggplot(df_lineal, aes(x = tiempo, y = valor, group = serie_id)) +
  geom_line(alpha = 0.3, color = "black") +
  labs(title = "100 series con tendencia lineal aleatoria y ruido",
       x = "Tiempo", y = "Valor") +
  theme_minimal(base_size = 14)

set.seed(123) 
df_lineal$random <- sample(1:10, size = nrow(df_lineal), replace = TRUE)
df_lineal$tipo <- 1
fwrite(df_lineal, "Scripts/Conclusiones/series1.csv")
summ