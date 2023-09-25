library(ggplot2)
library(lattice)
library(caret)
library(fpp3)
library(lattice)
library(forecast)
library(purrr)

path <- "DeustoTech GoiEner/RestultadosSemana1.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina

unzip(path, exdir = tempdir) # descomprime. Tarda un poco


# Lista de archivos CSV en la carpeta extraída

archivos <- list.files(tempdir, pattern = ".csv$", recursive = TRUE, full.names = TRUE)

porMedia <- read.csv(archivos[1]) %>% as_tibble()
porNaive <- read.csv(archivos[2]) %>% as_tibble()
porSNaive <- read.csv(archivos[3]) %>% as_tibble()



# analisis de los resultados usando la media

# distribución del error MAE usando la media

maeMediaM <- round(mean(porMedia$MAE), 4) # error medio usando la media 

minMaeM <- round(min(porMedia$MAE), 4)
maxMaeM <- round(max(porMedia$MAE), 4)

ggplot(data = porMedia, aes(x = MAE)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(
    title = "Distribución del Error MAE",
    x = "Error MAE (kWh)"
  ) + scale_x_continuous(limits = c(minMaeM, 2))


ggplot(data = porMedia, aes(y = MAE)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(
    title = "Distribución del Error MAE",
    y = "Error MAE"
  )



# Agrupa los datos por la columna "hora" y calcula el error MAE y RMSE medio
porMediaAgrupado <- porMedia %>%
  group_by(Hora) %>%
  summarise(
    MAE_medio = round(mean(MAE), 4),
    RMSE_medio = round(mean(RMSE), 4)
  )

# Si la columna "hora" es un string con el formato "Hora X", puedes convertirla a numérica
# para que se ordene correctamente en el gráfico.
porMediaAgrupado <- porMediaAgrupado %>%
  mutate(hora = as.numeric(sub("Hora ", "", Hora))) %>% select(-Hora) %>% arrange(hora)

# Muestra la tibble resultado_agrupado
print(porMediaAgrupado)


indice_min_mae <- which.min(porMediaAgrupado$MAE_medio)
indice_max_mae <- which.max(porMediaAgrupado$MAE_medio)
indice_min_rmse <- which.min(porMediaAgrupado$RMSE_medio)
indice_max_rmse <- which.max(porMediaAgrupado$RMSE_medio)

# Crea un gráfico de líneas con puntos destacados para MAE
ggplot(data = porMediaAgrupado, aes(x = hora, y = MAE_medio)) +
  geom_line(color = "black") +
  geom_point(aes(color = ifelse(hora == hora[indice_min_mae], "Mínimo", 
                                ifelse(hora == hora[indice_max_mae], "Máximo", "Normal"))), 
             size = 3, show.legend = F) +
  scale_color_manual(values = c("Normal" = "black", "Mínimo" = "green", "Máximo" = "red")) +
  labs(
    title = "MAE medio usando la Media para predecir",
    x = "Hora",
    y = "MAE Medio (kWh)"
  ) + scale_x_continuous(breaks = porMediaAgrupado$hora) + 
  geom_text(aes(label = ifelse(hora == hora[indice_min_mae], round(MAE_medio, 2), 
ifelse(hora == hora[indice_max_mae], round(MAE_medio, 2), ""))),
vjust = -1.25, size = 3)



# Crea un gráfico de líneas con puntos destacados para RMSE
ggplot(data = porMediaAgrupado, aes(x = hora, y = RMSE_medio)) +
  geom_line(color = "black") +
  geom_point(aes(color = ifelse(hora == hora[indice_min_rmse], "Mínimo", 
                                ifelse(hora == hora[indice_max_rmse], "Máximo", "Normal"))), 
             size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("Normal" = "black", "Mínimo" = "green", "Máximo" = "red")) +
  labs(
    title = "RMSE medio usando la Media para predecir",
    x = "Hora",
    y = "RMSE Medio"
  ) +
  scale_x_continuous(breaks = porMediaAgrupado$hora) + 
  geom_text(aes(label = ifelse(hora == hora[indice_min_rmse], round(RMSE_medio, 2), 
                               ifelse(hora == hora[indice_max_rmse], round(RMSE_medio, 2), ""))),
            vjust = -1.25, size = 3)


# en ambos vemos que usando la media, la hora que menor error medio 
# es a las 5 am, y la que peor a la 1 pm. Igualmente, son errores bastante altos
# de 8 am a 6pm es cuando peor es la predicción




# METODO NAIVE

# Supongamos que tienes una tibble llamada porNaive
# Reemplaza porNaive con el nombre real de tu tibble

porNaiveAgrupado <- porNaive %>%
  group_by(Hora) %>%
  summarise(
    MAE_medio = round(mean(MAE), 4),
    RMSE_medio = round(mean(RMSE), 4)
  )

porNaiveAgrupado <- porNaiveAgrupado %>%
  mutate(hora = as.numeric(sub("Hora ", "", Hora))) %>% select(-Hora) %>% arrange(hora)

indice_min_mae_naive <- which.min(porNaiveAgrupado$MAE_medio)
indice_max_mae_naive <- which.max(porNaiveAgrupado$MAE_medio)

# Crea un gráfico de líneas con puntos destacados para MAE con método Naive
ggplot(data = porNaiveAgrupado, aes(x = hora, y = MAE_medio)) +
  geom_line(color = "black") +
  geom_point(aes(color = ifelse(hora == hora[indice_min_mae_naive], "Mínimo", 
                                ifelse(hora == hora[indice_max_mae_naive], "Máximo", "Normal"))), 
             size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("Normal" = "black", "Mínimo" = "green", "Máximo" = "red")) +
  labs(
    title = "MAE medio usando Método Naive",
    x = "Hora",
    y = "MAE Medio (kWh)"
  ) + scale_x_continuous(breaks = porNaiveAgrupado$hora) + 
  geom_text(aes(label = ifelse(hora == hora[indice_min_mae_naive], round(MAE_medio, 2), 
                               ifelse(hora == hora[indice_max_mae_naive], round(MAE_medio, 2), ""))),
            vjust = -1.25, size = 3)


ggplot(data = porNaiveAgrupado, aes(x = hora, y = RMSE_medio)) +
  geom_line(color = "black") +
  geom_point(aes(color = ifelse(hora == hora[indice_min_rmse], "Mínimo", 
                                ifelse(hora == hora[indice_max_rmse], "Máximo", "Normal"))), 
             size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("Normal" = "black", "Mínimo" = "green", "Máximo" = "red")) +
  labs(
    title = "RMSE medio usando la Media para predecir",
    x = "Hora",
    y = "RMSE Medio"
  ) +
  scale_x_continuous(breaks = porNaiveAgrupado$hora) + 
  geom_text(aes(label = ifelse(hora == hora[indice_min_rmse], round(RMSE_medio, 2), 
                               ifelse(hora == hora[indice_max_rmse], round(RMSE_medio, 2), ""))),
            vjust = -1.25, size = 3)




# COMPARACION MEDIA VS NAIVE

# Combinar los tibbles por hora
MediaVSNaive <- merge(porMediaAgrupado, porNaiveAgrupado, by = "hora", all = TRUE)

# Crear un nuevo tibble con las columnas deseadas
comparacion <- tibble(
  hora = MediaVSNaive$hora,
  MAE_m = MediaVSNaive$MAE_medio.x,
  RMSE_m = MediaVSNaive$RMSE_medio.x,
  MAE_n = MediaVSNaive$MAE_medio.y,
  RMSE_n = MediaVSNaive$RMSE_medio.y
)

# Imprimir el nuevo tibble
print(comparacion)
 

# Comparacion de mae
ggplot(data = comparacion, aes(x = hora)) +
  geom_line(aes(y = MAE_m, color = "MAE Media"), size = 1) +
  geom_point(aes(y = MAE_m, color = "MAE Media"), size = 3) +
  geom_line(aes(y = MAE_n, color = "MAE Naive"), size = 1) +
  geom_point(aes(y = MAE_n, color = "MAE Naive"), size = 3) +
  scale_color_manual(values = c("MAE Media" = "dodgerblue2", "MAE Naive" = "orange"), name = "",
                     labels = c("Media", "Naive")) +
  labs(
    title = "Comparación de MAE entre Media y Naive",
    x = "Hora",
    y = "MAE Medio (kWh)"
  ) + theme(legend.position = "top") +
  scale_x_continuous(breaks = comparacion$hora)

# comparacion de rmse

ggplot(data = comparacion, aes(x = hora)) +
  geom_line(aes(y = RMSE_m, color = "RMSE Media"), size = 1) +
  geom_point(aes(y = RMSE_m, color = "RMSE Media"), size = 3) +
  geom_line(aes(y = RMSE_n, color = "RMSE Naive"), size = 1) +
  geom_point(aes(y = RMSE_n, color = "RMSE Naive"), size = 3) +
  scale_color_manual(values = c("RMSE Media" = "dodgerblue2", "RMSE Naive" = "orange"), name = "",
                     labels = c("Media", "Naive")) +
  labs(
    title = "Comparación de RMSE entre Media y Naive",
    x = "Hora",
    y = "RMSE Medio"
  ) + theme(legend.position = "top") +
  scale_x_continuous(breaks = comparacion$hora)



# Seasonal Naive

porSnAgrupada <- porSNaive %>%
  group_by(DiaDeLaSemana, Hora) %>%
  summarize(
    MAE_medio = round(mean(MAE), 4),
    RMSE_medio = round(mean(RMSE), 4)
  ) %>% arrange(Hora)

# Supongamos que tienes una tibble llamada porSnAgrupada
# Reemplaza 'porSnAgrupada' con el nombre real de tu tibble

ggplot(data = porSnAgrupada, aes(x = Hora, y = MAE_medio, color = DiaDeLaSemana)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "MAE Medio por Día de la Semana",
    x = "Hora",
    y = "MAE Medio"
  ) +
  scale_color_discrete() + theme(legend.position = "top") +
  scale_x_continuous(breaks = porSnAgrupada$Hora)



ggplot(data = porSnAgrupada, aes(x = Hora, y = MAE_medio, color = DiaDeLaSemana)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "MAE Medio por Día de la Semana",
    x = "Hora",
    y = "MAE Medio"
  ) +
  scale_color_discrete() + theme(legend.position = "top") +
  coord_cartesian(xlim = c(8, 21)) + scale_x_continuous(breaks = porSnAgrupada$Hora)


## RESULTADOS OBTENIDOS ##

porMediaAgrupadoKWH <- porMedia %>%
  mutate(Hora = as.numeric(sub("Hora ", "", Hora))) %>% 
  group_by(Hora) %>%
  arrange(Hora) %>%
  summarise( KWH = round(mean(media_entrenamiento), 4),)

print(porMediaAgrupadoKWH, n = 24)

porNaiveAgrupadoKWH <- porNaive %>%
  mutate(Hora = as.numeric(sub("Hora ", "", Hora))) %>% 
  group_by(Hora) %>%
  arrange(Hora) %>%
  summarise( KWH = round(mean(entrenamiento), 4),)

print(porNaiveAgrupadoKWH, n= 24)

  
porSnAgrupadoKWH <- porSNaive %>%
  mutate(Hora = as.numeric(sub("Hora ", "", Hora))) %>% 
  group_by(Hora) %>%
  arrange(Hora) %>%
  summarise( KWH = round(mean(Prediccion), 4),)

print(porSnAgrupadoKWH, n = 24)

#grafico del consumo predecido de las tres formas distintas
ggplot() +
  geom_line(data = porMediaAgrupadoKWH, aes(x = Hora, y = KWH, color = "Media"), linetype = "solid") +
  geom_line(data = porNaiveAgrupadoKWH, aes(x = Hora, y = KWH, color = "Naive"), linetype = "solid") +
  labs(title = "Comparación de Series Temporales",
       x = "Hora",
       y = "Valor Predicho") +
  scale_color_manual(values = c("Media" = "blue", "Naive" = "red"), 
                     labels = c("Media", "Naive")) +
  theme_minimal() + theme(legend.position = "top") + 
  scale_x_continuous(breaks = porSnAgrupadoKWH$Hora)

#grafico del consumo del seasonal naive por dia de la semana

porSnAgrupadoKWH2 <- porSNaive %>%
  group_by(DiaDeLaSemana, Hora) %>%
  summarize(
    KWH = round(mean(Prediccion), 4)
  ) %>% arrange(Hora)

head(porSnAgrupadoKWH2)

ggplot(data = porSnAgrupadoKWH2, aes(x = Hora, y = KWH, color = DiaDeLaSemana)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Consumo Medio por Día de la Semana",
    x = "Hora",
    y = "Consumo Medio"
  ) +
  scale_color_discrete() + theme(legend.position = "top") +
  scale_x_continuous(breaks = porSnAgrupadoKWH2$Hora)


porMediaRango = porMedia %>%  mutate(Hora = as.numeric(sub("Hora ", "", Hora)),
                                     Maximo = as.numeric(sub(".*Máximo: ([0-9.]+).*", "\\1", rango)),
                                     Minimo = as.numeric(sub(".*Mínimo: ([0-9.]+).*", "\\1", rango))) %>%
  select(Hora, Maximo, Minimo, media_entrenamiento) %>%
  group_by(Hora) %>%
  summarise(
    Maximo = mean(Maximo, na.rm = T),
    Minimo = mean(Minimo, na.rm = T),
    KWH = mean(media_entrenamiento, na.rm = TRUE))


ggplot(porMediaRango) +
  geom_line(aes(x = Hora, y = KWH), color = "blue", size = 1) +
  geom_ribbon(aes(x = Hora, ymin = Minimo, ymax = Maximo), fill = "lightblue", alpha = 0.5) +
  labs(title = "Predicción de la media con Rango de Confianza",
       x = "Hora",
       y = "Valor Predicho") +
  theme_minimal() +
  scale_x_continuous(breaks = porMediaRango$Hora)

porNaiveRango = porNaive %>%  mutate(Hora = as.numeric(sub("Hora ", "", Hora)),
                                     Maximo = as.numeric(sub(".*Máximo: ([0-9.]+).*", "\\1", Rango)),
                                     Minimo = as.numeric(sub(".*Mínimo: ([0-9.]+).*", "\\1", Rango))) %>%
  select(Hora, Maximo, Minimo, entrenamiento) %>%
  group_by(Hora) %>%
  summarise(
    Maximo = mean(Maximo, na.rm = T),
    Minimo = mean(Minimo, na.rm = T),
    KWH = mean(entrenamiento, na.rm = TRUE))
  
ggplot(porNaiveRango) +
  geom_line(aes(x = Hora, y = KWH), color = "green", size = 1) +
  geom_ribbon(aes(x = Hora, ymin = Minimo, ymax = Maximo), fill = "lightgreen", alpha = 0.5) +
  labs(title = "Predicción del último valor con Rango de Confianza",
       x = "Hora",
       y = "Valor Predicho") +
  theme_minimal() +
  scale_x_continuous(breaks = porNaiveRango$Hora)

porSNRango = porSNaive %>%  mutate(Hora = as.numeric(sub("Hora ", "", Hora)),
                                     Maximo = as.numeric(sub(".*Máximo: ([0-9.]+).*", "\\1", Rango)),
                                     Minimo = as.numeric(sub(".*Mínimo: ([0-9.]+).*", "\\1", Rango))) %>%
  select(Hora, DiaDeLaSemana, Maximo, Minimo, Prediccion) %>%
  group_by(Hora) %>%
  summarise(
    Maximo = mean(Maximo, na.rm = T),
    Minimo = mean(Minimo, na.rm = T),
    KWH = mean(Prediccion, na.rm = TRUE))

ggplot(porSNRango) +
  geom_line(aes(x = Hora, y = KWH), color = "red", size = 1) +
  geom_ribbon(aes(x = Hora, ymin = Minimo, ymax = Maximo), fill = "#FA9191", alpha = 0.5) +
  labs(title = "Predicción de la media del dia de la semana con Rango de Confianza",
       x = "Hora",
       y = "Valor Predicho") +
  theme_minimal() +
  scale_x_continuous(breaks = porSNRango$Hora)

