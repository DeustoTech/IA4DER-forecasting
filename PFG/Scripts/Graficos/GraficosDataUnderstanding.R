library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(data.table)

carpeta <- "NUEVOS DATOS/OriginalData/imp_csv/"
archivos <- list.files(path = carpeta, pattern = "\\.csv$", full.names = TRUE)[200]
datos_list <- lapply(archivos, function(file) {
  df <- fread(file)
  df[, ID := tools::file_path_sans_ext(basename(file))]
  return(df)
})
datos <- rbindlist(datos_list)
setkey(datos, ID)

#fwrite(datos, "NUEVOS DATOS/OriginalData/imp_csv_merged.csv")
#datos <- fread("NUEVOS DATOS/OriginalData/imp_csv_merged.csv")

datos$timestamp <- as.POSIXct(datos$timestamp, format="%Y-%m-%d %H:%M:%S")

# Extraer día de la semana y hora
datos$dia_semana <- lubridate::wday(datos$timestamp, label = TRUE)
datos$hora <- lubridate::hour(datos$timestamp)

# Crear etiqueta para horas de la semana
datos$hora_semana <- paste(substr(datos$dia_semana, 1, 3), 
                           sprintf("%02dh", datos$hora))

dias <- c("lu" = "Mon", "ma" = "Tue", "mi" = "Wed",
          "ju" = "Thu", "vi" = "Fri", "sá" = "Sat", "do" = "Sun")

# Quitar las barras invertidas y separar día y hora
datos$hora_semana <- gsub("\\\\", "", datos$hora_semana)  # elimina los '\'

# Reemplazar los días por las abreviaciones en inglés
datos$hora_semana <- sapply(datos$hora_semana, function(x) {
  dia <- substr(x, 1, 2)
  hora <- substr(x, 4, nchar(x))
  paste0(dias[dia], " ", hora)
})

# Crear vector ordenado con todas las horas de la semana
dias <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
horas <- sprintf("%02dh", c(0, 6, 12, 18))
horas_de_semana <- c()
for (dia in dias) {
  for (h in horas) {
    horas_de_semana <- c(horas_de_semana, paste(dia, h))
  }
}

# Todos los puntos horarios para un eje X completo
todos_puntos_horarios <- c()
for (dia in dias) {
  for (h in 0:23) {
    todos_puntos_horarios <- c(todos_puntos_horarios, 
                               paste(dia, sprintf("%02dh", h)))
  }
}

# Convertir a factor para mantener el orden correcto
datos$hora_semana_factor <- factor(
  datos$hora_semana,
  levels = todos_puntos_horarios
)

# Calcular estadísticas por hora de la semana
estadisticas <- datos %>%
  group_by(hora_semana_factor) %>%
  summarize(
    mediana = median(kWh, na.rm = TRUE),
    q1 = quantile(kWh, 0.25, na.rm = TRUE),
    q3 = quantile(kWh, 0.75, na.rm = TRUE)
  )


ggplot(estadisticas, aes(x = hora_semana_factor)) +
  geom_ribbon(aes(ymin = q1, ymax = q3, y = mediana, group = 1), fill = "grey80") +
  geom_line(aes(y = mediana, group = 1), color = "black", size = 0.5) +
  scale_x_discrete(breaks = horas_de_semana) +
  labs(
    x = "Hours of the week",
    y = "Consumed energy (kWh)",
    title = "Median and confidence interval (1st and 3rd quartiles) of electricity consumption from Jun 2021 to Jun 2022"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

