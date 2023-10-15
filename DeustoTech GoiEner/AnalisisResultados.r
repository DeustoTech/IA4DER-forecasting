library(ggplot2)
library(lattice)
library(caret)
library(fpp3)
library(lattice)
library(forecast)
library(purrr)
library(data.table)
library(tidyverse)

path <- "ResultadosModelos.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina

unzip(path, exdir = tempdir) # descomprime. Tarda un poco


# Lista de archivos CSV en la carpeta extraída

archivos <- list.files(tempdir, pattern = ".csv$", recursive = TRUE, full.names = TRUE)

# Resultados Semana 2

csvResultados <- grep("resultadosTotales2.csv", archivos, value = TRUE)
csvSVM <- grep("resultadosSVM.csv", archivos, value = TRUE)
csvMed <- grep("resultadosMedia2.csv", archivos, value = TRUE)
csvNav <- grep("resultadosNaiveDia2.csv", archivos, value = TRUE)
csvSnav <- grep("resultadosSnaive2.csv", archivos, value = TRUE)
csvSVM2 <- grep("ResultadosSVM_h.csv", archivos, value = TRUE)



resultados <- fread(csvResultados)
svm <- fread(csvSVM)
media <- fread(csvMed)
naive <- fread(csvNav)
snaive <- fread(csvSnav)
svm_h <- fread(csvSVM2)

media <- media %>% na.omit() %>% filter(
  is.finite(MAE),
  is.finite(RMSE),
  is.finite(MASE),
  is.finite(MAPE)
) %>% mutate(Modelo = "Media") %>% mutate(Predicted = media_entrenamiento, Hora = as.numeric(sub("Hora ", "", Hora))) %>%
  select(-rango, -media_entrenamiento)


naive <- naive %>% na.omit() %>% filter(
  is.finite(MAE),
  is.finite(RMSE),
  is.finite(MAPE)
) %>% mutate(Modelo = "Naive") %>% mutate(Predicted = entrenamiento, 
Hora = as.numeric(sub("Hora ", "", Hora))) %>% select(-Rango, -entrenamiento)


snaive <- snaive %>% na.omit() %>% filter(
  is.finite(MAE),
  is.finite(RMSE),
  is.finite(MASE),
  is.finite(MAPE)
) %>% mutate(Modelo = "SNaive", Hora = as.numeric(sub("Hora ", "", Hora)), 
                                                  Predicted = Prediccion) %>%
  select(-Rango, -Prediccion, -DiaDeLaSemana)




resultados <- resultados %>% na.omit() %>% filter(
  is.finite(sMAPE),
  is.finite(RMSE),
  is.finite(MASE),
)
svm <- svm %>% na.omit() %>% filter(
  is.finite(sMAPE),
  is.finite(RMSE),
  is.finite(MASE),
)
svm_h <- svm_h %>% filter(
  is.finite(sMAPE),
  is.finite(RMSE),
  # is.finite(MASE),
)



options(digits = 4)

# TIBBLE AGRUPADA POR MODELOS Y HORAS. 24 FILAS (UNA HORA) POR CADA MODELO

resultados <- bind_rows(resultados, svm_h, media, naive, snaive)


# SOLO SI QUEREMOS AGRUPAR POR MODELO 
resultados <- resultados %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = mean(Predicted, na.rm = T),
    sMAPE= mean(sMAPE, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE),
    MASE = mean(MASE, na.rm = TRUE)
  ) %>%
  ungroup


# BOXPLOT RMSE ENTRE MODELOS



# Filtra las filas con el tipo de error deseado y elimina NA
filtradoRMSE <- resultados %>%
  filter(!is.na({{ RMSE }})) %>% filter(RMSE < quantile(resultados$RMSE, 0.75))


# Dividir los datos en una lista de data frames por Modelo
divididoRMSE <- split(filtradoRMSE$RMSE, filtradoRMSE$Modelo)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoRMSE))

# Crear un boxplot para cada modelo
boxplot(divididoRMSE, 
        main = "RMSE por Modelo", 
        ylab = "RMSE",
        col = colores,
        names = names(divididoRMSE),
        names.arg = resultados$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X


# BOXPLOT MASE ENTRE MODELOS


filtradoMASE <- resultados %>%
  filter(!is.na({{ MASE }})) %>% filter(MASE < 2)


# Dividir los datos en una lista de data frames por Modelo
divididoMASE <- split(filtradoMASE$MASE, filtradoMASE$Modelo)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoMASE))

# Crear un boxplot para cada modelo
boxplot(divididoMASE, 
        main = "MASE por Modelo", 
        ylab = "MASE",
        col = colores,
        names = names(divididoMASE),
        names.arg = resultados$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X

# BOXPLOT MASE SOLO ETS  ARIMA y NN


filtradoMASE2 <- resultados %>%
  filter(!is.na({{ MASE }})) %>% filter((Modelo == "ETS" | Modelo == "ARIMA" | Modelo == "Red Neuronal") & 
                                          MASE < 1.6884 )


# Dividir los datos en una lista de data frames por Modelo
divididoMASE2 <- split(filtradoMASE2$MASE, filtradoMASE2$Modelo)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoMASE2))

# Crear un boxplot para cada modelo
boxplot(divididoMASE2, 
        main = "MASE por Modelo", 
        ylab = "MASE",
        col = colores,
        names = names(divididoMASE2),
        names.arg = resultados$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X


# BOXPLOT MAPE POR MODELOS

filtradoMAPE <- resultados %>%
  filter(!is.na({{ MAPE }})) %>% filter(MAPE < 1.5455)


# Dividir los datos en una lista de data frames por Modelo
divididoMAPE <- split(filtradoMAPE$MAPE, filtradoMAPE$Modelo)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoMAPE))

# Crear un boxplot para cada modelo
boxplot(divididoMAPE, 
        main = "MAPE por Modelo", 
        ylab = "MAPE",
        col = colores,
        names = names(divididoMAPE),
        names.arg = resultados$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X


# BOXPLOT SMAPE POR MODELOS

filtradoSMAPE <- resultados %>%
  filter(!is.na(sMAPE)) %>% filter(sMAPE < quantile(resultados$sMAPE, 0.75, na.rm = T))


# Dividir los datos en una lista de data frames por Modelo
divididoSMAPE <- split(filtradoSMAPE$sMAPE, filtradoSMAPE$Modelo)

# Crear un vector de colores para los boxplots
colores <- rainbow(length(divididoSMAPE))

# Crear un boxplot para cada modelo
boxplot(divididoSMAPE, 
        main = "sMAPE por Modelo", 
        ylab = "sMAPE",
        col = colores,
        names = names(divididoSMAPE),
        names.arg = resultados$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X





# Agrupar por Modelo y tipo de error (sMAPE, RMSE, MASE) y calcular la media
errMedios <- resultados %>%
  group_by(Modelo) %>%
  summarize(RMSE = mean(RMSE, na.rm = TRUE),
            sMAPE = mean(sMAPE, na.rm = T),
            MASE = mean(MASE, na.rm = T)) %>%
  ungroup()


# Grafico sMAPE

colores_modelos <- c("cadetblue3", "#EE3B3B", "#00CD00", "#CD1076")  # Puedes agregar más colores si es necesario


ggplot(data = errMedios, aes(x = Modelo, y = sMAPE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = sMAPE, label = sprintf("%.4f", sMAPE)), vjust = -0.25) + 
  labs(
    title = "Comparación de sMAPE Medios",
    x = "Modelo",
    y = "sMAPE Medio"
  ) +
  scale_fill_manual(values = colores_modelos, 
                    name = "") +
  theme(legend.position = "top")

# Grafico MASE

ggplot(data = errMedios, aes(x = Modelo, y = MASE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = MASE, label = sprintf("%.4f", MASE)), vjust = -0.25) + 
  labs(
    title = "Comparación de MASE Medios",
    x = "Modelo",
    y = "sMAPE Medio"
  ) +
  scale_fill_manual(values = colores_modelos, 
                    name = "") +
  theme(legend.position = "top") 


# Grafico RMSE

ggplot(data = errMedios, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.4f", RMSE)), vjust = -0.5) + 
  labs(
    title = "Comparación de RMSE Medios",
    x = "Modelo",
    y = "RMSE Medio"
  ) +
  scale_fill_manual(values = colores_modelos, 
                    name = "") +
  theme(legend.position = "top")

# Distribución de los errores

# sMAPE


ggplot(data = resultados, aes(x = Modelo, y = sMAPE)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Error sMAPE",
    y = "Error sMAPE"
  ) +
  facet_wrap(~Modelo, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = colores_modelos)

# RMSE d
ggplot(data = resultados, aes(x = Modelo, y = RMSE)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Error RMSE",
    y = "Error RMSE"
  ) +
  facet_wrap(~Modelo, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = colores_modelos)

# MASE

ggplot(data = resultados, aes(x = Modelo, y = sMAPE)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Error MASE",
    y = "Error MASE"
  ) +
  facet_wrap(~Modelo, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = colores_modelos)


# TIMEPLOTS

# COMPARACIÓN DE ERRORES EN UN DÍA

# sMAPE

ggplot(data = resultados, aes(x = Hora, y = sMAPE, color = Modelo)) +
  geom_line(size = 1) +
  labs(
    title = "Error sMAPE en el Tiempo por Modelo",
    x = "Hora",
    y = "sMAPE"
  ) +
  scale_fill_manual(values = colores_modelos, 
                    name = "") +
  theme(legend.position = "top")




# RMSE

ggplot(data = resultados, aes(x = Hora, y = RMSE, color = Modelo)) +
  geom_line(size = 1) +
  labs(
    title = "Error RMSE en el Tiempo por Modelo",
    x = "Hora",
    y = "RMSE"
  ) +
  scale_color_manual(values = colores_modelos)+
  theme(legend.position = "top")

# MASE

ggplot(data = resultados, aes(x = Hora, y = MASE, color = Modelo)) +
  geom_line(size = 1) +
  labs(
    title = "Error MASE en el Tiempo por Modelo",
    x = "Hora",
    y = "MASE"
  ) +
  scale_color_manual(values = colores_modelos)+
  theme(legend.position = "top")

# PREDICCION

ggplot(data = resultados, aes(x = Hora, y = Prediccion, color = Modelo)) +
  geom_line() +
  labs(
    title = "Predicción del consumo en el tiempo en función del modelo",
    x = "Hora",
    y = "Consumo (kWh)"
  ) +
  scale_color_manual(values = colores_modelos)




#DISTRIBUCIÓN DE LOS ERRORES DE CADA MODELO (ESTADISTICOS Y AI)

csvResultados <- grep("resultadosTotales.csv", archivos, value = TRUE)
csvSVM <- grep("resultadosSVM.csv", archivos, value = TRUE)

resultados <- fread(csvResultados)
svm <- fread(csvSVM)
resultados

resultados <- resultados %>% na.omit() %>% filter(
  is.finite(sMAPE),
  is.finite(RMSE),
  is.finite(MASE),
)
svm <- svm %>% na.omit() %>% filter(
  is.finite(sMAPE),
  is.finite(RMSE),
  is.finite(MASE),
)
options(digits = 4)

resultadosArima <- resultados %>% filter(Modelo == "ARIMA") %>% distinct()
resultadosETS <- resultados %>% filter(Modelo == "ETS") %>% distinct()
resultadosRN <- resultados %>% filter(Modelo == "Red Neuronal") %>% distinct()
resultadosSVM <- svm %>% distinct()

#ARIMA
boxplot(list(sMAPE = resultadosArima$sMAPE, RMSE = resultadosArima$RMSE, MASE = resultadosArima$MASE),
        main = "Boxplot errores de la ARIMA", 
        xlab = "modelos", ylab = "error", 
        col = c("#EA5A5A", "#35C852", "#4D62BA" ), fill = "gray")

#solo del MASE pq no se ve bien
boxplot(resultadosArima$MASE, main = "Boxplot del MASE de la ARIMA", 
        xlab = "modelos", ylab = "error", 
        col = "#4D62BA", fill = "gray")

#ETS
boxplot(list(sMAPE = resultadosETS$sMAPE, RMSE = resultadosETS$RMSE, MASE = resultadosETS$MASE),
        main = "Boxplot errores de ETS", 
        xlab = "modelos", ylab = "error", 
        col = c("#EA5A5A", "#35C852", "#4D62BA" ), fill = "gray")

#solo del MASE pq no se ve bien
boxplot(resultadosETS$MASE, main = "Boxplot del MASE de la ETS", 
        xlab = "modelos", ylab = "error", 
        col = "#4D62BA", fill = "gray")

#REDES NEURONALES
boxplot(list(sMAPE = resultadosRN$sMAPE, RMSE = resultadosRN$RMSE, MASE = resultadosRN$MASE),
        main = "Boxplot errores de Redes Neuronales", 
        xlab = "modelos", ylab = "error", 
        col = c("#EA5A5A", "#35C852", "#4D62BA" ), fill = "gray")

boxplot(resultadosRN$sMAPE, main = "Boxplot del sMAPE de Redes neuronales", 
        xlab = "modelos", ylab = "error", 
        col = "#EA5A5A", fill = "gray")

#SVM
boxplot(list(sMAPE = resultadosSVM$sMAPE, RMSE = resultadosSVM$RMSE, MASE = resultadosSVM$MASE),
        main = "Boxplot errores de SVM", 
        xlab = "modelos", ylab = "error", 
        col = c("#EA5A5A", "#35C852", "#4D62BA" ), fill = "gray")

boxplot(resultadosSVM$sMAPE, main = "Boxplot del sMAPE de SVM", 
        xlab = "sMAPE", ylab = "error", 
        col = "#EA5A5A", fill = "gray")
boxplot(resultadosSVM$RMSE, main = "Boxplot del RMSE de SVM", 
        xlab = "RMSE", ylab = "error", 
        col = "#35C852", fill = "gray")
boxplot(resultadosSVM$MASE, main = "Boxplot del MASE de SVM", 
        xlab = "MASE", ylab = "error", 
        col = "#4D62BA", fill = "gray")


#PREDICCIONES DE LOS MODELOS (ESTADISTICOS Y AI)
options(digits = 4)
resultadosArima_H <- resultadosArima %>% group_by(Hora) %>% summarise(
  Prediccion = mean(Predicted, na.rm = T),
  sMAPE= mean(sMAPE, na.rm = TRUE),
  RMSE = mean(RMSE, na.rm = TRUE),
  MASE = mean(MASE, na.rm = TRUE) 
)

resultadosETS_H <- resultadosETS %>% group_by(Hora) %>% summarise(
  Prediccion = mean(Predicted, na.rm = T),
  sMAPE= mean(sMAPE, na.rm = TRUE),
  RMSE = mean(RMSE, na.rm = TRUE),
  MASE = mean(MASE, na.rm = TRUE) 
)

resultadosRN_H <- resultadosRN %>% group_by(Hora) %>% summarise(
  Prediccion = mean(Predicted, na.rm = T),
  sMAPE= mean(sMAPE, na.rm = TRUE),
  RMSE = mean(RMSE, na.rm = TRUE),
  MASE = mean(MASE, na.rm = TRUE) 
)

resultadosSVM_H <- resultadosSVM %>% group_by(Hora) %>% summarise(
  Prediccion = mean(Predicted, na.rm = T),
  sMAPE= mean(sMAPE, na.rm = TRUE),
  RMSE = mean(RMSE, na.rm = TRUE),
  MASE = mean(MASE, na.rm = TRUE) 
)

ggplot(resultadosArima_H, aes(x = Hora, y = Prediccion)) +
  geom_line(color = "#474A85") +
  labs(title = "Predicción de consumo ARIMA", x = "Hora", y = "Predicción") +
  scale_x_continuous(breaks = resultadosArima_H$Hora) +
  theme_minimal()

ggplot(resultadosETS_H, aes(x = Hora, y = Prediccion)) +
  geom_line(color = "#47854D") +
  labs(title = "Predicción de consumo ETS", x = "Hora", y = "Predicción") +
  scale_x_continuous(breaks = resultadosETS_H$Hora) +
  theme_minimal()

ggplot(resultadosRN_H, aes(x = Hora, y = Prediccion)) +
  geom_line(color = "#854B47") +
  labs(title = "Predicción de consumo Redes Neuronales", x = "Hora", y = "Predicción") +
  scale_x_continuous(breaks = resultadosRN_H$Hora) +
  theme_minimal()

ggplot(resultadosSVM_H, aes(x = Hora, y = Prediccion)) +
  geom_line(color = "#AB066D") +
  labs(title = "Predicción de consumo SVM", x = "Hora", y = "Predicción") +
  scale_x_continuous(breaks = resultadosSVM_H$Hora) +
  theme_minimal()

ggplot() +
  geom_line(data = resultadosArima_H, aes(x = Hora, y = Prediccion, color = "ARIMA")) +
  geom_line(data = resultadosETS_H, aes(x = Hora, y = Prediccion, color = "ETS")) +
  geom_line(data = resultadosRN_H, aes(x = Hora, y = Prediccion, color = "RN")) +
  geom_line(data = resultadosSVM_H, aes(x = Hora, y = Prediccion, color = "SVM")) 
  labs(title = "Consumo Predicho por Hora", x = "Hora", y = "Predicción") +
  scale_color_manual(values = c(ARIMA = "#474A85", ETS = "#47854D", RN = "#854B47", SVM = "#AB066D")) +
  theme_minimal() +
  scale_x_continuous(breaks = resultadosSVM_H$Hora)



porMedia <- read.csv(archivos[1]) %>% as_tibble()
porNaive <- read.csv(archivos[2]) %>% as_tibble()
porSNaive <- read.csv(archivos[3]) %>% as_tibble()

porMedia <- porMedia %>%
  filter(
    is.finite(MAE),
    is.finite(RMSE),
    is.finite(MAPE),
    is.finite(MASE),
    is.finite(media_entrenamiento)
  )
  

porNaive <- porNaive %>%
  filter(
    is.finite(MAE),
    is.finite(RMSE),
    is.finite(MAPE),
    is.finite(entrenamiento)
  )


porSNaive <- porSNaive %>%
  filter(
    is.finite(MAE),
    is.finite(RMSE),
    is.finite(MAPE),
    is.finite(MASE),
    is.finite(Prediccion)
  )

# Errores del modelo usando la media

rmseMed <- round(mean(porMedia$RMSE), 4)
mapeMed <- round(mean(porMedia$MAPE), 4)
maseMed <- round(mean(porMedia$MASE), 4)

# Errores del modelo naive

rmseNaive <- round(mean(porNaive$RMSE), 4)
mapeNaive <- round(mean(porNaive$MAPE), 4)

# Errores del modelo seasonal naive

rmseSnav <- round(mean(porSNaive$RMSE), 4)
mapeSnav <- round(mean(porSNaive$MAPE), 4)
maseSnav <- round(mean(porSNaive$MASE), 4)

# tibbles para comparar resultados

valoresRMSE <- tibble(
  Modelo = c("Media", "Naive", "Seasonal Naive"),
  RMSE = c(rmseMed, rmseNaive, rmseSnav)
)

valoresMAPE <- tibble(
  Modelo = c("Media", "Naive", "Seasonal Naive"),
  MAPE = c(mapeMed, mapeNaive, mapeSnav)
)

valoresMASE <- tibble(
  Modelo = c("Media", "Seasonal Naive"),
  MASE = c(maseMed, maseSnav)
)

# graficos comparando errores medios entre modelos


# Grafico rmse

ggplot(data = valoresRMSE, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.4f", RMSE)), vjust = -0.5) + 
  labs(
    title = "Comparación de RMSE Medios",
    x = "Modelo",
    y = "RMSE Medio"
  ) +
  scale_fill_manual(values = c("Media" = "dodgerblue2", "Naive" = "orange", "Seasonal Naive" = "hotpink1"), 
                    name = "") +
  theme(legend.position = "top")


# grafico mape

ggplot(data = valoresMAPE, aes(x = Modelo, y = MAPE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.4f", MAPE)), vjust = -0.5) + 
  labs(
    title = "Comparación de MAPE Medios",
    x = "Modelo",
    y = "MAPE Medio"
  ) +
  scale_fill_manual(values = c("Media" = "dodgerblue2", "Naive" = "orange", "Seasonal Naive" = "hotpink1"), 
                    name = "") + theme(legend.position = "top") 


# grafico mase

ggplot(data = valoresMASE, aes(x = Modelo, y = MASE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.4f", MASE)), vjust = -0.5) +
  labs(
    title = "Comparación de MASE Medios",
    x = "Modelo",
    y = "MASE Medio"
  ) +
  scale_fill_manual(values = c("Media" = "dodgerblue2", "Seasonal Naive" = "hotpink1"), name = "") +
  theme(legend.position = "top")


# distribución del error MAE usando la media

maeMediaM <- round(mean(porMedia$MAE), 4) # error medio usando la media 



minMaeM <- round(min(porMedia$MAE), 4)
maxMaeM <- round(max(porMedia$MAE), 4)

ggplot(data = porMedia, aes(x = MAE)) +
  geom_histogram(binwidth = 0.1, fill = "dodgerblue2", color = "orange") +
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


# boxplots

# RMSE MEDIA




ggplot(data = porMedia, aes(y = RMSE)) +
  geom_boxplot(fill = "dodgerblue2", color = "dodgerblue2") +
  labs(title = "Boxplot del RMSE de la Media", y = "RMSE")

# MAPE MEDIA

ggplot(data = porMedia, aes(y = MAPE)) +
  geom_boxplot(fill = "dodgerblue2", color = "dodgerblue2") +
  labs(title = "Boxplot del MAPE de la Media", y = "MAPE")

# MASE MEDIA

ggplot(data = porMedia, aes(y = MASE)) +
  geom_boxplot(fill = "dodgerblue2", color = "dodgerblue2") +
  labs(title = "Boxplot del MASE de la Media", y = "MASE")

# RMSE NAIVE

ggplot(data = porNaive, aes(y = RMSE)) +
  geom_boxplot(fill = "orange", color = "orange") +
  labs(title = "Boxplot del RMSE del Naive", y = "RMSE")

# MAPE MEDIA

ggplot(data = porNaive, aes(y = MAPE)) +
  geom_boxplot(fill = "orange", color = "orange") +
  labs(title = "Boxplot del MAPE del Naive", y = "MAPE")



# RMSE SNAIVE

ggplot(data = porSNaive, aes(y = RMSE)) +
  geom_boxplot(fill = "hotpink1", color = "hotpink1") +
  labs(title = "Boxplot del RMSE de la SNaive", y = "RMSE")

# MAPE SNAIVE

ggplot(data = porSNaive, aes(y = MAPE)) +
  geom_boxplot(fill = "hotpink1", color = "hotpink1") +
  labs(title = "Boxplot del MAPE de la SNaive", y = "MAPE")

# MASE SNAIVE

ggplot(data = porSNaive, aes(y = MASE)) +
  geom_boxplot(fill = "hotpink1", color = "hotpink1") +
  labs(title = "Boxplot del MASE de la Snaive", y = "MASE")


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
  labs(title = "Predicción Del último valor con Rango de Confianza",
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

