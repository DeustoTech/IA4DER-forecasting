library(ggplot2)
library(lattice)
library(caret)
library(fpp3)
library(lattice)
library(forecast)
library(purrr)
library(data.table)
library(tidyverse)



L <- fread("ResultadosNuevosL.csv")
CT <- fread("ResultadosNuevosCT.csv")
CUPS <- fread("ResultadosNuevosCUPS.csv")

#graficos 

nObs <- min(table(CT$Modelo), na.rm = T)

set.seed(34234)


CT <- CT %>%
  group_by(Modelo) %>%
  slice_head(n = nObs)

filtrado <- CT %>%
  filter(RMSE < quantile(CT$RMSE, 0.75))

dividido <- split(filtrado$RMSE, filtrado$Modelo)

colores <- rainbow(length(dividido))

# Crear un boxplot para cada modelo
boxplot(dividido, 
        main = "RMSE por Modelo", 
        ylab = "RMSE",
        col = colores,
        names = names(dividido),
        names.arg = CT$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X







resultadosGroupedMean <- CT %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = mean(Predicted, na.rm = T),
    Real = mean(Real, na.rm = T),
    sMAPE= mean(sMAPE, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
  ) %>%
  ungroup

errMediana <- CT %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = median(Predicted, na.rm = T),
    Real = median(Real, na.rm = T),
    sMAPE= median(sMAPE, na.rm = TRUE),
    RMSE = median(RMSE, na.rm = TRUE),
    MAPE = median(MAPE, na.rm = TRUE)
  ) %>%
  ungroup


colores <- rainbow(length(errMediana))



ggplot(data = errMediana, aes(x = Modelo, y = MAPE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = MAPE, label = sprintf("%.4f", MAPE)), vjust = -0.25) + 
  labs(
    title = "Comparación de MAPE mediana",
    x = "Modelo",
    y = "MAPE"
  ) +
  scale_fill_manual(values = colores, 
                    name = "") +
  theme(legend.position = "top") 
