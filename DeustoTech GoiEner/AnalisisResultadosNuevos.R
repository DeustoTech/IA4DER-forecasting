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

############# CENTROS DE TRANSFORMACION ##############
nObs <- min(table(CT$Modelo), na.rm = T)
set.seed(34234)

CT1 <- CT %>%
  group_by(Modelo) %>%
  slice_head(n = nObs)


resultadosGroupedMean <- CT1 %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = mean(Predicted, na.rm = T),
    Real = mean(Real, na.rm = T),
    sMAPE= mean(sMAPE, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
  ) %>%
  ungroup

errMediana <- CT1 %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = median(Predicted, na.rm = T),
    Real = median(Real, na.rm = T),
    sMAPE= median(sMAPE, na.rm = TRUE),
    RMSE = median(RMSE, na.rm = TRUE),
    MAPE = median(MAPE, na.rm = TRUE)
  ) %>%
  ungroup


#RMSE
filtrado <- CT1 %>%
  filter(RMSE < quantile(CT1$RMSE, 0.75))

dividido <- split(filtrado$RMSE, filtrado$Modelo)

colores <- rainbow(length(dividido))

# Crear un boxplot para cada modelo
boxplot(dividido, 
        main = "RMSE por Modelo", 
        ylab = "RMSE",
        col = colores,
        names = names(dividido),
        names.arg = CT1$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X

colores <- rainbow(length(errMediana) + 1)

ggplot(data = errMediana, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = RMSE, label = sprintf("%.4f", RMSE)), vjust = -0.25) + 
  labs(
    title = "Comparación de RMSE mediana",
    x = "Modelo",
    y = "RMSE"
  ) +
  scale_fill_manual(values = colores, 
                    name = "") +
  theme(legend.position = "top") 

#sMAPE
filtrado <- CT1 %>%
  filter(sMAPE < quantile(CT1$sMAPE, 0.75))

dividido <- split(filtrado$sMAPE, filtrado$Modelo)

colores <- rainbow(length(dividido))

boxplot(dividido, 
        main = "sMAPE por Modelo", 
        ylab = "sMAPE",
        col = colores,
        names = names(dividido),
        names.arg = CT1$Modelo,
        las = 2)  



colores <- rainbow(length(errMediana) + 1)

ggplot(data = errMediana, aes(x = Modelo, y = sMAPE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = sMAPE, label = sprintf("%.4f", sMAPE)), vjust = -0.25) + 
  labs(
    title = "Comparación de sMAPE mediana",
    x = "Modelo",
    y = "sMAPE"
  ) +
  scale_fill_manual(values = colores, 
                    name = "") +
  theme(legend.position = "top") 

#MAPE
filtrado <- CT1 %>%
  filter(MAPE < quantile(CT1$MAPE, 0.75))

dividido <- split(filtrado$MAPE, filtrado$Modelo)

colores <- rainbow(length(dividido))

boxplot(dividido, 
        main = "MAPE por Modelo", 
        ylab = "MAPE",
        col = colores,
        names = names(dividido),
        names.arg = CT1$Modelo,
        las = 2)  



colores <- rainbow(length(errMediana) + 1)

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


CT_Media <- CT %>% filter(Modelo == "Media") %>% group_by(Hora) %>% 
                    summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_Media, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo MEDIA") +
  theme_minimal()

CT_Naive <- CT %>% filter(Modelo == "Naive") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_Naive, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NAIVE") +
  theme_minimal()

CT_sNaive <- CT %>% filter(Modelo == "SNaive") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_sNaive, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo sNAIVE") +
  theme_minimal()

CT_Arima <- CT %>% filter(Modelo == "Arima") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_Arima, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ARIMA") +
  theme_minimal()

CT_ETS <- CT %>% filter(Modelo == "ETS") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_ETS, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ETS") +
  theme_minimal()

CT_NN <- CT %>% filter(Modelo == "NN") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_NN, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NN") +
  theme_minimal()

CT_SVM <- CT %>% filter(Modelo == "SVM") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_SVM, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo SVM") +
  theme_minimal()


################# LINEAS ##################
nObs <- min(table(L$Modelo), na.rm = T)
set.seed(34234)

L1 <- L %>%
  group_by(Modelo) %>%
  slice_head(n = nObs)


resultadosGroupedMean <- L1 %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = mean(Predicted, na.rm = T),
    Real = mean(Real, na.rm = T),
    sMAPE= mean(sMAPE, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
  ) %>%
  ungroup

errMediana <- L1 %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = median(Predicted, na.rm = T),
    Real = median(Real, na.rm = T),
    sMAPE= median(sMAPE, na.rm = TRUE),
    RMSE = median(RMSE, na.rm = TRUE),
    MAPE = median(MAPE, na.rm = TRUE)
  ) %>%
  ungroup


#RMSE
filtrado <- L1 %>%
  filter(RMSE < quantile(L1$RMSE, 0.75))

dividido <- split(filtrado$RMSE, filtrado$Modelo)

colores <- rainbow(length(dividido))

# Crear un boxplot para cada modelo
boxplot(dividido, 
        main = "RMSE por Modelo", 
        ylab = "RMSE",
        col = colores,
        names = names(dividido),
        names.arg = L1$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X

colores <- rainbow(length(errMediana) + 1)

ggplot(data = errMediana, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = RMSE, label = sprintf("%.4f", RMSE)), vjust = -0.25) + 
  labs(
    title = "Comparación de RMSE mediana",
    x = "Modelo",
    y = "RMSE"
  ) +
  scale_fill_manual(values = colores, 
                    name = "") +
  theme(legend.position = "top") 

#sMAPE
filtrado <- L1 %>%
  filter(sMAPE < quantile(L1$sMAPE, 0.75))

dividido <- split(filtrado$sMAPE, filtrado$Modelo)

colores <- rainbow(length(dividido))

boxplot(dividido, 
        main = "sMAPE por Modelo", 
        ylab = "sMAPE",
        col = colores,
        names = names(dividido),
        names.arg = L1$Modelo,
        las = 2)  



colores <- rainbow(length(errMediana) + 1)

ggplot(data = errMediana, aes(x = Modelo, y = sMAPE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = sMAPE, label = sprintf("%.4f", sMAPE)), vjust = -0.25) + 
  labs(
    title = "Comparación de sMAPE mediana",
    x = "Modelo",
    y = "sMAPE"
  ) +
  scale_fill_manual(values = colores, 
                    name = "") +
  theme(legend.position = "top") 

#MAPE
filtrado <- L1 %>%
  filter(MAPE < quantile(L1$MAPE, 0.75))

dividido <- split(filtrado$MAPE, filtrado$Modelo)

colores <- rainbow(length(dividido))

boxplot(dividido, 
        main = "MAPE por Modelo", 
        ylab = "MAPE",
        col = colores,
        names = names(dividido),
        names.arg = L1$Modelo,
        las = 2)  



colores <- rainbow(length(errMediana) + 1)

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


L_Media <- L %>% filter(Modelo == "Media") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_Media, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo MEDIA") +
  theme_minimal()

L_Naive <- L %>% filter(Modelo == "Naive") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_Naive, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NAIVE") +
  theme_minimal()

L_sNaive <- L %>% filter(Modelo == "SNaive") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_sNaive, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo sNAIVE") +
  theme_minimal()

L_Arima <- L %>% filter(Modelo == "Arima") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_Arima, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ARIMA") +
  theme_minimal()

L_ETS <- L %>% filter(Modelo == "ETS") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_ETS, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ETS") +
  theme_minimal()

L_NN <- L %>% filter(Modelo == "NN") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_NN, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NN") +
  theme_minimal()

L_SVM <- L %>% filter(Modelo == "SVM") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_SVM, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo SVM") +
  theme_minimal()


########### CLIENTES NORMALES #################

nObs <- min(table(CUPS$Modelo), na.rm = T)
set.seed(34234)

CUPS1 <- CUPS %>%
  group_by(Modelo) %>%
  slice_head(n = nObs)


resultadosGroupedMean <- CUPS1 %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = mean(Predicted, na.rm = T),
    Real = mean(Real, na.rm = T),
    sMAPE= mean(sMAPE, na.rm = TRUE),
    RMSE = mean(RMSE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
  ) %>%
  ungroup

errMediana <- CUPS1 %>%
  group_by(Modelo) %>%
  summarise(
    Prediccion = median(Predicted, na.rm = T),
    Real = median(Real, na.rm = T),
    sMAPE= median(sMAPE, na.rm = TRUE),
    RMSE = median(RMSE, na.rm = TRUE),
    MAPE = median(MAPE, na.rm = TRUE)
  ) %>%
  ungroup


#RMSE
filtrado <- CUPS1 %>%
  filter(RMSE < quantile(CUPS1$RMSE, 0.75))

dividido <- split(filtrado$RMSE, filtrado$Modelo)

colores <- rainbow(length(dividido))

# Crear un boxplot para cada modelo
boxplot(dividido, 
        main = "RMSE por Modelo", 
        ylab = "RMSE",
        col = colores,
        names = names(dividido),
        names.arg = CUPS1$Modelo,
        las = 2)  # Etiquetas de los modelos en el eje X

colores <- rainbow(length(errMediana) + 1)

ggplot(data = errMediana, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = RMSE, label = sprintf("%.4f", RMSE)), vjust = -0.25) + 
  labs(
    title = "Comparación de RMSE mediana",
    x = "Modelo",
    y = "RMSE"
  ) +
  scale_fill_manual(values = colores, 
                    name = "") +
  theme(legend.position = "top") 

#sMAPE
filtrado <- CUPS1 %>%
  filter(sMAPE < quantile(CUPS1$sMAPE, 0.75))

dividido <- split(filtrado$sMAPE, filtrado$Modelo)

colores <- rainbow(length(dividido))

boxplot(dividido, 
        main = "sMAPE por Modelo", 
        ylab = "sMAPE",
        col = colores,
        names = names(dividido),
        names.arg = CUPS1$Modelo,
        las = 2)  



colores <- rainbow(length(errMediana) + 1)

ggplot(data = errMediana, aes(x = Modelo, y = sMAPE, fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x =Modelo, y = sMAPE, label = sprintf("%.4f", sMAPE)), vjust = -0.25) + 
  labs(
    title = "Comparación de sMAPE mediana",
    x = "Modelo",
    y = "sMAPE"
  ) +
  scale_fill_manual(values = colores, 
                    name = "") +
  theme(legend.position = "top") 

#MAPE
filtrado <- CUPS1 %>%
  filter(MAPE < quantile(CUPS1$MAPE, 0.75))

dividido <- split(filtrado$MAPE, filtrado$Modelo)

colores <- rainbow(length(dividido))

boxplot(dividido, 
        main = "MAPE por Modelo", 
        ylab = "MAPE",
        col = colores,
        names = names(dividido),
        names.arg = CUPS1$Modelo,
        las = 2)  



colores <- rainbow(length(errMediana) + 1)

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


CUPS_Media <- CUPS %>% filter(Modelo == "Media") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_Media, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo MEDIA") +
  theme_minimal()

CUPS_Naive <- CUPS %>% filter(Modelo == "Naive") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_Naive, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NAIVE") +
  theme_minimal()

#CUPS_sNaive <- CUPS %>% filter(Modelo == "Snaive") %>% group_by(Hora) %>% 
#  summarise(Real = mean(Real), Predicted = mean(Predicted))
#
#ggplot(CUPS_sNaive, aes(x = Hora)) +
#  geom_line(aes(y = Real, color = "Real")) +
#  geom_line(aes(y = Predicted, color = "Predicted")) +
#  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
#  labs(x = "Horas", y = "Valores", title = "modelo sNAIVE") +
#  theme_minimal()

CUPS_Arima <- CUPS %>% filter(Modelo == "Arima") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_Arima, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ARIMA") +
  theme_minimal()

CUPS_ETS <- CUPS %>% filter(Modelo == "ETS") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_ETS, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ETS") +
  theme_minimal()

CUPS_NN <- CUPS %>% filter(Modelo == "NN") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_NN, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NN") +
  theme_minimal()

CUPS_SVM <- CUPS %>% filter(Modelo == "SVM") %>% group_by(Hora) %>% 
  summarise(Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_SVM, aes(x = Hora)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo SVM") +
  theme_minimal()
