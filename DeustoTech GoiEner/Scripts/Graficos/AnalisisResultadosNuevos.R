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
percentiles <- CT_Media %>%
  summarise(
    p25 = quantile(Real, 0.25),
    p75 = quantile(Real, 0.75)
  )

CT_Media <- CT %>% filter(Modelo == "Media") %>% group_by(Hora) %>% 
                    summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
                              p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
                              Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_Media, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo MEDIA") +
  theme_minimal()


CT_Naive <- CT %>% filter(Modelo == "Naive") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_Naive, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NAIVE") +
  theme_minimal()



CT_sNaive <- CT %>% filter(Modelo == "SNaive") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_sNaive, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo sNAIVE") +
  theme_minimal()



CT_Arima <- CT %>% filter(Modelo == "Arima") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_Arima, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ARIMA") +
  theme_minimal()


CT_ETS <- CT %>% filter(Modelo == "ETS") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_ETS, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ETS") +
  theme_minimal()

CT_NN <- CT %>% filter(Modelo == "NN") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_NN, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NN") +
  theme_minimal()


CT_SVM <- CT %>% filter(Modelo == "SVM") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CT_SVM, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
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
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_Media, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo MEDIA") +
  theme_minimal()


L_Naive <- L %>% filter(Modelo == "Naive") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_Naive, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NAIVE") +
  theme_minimal()



L_sNaive <- L %>% filter(Modelo == "SNaive") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_sNaive, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo sNAIVE") +
  theme_minimal()



L_Arima <- L %>% filter(Modelo == "Arima") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_Arima, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ARIMA") +
  theme_minimal()


L_ETS <- L %>% filter(Modelo == "ETS") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_ETS, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ETS") +
  theme_minimal()

L_NN <- L %>% filter(Modelo == "NN") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_NN, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NN") +
  theme_minimal()


L_SVM <- L %>% filter(Modelo == "SVM") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(L_SVM, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
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
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_Media, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo MEDIA") +
  theme_minimal()


CUPS_Naive <- CUPS %>% filter(Modelo == "Naive") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_Naive, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NAIVE") +
  theme_minimal()


#
#CUPS_sNaive <- CUPS %>% filter(Modelo == "SNaive") %>% group_by(Hora) %>% 
#  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
#            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
#            Real = mean(Real), Predicted = mean(Predicted))
#
#ggplot(CUPS_sNaive, aes(x = Hora)) +
#  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
#  geom_line(aes(y = Real, color = "Real")) +
#  
#  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
#  geom_line(aes(y = Predicted, color = "Predicted")) +
#  
#  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
#  labs(x = "Horas", y = "Valores", title = "modelo sNAIVE") +
#  theme_minimal()



CUPS_Arima <- CUPS %>% filter(Modelo == "Arima") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_Arima, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ARIMA") +
  theme_minimal()


CUPS_ETS <- CUPS %>% filter(Modelo == "ETS") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_ETS, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo ETS") +
  theme_minimal()

CUPS_NN <- CUPS %>% filter(Modelo == "NN") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_NN, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo NN") +
  theme_minimal()


CUPS_SVM <- CUPS %>% filter(Modelo == "SVM") %>% group_by(Hora) %>% 
  summarise(r25 = quantile(Real, 0.25), r75 = quantile(Real, 0.75),
            p25 = quantile(Predicted, 0.25), p75 = quantile(Predicted, 0.75),
            Real = mean(Real), Predicted = mean(Predicted))

ggplot(CUPS_SVM, aes(x = Hora)) +
  geom_ribbon(aes(ymin = r25, ymax = r75), fill = "#C3DBF7", alpha = 0.3) +  
  geom_line(aes(y = Real, color = "Real")) +
  
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#F7CBC3", alpha = 0.3) +  
  geom_line(aes(y = Predicted, color = "Predicted")) +
  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +
  labs(x = "Horas", y = "Valores", title = "modelo SVM") +
  theme_minimal()




###################################################################################
################################## GRÁFICOS NUEVOS ################################
###################################################################################

#esto luego hay que hacerlo solo para una serie temporal, pero todavia no he 
#vuelto a ejecutar lo otro y los resultados no tienen el nombre de la serie 
#temporal para poder filtrar una y quedarnos solo con una

########### CENTROS DE TRANSFORMACIÓN #################

### MEDIA ###
CT_Media_2 <- CT %>% filter(Modelo == "Media") %>% select(Hora, Real, Predicted)
  
CT_Media_2 <- CT_Media_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CT_Media_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CT_Media_2$Valor, c(0, 0.75))

ggplot(CT_Media_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()


### NAIVE ###
CT_Naive_2 <- CT %>% filter(Modelo == "Naive") %>% select(Hora, Real, Predicted)

CT_Naive_2 <- CT_Naive_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CT_Naive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CT_Naive_2$Valor, c(0, 0.75))

ggplot(CT_Naive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### sNAIVE ###
CT_sNaive_2 <- CT %>% filter(Modelo == "SNaive") %>% select(Hora, Real, Predicted)

CT_sNaive_2 <- CT_sNaive_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CT_sNaive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CT_sNaive_2$Valor, c(0, 0.75))

ggplot(CT_sNaive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### ARIMA ###
CT_Arima_2 <- CT %>% filter(Modelo == "Arima") %>% select(Hora, Real, Predicted)

CT_Arima_2 <- CT_Arima_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CT_Arima_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CT_Arima_2$Valor, c(0, 0.75))

ggplot(CT_Arima_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### ETS ###
CT_ETS_2 <- CT %>% filter(Modelo == "ETS") %>% select(Hora, Real, Predicted)

CT_ETS_2 <- CT_ETS_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CT_ETS_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CT_ETS_2$Valor, c(0, 0.75))

ggplot(CT_ETS_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### NN ###
CT_NN_2 <- CT %>% filter(Modelo == "NN") %>% select(Hora, Real, Predicted)

CT_NN_2 <- CT_NN_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CT_NN_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CT_NN_2$Valor, c(0, 0.75))

ggplot(CT_NN_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### SVM ###
CT_SVM_2 <- CT %>% filter(Modelo == "SVM") %>% select(Hora, Real, Predicted)

CT_SVM_2 <- CT_SVM_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CT_SVM_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CT_SVM_2$Valor, c(0, 0.75))

ggplot(CT_SVM_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

########################## LINEAS ############################

L_Media_2 <- L %>% filter(Modelo == "Media") %>% select(Hora, Real, Predicted)

L_Media_2 <- L_Media_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(L_Media_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(L_Media_2$Valor, c(0, 0.75))

ggplot(L_Media_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### NAIVE ###
L_Naive_2 <- L %>% filter(Modelo == "Naive") %>% select(Hora, Real, Predicted)

L_Naive_2 <- L_Naive_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(L_Naive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(L_Naive_2$Valor, c(0, 0.75))

ggplot(L_Naive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### sNAIVE ###
L_sNaive_2 <- L %>% filter(Modelo == "SNaive") %>% select(Hora, Real, Predicted)

L_sNaive_2 <- L_sNaive_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(L_sNaive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
# este rarete
y_range <- quantile(L_sNaive_2$Valor, c(0, 0.75))

ggplot(L_sNaive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### ARIMA ###
L_Arima_2 <- L %>% filter(Modelo == "Arima") %>% select(Hora, Real, Predicted)

L_Arima_2 <- L_Arima_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(L_Arima_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(L_Arima_2$Valor, c(0, 0.75))

ggplot(L_Arima_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### ETS ###
L_ETS_2 <- L %>% filter(Modelo == "ETS") %>% select(Hora, Real, Predicted)

L_ETS_2 <- L_ETS_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(L_ETS_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(L_ETS_2$Valor, c(0, 0.75))

ggplot(L_ETS_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### NN ###
L_NN_2 <- L %>% filter(Modelo == "NN") %>% select(Hora, Real, Predicted)

L_NN_2 <- L_NN_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(L_NN_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(L_NN_2$Valor, c(0, 0.75))

ggplot(L_NN_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### SVM ###
L_SVM_2 <- L %>% filter(Modelo == "SVM") %>% select(Hora, Real, Predicted)

L_SVM_2 <- L_SVM_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(L_SVM_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(L_SVM_2$Valor, c(0, 0.75))

ggplot(L_SVM_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()


################### CLIENTES NORMALES ########################

CUPS_Media_2 <- CUPS %>% filter(Modelo == "Media") %>% select(Hora, Real, Predicted)

CUPS_Media_2 <- CUPS_Media_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CUPS_Media_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CUPS_Media_2$Valor, c(0, 0.75))

ggplot(CUPS_Media_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### NAIVE ###
CUPS_Naive_2 <- CUPS %>% filter(Modelo == "Naive") %>% select(Hora, Real, Predicted)

CUPS_Naive_2 <- CUPS_Naive_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CUPS_Naive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CUPS_Naive_2$Valor, c(0, 0.75))

ggplot(CUPS_Naive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### sNAIVE ###
#CUPS_sNaive_2 <- CUPS %>% filter(Modelo == "SNaive") %>% select(Hora, Real, Predicted)
#
#CUPS_sNaive_2 <- CUPS_sNaive_2 %>%
#  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")
#
# CON OUTLIERS
#ggplot(CUPS_sNaive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
#  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
#  scale_fill_manual(values = c("blue", "red")) +
#  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
#  theme_minimal()
#
#SIN  OUTLIERS
#y_range <- quantile(CUPS_sNaive_2$Valor, c(0, 0.75))
#
#ggplot(CUPS_sNaive_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
#  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
#  scale_fill_manual(values = c("blue", "red")) +
#  ylim(y_range) + 
#  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
#  theme_minimal()

### ARIMA ###
CUPS_Arima_2 <- CUPS %>% filter(Modelo == "Arima") %>% select(Hora, Real, Predicted)

CUPS_Arima_2 <- CUPS_Arima_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CUPS_Arima_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CUPS_Arima_2$Valor, c(0, 0.75))

ggplot(CUPS_Arima_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### ETS ###
CUPS_ETS_2 <- CUPS %>% filter(Modelo == "ETS") %>% select(Hora, Real, Predicted)

CUPS_ETS_2 <- CUPS_ETS_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CUPS_ETS_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CUPS_ETS_2$Valor, c(0, 0.75))

ggplot(CUPS_ETS_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### NN ###
CUPS_NN_2 <- CUPS %>% filter(Modelo == "NN") %>% select(Hora, Real, Predicted)

CUPS_NN_2 <- CUPS_NN_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CUPS_NN_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CUPS_NN_2$Valor, c(0, 0.75))

ggplot(CUPS_NN_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

### SVM ###
CUPS_SVM_2 <- CUPS %>% filter(Modelo == "SVM") %>% select(Hora, Real, Predicted)

CUPS_SVM_2 <- CUPS_SVM_2 %>%
  pivot_longer(cols = c(Real, Predicted), names_to = "Tipo", values_to = "Valor")

# CON OUTLIERS
ggplot(CUPS_SVM_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

#SIN  OUTLIERS
y_range <- quantile(CUPS_SVM_2$Valor, c(0, 0.75))

ggplot(CUPS_SVM_2, aes(x = factor(Hora), y = Valor, fill = Tipo)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.35) +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(y_range) + 
  labs(x = "Hora", y = "Valores", title = "Boxplots de Valores Reales y Predichos por Hora") +
  theme_minimal()

