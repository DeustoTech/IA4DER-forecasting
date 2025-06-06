library(data.table)
library(vtable)
library(zoo)
library(forecast)
library(neuralnet)
library(e1071)
library(doFuture)
library(matrixStats)
library(PMCMRplus)
library(rcompanion)
library(multcompView)
library(boot)
library(stringr)
library(arrow)
library(future.apply)
library(nnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(FNN) 

#FFORMA GRUPO 0 SOLO CON VARIABLE RANDOM
df <- fread("Scripts/Conclusiones/modelos_rw_lr_tipo0.csv")

df_model <- df[, .(random, mape_rw, mape_lr)]
df_model <- na.omit(df_model)

knn_rw <- knn.reg(train = df_model[, .(random)], y = df_model$mape_rw, k = 5)
knn_lr <- knn.reg(train = df_model[, .(random)], y = df_model$mape_lr, k = 5)

valid_rows <- complete.cases(df[, .(tipo, random, mape_rw, mape_lr)])

df[valid_rows, Predicted_mape_rw := knn_rw$pred]
df[valid_rows, Predicted_mape_lr := knn_lr$pred]

df[, MAPE_Predicted_mape_rw := abs(mape_rw - Predicted_mape_rw) / mape_rw * 100]
df[, MAPE_Predicted_mape_lr := abs(mape_lr - Predicted_mape_lr) / mape_lr * 100]

##fforma grupo 0
cols <- c("serie_id", "tiempo", "random", "tipo", "real", "rw", "lr", "mape_rw", "mape_lr")
pb <- df[, ..cols]

modelosC <- c( "rw", "lr")
mape_cols <- paste0("Predicted_mape_", modelosC) #prediccion del error

aux <- df[, c(modelosC, mape_cols), with = FALSE]  # Extraemos todas las columnas necesarias en una tabla auxiliar

setnames(aux, modelosC, paste0("consumo_", modelosC))
setnames(aux, mape_cols, paste0("mape_", modelosC))

# Calcular los pesos = 1 / mape (para cada columna de modeloC)
for (mc in modelosC) {
  aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
}

numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
pbarra_name <- "FFORMA"
pb[, (pbarra_name) := numerador / denominador]
pb[, ("mape_FFORMA") := abs((real - FFORMA) / real) * 100]
fwrite(pb, "Scripts/Conclusiones/modelos_rw_lr_ff_tipo0.csv")

mape_largo <- pb %>%
  select(starts_with("mape_")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "modelo", values_to = "mape") %>%
  mutate(modelo = gsub("mape_", "", modelo))

mape_filtrado <- mape_largo %>%
  group_by(modelo) %>%
  mutate(
    Q1 = quantile(mape, 0.25, na.rm = TRUE),
    Q3 = quantile(mape, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(mape >= Q1 - 1.5 * IQR, mape <= Q3 + 1.5 * IQR) %>%
  ungroup()

medianas <- mape_filtrado %>%
  group_by(modelo) %>%
  summarise(mediana = median(mape, na.rm = TRUE))

ggplot(mape_filtrado, aes(x = modelo, y = mape)) +
  geom_boxplot(fill = "skyblue") +
  geom_text(data = medianas,
            aes(x = modelo, y = mediana + 1, label = round(mediana, 2)),
            vjust = 0,
            size = 3.5,
            fontface = "bold") +
  labs(title = "MAPE error of base models and FFORMA - group 0",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()


########## FFORMA GRUPO 1 SOLO CON VARIABLE RANDOM #########
df <- fread("Scripts/Conclusiones/modelos_rw_lr_tipo1.csv")

df_model <- df[, .(random, mape_rw, mape_lr)]
df_model <- na.omit(df_model)

knn_rw <- knn.reg(train = df_model[, .(random)], y = df_model$mape_rw, k = 5)
knn_lr <- knn.reg(train = df_model[, .(random)], y = df_model$mape_lr, k = 5)

valid_rows <- complete.cases(df[, .(tipo, random, mape_rw, mape_lr)])

df[valid_rows, Predicted_mape_rw := knn_rw$pred]
df[valid_rows, Predicted_mape_lr := knn_lr$pred]

df[, MAPE_Predicted_mape_rw := abs(mape_rw - Predicted_mape_rw) / mape_rw * 100]
df[, MAPE_Predicted_mape_lr := abs(mape_lr - Predicted_mape_lr) / mape_lr * 100]

##fforma grupo 1
cols <- c("serie_id", "tiempo", "random", "tipo", "real", "rw", "lr", "mape_rw", "mape_lr")
pb <- df[, ..cols]

modelosC <- c( "rw", "lr")
mape_cols <- paste0("Predicted_mape_", modelosC) #prediccion del error

aux <- df[, c(modelosC, mape_cols), with = FALSE]  # Extraemos todas las columnas necesarias en una tabla auxiliar

setnames(aux, modelosC, paste0("consumo_", modelosC))
setnames(aux, mape_cols, paste0("mape_", modelosC))

# Calcular los pesos = 1 / mape (para cada columna de modeloC)
for (mc in modelosC) {
  aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
}

numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
pbarra_name <- "FFORMA"
pb[, (pbarra_name) := numerador / denominador]
pb[, ("mape_FFORMA") := abs((real - FFORMA) / real) * 100]
fwrite(pb, "Scripts/Conclusiones/modelos_rw_lr_ff_tipo1.csv")

mape_largo <- pb %>%
  select(starts_with("mape_")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "modelo", values_to = "mape") %>%
  mutate(modelo = gsub("mape_", "", modelo))

mape_filtrado <- mape_largo %>%
  group_by(modelo) %>%
  mutate(
    Q1 = quantile(mape, 0.25, na.rm = TRUE),
    Q3 = quantile(mape, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(mape >= Q1 - 1.5 * IQR, mape <= Q3 + 1.5 * IQR) %>%
  ungroup()

medianas <- mape_filtrado %>%
  group_by(modelo) %>%
  summarise(mediana = median(mape, na.rm = TRUE))

ggplot(mape_filtrado, aes(x = modelo, y = mape)) +
  geom_boxplot(fill = "skyblue") +
  geom_text(data = medianas,
            aes(x = modelo, y = mediana + 1, label = round(mediana, 2)),
            vjust = 0,
            size = 3.5,
            fontface = "bold") +
  labs(title = "MAPE error of base models and FFORMA - group 1",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()


#########3 GRUPOS 0 Y 1##########
######## SOLO RANDOM
df <- fread("Scripts/Conclusiones/modelos_rw_lr_tipos0y1.csv")

df_model <- df[, .(tipo, random, mape_rw, mape_lr)]
df_model <- na.omit(df_model)

knn_rw <- knn.reg(train = df_model[, .( random)], y = df_model$mape_rw, k = 5)
knn_lr <- knn.reg(train = df_model[, .(random)], y = df_model$mape_lr, k = 5)

valid_rows <- complete.cases(df[, .(tipo, random, mape_rw, mape_lr)])

df[valid_rows, Predicted_mape_rw := knn_rw$pred]
df[valid_rows, Predicted_mape_lr := knn_lr$pred]

df[, MAPE_Predicted_mape_rw := abs(mape_rw - Predicted_mape_rw) / mape_rw * 100]
df[, MAPE_Predicted_mape_lr := abs(mape_lr - Predicted_mape_lr) / mape_lr * 100]

##fforma grupos 0 y 1
cols <- c("serie_id", "tiempo", "random", "tipo", "valor", "rw", "lr", "mape_rw", "mape_lr")
pb <- df[, ..cols]

modelosC <- c( "rw", "lr")
mape_cols <- paste0("Predicted_mape_", modelosC) #prediccion del error

aux <- df[, c(modelosC, mape_cols), with = FALSE]  # Extraemos todas las columnas necesarias en una tabla auxiliar

setnames(aux, modelosC, paste0("consumo_", modelosC))
setnames(aux, mape_cols, paste0("mape_", modelosC))

# Calcular los pesos = 1 / mape (para cada columna de modeloC)
for (mc in modelosC) {
  aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
}

numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
pbarra_name <- "FFORMA"
pb[, (pbarra_name) := numerador / denominador]
pb[, ("mape_FFORMA") := abs((valor - FFORMA) / valor) * 100]
fwrite(pb, "Scripts/Conclusiones/modelos_rw_lr_ffr_tipos0y1.csv")

mape_largo <- pb %>%
  select(starts_with("mape_")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "modelo", values_to = "mape") %>%
  mutate(modelo = gsub("mape_", "", modelo))

mape_filtrado <- mape_largo %>%
  group_by(modelo) %>%
  mutate(
    Q1 = quantile(mape, 0.25, na.rm = TRUE),
    Q3 = quantile(mape, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(mape >= Q1 - 1.5 * IQR, mape <= Q3 + 1.5 * IQR) %>%
  ungroup()

medianas <- mape_filtrado %>%
  group_by(modelo) %>%
  summarise(mediana = median(mape, na.rm = TRUE))

ggplot(mape_filtrado, aes(x = modelo, y = mape)) +
  geom_boxplot(fill = "skyblue") +
  geom_text(data = medianas,
            aes(x = modelo, y = mediana + 1, label = round(mediana, 2)),
            vjust = 0,
            size = 3.5,
            fontface = "bold") +
  labs(title = "MAPE error of base models and FFORMA - group 0 and 1",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()


######## RANDOM Y TIPO
df <- fread("Scripts/Conclusiones/modelos_rw_lr_tipos0y1.csv")

df_model <- df[, .(tipo, random, mape_rw, mape_lr)]
df_model <- na.omit(df_model)

knn_rw <- knn.reg(train = df_model[, .(tipo, random)], y = df_model$mape_rw, k = 5)
knn_lr <- knn.reg(train = df_model[, .(tipo, random)], y = df_model$mape_lr, k = 5)

valid_rows <- complete.cases(df[, .(tipo, random, mape_rw, mape_lr)])

df[valid_rows, Predicted_mape_rw := knn_rw$pred]
df[valid_rows, Predicted_mape_lr := knn_lr$pred]

df[, MAPE_Predicted_mape_rw := abs(mape_rw - Predicted_mape_rw) / mape_rw * 100]
df[, MAPE_Predicted_mape_lr := abs(mape_lr - Predicted_mape_lr) / mape_lr * 100]

##fforma grupos 0 y 1
cols <- c("serie_id", "tiempo", "random", "tipo", "valor", "rw", "lr", "mape_rw", "mape_lr")
pb <- df[, ..cols]

modelosC <- c( "rw", "lr")
mape_cols <- paste0("Predicted_mape_", modelosC) #prediccion del error

aux <- df[, c(modelosC, mape_cols), with = FALSE]  # Extraemos todas las columnas necesarias en una tabla auxiliar

setnames(aux, modelosC, paste0("consumo_", modelosC))
setnames(aux, mape_cols, paste0("mape_", modelosC))

# Calcular los pesos = 1 / mape (para cada columna de modeloC)
for (mc in modelosC) {
  aux[, paste0("w_", mc) := 1 / get(paste0("mape_", mc))]
}

numerador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("consumo_", mc)) * get(paste0("w_", mc)))) ]
denominador <- aux[, Reduce(`+`, lapply(modelosC, function(mc) get(paste0("w_", mc)))) ]
pbarra_name <- "FFORMA"
pb[, (pbarra_name) := numerador / denominador]
pb[, ("mape_FFORMA") := abs((valor - FFORMA) / valor) * 100]
fwrite(pb, "Scripts/Conclusiones/modelos_rw_lr_ffrt_tipos0y1.csv")

mape_largo <- pb %>%
  select(starts_with("mape_")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "modelo", values_to = "mape") %>%
  mutate(modelo = gsub("mape_", "", modelo))

mape_filtrado <- mape_largo %>%
  group_by(modelo) %>%
  mutate(
    Q1 = quantile(mape, 0.25, na.rm = TRUE),
    Q3 = quantile(mape, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(mape >= Q1 - 1.5 * IQR, mape <= Q3 + 1.5 * IQR) %>%
  ungroup()

medianas <- mape_filtrado %>%
  group_by(modelo) %>%
  summarise(mediana = median(mape, na.rm = TRUE))

ggplot(mape_filtrado, aes(x = modelo, y = mape)) +
  geom_boxplot(fill = "skyblue") +
  geom_text(data = medianas,
            aes(x = modelo, y = mediana + 1, label = round(mediana, 2)),
            vjust = 0,
            size = 3.5,
            fontface = "bold") +
  labs(title = "MAPE error of base models and FFORMA - group 0 and 1",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()
