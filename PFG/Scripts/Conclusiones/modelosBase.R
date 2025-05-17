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

plan(multisession)

F_DAYS <- 7
T_DAYS <- 143
MODELS <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")


df <- fread("Scripts/Conclusiones/series0.csv")
resultados_completos <- list()

for (sid in unique(df$serie_id)) {
  serie_data <- df %>% filter(serie_id == sid) %>% arrange(tiempo)
  r <- ts(serie_data$valor, frequency = 1)
  
  train <- window(r, end = T_DAYS)
  test  <- window(r, start = T_DAYS + 1)
  
  f <- data.frame(real = test)
  
  # Modelos simples
  f$mean  <- rep(mean(train), F_DAYS)
  f$rw    <- rep(tail(train, 1), F_DAYS)
  f$naive <- stats::lag(train, -F_DAYS) %>% tail(F_DAYS)
  
  f$simple <- rowMeans(data.frame(
    a = stats::lag(train, -F_DAYS)     %>% tail(F_DAYS),
    b = stats::lag(train, -F_DAYS*2)   %>% tail(F_DAYS),
    c = stats::lag(train, -F_DAYS*3)   %>% tail(F_DAYS)
  ), na.rm = TRUE)
  
  # Entrenamiento para modelos con variables
  if (length(train) > F_DAYS) {
    lagged <- embed(train, 2)
    TRAINSET <- data.frame(real = lagged[,1], past = lagged[,2])
    PREDICT  <- data.frame(past = tail(train, F_DAYS))
  } else {
    TRAINSET <- NULL
    PREDICT <- NULL
  }
  
  # Modelos avanzados
  tryCatch({LM    <- lm(real ~ past, data = TRAINSET)}, error=function(e){})
  tryCatch({ARIMA <- auto.arima(train)}, error=function(e){})
  tryCatch({ES    <- ses(train, h = F_DAYS)}, error=function(e){})
  tryCatch({NN    <- nnetar(train)}, error=function(e){})
  tryCatch({SVM   <- tune(svm, real ~ past, data = TRAINSET, ranges = list(epsilon = seq(0,1,0.2), cost = seq(1,100,10)))}, error=function(e){})
  
  # Predicciones
  tryCatch({f$lr    <- forecast(LM, newdata = PREDICT)$mean}, error=function(e){})
  tryCatch({f$arima <- forecast(ARIMA, h = F_DAYS)$mean}, error=function(e){})
  tryCatch({f$ses   <- forecast(ES, h = F_DAYS)$mean}, error=function(e){})
  tryCatch({f$ann   <- forecast(NN, h = F_DAYS)$mean}, error=function(e){})
  tryCatch({f$svm   <- predict(SVM$best.model, newdata = PREDICT)}, error=function(e){})
  
  # Ensamble
  f$ens <- rowMedians(as.matrix(f[, MODELS[1:9]]), na.rm = TRUE)
  
  # Extraer las columnas originales de df para los últimos 7 días
  ultimos_dias <- serie_data %>%
    arrange(tiempo) %>%
    slice((T_DAYS + 1):(T_DAYS + F_DAYS))
  
  # Asegurar que real es igual a valor para claridad
  f$real <- ultimos_dias$valor
  
  # Combinar columnas originales + predicciones
  f_completo <- cbind(ultimos_dias, f[, c("real", MODELS)])
  
  # Guardar en lista
  resultados_completos[[length(resultados_completos) + 1]] <- f_completo

}

# Guardar CSV
final_results <- do.call(rbind, resultados_completos)

for (m in MODELS) {
  final_results[[m]] <- as.numeric(final_results[[m]])
}
for (m in MODELS) {
  final_results[[paste0("mape_", m)]] <- abs((final_results$real - final_results[[m]]) / final_results$real) * 100
}


write.csv(final_results, "Scripts/Conclusiones/modelosBaseTODOS_grupo0.csv", row.names = FALSE)


mape_largo <- final_results %>%
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
  labs(title = "MAPE error of base models",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()


############### SOLO RW Y  LR #####################
F_DAYS <- 7
T_DAYS <- 143
MODELS <- c("rw", "lr")


df <- fread("Scripts/Conclusiones/series0.csv")
resultados_completos <- list()

for (sid in unique(df$serie_id)) {
  serie_data <- df %>% filter(serie_id == sid) %>% arrange(tiempo)
  r <- ts(serie_data$valor, frequency = 1)
  
  train <- window(r, end = T_DAYS)
  test  <- window(r, start = T_DAYS + 1)
  
  f <- data.frame(real = test)
  
  # Modelos simples
  f$rw    <- rep(tail(train, 1), F_DAYS)

  # Entrenamiento para modelos con variables
  if (length(train) > F_DAYS) {
    lagged <- embed(train, 2)
    TRAINSET <- data.frame(real = lagged[,1], past = lagged[,2])
    PREDICT  <- data.frame(past = tail(train, F_DAYS))
  } else {
    TRAINSET <- NULL
    PREDICT <- NULL
  }
  
  # Modelos avanzados
  tryCatch({LM    <- lm(real ~ past, data = TRAINSET)}, error=function(e){})
  # Predicciones
  tryCatch({f$lr    <- forecast(LM, newdata = PREDICT)$mean}, error=function(e){})
  
  # Extraer las columnas originales de df para los últimos 7 días
  ultimos_dias <- serie_data %>%
    arrange(tiempo) %>%
    slice((T_DAYS + 1):(T_DAYS + F_DAYS))
  
  # Asegurar que real es igual a valor para claridad
  f$real <- ultimos_dias$valor
  
  # Combinar columnas originales + predicciones
  f_completo <- cbind(ultimos_dias, f[, c("real", MODELS)])
  
  # Guardar en lista
  resultados_completos[[length(resultados_completos) + 1]] <- f_completo
  
}

# Guardar CSV
final_results <- do.call(rbind, resultados_completos)

for (m in MODELS) {
  final_results[[m]] <- as.numeric(final_results[[m]])
}
for (m in MODELS) {
  final_results[[paste0("mape_", m)]] <- abs((final_results$real - final_results[[m]]) / final_results$real) * 100
}


write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_grupo0.csv", row.names = FALSE)


mape_largo <- final_results %>%
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
  labs(title = "MAPE error of base models - group 0",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()



######### GRUPO 1 ############
F_DAYS <- 7
T_DAYS <- 143
MODELS <- c("rw", "lr")


df <- fread("Scripts/Conclusiones/series1.csv")
resultados_completos <- list()

for (sid in unique(df$serie_id)) {
  serie_data <- df %>% filter(serie_id == sid) %>% arrange(tiempo)
  r <- ts(serie_data$valor, frequency = 1)
  
  train <- window(r, end = T_DAYS)
  test  <- window(r, start = T_DAYS + 1)
  
  f <- data.frame(real = test)
  
  # Modelos simples
  f$rw    <- rep(tail(train, 1), F_DAYS)
  
  # Entrenamiento para modelos con variables
  if (length(train) > F_DAYS) {
    lagged <- embed(train, 2)
    TRAINSET <- data.frame(real = lagged[,1], past = lagged[,2])
    PREDICT  <- data.frame(past = tail(train, F_DAYS))
  } else {
    TRAINSET <- NULL
    PREDICT <- NULL
  }
  
  # Modelos avanzados
  tryCatch({LM    <- lm(real ~ past, data = TRAINSET)}, error=function(e){})
  # Predicciones
  tryCatch({f$lr    <- forecast(LM, newdata = PREDICT)$mean}, error=function(e){})
  
  # Extraer las columnas originales de df para los últimos 7 días
  ultimos_dias <- serie_data %>%
    arrange(tiempo) %>%
    slice((T_DAYS + 1):(T_DAYS + F_DAYS))
  
  # Asegurar que real es igual a valor para claridad
  f$real <- ultimos_dias$valor
  
  # Combinar columnas originales + predicciones
  f_completo <- cbind(ultimos_dias, f[, c("real", MODELS)])
  
  # Guardar en lista
  resultados_completos[[length(resultados_completos) + 1]] <- f_completo
  
}

# Guardar CSV
final_results <- do.call(rbind, resultados_completos)

for (m in MODELS) {
  final_results[[m]] <- as.numeric(final_results[[m]])
}
for (m in MODELS) {
  final_results[[paste0("mape_", m)]] <- abs((final_results$real - final_results[[m]]) / final_results$real) * 100
}


write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_grupo1.csv", row.names = FALSE)


mape_largo <- final_results %>%
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
  labs(title = "MAPE error of base models - group 1",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()


########## JUNTAR SERIES0 Y SERIES1.
series0 <- fread("Scripts/Conclusiones/series0.csv")
series1 <- fread("Scripts/Conclusiones/series1.csv")

series0 <- as.data.table(series0)
series1 <- as.data.table(series1)
series0[, grupo := NULL]
series1[, serie_id := serie_id + 100]
series <- rbind(series0, series1)

F_DAYS <- 7
T_DAYS <- 143
MODELS <- c("rw", "lr")


resultados_completos <- list()

for (sid in unique(series$serie_id)) {
  serie_data <- series %>% filter(serie_id == sid) %>% arrange(tiempo)
  r <- ts(serie_data$valor, frequency = 1)
  
  train <- window(r, end = T_DAYS)
  test  <- window(r, start = T_DAYS + 1)
  
  f <- data.frame(real = test)
  
  # Modelos simples
  f$rw    <- rep(tail(train, 1), F_DAYS)
  
  # Entrenamiento para modelos con variables
  if (length(train) > F_DAYS) {
    lagged <- embed(train, 2)
    TRAINSET <- data.frame(real = lagged[,1], past = lagged[,2])
    PREDICT  <- data.frame(past = tail(train, F_DAYS))
  } else {
    TRAINSET <- NULL
    PREDICT <- NULL
  }
  
  # Modelos avanzados
  tryCatch({LM    <- lm(real ~ past, data = TRAINSET)}, error=function(e){})
  # Predicciones
  tryCatch({f$lr    <- forecast(LM, newdata = PREDICT)$mean}, error=function(e){})
  
  # Extraer las columnas originales de df para los últimos 7 días
  ultimos_dias <- serie_data %>%
    arrange(tiempo) %>%
    slice((T_DAYS + 1):(T_DAYS + F_DAYS))
  
  # Asegurar que real es igual a valor para claridad
  f$real <- ultimos_dias$valor
  
  # Combinar columnas originales + predicciones
  f_completo <- cbind(ultimos_dias, f[, c("real", MODELS)])
  
  # Guardar en lista
  resultados_completos[[length(resultados_completos) + 1]] <- f_completo
  
}

# Guardar CSV
final_results <- do.call(rbind, resultados_completos)

for (m in MODELS) {
  final_results[[m]] <- as.numeric(final_results[[m]])
}
for (m in MODELS) {
  final_results[[paste0("mape_", m)]] <- abs((final_results$real - final_results[[m]]) / final_results$real) * 100
}


write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_grupo0y1.csv", row.names = FALSE)


mape_largo <- final_results %>%
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
  labs(title = "MAPE error of base models - group 1",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()


#OTRA VEZ PERO 186 SERIES ENTERAS PARA PREDECIR Y 14 ENTERAS DE TEST


# Leer y preparar datos
series0 <- fread("Scripts/Conclusiones/series0.csv")
series1 <- fread("Scripts/Conclusiones/series1.csv")

series0 <- as.data.table(series0)
series1 <- as.data.table(series1)
series0[, grupo := NULL]
series1[, serie_id := serie_id + 100]
series <- rbind(series0, series1)

# Definir IDs
train_ids <- c(1:93, 101:110)
test_ids  <- c(93:100, 193:200)

MODELS <- c("rw", "lr")
resultados_completos <- list()

# Dataset de entrenamiento completo
train_data <- series[serie_id %in% train_ids]

# Filtrar solo series con al menos 2 observaciones
valid_train_ids <- train_data[, .N, by = serie_id][N >= 2, serie_id]

# Aplicar embed por serie válida
lagged_all <- train_data[serie_id %in% valid_train_ids, .(
  embed_result = list(embed(valor, 2))
), by = serie_id]

# Expandir lista en data.frame
lagged_all_df <- do.call(rbind, lapply(lagged_all$embed_result, as.data.frame))

# Ahora crear TRAINSET
TRAINSET <- data.frame(
  real = lagged_all_df$V1,
  past = lagged_all_df$V2
)

# Entrenar modelo lineal
LM <- tryCatch({
  lm(real ~ past, data = TRAINSET)
}, error = function(e) NULL)

# Evaluar sobre las series de test
for (sid in test_ids) {
  serie_data <- series %>% filter(serie_id == sid) %>% arrange(tiempo)
  r <- ts(serie_data$valor, frequency = 1)
  
  # Modelo rw
  rw_pred <- rep(tail(r, 1), length(r))
  
  # Modelo lr
  if (!is.null(LM) && length(r) > 1) {
    PREDICT <- data.frame(past = head(r, -1))
    lr_pred <- tryCatch({
      c(NA, forecast(LM, newdata = PREDICT)$mean)
    }, error = function(e) rep(NA, length(r)))
  } else {
    lr_pred <- rep(NA, length(r))
  }
  
  f_completo <- data.frame(
    serie_data,
    real = serie_data$valor,
    rw = rw_pred,
    lr = lr_pred
  )
  
  resultados_completos[[length(resultados_completos) + 1]] <- f_completo
}

# Unir y calcular MAPE
final_results <- do.call(rbind, resultados_completos)

for (m in MODELS) {
  final_results[[m]] <- as.numeric(final_results[[m]])
  final_results[[paste0("mape_", m)]] <- abs((final_results$real - final_results[[m]]) / final_results$real) * 100
}

# Guardar CSV
write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_globals.csv", row.names = FALSE)



######## CON NUESTRAS SERIES

seriesN <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
seriesN <- seriesN %>% select(id, real, rw_pred, rw_mape)

mape_promedios <- seriesN[, .(mape_medio = median(rw_mape, na.rm = TRUE)), by = id]
mejores_ids <- mape_promedios[order(mape_medio)][1:100, id]
mejores_series <- seriesN[id %in% mejores_ids]
series <- mejores_series[, head(.SD, 150), by = id]

id_map <- data.table(
  id = unique(series$id),
  nuevo_id = 1:100
)
series0 <- merge(series, id_map, by = "id")
series0[, id := nuevo_id][, nuevo_id := NULL]
setorder(series0, id)

series0$tipo <- 0
set.seed(123)
series0[, random := sample(1:10, .N, replace = TRUE)]


series1 <- fread("Scripts/Conclusiones/series1.csv")

series0 <- as.data.table(series0)
series1 <- as.data.table(series1)
series1[, serie_id := serie_id + 100]
series0$valor <- series0$real
series0$tiempo <- series1$tiempo
series0$serie_id <- series0$id
series0 <- series0 %>% select(-real, - rw_pred, - rw_mape, - id)
series <- rbind(series0, series1)

# Definir IDs
train_ids <- c(1:93, 101:110)
test_ids  <- c(93:100, 193:200)

MODELS <- c("rw", "lr")
resultados_completos <- list()

# Dataset de entrenamiento completo
train_data <- series[serie_id %in% train_ids]

# Filtrar solo series con al menos 2 observaciones
valid_train_ids <- train_data[, .N, by = serie_id][N >= 2, serie_id]

# Aplicar embed por serie válida
lagged_all <- train_data[serie_id %in% valid_train_ids, .(
  embed_result = list(embed(valor, 2))
), by = serie_id]

# Expandir lista en data.frame
lagged_all_df <- do.call(rbind, lapply(lagged_all$embed_result, as.data.frame))

# Ahora crear TRAINSET
TRAINSET <- data.frame(
  real = lagged_all_df$V1,
  past = lagged_all_df$V2
)

# Entrenar modelo lineal
LM <- tryCatch({
  lm(real ~ past, data = TRAINSET)
}, error = function(e) NULL)

# Evaluar sobre las series de test
for (sid in test_ids) {
  serie_data <- series %>% filter(serie_id == sid) %>% arrange(tiempo)
  r <- ts(serie_data$valor, frequency = 1)
  
  # Modelo rw
  rw_pred <- rep(tail(r, 1), length(r))
  
  # Modelo lr
  if (!is.null(LM) && length(r) > 1) {
    PREDICT <- data.frame(past = head(r, -1))
    lr_pred <- tryCatch({
      c(NA, forecast(LM, newdata = PREDICT)$mean)
    }, error = function(e) rep(NA, length(r)))
  } else {
    lr_pred <- rep(NA, length(r))
  }
  
  f_completo <- data.frame(
    serie_data,
    real = serie_data$valor,
    rw = rw_pred,
    lr = lr_pred
  )
  
  resultados_completos[[length(resultados_completos) + 1]] <- f_completo
}

# Unir y calcular MAPE
final_results <- do.call(rbind, resultados_completos)

for (m in MODELS) {
  final_results[[m]] <- as.numeric(final_results[[m]])
  final_results[[paste0("mape_", m)]] <- abs((final_results$real - final_results[[m]]) / final_results$real) * 100
}

# Guardar CSV
write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_globals.csv", row.names = FALSE)
