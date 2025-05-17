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

series0 <- fread("Scripts/Conclusiones/series0.csv")
series1 <- fread("Scripts/Conclusiones/series1.csv")
series0[, grupo := NULL]
series1[, serie_id := serie_id + 100]
series <- rbind(series0, series1)

train_ids <- c(1:92, 101:192)
test_ids  <- c(93:100, 193:200)

MODELS <- c("rw", "lr")
FREQ <- 1 

lagged_rows <- list()

for (sid in train_ids) {
  serie_data <- series[serie_id == sid][order(tiempo)]
  vals <- serie_data$valor
  if (length(vals) >= 2) {
    lagged <- embed(vals, 2)
    lagged_rows[[length(lagged_rows)+1]] <- data.frame(real = lagged[,1], past = lagged[,2])
  }
}
TRAINSET <- do.call(rbind, lagged_rows)


modelo_lr <- lm(real ~ past, data = TRAINSET)

resultados <- list()

for (sid in test_ids) {
  serie_data <- series[serie_id == sid][order(tiempo)]
  r <- ts(serie_data$valor, frequency = FREQ)
  
  # Modelo RW: persistencia última observación conocida
  rw_pred <- rep(tail(r, 1), length(r))
  
  # Modelo LR: usar valores rezagados como predictores
  if (length(r) >= 2) {
    past <- c(NA, head(r, -1))  # rezago 1
    pred_input <- data.frame(past = past)
    lr_pred <- rep(NA, length(r))
    
    # Predecir sólo en posiciones con valores válidos
    valid_rows <- which(!is.na(pred_input$past))
    if (length(valid_rows) > 0) {
      lr_pred[valid_rows] <- predict(modelo_lr, newdata = pred_input[valid_rows, , drop = FALSE])
    }
  } else {
    lr_pred <- rep(NA, length(r))
  }
  
  pred_df <- data.frame(
    serie_id = sid,
    tipo = unique(serie_data$tipo),
    random = serie_data$random,
    tiempo = serie_data$tiempo,
    valor = serie_data$valor,
    rw = rw_pred,
    lr = lr_pred
  )
  
  # Calcular MAPE
  for (m in MODELS) {
    pred_df[[paste0("mape_", m)]] <- abs((pred_df$valor - pred_df[[m]]) / pred_df$valor) * 100
  }
  
  resultados[[length(resultados) + 1]] <- pred_df
}

# Unir y guardar resultados
final_results <- rbindlist(resultados)

write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_globales.csv", row.names = FALSE)
