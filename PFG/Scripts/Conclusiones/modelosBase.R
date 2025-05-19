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

########## GRUPO 0 ############3
series0 <- fread("Scripts/Conclusiones/series0.csv")
train_ids <- c(1:93)
test_ids  <- c(93:100)
MODELS <- c("rw", "lr")
resultados_completos <- list()
train_data <- series0[serie_id %in% train_ids]
valid_train_ids <- train_data[, .N, by = serie_id][N >= 2, serie_id] # Filtrar solo series con al menos 2 observaciones
lagged_all <- train_data[serie_id %in% valid_train_ids, .(
  embed_result = list(embed(valor, 2))
), by = serie_id] # Aplicar embed por serie válida

lagged_all_df <- do.call(rbind, lapply(lagged_all$embed_result, as.data.frame))
TRAINSET <- data.frame(
  real = lagged_all_df$V1,
  past = lagged_all_df$V2
)
LM <- tryCatch({
  lm(real ~ past, data = TRAINSET)
}, error = function(e) NULL)

for (sid in test_ids) {
  serie_data <- series0 %>% filter(serie_id == sid) %>% arrange(tiempo)
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

# Guardar CSV
write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_tipo0.csv", row.names = FALSE)




########## GRUPO 1 ############3
series1 <- fread("Scripts/Conclusiones/series1.csv")
train_ids <- c(1:93)
test_ids  <- c(93:100)
MODELS <- c("rw", "lr")
resultados_completos <- list()
train_data <- series1[serie_id %in% train_ids]
valid_train_ids <- train_data[, .N, by = serie_id][N >= 2, serie_id] # Filtrar solo series con al menos 2 observaciones
lagged_all <- train_data[serie_id %in% valid_train_ids, .(
  embed_result = list(embed(valor, 2))
), by = serie_id] # Aplicar embed por serie válida

lagged_all_df <- do.call(rbind, lapply(lagged_all$embed_result, as.data.frame))
TRAINSET <- data.frame(
  real = lagged_all_df$V1,
  past = lagged_all_df$V2
)
LM <- tryCatch({
  lm(real ~ past, data = TRAINSET)
}, error = function(e) NULL)

for (sid in test_ids) {
  serie_data <- series1 %>% filter(serie_id == sid) %>% arrange(tiempo)
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

# Guardar CSV
write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_tipo1.csv", row.names = FALSE)


########## JUNTAR SERIES0 Y SERIES1 ##############
series0 <- fread("Scripts/Conclusiones/series0.csv")
series1 <- fread("Scripts/Conclusiones/series1.csv")
series1$serie_id <- series1$serie_id + 100

series <- rbind(series0, series1)

# Definir IDs
train_ids <- c(1:93, 101:193)
test_ids  <- c(94:100, 194:200)

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
write.csv(final_results, "Scripts/Conclusiones/modelos_rw_lr_tipos0y1.csv", row.names = FALSE)


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
  labs(title = "MAPE error of base models - both groups",
       x = "Model", y = "MAPE (%)") +
  theme_minimal()




