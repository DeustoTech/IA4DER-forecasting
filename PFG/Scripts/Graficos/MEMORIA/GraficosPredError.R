library(foreach)
library(doParallel)

# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS",  
               'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}
library(tidyverse)

## TODO ESTE CODIGO TARDA MUCHO ####

feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
feats$ID <- feats$id
feats <- feats %>% select(-id)
feats <- feats %>% select(ID, dia, hora, real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred, mean_mape, rw_mape, naive_mape, simple_mape, lr_mape, ann_mape, svm_mape, arima_mape, ses_mape, ens_mape)
feats$Real <- feats$real
feats <- feats %>% select(-real)

prep_mape_data <- function(df) {
  df %>%
    select(starts_with("MAPE_")) %>%
    pivot_longer(cols = everything(), names_to = "Model", values_to = "MAPE") %>%
    mutate(Model = str_remove(Model, "MAPE_"))
}

## DATOS POR DIA ##
combinedPredsDIA <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combinedPreds.csv")
combinedPredsDIA <- combinedPredsDIA %>% select(ID, starts_with("Real_"), starts_with("Predicted_"), starts_with("MAPE_"))
combinedPredsDIA <- combinedPredsDIA %>% distinct(ID, .keep_all = TRUE)

datosCombinadosDIA <- feats %>%
  left_join(combinedPredsDIA, by = "ID")
setDT(datosCombinadosDIA)

mape_dia <- prep_mape_data(datosCombinadosDIA)

# Calcular Q1, Q3, IQR y filtrar outliers
combined_long <- mape_dia %>%
  group_by(Model) %>%
  mutate(
    Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_limit = Q3 + 1.5 * IQR,
    lower_limit = Q1 - 1.5 * IQR
  ) %>%
  filter(MAPE <= upper_limit & MAPE >= lower_limit) %>%
  ungroup()

orden_modelos <- c("lm", "rf", "xgboost", "knn")
combined_long <- combined_long %>%
  mutate(tipo_modelo = str_extract(Model, "(?<=_)lm|rf|xgboost|knn")) %>%
  mutate(Model = factor(Model, levels = combined_long %>%
                          mutate(tipo_modelo = str_extract(Model, "(?<=_)lm|rf|xgboost|knn")) %>%
                          distinct(Model, tipo_modelo) %>%
                          arrange(factor(tipo_modelo, levels = orden_modelos), Model) %>%
                          pull(Model)))

# Calcular las medianas
medianas <- combined_long %>%
  group_by(Model) %>%
  summarize(Mediana = median(MAPE, na.rm = TRUE)) %>%
  arrange(Mediana)

# Seleccionar la variable con la mediana más baja
variables_baja_mediana <- head(medianas$Model, 1)

# Asignar colores en función de la variable con la mediana más baja
combined_long$Color <- ifelse(combined_long$Model %in% variables_baja_mediana, "Baja Mediana", "Otro")

# Generar el gráfico
ggplot(combined_long, aes(x = Model, y = MAPE, fill = Color)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)), 
               vjust = -0.5, color = "black", size = 3.5) +  # Etiquetas con la mediana
  scale_fill_manual(values = c("Baja Mediana" = "#5387E3", "Otro" = "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "MAPE distribution - reduced by day", x = "", y = "MAPE") +
  guides(fill = "none") 


##### DATOS POR DIA Y HORA ####
combinedPredsDIAyHORA <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/combinedPreds.csv")
combinedPredsDIAyHORA <- combinedPredsDIAyHORA %>% select(ID, starts_with("Real_"), starts_with("Predicted_"), starts_with("MAPE_"))
combinedPredsDIAyHORA <- combinedPredsDIAyHORA %>% distinct(ID, .keep_all = TRUE)

combinedPredsDIAyHORA <- feats %>%
  left_join(combinedPredsDIAyHORA, by = "ID")
setDT(combinedPredsDIAyHORA)

mape_diahora <- prep_mape_data(combinedPredsDIAyHORA)


# Calcular Q1, Q3, IQR y filtrar outliers
combined_long <- mape_diahora %>%
  group_by(Model) %>%
  mutate(
    Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_limit = Q3 + 1.5 * IQR,
    lower_limit = Q1 - 1.5 * IQR
  ) %>%
  filter(MAPE <= upper_limit & MAPE >= lower_limit) %>%
  ungroup()

orden_modelos <- c("lm", "rf", "xgboost", "knn")
combined_long <- combined_long %>%
  mutate(tipo_modelo = str_extract(Model, "(?<=_)lm|rf|xgboost|knn")) %>%
  mutate(Model = factor(Model, levels = combined_long %>%
                          mutate(tipo_modelo = str_extract(Model, "(?<=_)lm|rf|xgboost|knn")) %>%
                          distinct(Model, tipo_modelo) %>%
                          arrange(factor(tipo_modelo, levels = orden_modelos), Model) %>%
                          pull(Model)))

# Calcular las medianas
medianas <- combined_long %>%
  group_by(Model) %>%
  summarize(Mediana = median(MAPE, na.rm = TRUE)) %>%
  arrange(Mediana)

# Seleccionar la variable con la mediana más baja
variables_baja_mediana <- head(medianas$Model, 1)

# Asignar colores en función de la variable con la mediana más baja
combined_long$Color <- ifelse(combined_long$Model %in% variables_baja_mediana, "Baja Mediana", "Otro")

# Generar el gráfico
ggplot(combined_long, aes(x = Model, y = MAPE, fill = Color)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)), 
               vjust = -0.5, color = "black", size = 3.5) +  # Etiquetas con la mediana
  scale_fill_manual(values = c("Baja Mediana" = "#5387E3", "Otro" = "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "MAPE distribution - reduced by day and hour", x = "", y = "MAPE") +
  guides(fill = "none") 


###### DATOS POR HORA ####

feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
feats$ID <- feats$id
feats <- feats %>% select(-id)
feats <- feats %>% select(ID, dia, hora, real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred, mean_mape, rw_mape, naive_mape, simple_mape, lr_mape, ann_mape, svm_mape, arima_mape, ses_mape, ens_mape)
feats$Real <- feats$real
feats <- feats %>% select(-real)

prep_mape_data <- function(df) {
  df %>%
    select(starts_with("MAPE_")) %>%
    pivot_longer(cols = everything(), names_to = "Model", values_to = "MAPE") %>%
    mutate(Model = str_remove(Model, "MAPE_"))
}


combinedPredsHORA <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_HORA/combinedPreds.csv")
combinedPredsHORA <- combinedPredsHORA %>% select(ID, starts_with("Real_"), starts_with("Predicted_"), starts_with("MAPE_"))
combinedPredsHORA <- combinedPredsHORA %>% distinct(ID, .keep_all = TRUE)

combinedPredsHORA <- feats %>%
  left_join(combinedPredsHORA, by = "ID")
setDT(combinedPredsHORA)

mape_hora <- prep_mape_data(combinedPredsHORA)

# Calcular Q1, Q3, IQR y filtrar outliers
combined_long <- mape_hora %>%
  group_by(Model) %>%
  mutate(
    Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    upper_limit = Q3 + 1.5 * IQR,
    lower_limit = Q1 - 1.5 * IQR
  ) %>%
  filter(MAPE <= upper_limit & MAPE >= lower_limit) %>%
  ungroup()

orden_modelos <- c("lm", "rf", "xgboost", "knn")
combined_long <- combined_long %>%
  mutate(tipo_modelo = str_extract(Model, "(?<=_)lm|rf|xgboost|knn")) %>%
  mutate(Model = factor(Model, levels = combined_long %>%
                          mutate(tipo_modelo = str_extract(Model, "(?<=_)lm|rf|xgboost|knn")) %>%
                          distinct(Model, tipo_modelo) %>%
                          arrange(factor(tipo_modelo, levels = orden_modelos), Model) %>%
                          pull(Model)))

# Calcular las medianas
medianas <- combined_long %>%
  group_by(Model) %>%
  summarize(Mediana = median(MAPE, na.rm = TRUE)) %>%
  arrange(Mediana)

# Seleccionar la variable con la mediana más baja
variables_baja_mediana <- head(medianas$Model, 1)

# Asignar colores en función de la variable con la mediana más baja
combined_long$Color <- ifelse(combined_long$Model %in% variables_baja_mediana, "Baja Mediana", "Otro")

# Generar el gráfico
ggplot(combined_long, aes(x = Model, y = MAPE, fill = Color)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)), 
               vjust = -0.5, color = "black", size = 3.5) +  # Etiquetas con la mediana
  scale_fill_manual(values = c("Baja Mediana" = "#5387E3", "Otro" = "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "MAPE distribution - reduced by hour", x = "", y = "MAPE") +
  guides(fill = "none") 


#### HACER SOLO SUMMARY DE LOS ARCHIVOS PARA IR MAS RAPIDO ####

filtrar_y_resumir_opt <- function(df) {
  df %>%
    select(starts_with("MAPE_")) %>%
    pivot_longer(cols = everything(), names_to = "Model", values_to = "MAPE") %>%
    group_by(Model) %>%
    group_split() %>%
    map_dfr(function(grupo) {
      mape_vals <- grupo$MAPE
      modelo <- unique(grupo$Model)
      
      # Calcular estadísticas
      q1 <- quantile(mape_vals, 0.25, na.rm = TRUE)
      q3 <- quantile(mape_vals, 0.75, na.rm = TRUE)
      iqr_val <- q3 - q1
      lower <- q1 - 1.5 * iqr_val
      upper <- q3 + 1.5 * iqr_val
      
      mape_filtrado <- mape_vals[mape_vals >= lower & mape_vals <= upper]
      
      tibble(
        Model = modelo,
        Min = min(mape_filtrado, na.rm = TRUE),
        Q1 = quantile(mape_filtrado, 0.25, na.rm = TRUE),
        Median = median(mape_filtrado, na.rm = TRUE),
        Mean = mean(mape_filtrado, na.rm = TRUE),
        Q3 = quantile(mape_filtrado, 0.75, na.rm = TRUE),
        Max = max(mape_filtrado, na.rm = TRUE),
        IQR = IQR(mape_filtrado, na.rm = TRUE)
      )
    })
}

summary_dia <- filtrar_y_resumir_opt(datosCombinadosDIA)
summary_hora <- filtrar_y_resumir_opt(combinedPredsHORA)
summary_diahora <- filtrar_y_resumir_opt(combinedPredsDIAyHORA)


###### BOXPLOT SIMPLE CON LOS DATOS REDUCIDOS POR DIA ##########
feats <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
feats$ID <- feats$id
feats <- feats %>% select(-id)
feats <- feats %>% select(ID, dia, hora, real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred, mean_mape, rw_mape, naive_mape, simple_mape, lr_mape, ann_mape, svm_mape, arima_mape, ses_mape, ens_mape)
feats$Real <- feats$real
feats <- feats %>% select(-real)

combinedPredsDIA <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/combinedPreds.csv")
combinedPredsDIA <- combinedPredsDIA %>% select(ID, starts_with("Real_"), starts_with("Predicted_"), starts_with("MAPE_"))
combinedPredsDIA <- combinedPredsDIA %>% distinct(ID, .keep_all = TRUE)

datosCombinadosDIA <- feats %>%
  left_join(combinedPredsDIA, by = "ID")
setDT(datosCombinadosDIA)

mape_lm <- grep("MAPE_.*_lm$", names(datosCombinadosDIA), value = TRUE)
mape_rf <- grep("MAPE_.*_rf$", names(datosCombinadosDIA), value = TRUE)
mape_knn <- grep("MAPE_.*_knn$", names(datosCombinadosDIA), value = TRUE)
mape_xgb <- grep("MAPE_.*_xgboost$", names(datosCombinadosDIA), value = TRUE)

graficar_boxplot <- function(columnas) {
  data <- datosCombinadosDIA[, ..columnas]
  matrix_data <- as.matrix(data)
  matrix_data <- matrix_data[complete.cases(matrix_data) & rowSums(is.finite(matrix_data)) == length(columnas), ]
  par(mar = c(10, 4, 4, 2))
  b <- boxplot(matrix_data, outline = FALSE, las = 3, ylab = "MAPE")
  medianas <- apply(matrix_data, 2, median, na.rm = TRUE)
  text(
    x = 1:length(medianas),
    y = medianas + 7,  # Posición justo encima del upper whisker
    labels = round(medianas, 1),  # Redondear si lo deseas
    cex = 1.5
  )
}

graficar_boxplot(mape_lm)
graficar_boxplot(mape_rf)
graficar_boxplot(mape_knn)
graficar_boxplot(mape_xgb)

