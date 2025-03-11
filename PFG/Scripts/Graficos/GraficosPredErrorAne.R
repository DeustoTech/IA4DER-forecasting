library(foreach)
library(doParallel)
librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable", "stringr") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


predLM <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/combined_lm.csv")
predLM_red <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_lm.csv")
predRF_red <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_rf.csv")
predGBM_red <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_gbm.csv")
predNN_red <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_nn.csv")
predSVM_red <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combined_svm.csv")

predCombined_red <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/combinedPreds.csv")



#COMPARAR DATOS REDUCIDOS Y SIN REDUCIR

df1_mape <- predLM %>% select(starts_with("MAPE_"))
df2_mape <- predLM_red %>% select(starts_with("MAPE_"))

df1_mape$Dataset <- "datos sin reducir"
df2_mape$Dataset <- "datos reducidos"

df_mape <- bind_rows(df1_mape, df2_mape)

df_long <- df_mape %>%
  pivot_longer(cols = -Dataset, names_to = "Modelo", values_to = "MAPE")

df_filtered <- df_long %>%
  group_by(Modelo, Dataset) %>%
  mutate(Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
         Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         upper_limit = Q3 + 1.5 * IQR) %>%
  filter(MAPE <= upper_limit) %>%
  ungroup()

medianas <- df_filtered %>%
  group_by(Modelo, Dataset) %>%
  summarise(mediana = median(MAPE, na.rm = TRUE))


ggplot(df_filtered, aes(x = Modelo, y = MAPE, fill = Dataset)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.8)) + 
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)),
               position = position_dodge(0.8), vjust = -0.5, size = 3, color = "red") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Boxplot de MAPE sin Outliers", x = "Modelo", y = "MAPE", fill = "Dataset")


# GRAFICOS DE LOS DATOS REDUCIDOS (porque van mejor)

df_mape <- predCombined_red %>% select(starts_with("MAPE_"))

df_long <- df_mape %>%
  pivot_longer(everything(), names_to = "Modelo", values_to = "MAPE")

df_long <- df_long %>%
  mutate(TipoModelo = str_extract(Modelo, "[^_]+$"))

df_filtered <- df_long %>%
  group_by(Modelo) %>%
  mutate(Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
         Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         upper_limit = Q3 + 1.5 * IQR) %>%
  filter(MAPE <= upper_limit) %>%
  ungroup()

modelos_unicos <- unique(df_filtered$TipoModelo)

pdf("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/boxplots_por_modelo.pdf", width = 8, height = 6)

for (modelo in modelos_unicos) {
  p <- ggplot(df_filtered %>% filter(TipoModelo == modelo), aes(x = Modelo, y = MAPE)) +
    geom_boxplot(outlier.shape = NA) +  
    stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)),
                 position = position_nudge(y = 5), size = 3, color = "red") +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = paste("Boxplot de MAPE - Modelo", modelo), x = "Modelo", y = "MAPE")
  
  print(p) 
}

dev.off() 


## CON LAS PREDICCIONES 
df_preds <- predCombined_red %>% select(starts_with("Predicted_"))

df_long <- df_preds %>%
  pivot_longer(everything(), names_to = "Modelo", values_to = "PREDICTION")

df_long <- df_long %>%
  mutate(TipoModelo = str_extract(Modelo, "[^_]+$"))

df_filtered <- df_long %>%
  group_by(Modelo) %>%
  mutate(Q1 = quantile(PREDICTION, 0.25, na.rm = TRUE),
         Q3 = quantile(PREDICTION, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         upper_limit = Q3 + 1.5 * IQR) %>%
  filter(PREDICTION <= upper_limit) %>%
  ungroup()

modelos_unicos <- unique(df_filtered$TipoModelo)

pdf("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO/boxplots_por_modelo_prediccion.pdf", width = 8, height = 6)

for (modelo in modelos_unicos) {
  p <- ggplot(df_filtered %>% filter(TipoModelo == modelo), aes(x = Modelo, y = PREDICTION)) +
    geom_boxplot(outlier.shape = NA) +  
    stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)),
                 position = position_nudge(y = 5), size = 3, color = "red") +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = paste("Boxplot de predicicones - Modelo", modelo), x = "Modelo", y = "PREEDICTION")
  
  print(p) 
}

if (any(grepl("^Real_", names(predCombined_red)))) {
  df_real <- predCombined_red %>% select(starts_with("Real_"))  # Filtrar columnas "Real_"
  
  df_real_long <- df_real %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor")
  
  # Aplicar el mismo filtro de outliers
  df_real_filtered <- df_real_long %>%
    group_by(Variable) %>%
    mutate(Q1 = quantile(Valor, 0.25, na.rm = TRUE),
           Q3 = quantile(Valor, 0.75, na.rm = TRUE),
           IQR = Q3 - Q1,
           upper_limit = Q3 + 1.5 * IQR) %>%
    filter(Valor <= upper_limit) %>%
    ungroup()
  
  p_real <- ggplot(df_real_filtered, aes(x = Variable, y = Valor)) +
    geom_boxplot(outlier.shape = NA) +
    stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)),
                 position = position_nudge(y = 5), size = 3, color = "red") +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    labs(title = "Boxplot de valores reales (sin outliers)", x = "Variable", y = "Valor")
  
  print(p_real)  # Agregar al PDF
}

dev.off()  # Cerrar el archivo PDF

