library(foreach)
library(doParallel)
librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

datos <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
datosRed <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE_reducido.csv")

library(ggplot2)
library(dplyr)
library(tidyr)

df_mape <- datos %>% select(ends_with("_mape")) 
df_long <- df_mape %>%
  pivot_longer(everything(), names_to = "Modelo", values_to = "MAPE")

df_filtered <- df_long %>%
  group_by(Modelo) %>%
  mutate(Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
         Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         upper_limit = Q3 + 1.5 * IQR) %>%
  filter(MAPE <= upper_limit) %>%  
  ungroup()

medianas <- df_filtered %>%
  group_by(Modelo) %>%
  summarise(mediana = median(MAPE, na.rm = TRUE))

ggplot(df_filtered, aes(x = reorder(Modelo, MAPE, median), y = MAPE)) +
  geom_boxplot(outlier.shape = NA) +  
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)),
               position = position_nudge(y = 5), size = 3, color = "red") +  
  coord_flip() + 
  theme_minimal() +
  labs(title = "Boxplot de MAPE de los datos sin reducir", x = "Modelo", y = "MAPE")




df_mape <- datosRed %>% select(ends_with("_mape")) 
df_long <- df_mape %>%
  pivot_longer(everything(), names_to = "Modelo", values_to = "MAPE")

df_filtered <- df_long %>%
  group_by(Modelo) %>%
  mutate(Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
         Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         upper_limit = Q3 + 1.5 * IQR) %>%
  filter(MAPE <= upper_limit) %>%  
  ungroup()

medianas <- df_filtered %>%
  group_by(Modelo) %>%
  summarise(mediana = median(MAPE, na.rm = TRUE))

ggplot(df_filtered, aes(x = Modelo, y = MAPE)) +
  geom_boxplot(outlier.shape = NA) +  # No mostrar outliers
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y), 2)),
               position = position_nudge(y = 5), size = 3, color = "red") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(title = "Boxplot de MAPE con datos REDUCIDOS", x = "Modelo", y = "MAPE")



