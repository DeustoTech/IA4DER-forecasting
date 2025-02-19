library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'car', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mice", "mltools", "zoo") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


#JUNTAR DATOS DE PREDICCIONES
folder <- "PFG/NUEVOS DATOS/goi4_pst_preds"
files_2 <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
data_list <- list()

for (file in files_2) {
  
  df <- fread(file)
  
  original_cols <- names(df)
  pred_cols <- setdiff(original_cols, "real")
  
  id <- tools::file_path_sans_ext(basename(file))
  df[, id := id]
  
  df[, dia := rep(1:7, each = 24)]  # Días del 1 al 7
  df[, hora := rep(0:23, times = 7)]  # Horas de 0 a 23
  
  setnames(df, old = pred_cols, new = paste0(pred_cols, "_pred"))
  data_list[[length(data_list) + 1]] <- df
}

combined_df <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
column_order <- c("id", "dia", "hora", "real", setdiff(names(combined_df), c("id", "dia", "hora", "real")))
setcolorder(combined_df, column_order)
fwrite(combined_df, "PFG/NUEVOS DATOS/DATOS ERROR NUEVO/goi4_pst_preds.csv")


#VOLVER A CALCULAR MAPES Y RMSE
preds <- fread("PFG/NUEVOS DATOS/DATOS ERROR NUEVO/goi4_pst_preds.csv")

pred_cols <- grep("_pred$", names(preds), value = TRUE)
new_names <- gsub("_pred", "", pred_cols)

calc_mape <- function(real, pred) {
  return(abs((real - pred) / real) * 100)
}

calc_rmse <- function(real, pred) {
  return((real - pred)^2)
}

for (i in seq_along(pred_cols)) {
  preds[[paste0(new_names[i], "_mape")]] <- calc_mape(preds$real, preds[[pred_cols[i]]])
  preds[[paste0(new_names[i], "_rmse")]] <- calc_rmse(preds$real, preds[[pred_cols[i]]])
}

# Mostrar los primeros resultados
fwrite(preds, "PFG/NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

#GRAFICO PARA VERLO MEJOR
preds_MAPE_RMSE <- fread("PFG/NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
allMetadata <- fread("PFG/NUEVOS DATOS/DATOS ERROR CRUZ/allMetadata.csv")

mape_cols <- grep("_mape", names(preds_MAPE_RMSE), value = TRUE)
rmse_cols <- grep("_rmse", names(preds_MAPE_RMSE), value = TRUE)
error_cols <- grep("_error", names(allMetadata), value = TRUE)

mape_long <- preds_MAPE_RMSE %>%
  pivot_longer(cols = all_of(mape_cols), names_to = "Metodo", values_to = "MAPE")


# Calcular los límites del IQR
iqr_limits <- mape_long %>%
  group_by(Metodo) %>%
  summarise(
    Q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower = Q1 - 1.5 * IQR,
    Upper = Q3 + 1.5 * IQR
  )

# Filtrar outliers
mape_filtered <- mape_long %>%
  inner_join(iqr_limits, by = "Metodo") %>%
  filter(MAPE >= Lower & MAPE <= Upper)

# Graficar sin outliers
ggplot(mape_filtered, aes(x = Metodo, y = MAPE)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "MAPE de las predicciones base (sin outliers)", x = "", y = "MAPE")



ggplot(mape_long, aes(x = Metodo, y = MAPE)) +
  geom_boxplot() +  # Barras sin leyenda
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste("MAPE de las predicciones base"), x = "", y = "MAPE")

rmse_long <- preds_MAPE_RMSE %>%
  pivot_longer(cols = rmse_cols, names_to = "Metodo", values_to = "RMSE")

ggplot(RMSE_long, aes(x = reorder(Metodo, RMSE), y = RMSE, fill = Metodo)) +
  geom_col(show.legend = FALSE) +  # Barras sin leyenda
  coord_flip() +  # Voltear el gráfico para mejor lectura
  theme_minimal() +
  labs(title = "Comparación de RMSE entre Métodos", x = "Método", y = "RMSE") +
  geom_text(aes(label = round(RMSE, 2)), hjust = -0.2)  # Agregar valores en las barras

error_long <- allMetadata %>%
  pivot_longer(cols = all_of(error_cols), names_to = "Metodo", values_to = "MAPE")

ggplot(error_long, aes(x = reorder(Metodo, MAPE), y = MAPE, fill = Metodo)) +
  geom_col(show.legend = FALSE) +  # Barras sin leyenda
  coord_flip() +  # Voltear el gráfico para mejor lectura
  theme_minimal() +
  labs(title = "Comparación de MAPE entre Métodos", x = "Método", y = "MAPE") +
  geom_text(aes(label = round(MAPE, 2)), hjust = -0.2)  # Agregar valores en las barras


#JUNTAR CON METADATA (features)
combined <- fread("PFG/NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
metadata <- fread("PFG/NUEVOS DATOS/OriginalData/metadata.csv")

colnames(metadata)[colnames(metadata) == "user"] <- "id"

allMetadata <- merge(combined, metadata, by = "id")
fwrite(allMetadata, "PFG/NUEVOS DATOS/DATOS ERROR NUEVO/allMetadataDEF.csv", row.names = FALSE)
