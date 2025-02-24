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

errorNuevo <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/allMetadataDEF.csv")
errorCruz <- fread("NUEVOS DATOS/DATOS ERROR CRUZ/allMetadata.csv")

errorNuevo <- as.data.table(errorNuevo)
errorCruz <- as.data.table(errorCruz)

mape_cols <- grep("_mape$", names(errorNuevo), value = TRUE)
error_cols <- grep("_error$", names(errorCruz), value = TRUE)

errorNuevo <- errorNuevo[, ..mape_cols]
errorCruz <- errorCruz[, ..error_cols]

errorNuevo_long <- melt(errorNuevo, measure.vars = mape_cols, variable.name = "modelo", value.name = "error")
errorCruz_long <- melt(errorCruz, measure.vars = error_cols, variable.name = "modelo", value.name = "error")

#añadir columna para identificar el dataset
errorNuevo_long[, dataset := "MAPE"]
errorCruz_long[, dataset := "ERROR"]

df_combined <- rbind(errorNuevo_long, errorCruz_long, fill=T)

Q3 <- df_combined[, quantile(error, 0.75, na.rm = TRUE), by = modelo]$V1
IQR_val <- df_combined[, IQR(error, na.rm = TRUE), by = modelo]$V1
limite_superior <- Q3 + 1.5 * IQR_val

limites <- data.table(modelo = unique(df_combined$modelo), limite_superior)

df_filtrado <- merge(df_combined, limites, by = "modelo")
df_filtrado <- df_filtrado[error <= limite_superior]

medianas <- df_filtrado[, .(mediana = median(error, na.rm = TRUE)), by = .(modelo, dataset)]

# Graficar el boxplot con la mediana como etiqueta
ggplot(df_filtrado, aes(x = modelo, y = error, fill = dataset)) +
  geom_boxplot(outlier.shape = NA) +  # No mostrar outliers
  geom_text(data = medianas, aes(x = modelo, y = mediana, label = round(mediana, 4)),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +  # Agregar mediana sobre la caja
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas
  labs(title = "Distribución de errores por modelo",
       x = "Modelo", y = "Error") +
  scale_fill_manual(values = c("MAPE" = "blue", "ERROR" = "red")) +
  theme_minimal()

