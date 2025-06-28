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

### GRAFICO DE LINEAS ###
df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

datos_media <- df %>%
  group_by(dia, hora) %>%
  summarise(
    Real = mean(real, na.rm = TRUE),
    mean_pred = mean(mean_pred, na.rm = TRUE),
    rw_pred = mean(rw_pred, na.rm = TRUE),
    naive_pred = mean(naive_pred, na.rm = TRUE),
    simple_pred = mean(simple_pred, na.rm = TRUE),
    lr_pred = mean(lr_pred, na.rm = TRUE),
    ann_pred = mean(ann_pred, na.rm = TRUE),
    svm_pred = mean(svm_pred, na.rm = TRUE),
    arima_pred = mean(arima_pred, na.rm = TRUE),
    ses_pred = mean(ses_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_media %>%
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosBase/modelos_base_prediccion_media.pdf", width = 10, height = 6)

for (d in unique(datos_grafico$dia)) {
  
  datos_dia <- datos_grafico %>% filter(dia == d)
  
  p <- ggplot(datos_dia, aes(x = hora, y = Prediction, color = Model, group = Model)) +
    geom_line() +
    geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
    labs(title = paste("Predictions (mean) vs Real - ", dias_semana[d]),
         x = "Hour",
         y = "Consumption in kWh (mean)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Mejor visibilidad del eje X
  
  print(p)
}

dev.off()


df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

datos_mediana <- df %>%
  group_by(dia, hora) %>%
  summarise(
    Real = median(real, na.rm = TRUE),
    mean_pred = median(mean_pred, na.rm = TRUE),
    rw_pred = median(rw_pred, na.rm = TRUE),
    naive_pred = median(naive_pred, na.rm = TRUE),
    simple_pred = median(simple_pred, na.rm = TRUE),
    lr_pred = median(lr_pred, na.rm = TRUE),
    ann_pred = median(ann_pred, na.rm = TRUE),
    svm_pred = median(svm_pred, na.rm = TRUE),
    arima_pred = median(arima_pred, na.rm = TRUE),
    ses_pred = median(ses_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_mediana %>%
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosBase/modelos_base_prediccion_mediana.pdf", width = 10, height = 6)

for (d in unique(datos_grafico$dia)) {
  
  datos_dia <- datos_grafico %>% filter(dia == d)
  
  p <- ggplot(datos_dia, aes(x = hora, y = Prediction, color = Model, group = Model)) +
    geom_line() +
    geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +  # Línea de datos reales
    labs(title = paste("Predictions (median) vs Real - ", dias_semana[d]),
         x = "Hour",
         y = "Consumption in kWh (median) (kWh)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Mejor visibilidad del eje X
  
  print(p)
}

dev.off()


d <- data.table::fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
d <- d %>% select(ends_with("mape"))
d <- d[,-c("ens_mape")]

nuevos_fforma <- fread("Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")
base <- nuevos_fforma %>% select(ends_with("_forec") & starts_with("MAPE_"))

datos_todos <- cbind(d, base)
da <- datos_todos[is.finite(rowSums(datos_todos)),]
setDT(da)
setnames(da, 
         old = names(da),
         new = gsub("^MAPE_(.*?)_forec$", "\\1_o", 
                    gsub("_mape$", "_m", names(da))))

n <- names(da)
colores <- ifelse(grepl("_m$", n), "lightblue", ifelse(grepl("_o$", n), "orange", "gray"))

modelos_naive <- c("naive", "simple", "rw", "snaive", "rw_drift", "mean", "naive")

da <- as.matrix(da)
rownames(da) <- 1:nrow(da)
colnames(da) <- n

orden_medianas <- order(robustbase::colMedians(da))
d <- da[, orden_medianas]
nombres_limpios <- gsub("(_m|_o)$", "", colnames(da)[orden_medianas])
colores_ordenados <- colores[orden_medianas]

# Lista de modelos baseline
modelos_baseline <- c("naive", "snaive", "rw", "simple", "rw_drift", "mean")

# Graficar sin nombres (dejas espacio para dibujarlos tú)
par(mar = c(10.5, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2, col = colores_ordenados,
             names = rep("", length(nombres_limpios)), cex.axis = 1.3, ylab = "MAPE [%]")

# Etiquetas personalizadas
etiquetas <- nombres_limpios
modelos_baseline <- c("naive", "snaive", "rw", "simple", "rw_drift", "mean")

# Dibujar eje x con etiquetas personalizadas
axis(1, at = 1:length(etiquetas), labels = FALSE)

# Texto con color y estilo condicional
for (i in seq_along(etiquetas)) {
  modelo <- etiquetas[i]
  is_baseline <- modelo %in% modelos_baseline
  col <- if (is_baseline) "red" else "black"
  font <- if (is_baseline) 2 else 1  # 2 = bold, 1 = normal
  text(x = i, y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
       labels = modelo, srt = 90, adj = 1, xpd = TRUE, cex = 1.3,
       col = col, font = font)
}


friedman_result <- friedman.test(d)
print(friedman_result)

f <- PMCMRplus::frdAllPairsNemenyiTest(d)

p <- rbind(1,f$p.value)
p <- cbind(p, 1)
diag(p) <- 1
p <- as.matrix(Matrix::forceSymmetric(p, "L"))
rownames(p)[dim(f$p.value)+1] <- rownames(f$p.value)[dim(f$p.value)[1]]
colnames(p)[dim(f$p.value)+1] <- rownames(f$p.value)[dim(f$p.value)[1]]

l <- multcompView::multcompLetters(p)

text(
  x=c(1:length(colnames(d))),
  y=b$stats[nrow(b$stats),] + 7,
  labels = as.character(l$Letters),
  cex = 1.3
)

medianas <- apply(d, 2, median, na.rm = TRUE)
text(
  x = 1:length(medianas),
  y = medianas + 10,
  labels = round(medianas, 1),
  cex = 1.3
)

