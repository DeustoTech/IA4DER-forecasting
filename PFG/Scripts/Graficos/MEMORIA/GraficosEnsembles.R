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
    ses_pred = mean(ses_pred, na.rm = TRUE),
    ens_pred = mean(ens_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_media %>%
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosEnsemble/modelos_baseyEns_prediccion_media.pdf", width = 10, height = 6)

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
    ses_pred = median(ses_pred, na.rm = TRUE),
    ens_pred = median(ens_pred, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(hora = factor(hora, levels = 0:23))

datos_grafico <- datos_mediana %>%
  pivot_longer(cols = c(ends_with("_pred")), 
               names_to = "Model", 
               values_to = "Prediction")

dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf("NuevosResultados/ModelosEnsemble/modelos_baseyEns_prediccion_mediana.pdf", width = 10, height = 6)

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


#boxplot del ensemble
df <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
df <- df %>% select(ens_mape)

graficar_boxplot <- function(data) {
  data_filtrada <- data[is.finite(data[[1]]), , drop = FALSE]
  mediana <- median(data_filtrada[[1]], na.rm = TRUE)
  par(mar = c(6, 4, 4, 2))
  b <- boxplot(data_filtrada, outline = FALSE, ylab = "MAPE", xaxt = "n")
  axis(1, at = 1, labels = "ens_mape")
  
  text(
    x = 1,
    y = mediana + 7,
    labels = round(mediana, 1),
    cex = 1.5
  )
}

graficar_boxplot(df)


######## GRAFICOS FFORMA ########
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")
datosMAPE <- datosMAPE %>% select(matches("^FFORMA_.*_MAPE$"))

graficar_boxplot <- function(data) {
  matrix_data <- as.matrix(data)
  matrix_data <- matrix_data[complete.cases(matrix_data) & rowSums(is.finite(matrix_data)) == length(data), ]
  #par(mar = c(10, 4, 4, 2))
  b <- boxplot(matrix_data, outline = FALSE,  ylab = "MAPE")
  medianas <- apply(matrix_data, 2, median, na.rm = TRUE)
  text(
    x = 1:length(medianas),
    y = medianas + 7,  # Posición justo encima del upper whisker
    labels = round(medianas, 1),  # Redondear si lo deseas
    cex = 1.5
  )
}

graficar_boxplot(datosMAPE)


########## GRAFICOS FFORMA PABLO ##########
nuevos_fforma <- fread("Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")

graficar_boxplot <- function(data) {
  matrix_data <- as.matrix(data)
  matrix_data <- matrix_data[complete.cases(matrix_data) & rowSums(is.finite(matrix_data)) == ncol(data), ]
  
  # Calcular medianas y ordenarlas
  medianas <- apply(matrix_data, 2, median, na.rm = TRUE)
  orden <- order(medianas)
  
  # Reordenar datos y nombres de columnas
  matrix_data_ordenado <- matrix_data[, orden]
  nombres_ordenados <- colnames(data)[orden]
  medianas_ordenadas <- medianas[orden]
  
  # Ajustar márgenes y graficar
  par(mar = c(12, 4, 4, 2))
  b <- boxplot(matrix_data_ordenado, outline = FALSE, ylab = "MAPE", las = 2, names = nombres_ordenados)
  
  # Añadir valores de la mediana encima de cada caja
  text(
    x = 1:length(medianas_ordenadas),
    y = medianas_ordenadas + 7,
    labels = round(medianas_ordenadas, 1),
    cex = 1.5
  )
}


#modelos base
base <- nuevos_fforma %>% select(ends_with("_forec") & starts_with("MAPE_"))
graficar_boxplot(base)

#modelos fforma
ff <- nuevos_fforma %>% select(ends_with("_ff") & starts_with("MAPE_"))
graficar_boxplot(ff)


########## GRAFICO FFORMA MIO Y PABLO
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")
d_base <- datosMAPE %>% select(matches("^FFORMA_.*_MAPE$"), ens_mape)

nuevos_fforma <- fread("Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")
ff <- nuevos_fforma %>% select(ends_with("_ff") & starts_with("MAPE_"))

datos_todos <- cbind(d_base, ff)
d <- datos_todos[is.finite(rowSums(datos_todos)),]

setDT(d)
setnames(d,
         old = names(d),
         new = gsub("^MAPE_(.*)_ff$", "\\1_o",
                    gsub("(?i)_mape$", "_m", names(d), perl = TRUE)))

n <- names(d)
colores <- ifelse(grepl("_m$", n), "lightgreen", ifelse(grepl("_o$", n), "#FFDAB9", "gray"))
d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n
orden <- order(robustbase::colMedians(d))
d <- d[, orden]
colores <- colores[orden] 

par(mar = c(10.5, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2, col = colores, ylab = "MAPE")

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
  as.character(print(l))
)

medianas <- apply(d, 2, median, na.rm = TRUE)
text(
  x = 1:length(medianas),
  y = medianas + 10,
  labels = round(medianas, 1),
  cex = 1.3
)


