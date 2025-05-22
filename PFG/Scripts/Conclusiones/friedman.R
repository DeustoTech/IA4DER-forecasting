library(tidyverse)

library(foreach)
library(doParallel)

to_install <- c("ggplot2", "lattice", "caret", "fpp3", "class",
                "forecast", "Metrics", "fable", 
                "data.table", "xts", "future", "foreach", "doParallel", "RSNNS", "TTR", 
                "quantmod", "car", "e1071", "nnet", "tools", "doFuture", "neuralnet", "gbm", 
                "randomForest", "mice", "mltools", "zoo", "mlr3", "mlr3learners", "mlr3tuning", "mlr3verse",
                "paradox", "xgboost", "kknn") 

installed <- rownames(installed.packages())
for (lib in to_install) {
  if (!(lib %in% installed)) install.packages(lib, dependencies = TRUE)
  library(lib, character.only = TRUE)
}

# PARA MODELOS BASE #
d <- data.table::fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

d <- d %>% select(ends_with("mape"))
d <- d[is.finite(rowSums(d)),]
d <- d[,-c("ens_mape")]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,1]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
b <- boxplot(d,outline=F)

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
  y=b$stats[nrow(b$stats),] + 10,
  as.character(print(l))
)


# PARA MODELOS BASE + ENSEMBLE SIMPLE #
d <- data.table::fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")

d <- d %>% select(ends_with("mape"))
d <- d[is.finite(rowSums(d)),]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,1]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
b <- boxplot(d,outline=F)

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
  y=b$stats[nrow(b$stats),] + 10,
  as.character(print(l))
)


###### MODELOS FFORMA #######
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")
d <- datosMAPE %>% select( matches("^FFORMA_.*_MAPE$"))

d <- d[is.finite(rowSums(d)),]

n <- gsub("_MAPE$", "", names(d))

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
b <- boxplot(d, outline = FALSE)

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
  y=b$stats[nrow(b$stats),] + 5,
  as.character(print(l))
)



######### FFORMA PERO REDUCIENDO POR LA MEDIANA ############

datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")
d <- datosMAPE %>% select( matches("^FFORMA_.*_MAPE$"))
d$id <- datosMAPE$ID
setDT(d)
d_median <- d[, lapply(.SD, median, na.rm = TRUE), by = id, .SDcols = patterns("^FFORMA_.*")]
d_median <- d_median %>% select(-id)
d <- d_median[is.finite(rowSums(d_median)),]

n <- gsub("_MAPE$", "", names(d))

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
b <- boxplot(d, outline = FALSE)

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
  y=b$stats[nrow(b$stats),] + 2,
  as.character(print(l))
)



###### MODELOS FFORMA DE PABLO 
datosMAPE <- fread("Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")
d <- datosMAPE %>% select( matches("^MAPE_.*"))
d <- d[is.finite(rowSums(d)),]

n <- gsub("^MAPE_", "", names(d))

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
par(mar = c(10.5, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2)

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

#solo para modelos base
d <- datosMAPE %>% select(ends_with("_forec") & starts_with("MAPE_"))
d <- d[is.finite(rowSums(d)),]

n <- gsub("^MAPE_", "", names(d))

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
par(mar = c(10.5, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2)

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

#solo para modelos ensemble
d <- datosMAPE %>% select(ends_with("_ff") & starts_with("MAPE_"))
d <- d[is.finite(rowSums(d)),]

n <- gsub("^MAPE_", "", names(d))

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
par(mar = c(10.5, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2)

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

#para modelos fforma mios y de pablo
datosMAPE <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")
d_base <- datosMAPE %>% select(matches("^FFORMA_.*_MAPE$"))

nuevos_fforma <- fread("Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")
ff <- nuevos_fforma %>% select(ends_with("_ff") & starts_with("MAPE_"))

datos_todos <- cbind(d_base, ff)
d <- datos_todos[is.finite(rowSums(datos_todos)),]
n <- names(d)

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
par(mar = c(12, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2)

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


######### MODELOS GLOBALES 
archivos <- c(
  "bolt_mini_fixed_errors.csv", "bolt_tiny_fixed_errors.csv", 
  "chronos_t5_small_fixed_errors.csv", "timesfm_fixed_errors.csv"
)
ruta_archivos <- "NuevosResultados/TimesFM/errores/"
lista_mapes <- list()
for (archivo in archivos) {
  df <- fread(file.path(ruta_archivos, archivo))
  nombre_variable <- gsub("_fixed_errors.csv", "", archivo)
  lista_mapes[[nombre_variable]] <- df$mape  # Guardar vector en lista
}
max_len <- max(sapply(lista_mapes, length))
lista_con_NA <- lapply(lista_mapes, function(x) {
  length(x) <- max_len  # Rellena con NA automáticamente
  x
})
mape_globales <- as.data.frame(lista_con_NA)

d <- mape_globales
d <- d[is.finite(rowSums(d)), ]

# Poner nombres más cortos (si hace falta)
n <- colnames(d)

# Convertir a matriz
d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

# Ordenar por mediana robusta
d <- d[, order(robustbase::colMedians(d))]

# Boxplot ordenado
par(mar = c(10.5, 4, 4, 2))  # espacio para etiquetas en el eje x
b <- boxplot(d, outline = FALSE, las = 2, ylab = "MAPE")

# Test de Friedman
friedman_result <- friedman.test(d)
print(friedman_result)

# Test post-hoc de Nemenyi
f <- PMCMRplus::frdAllPairsNemenyiTest(d)

# Construcción de la matriz de p-valores
p <- rbind(1, f$p.value)
p <- cbind(p, 1)
diag(p) <- 1
p <- as.matrix(Matrix::forceSymmetric(p, "L"))

rownames(p)[ncol(p)] <- colnames(d)[ncol(d)]
colnames(p)[ncol(p)] <- colnames(d)[ncol(d)]

# Letras de significancia
l <- multcompView::multcompLetters(p)

# Añadir letras encima de cada caja
text(
  x = 1:length(colnames(d)),
  y = b$stats[nrow(b$stats), ] + 7,
  labels = as.character(l$Letters),
  cex = 1.5
)



##### PARA TODOS LOS MODELOS 
# cargar modelos base con ensemble simple
library(RColorBrewer)

d_base <- data.table::fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
d_base <- d_base %>% select(ends_with("mape"))
d_base <- d_base[is.finite(rowSums(d_base)),]

#cargar fforma mios
d_fforma <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/FFORMA_MAPE.csv")
d_fforma <- d_fforma %>% select(matches("^FFORMA_.*_MAPE$"))

#cargar fforma de pablo
d_nuevos <- fread("Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")
d_nuevos <- d_nuevos %>% select(starts_with("MAPE_"))

#cargar modelos globales
archivos <- c(
  "bolt_mini_fixed_errors.csv", "bolt_tiny_fixed_errors.csv", 
  "chronos_t5_small_fixed_errors.csv", "timesfm_fixed_errors.csv"
)
ruta_archivos <- "NuevosResultados/TimesFM/errores/"
lista_mapes <- list()
for (archivo in archivos) {
  df <- fread(file.path(ruta_archivos, archivo))
  nombre_variable <- gsub("_fixed_errors.csv", "", archivo)
  lista_mapes[[nombre_variable]] <- df$mape  # Guardar vector en lista
}
max_len <- max(sapply(lista_mapes, length))
lista_con_NA <- lapply(lista_mapes, function(x) {
  length(x) <- max_len  # Rellena con NA automáticamente
  x
})
d_globales <- as.data.frame(lista_con_NA)

datos_todos <- cbind(d_base, d_fforma, d_nuevos, d_globales)

d <- datos_todos[is.finite(rowSums(datos_todos)),]

n <- names(d)

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
par(mar = c(12, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2)

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

letras <- as.character(l$Letters)
posiciones_x <- 1:length(colnames(d))
posiciones_y <- b$stats[nrow(b$stats),] + 7

# Paleta de colores (puedes personalizarla o ampliarla si tienes más letras únicas)
colores_letras <- rep(brewer.pal(9, "Set1"), length.out = length(unique(letras)))
nombres_letras <- unique(letras)
colores_map <- setNames(colores_letras, nombres_letras)

# Añadir letras con color y negrita
for (i in seq_along(letras)) {
  text(
    x = posiciones_x[i],
    y = posiciones_y[i],
    labels = letras[i],
    col = colores_map[letras[i]],
    font = 2  # Negrita
  )
}


