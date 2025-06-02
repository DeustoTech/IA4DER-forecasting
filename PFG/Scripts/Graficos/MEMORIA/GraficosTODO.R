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

# cargar modelos base con ensemble simple
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

graficar_boxplot <- function(data, grupos) {
  matrix_data <- as.matrix(data)
  matrix_data <- matrix_data[complete.cases(matrix_data) & rowSums(is.finite(matrix_data)) == ncol(data), ]
  medianas <- apply(matrix_data, 2, median, na.rm = TRUE)
  orden <- order(medianas)
  matrix_data_ordenado <- matrix_data[, orden]
  nombres_ordenados <- colnames(data)[orden]
  medianas_ordenadas <- medianas[orden]
  grupos_ordenados <- grupos[orden]
  colores_por_grupo <- c(
    "d_base" = "lightblue",
    "d_fforma" = "lightgreen",
    "d_nuevos" = "orange",
    "d_globales" = "pink"
  )
  colores <- colores_por_grupo[grupos_ordenados]
  par(mar = c(12, 4, 4, 1))
  b <- boxplot(matrix_data_ordenado, outline = FALSE, ylab = "MAPE", las = 2, names = nombres_ordenados,
               col = colores)
  text(
    x = 1:length(medianas_ordenadas),
    y = medianas_ordenadas + 7,
    labels = round(medianas_ordenadas, 1),
    cex = 0.8
  )
}

grupos <- c(
  rep("d_base", ncol(d_base)),
  rep("d_fforma", ncol(d_fforma)),
  rep("d_nuevos", ncol(d_nuevos)),
  rep("d_globales", ncol(d_globales))
)

graficar_boxplot(datos_todos, grupos)


###### entero con friedman
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
setnames(d,
         old = names(d),
         new = gsub("^MAPE_(.*?)_forec$", "\\1_ob", 
                    gsub("^MAPE_(.*?)_ff$", "\\1_of", 
                         gsub("(?i)_mape$", "_m", names(d), perl = TRUE))))
n <- names(d)
colores <- ifelse(n == "ens_m", "lightgreen",                       # excepción explícita
                  ifelse(grepl("^FFORMA_.*_m$", n), "lightgreen",         # FFORMA_..._m
                         ifelse(grepl("_m$", n),           "lightblue",          # ..._m
                                ifelse(grepl("_ob$", n),          "orange",             # ..._ob
                                       ifelse(grepl("_of$", n),          "#FFDAB9",            # ..._of
                                              "#f9a9e8"))))) 


d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n
orden <- order(robustbase::colMedians(d))
d <- d[, orden]
colores <- colores[orden] 
par(mar = c(12, 4, 4, 2))
b <- boxplot(d, outline = FALSE, las = 2, ylab ="MAPE", col = colores)

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

medianas <- apply(d, 2, median, na.rm = TRUE)
text(
  x = 1:length(medianas),
  y = medianas + 10,
  labels = round(medianas, 1),
  cex = 0.8
)
