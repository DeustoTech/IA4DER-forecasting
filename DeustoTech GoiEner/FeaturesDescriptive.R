library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

features <- read.csv("features.csv")

summary(features)

cols <- c("P_T2.0_VALLE", "P_T2.0_LLANO", "P_T2.0_PICO", "P_T_SOLAR_PICO", "P_T_SOLAR_LLANO", "P_T_SOLAR_SPICO", "P_T_SOLAR_SLLANO")
medias <- colMeans(features[cols])

barplot(medias, 
        main = "Media de Valores por Categoría",
        xlab = "Categorías",
        ylab = "Media",
        col = "blue",  # Cambia el color de las barras si es necesario
        names.arg = colnames(medias),  # Etiquetas en el eje X
        cex.names = 0.8  # Tamaño de las etiquetas en el eje X
)

plot(ecdf(features$ZERO))
plot(ecdf(features$IMPUTED))
plot(ecdf(features$MAX/features$POT_NOM))
ecdf(features$MAX/BfeaturesPOT_NOM)(0.8)


