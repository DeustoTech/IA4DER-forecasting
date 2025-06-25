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

series0 <- fread("Scripts/Conclusiones/series0.csv")
series1 <- fread("Scripts/Conclusiones/series1.csv")


####### GRAFICOS DESCRIPCION SERIES ############
### SERIES 0
ggplot(series0, aes(x = tiempo, y = valor, group = serie_id)) +
  geom_line(alpha = 0.5, linewidth = 0.7) +
  labs(
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
  )

series_filtradas <- series0 %>%
  group_by(serie_id) %>%
  filter(max(valor) <= 0.13) %>%
  ungroup()

# Graficar solo esas series
ggplot(series_filtradas, aes(x = tiempo, y = valor, group = serie_id)) +
  geom_line(alpha = 0.5, linewidth = 0.7) +
  labs(
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
  )


###SERIES1
ggplot(series1, aes(x = tiempo, y = valor, group = serie_id)) +
  geom_line(alpha = 0.5, linewidth = 0.8) +
  labs(
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),
  )

graficar_boxplot <- function(data) {
  matrix_data <- as.matrix(data)
  matrix_data <- matrix_data[complete.cases(matrix_data) & rowSums(is.finite(matrix_data)) == ncol(data), ]
  
  medianas <- apply(matrix_data, 2, median, na.rm = TRUE)
  orden <- order(medianas)
  
  matrix_data_ordenado <- matrix_data[, orden]
  nombres_ordenados <- colnames(data)[orden]
  medianas_ordenadas <- medianas[orden]
  
  colores <- ifelse(nombres_ordenados == "mape_FFORMA", "lightblue",
                    ifelse(nombres_ordenados == "mape_rw",     "lightpink",
                           ifelse(nombres_ordenados == "mape_lr",     "lightgreen",
                                  "gray")))
  par(mar = c(10.5, 4, 4, 2))
  b <- boxplot(matrix_data_ordenado, 
               outline = FALSE, 
               ylab = "MAPE", 
               xaxt = "n",
               col = colores)  
  
  # Dibujar eje x con nombres más grandes
  axis(side = 1, at = 1:length(nombres_ordenados), 
       labels = gsub("(?i)^mape_", "", nombres_ordenados, perl = TRUE), cex.axis = 2)
  
  text(
    x = 1:length(medianas_ordenadas),
    y = medianas_ordenadas + 28,
    labels = round(medianas_ordenadas, 1),
    cex = 1.8
  )
}

####### GRAFICOS MODELOS BASE y FFORMA DE TIPO 0 Y TIPO 1 POR SEPARADO ############
## LR Y RW DE TIPO 0
modelos_tipo0 <- fread("Scripts/Conclusiones/modelos_rw_lr_ff_tipo0.csv")
modelos_tipo0 <- modelos_tipo0 %>% select(starts_with("mape_"))
modelos_tipo0 <- modelos_tipo0 %>% select(- mape_FFORMA)
graficar_boxplot(modelos_tipo0)

## LR RW y FF DE TIPO 0
modelos_tipo0 <- fread("Scripts/Conclusiones/modelos_rw_lr_ff_tipo0.csv")
modelos_tipo0 <- modelos_tipo0 %>% select(starts_with("mape_"))
graficar_boxplot(modelos_tipo0)

## LR Y RW DE TIPO 1
modelos_tipo1 <- fread("Scripts/Conclusiones/modelos_rw_lr_ff_tipo1.csv")
modelos_tipo1 <- modelos_tipo1 %>% select(starts_with("mape_"))
modelos_tipo1 <- modelos_tipo1 %>% select(- mape_FFORMA)
graficar_boxplot(modelos_tipo1)

## LR RW y FF DE TIPO 1
modelos_tipo1 <- fread("Scripts/Conclusiones/modelos_rw_lr_ff_tipo1.csv")
modelos_tipo1 <- modelos_tipo1 %>% select(starts_with("mape_"))
graficar_boxplot(modelos_tipo1)

####### GRAFICOS MODELOS BASE y FFORMA DE TIPO 0 Y TIPO 1 JUNTOS ############
## LR y RW DE TIPOS 0 Y 1
modelos_tipos <- fread("Scripts/Conclusiones/modelos_rw_lr_ffr_tipos0y1.csv")
modelos_tipos <- modelos_tipos %>% select(starts_with("mape_"))
modelos_tipos <- modelos_tipos %>% select(- mape_FFORMA)
graficar_boxplot(modelos_tipos)

## LR RW Y FF SOLO CON RANDOM DE TIPOS 0 Y 1
modelos_tipos <- fread("Scripts/Conclusiones/modelos_rw_lr_ffr_tipos0y1.csv")
modelos_tipos <- modelos_tipos %>% select(starts_with("mape_"))
graficar_boxplot(modelos_tipos)

## LR RW Y FF CON RANDOM Y TIPO DE TIPOS 0 Y 1
modelos_tipos <- fread("Scripts/Conclusiones/modelos_rw_lr_ffrt_tipos0y1.csv")
modelos_tipos <- modelos_tipos %>% select(starts_with("mape_"))
graficar_boxplot(modelos_tipos)


########## FRIEDMAN ############
# TIPO 0 CON lr, rw, y ff
modelos_tipo0 <- fread("Scripts/Conclusiones/modelos_rw_lr_ff_tipo0.csv")
d <- modelos_tipo0 %>% select(starts_with("mape_"))
d <- d[is.finite(rowSums(d)),]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,2]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

orden_deseado <- c("rw", "lr", "FFORMA")
d <- d[, orden_deseado]
labels <- colnames(d)
colores <- ifelse(labels == "FFORMA", "lightblue",
                  ifelse(labels == "lr",     "lightgreen",
                         ifelse(labels == "rw",     "lightpink",
                                "gray")))
b <- boxplot(d,outline=F, col = colores, ylab = "MAPE",  xaxt = "n")
labels <- colnames(d)
axis(1, at = 1:length(labels), labels = FALSE)
text(
  x = 1:length(labels),
  y = par("usr")[3] - 0.06 * diff(par("usr")[3:4]), 
  labels = labels,
  xpd = TRUE,
  adj = 0.5,
  cex = 2
)

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
  y=b$stats[nrow(b$stats),] + 4,
  as.character(print(l)),
  cex = 1.8
)

medianas <- apply(d, 2, median, na.rm = TRUE)
text(
  x = 1:length(medianas),
  y = 80,
  labels = round(medianas, 1),
  cex = 1.7
)

# TIPO 1 CON lr, rw, y ff
modelos_tipo1 <- fread("Scripts/Conclusiones/modelos_rw_lr_ff_tipo1.csv")
d <- modelos_tipo1 %>% select(starts_with("mape_"))
d <- d[is.finite(rowSums(d)),]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,2]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

orden_deseado <- c("rw", "lr", "FFORMA")
d <- d[, orden_deseado]
labels <- colnames(d)
colores <- ifelse(labels == "FFORMA", "lightblue",
                  ifelse(labels == "lr",     "lightgreen",
                         ifelse(labels == "rw",     "lightpink",
                                "gray")))
b <- boxplot(d,outline=F, col = colores, ylab = "MAPE",  xaxt = "n")
labels <- colnames(d)
axis(1, at = 1:length(labels), labels = FALSE)
text(
  x = 1:length(labels),
  y = par("usr")[3] - 0.06 * diff(par("usr")[3:4]), 
  labels = labels,
  xpd = TRUE,
  adj = 0.5,
  cex = 2
)

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
  y=b$stats[nrow(b$stats),] + 15,
  as.character(print(l)),
  cex = 1.8
)

medianas <- apply(d, 2, median, na.rm = TRUE)
text(
  x = 1:length(medianas),
  y = 200,
  labels = round(medianas, 1),
  cex = 1.7
)




# TIPOS 1 Y 2 CON lr, rw
modelos_tipo1 <- fread("Scripts/Conclusiones/modelos_rw_lr_ffr_tipos0y1.csv")

d <- modelos_tipo1 %>% select(starts_with("mape_"))
d <- d %>% select(- mape_FFORMA)
d <- d[is.finite(rowSums(d)),]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,2]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
b <- boxplot(d,outline=F, )

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
  as.character(print(l$Letters)),
  cex = 1.5
)


# TIPOS 1 Y 2 CON lr, rw, y ff solo con random
modelos_tipo1 <- fread("Scripts/Conclusiones/modelos_rw_lr_ffr_tipos0y1.csv")

d <- modelos_tipo1 %>% select(starts_with("mape_"))
d <- d[is.finite(rowSums(d)),]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,2]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
labels <- colnames(d)
colores <- ifelse(labels == "FFORMA", "lightblue",
                  ifelse(labels == "lr",     "lightgreen",
                         ifelse(labels == "rw",     "lightpink",
                                "gray")))
b <- boxplot(d,outline=F, col = colores, ylab = "MAPE" , cex.axis = 1.5)

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
  as.character(print(l)),
  cex = 1.8
)
medianas <- apply(d, 2, median, na.rm = TRUE)
text(
  x = 1:length(medianas),
  y = medianas + 15,
  labels = round(medianas, 1),
  cex = 1.7
)


# TIPOS 1 Y 2 CON lr, rw, y ff con random y tipo
modelos_tipo1 <- fread("Scripts/Conclusiones/modelos_rw_lr_ffrt_tipos0y1.csv")

d <- modelos_tipo1 %>% select(starts_with("mape_"))
d <- d[is.finite(rowSums(d)),]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,2]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
labels <- colnames(d)
colores <- ifelse(labels == "FFORMA", "lightblue",
                  ifelse(labels == "lr",     "lightgreen",
                         ifelse(labels == "rw",     "lightpink",
                                "gray")))
b <- boxplot(d,outline=F, col = colores, ylab = "MAPE", cex.axis = 1.5 )

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
  as.character(print(l)),
  cex = 1.8
)
medianas <- apply(d, 2, median, na.rm = TRUE)
text(
  x = 1:length(medianas),
  y = 50,
  labels = round(medianas, 1),
  cex = 1.7
)
