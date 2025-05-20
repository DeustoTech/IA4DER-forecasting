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
d <- datosMAPE %>% select(matches("^FFORMA_.*_MAPE$"))
d <- d[is.finite(rowSums(d)),]

n <- gsub("_MAPE$", "", names(d))

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
