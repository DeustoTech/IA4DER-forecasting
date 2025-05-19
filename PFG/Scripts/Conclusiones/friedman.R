library(tidyverse)

d <- data.table::fread("preds_MAPE_RMSE.csv")

d <- d %>% select(ends_with("mape"))
d <- d[is.finite(rowSums(d)),]
d <- d[,-c("ens_mape")]

n <- stringr::str_split_fixed(names(d),pattern="_",n=2)[,1]

d <- as.matrix(d)
rownames(d) <- 1:nrow(d)
colnames(d) <- n

d <- d[,order(robustbase::colMedians(d))]
b <- boxplot(d,outline=F)

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
