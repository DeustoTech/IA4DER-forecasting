library(doFuture)
library(PMCMRplus)
library(mlr)
plan(multicore)

mape <- foreach(i=list.files("Scripts/FFORMA_errorNuevo/out",full.names="T"),.combine=rbind) %dofuture% {
  d <- t(read.csv(i,row.names=1))

  r <- d[,"real"]
  z <- r != 0

  out <- apply(d[,-1],2,function(x) {100*median(abs(x[z]-r[z])/r[z],na.rm=T)})
  return(out)
}

rmse <- foreach(i=list.files("out",full.names="T"),.combine=rbind) %dofuture% {
  d <- t(read.csv(i,row.names=1))
  r <- d[,"real"]

  out <- apply(d[,-1],2,function(x) {sqrt(median((r-x)^2,na.rm=T))})
  return(out)
}

summary(mape)
summary(rmse)

boxplot(mape,outline=F)
boxplot(rmse,outline=F)

fm <- frdAllPairsMillerTest(mape[complete.cases(mape),])
fr <- frdAllPairsMillerTest(rmse)


library(tidyverse)
library(furrr)

plan(multisession, workers = parallel::detectCores() - 1)
ruta <- "Scripts/FFORMA_errorNuevo/out"
archivos <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE)
procesar_archivo <- function(archivo) {
  d <- t(read.csv(archivo, row.names = 1))
  df <- as.data.frame(d)
  df$id <- tools::file_path_sans_ext(basename(archivo))
  modelos <- setdiff(colnames(df), c("real", "id"))
  for (modelo in modelos) {
    df[[paste0("MAPE_", modelo)]] <- ifelse(
      df$real != 0,
      abs(df[[modelo]] - df$real) / df$real * 100,
      NA
    )
  }
  return(df)
}
resultados <- future_map_dfr(archivos, procesar_archivo, .progress = TRUE)
fwrite(resultados, "Scripts/FFORMA_errorNuevo/modelosNuevosFFORMA_MAPE.csv")

