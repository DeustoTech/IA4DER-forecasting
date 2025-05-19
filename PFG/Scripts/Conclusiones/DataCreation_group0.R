library(data.table)
library(vtable)
library(zoo)
library(forecast)
library(neuralnet)
library(e1071)
library(doFuture)
library(matrixStats)
library(PMCMRplus)
library(rcompanion)
library(multcompView)
library(boot)
library(stringr)
library(arrow)
library(future.apply)
library(nnet)
library(dplyr)
library(tidyr)
library(ggplot2)

seriesN <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE.csv")
series1 <- fread("Scripts/Conclusiones/Series1.csv")

seriesN <- seriesN %>% select(id, real, rw_pred, rw_mape)

mape_promedios <- seriesN[, .(mape_medio = median(rw_mape, na.rm = TRUE)), by = id]
mejores_ids <- mape_promedios[order(mape_medio)][1:100, id] #opcion1: 201:300 // opcion2: 1:100
mejores_series <- seriesN[id %in% mejores_ids]
series <- mejores_series[, head(.SD, 150), by = id]

id_map <- data.table(
  id = unique(series$id),
  nuevo_id = 1:100
)
series0 <- merge(series, id_map, by = "id")
series0[, id := nuevo_id][, nuevo_id := NULL]
setorder(series0, id)

series0$tipo <- 0
set.seed(123)
series0[, random := sample(1:10, .N, replace = TRUE)]

series0$serie_id <- series0$id
series0$tiempo <- series1$tiempo
series0$valor <- series0$real

series0 <- series0 %>% select(serie_id, tiempo, valor, random, tipo)

fwrite(series0, "Scripts/Conclusiones/series0.csv")
