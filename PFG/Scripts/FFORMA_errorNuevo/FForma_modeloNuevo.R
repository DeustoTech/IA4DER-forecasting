#library(foreach)
#library(doParallel)

# to_install <- c("ggplot2", "lattice", "caret", "fpp3", "class",
#                 "forecast", "Metrics", "fable", 
#                 "data.table", "xts", "future", "foreach", "doParallel", "RSNNS", "TTR", 
#                 "quantmod", "car", "e1071", "nnet", "tools", "doFuture", "neuralnet", "gbm", 
#                 "randomForest", "mice", "mltools", "zoo", "mlr3", "mlr3learners", "mlr3tuning", "mlr3verse",
#                 "paradox", "xgboost", "kknn", "scales") 
# 
# installed <- rownames(installed.packages())
# for (lib in to_install) {
#   if (!(lib %in% installed)) install.packages(lib, dependencies = TRUE)
#   library(lib, character.only = TRUE)
# }
# 
devtools::install_github("pmontman/customxgboost", upgrade = "never")
devtools::install_github("pmontman/fforma", upgrade = "never")
devtools::install_local("./fforma/",force=T)
devtools::reload(pkg="fforma")

library(readr)
library(dplyr)
library(purrr)
library(furrr)
library(fforma)
library(future)
#plan(multisession)
plan(multicore)

input_folder <- "goi4_pst"
output_folder <- "out"
save_folder <- "tmp"
chunk_size <- 500

# input_folder <- "NUEVOS DATOS/OriginalData/prueba"
# output_folder <- "NuevosResultados/FFORMA/out"
# save_folder <- "NuevosResultados/FFORMA/tmp"

dir.create(output_folder,showWarnings = FALSE, recursive = TRUE)
dir.create(save_folder,  showWarnings = FALSE, recursive = TRUE)

leer_csv_como_serie <- function(filepath, h = 24) {
  df <- read_csv(filepath, col_types = cols())
  kWh <- df$kWh
  ts_data <- ts(kWh, frequency = 24) 
  list(
    x = tail(head(ts_data, length(ts_data) - h), 21 * h),
    xx = tail(ts_data, 7 * h),
    h = h
  )
}

archivos <- list.files(input_folder, full.names = TRUE)
ids_series <- tools::file_path_sans_ext(basename(archivos))

validar_serie <- function(serie) {
  x <- serie$x
  return(length(x) > 24 && !all(is.na(x)) && sd(x, na.rm = TRUE) > 0)
}

ts_dataset <- future_map(archivos, leer_csv_como_serie, .options = furrr_options(seed = TRUE))
ts_dataset <- ts_dataset[map_lgl(ts_dataset, validar_serie)]
names(ts_dataset) <- ids_series[names(ts_dataset) %in% names(ts_dataset)]

saveRDS(ts_dataset, file = "data.rds") # ts_dataset <- readRDS(file = "data.rds")

options(future.globals.maxSize = 2 * 1024^3) #para que no de error

fforma_fit <- train_metalearning(
  ts_dataset,
  chunk_size = chunk_size,
#  objective = "selection",
  save_foldername = save_folder
)

saveRDS(fforma_fit, file = "fforma_fit.rds") # fforma_fit <- readRDS(file = "fforma_fit.rds")

fforma_forec <- forecast_metalearning(
  fforma_fit,
  ts_dataset,
  chunk_size = chunk_size,
  save_foldername = save_folder
)

# fforma_forec <- readRDS(file = "fforma_forec.rds")

walk2(fforma_forec$dataset, names(fforma_forec$dataset), function(serie, id) {
  predicciones <- tibble(
    paso = 1:length(serie$ff_meta_avg),
    prediccion = serie$ff_meta_avg,
    real = serie$xx
  )
  
  write_csv(predicciones, file.path(output_folder, paste0(id, "_forecast.csv")))
  if (!is.null(serie$smape)) {
    metricas <- tibble(
      id = id,
      SMAPE = serie$smape,
      MASE = serie$mase,
      OWA = serie$owa
    )
    write_csv(metricas, file.path(output_folder, paste0(id, "_metrics.csv")))
  }
})



