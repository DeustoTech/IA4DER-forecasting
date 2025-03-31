library(foreach)
library(doParallel)

to_install <- c("ggplot2", "lattice", "caret", "fpp3", "class",
                "forecast", "Metrics", "fable", 
                "data.table", "xts", "future", "foreach", "doParallel", "RSNNS", "TTR", 
                "quantmod", "car", "e1071", "nnet", "tools", "doFuture", "neuralnet", "gbm", 
                "randomForest", "mice", "mltools", "zoo", "mlr3", "mlr3learners", "mlr3tuning", "mlr3verse",
                "paradox", "xgboost", "kknn", "scales") 

installed <- rownames(installed.packages())
for (lib in to_install) {
  if (!(lib %in% installed)) install.packages(lib, dependencies = TRUE)
  library(lib, character.only = TRUE)
}

devtools::install_github("pmontman/customxgboost", upgrade = "never")
devtools::install_github("pmontman/fforma", upgrade = "never")


library(fforma)
library(readr)
library(dplyr)
library(purrr)
library(future)


input_folder <- "NUEVOS DATOS/OriginalData/prueba/"
output_folder <- "NuevosResultados/FFORMA/predicciones_fforma_prueba"
dir.create(output_folder, showWarnings = FALSE)
chunk_size <- 10
save_folder <- "NuevosResultados/FFORMA/temp_fforma"

#plan(multisession)
leer_csv_como_serie <- function(filepath, h = 24) {
  df <- read_csv(filepath, col_types = cols())
  kWh <- df$kWh
  ts_data <- ts(kWh, frequency = 24) 
  list(
    x = head(ts_data, length(ts_data) - h),
    xx = tail(ts_data, h),
    h = h
  )
}

archivos <- list.files(input_folder, full.names = TRUE)
ids_series <- tools::file_path_sans_ext(basename(archivos))
ts_dataset <- map(archivos, leer_csv_como_serie)
names(ts_dataset) <- ids_series

options(future.globals.maxSize = 2 * 1024^3) #para que no de error

fforma_fit <- train_metalearning(
  ts_dataset,
  chunk_size = chunk_size,
  save_foldername = save_folder
)

fforma_forec <- forecast_metalearning(
  fforma_fit,
  ts_dataset,
  chunk_size = chunk_size,
  save_foldername = save_folder
)

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

