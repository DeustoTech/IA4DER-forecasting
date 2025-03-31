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

devtools::install_github("pmontman/customxgboost")
devtools::install_github("pmontman/fforma")

