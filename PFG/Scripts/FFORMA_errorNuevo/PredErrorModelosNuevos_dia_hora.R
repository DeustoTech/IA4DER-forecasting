# FFORMA SCRIPT: PREDICT ERROR OF EACH MODEL GIVEN A SET OF FEATURES

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

paths <- c(
  "NuevosResultados",
  "NuevosResultados/PrediccionErrorNuevo",
  "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE",
  "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA"
)

for (path in paths) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

#EJECUTAR
metadataNew <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/allMetadataDEF.csv")

df <- metadataNew %>% select(- contains("_rmse")) #quitamos rmse porque no lo estamos usando

df[sapply(df, is.infinite)] <- NA
df[is.na(cnae),     cnae:=9]
df[is.na(zip_code), zip_code:=0]

df[,p:=rescale(pmax(df$p1,df$p2,df$p3,df$p4,df$p5,df$p6,na.rm=TRUE))] # by default rescale to to = c(0, 1)

df$contracted_tariff <- factor(df$contracted_tariff)
df$cnae     <- factor(floor(df$cnae/1000))
df$zip_code <- factor(floor(df$zip_code/1000))
df$pf       <- factor(floor(df$p*100))
df$dia      <- factor(df$dia)
df$hora     <- factor(df$hora)

## Remove outlayers
MAPE   <- c("mean_mape","rw_mape","naive_mape","simple_mape","lr_mape","ann_mape","svm_mape","arima_mape","ses_mape","ens_mape")
ERRORS <- c(MAPE)
for (e in ERRORS)  # Replace outliers with NA
{
  z <- as.numeric(quantile(df[,..e],c(0.25,0.75),na.rm=T))
  df[,(e):=ifelse(get(e)>z[2]+1.5*(z[2]-z[1]), NA, get(e))]
}

## Scale MAPE to [0,1] as it seems caret is not doing a good job
mmape <- max(df[,..MAPE],na.rm=T)
for (e in MAPE) df[,(e):=get(e)/mmape]


numerical_columns <- c("real", "mean_pred", "rw_pred", "naive_pred", "simple_pred",
                       "lr_pred", "ann_pred", "svm_pred", "arima_pred", "ses_pred", 
                       "ens_pred", "mean_mape", "rw_mape", "naive_mape", "simple_mape", 
                       "lr_mape", "ann_mape", "svm_mape", "arima_mape", "ses_mape", 
                       "ens_mape")

non_numerical_columns <- setdiff(names(df), c("id", "dia","hora", numerical_columns))

df <- df[, c(.SD[, lapply(.SD, median, na.rm = TRUE), .SDcols = numerical_columns],  # Media en numéricas
             .SD[, lapply(.SD, first), .SDcols = non_numerical_columns]),           # Primer valor en no numéricas
         by = c("id")]

#eliminar columnas que tiene demasiados na (solo son p3,p4,p5,p6)
cols_na_threshold <- sapply(df, function(x) mean(is.na(x))) > 0.3 
df <- df[, !cols_na_threshold, with = FALSE]

fwrite(df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/preds_MAPE_reducido.csv")


###############

df <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/preds_MAPE_reducido.csv")

mape_targets <- c("mean_mape", "rw_mape", "naive_mape", "simple_mape", 
                  "lr_mape", "ann_mape", "svm_mape", "arima_mape", "ses_mape", "ens_mape")
mape_targets <- c("lr_mape", "arima_mape", "ses_mape", "ens_mape")

# Función general
for (target in mape_targets) {
  
  features <- setdiff(
    names(df), 
    c(grep("_pred", names(df), value = TRUE), grep("_mape", names(df), value = TRUE), target)
  )
  
  resultados_base <- df[, .(id, real, target_value = get(target))]
  
  dataset_modelo <- df[, c(features, target), with = FALSE]
  dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))]
  dataset_modelo <- dataset_modelo[, !c("id", "real"), with = FALSE]
  dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) if (is.character(x)) as.numeric(as.factor(x)) else x)]
  
  #preguntar a cruz si esto es buena idea
  #dataset_modelo[[target]][dataset_modelo[[target]] == 0] <- 1e-5
  #resultados_base$target_value[resultados_base$target_value == 0] <- 1e-5
  
  task <- TaskRegr$new(id = paste0("predict_", target), backend = dataset_modelo, target = target)
  
  tune_and_train <- function(learner_id, search_space, file_name, model_name, extra_params = list()) {
    learner <- lrn(learner_id)
    
    if (length(extra_params) > 0) {
      learner$param_set$values <- extra_params
    }
    
    at <- AutoTuner$new(
      learner = learner,
      resampling = rsmp("cv", folds = 3),
      measure = msr("regr.rmse"),
      search_space = search_space,
      tuner = tnr("random_search"),
      terminator = trm("evals", n_evals = 10),
      store_tuning_instance = TRUE,
      store_models = TRUE
    )
    
    
    tryCatch({
      at$train(task)
      
      # Guardar archivo archive solo con combinaciones válidas (sin NA)
      archive_dt <- at$archive$data[!is.na(regr.rmse)]
      list_cols <- sapply(archive_dt, is.list)
      if (any(list_cols)) {
        archive_dt <- archive_dt[, !list_cols, with = FALSE]
      }
      fwrite(archive_dt, paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/Archive_", model_name, "_", target, ".csv"))
      
      saveRDS(at, paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/Modelo_", model_name, "_", target, ".rds"))
      
      pred <- at$predict(task)
      
      resultados <- data.table(
        id = resultados_base$id,
        Real_value = resultados_base$target_value,
        predicted_value = pred$response,
        error_mape = abs((pred$response - resultados_base$target_value) / resultados_base$target_value) * 100
      )
      
      fwrite(resultados, file_name)
    }, error = function(e) {
      message(paste0("Error en tuning para ", model_name, " con target ", target, ": ", e$message))
    })
  }
  
  # Random Forest
  tune_and_train("regr.ranger", ps(
    mtry = p_int(1, length(task$feature_names)),
    min.node.size = p_int(1, 10),
    num.trees = p_int(100, 300)
  ), paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/Pred_", target, "_rf.csv"), "rf")
  
  # XGBoost
  tune_and_train("regr.xgboost", ps(
    eta = p_dbl(0.01, 0.3),
    max_depth = p_int(3, 8),
    nrounds = p_int(50, 150)
  ), paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/Pred_", target, "_xgboost.csv"), "xgboost")
  
  # SVM
  #tune_and_train("regr.svm", ps(
  #  cost = p_dbl(0.1, 10),
  #  gamma = p_dbl(0.01, 0.1)
  #), paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA/Pred_", target, "_svm.csv"), "svm",
  #extra_params = list(type = "eps-regression", kernel = "radial"))
  
  # KNN
  tune_and_train("regr.kknn", ps(
    k = p_int(3, 15)
  ), paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/Pred_", target, "_knn.csv"), "knn")
  
  # Linear Regression (sin tuning)
  learner_lm <- lrn("regr.lm")
  learner_lm$train(task)
  saveRDS(learner_lm, paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/Modelo_lm_", target, ".rds"))
  
  pred_lm <- learner_lm$predict(task)
  resultados_lm <- data.table(
    id = resultados_base$id,
    Real_value = resultados_base$target_value,
    predicted_value = pred_lm$response,
    error_mape = abs((pred_lm$response - resultados_base$target_value) / resultados_base$target_value) * 100
  )
  
  fwrite(resultados_lm, paste0("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO_DIA_HORA/Pred_", target, "_lm.csv"))
}

