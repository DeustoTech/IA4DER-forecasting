# FFORMA SCRIPT: PREDICT ERROR OF EACH MODEL GIVEN A SET OF FEATURES

library(foreach)
library(doParallel)

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "forecast", "Metrics", "fable", "data.table", "xts", 
               "future", "foreach", "doParallel", "RSNNS", "TTR", 
               "quantmod", "car", "e1071", "nnet", "tools", "doFuture", 
               "neuralnet", "gbm", "randomForest", "mice", "mltools", "zoo",
               "scales", "stringr", "skimr")

# Comprobar qué paquetes no están instalados
paquetes_faltantes <- librerias[!(librerias %in% installed.packages()[, "Package"])]

# Instalar los paquetes que faltan
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

# Cargar todos los paquetes
invisible(lapply(librerias, library, character.only = TRUE))
# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'car', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mice", "mltools", "zoo") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3verse)
library(paradox)
library(xgboost)
library(kknn)

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

df <- df %>% select ( - hora)

non_numerical_columns <- setdiff(names(df), c("id", "dia",numerical_columns))

df <- df[, c(.SD[, lapply(.SD, median, na.rm = TRUE), .SDcols = numerical_columns],  # Media en numéricas
                             .SD[, lapply(.SD, first), .SDcols = non_numerical_columns]),           # Primer valor en no numéricas
                         by = c("id", "dia")]

#eliminar columnas que tiene demasiados na (solo son p3,p4,p5,p6)
cols_na_threshold <- sapply(df, function(x) mean(is.na(x))) > 0.3 
df <- df[, !cols_na_threshold, with = FALSE]

fwrite(df, "NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO2/preds_MAPE_reducido.csv")


###############

df <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/REDUCIDO2/preds_MAPE_reducido.csv")

target <- "mean_mape"


features <- setdiff(
  names(df), 
  c( grep("_pred", names(df), value = TRUE), grep("_mape", names(df), value = TRUE), target)
)

dataset_modelo <- df[, c(features, target), with = FALSE]
dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))]
id_column <- dataset_modelo$id
dataset_modelo <- dataset_modelo[, !"id", with = FALSE]
dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) if (is.character(x)) as.factor(x) else x)]
dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) {
  if (is.character(x) || is.factor(x)) {
    as.numeric(as.factor(x))
  } else {
    x
  }
})]
dataset_modelo$id <- id_column

################3

task <- TaskRegr$new(id = "predict_mape", backend = dataset_modelo, target = target)
learners <- list(
  lrn("regr.ranger", id = "RandomForest"),
  lrn("regr.xgboost", id = "XGBoost"),
  #lrn("regr.svm", id = "SVM"),
  lrn("regr.kknn", id = "KNN"),
  lrn("regr.lm", id = "LinearRegression")
)
resampling <- rsmp("cv", folds = 5)
measure <- msr("regr.rmse")
bmr_grid <- benchmark_grid(
  tasks = task,
  learners = learners,
  resamplings = resampling
)

bmr <- benchmark(bmr_grid)

# ------------ 5. Resultados y comparación --------------
benchmark_results <- bmr$aggregate(measure)
print(benchmark_results)
autoplot(bmr)  # Si quieres ver un gráfico

# ------------ 6. Opcional: Mejor modelo y tuning --------------
# Tuning ejemplo sobre RandomForest
learner_rf <- lrn("regr.ranger")
search_space <- ps(
  mtry = p_int(1, length(features)),
  min.node.size = p_int(1, 20),
  num.trees = p_int(100, 500)
)

tuner <- tnr("grid_search", resolution = 5)

auto_tuner_rf <- AutoTuner$new(
  learner = learner_rf,
  resampling = resampling,
  measure = measure,
  search_space = search_space,
  tuner = tuner,
  terminator = trm("evals", n_evals = 20)
)

# Entrenar tuning
auto_tuner_rf$train(task)

# Ver mejor configuración
auto_tuner_rf$archive


task <- TaskRegr$new(id = "predict_mape", backend = dataset_modelo, target = target)

# ------------ 3. Entrenar un modelo final (Random Forest) y predecir --------------
learner_rf <- lrn("regr.xgboost", id = "XGBoost")
learner_rf$train(task)
pred_rf <- learner_rf$predict(task)

# ------------ 4. Guardar resultados en CSV --------------
resultados <- data.table(
  id = dataset_modelo$id,
  dia = dataset_modelo$dia,
  real = dataset_modelo$real,
  Predicted_mape_mean = pred_rf$response,
  Error_mape_mean = abs(pred_rf$response - dataset_modelo$mean_mape)
)

fwrite(resultados, "resultados_mape_mean.csv")


#############3

dataset_modelo <- dataset_modelo[1:15000, ]

task <- TaskRegr$new(id = "predict_mape", backend = dataset_modelo, target = target)

tune_and_train <- function(learner_id, search_space, file_name) {
  learner <- lrn(learner_id)
  at <- AutoTuner$new(
    learner = learner,
    resampling = rsmp("holdout"),  # tuning rápido
    measure = msr("regr.mape"),   # Usar MAPE como métrica de evaluación
    search_space = search_space,
    tuner = tnr("random_search"),
    terminator = trm("evals", n_evals = 10)  # tuning rápido
  )
  
  at$train(task)
  pred <- at$predict(task)
  
  resultados <- data.table(
    id = dataset_modelo$id,
    dia = dataset_modelo$dia,
    real = dataset_modelo$real,
    predicted_mape_mean = pred$response,
    error_mape_mean = abs((pred$response - dataset_modelo$mean_mape) / dataset_modelo$mean_mape) * 100
  )
  
  fwrite(resultados, file_name)
}

# Random Forest
tune_and_train("regr.ranger", ps(
  mtry = p_int(1, length(features)),
  min.node.size = p_int(1, 10),
  num.trees = p_int(100, 300)
), "resultados_rf.csv")

# XGBoost
tune_and_train("regr.xgboost", ps(
  eta = p_dbl(0.01, 0.3),
  max_depth = p_int(3, 8),
  nrounds = p_int(50, 150)
), "resultados_xgboost.csv")

# SVM
tune_and_train("regr.svm", ps(
  cost = p_dbl(0.1, 10),
  gamma = p_dbl(0.01, 0.1)
), "resultados_svm.csv")

# KNN
tune_and_train("regr.kknn", ps(
  k = p_int(3, 15)
), "resultados_knn.csv")

# Linear Regression (sin tuning, solo entrenamiento directo)
learner_lm <- lrn("regr.lm")
learner_lm$train(task)
pred_lm <- learner_lm$predict(task)
resultados_lm <- data.table(
  id = datos_reducidos$id,
  dia = datos_reducidos$dia,
  real = datos_reducidos$real,
  predicted_mape_mean = pred_lm$response,
  error_mape_mean = abs((pred_lm$response - datos_reducidos$mean_mape) / datos_reducidos$mean_mape) * 100
)
fwrite(resultados_lm, "resultados_linear_regression.csv")



##############3

df_model <- df[, c(features, target), with = FALSE]

numeric_cols <- sapply(df_model, is.numeric)

cor_matrix <- cor(df_model[, ..numeric_cols], use = "complete.obs")
high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)
cols_to_impute <- setdiff(names(df_model)[numeric_cols], 
                          c("id", "dia", colnames(cor_matrix)[high_cor]))

df_model_subset <- df_model[, .SD, .SDcols = c(cols_to_impute)]

set.seed(123)
imputation_method <- mice(df_model_subset, method = "pmm", m = 5, maxit = 50)

# Completar el dataset
df_model_imputed <- complete(imputation_method, 1)  # Usar el primer conjunto imputado
for (col in setdiff(names(df_model), names(df_model_subset))) {
  if (is.factor(df_model[[col]])) {
    df_model_imputed[[col]] <- df_model[[col]]
  }
}

set.seed(123)
trainIndex <- createDataPartition(df_model_imputed[[target]], p = 0.8, list = FALSE)
trainData <- df_model[trainIndex, ]
testData <- df_model[-trainIndex, ]

control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)




df_model_df <- as.data.frame(df_model_imputed)

# Modelo de prueba
test_model <- randomForest(
  as.formula(paste(target, "~ .")), 
  data = df_model_df
)
# Modelos a probar
model_list <- c("rf", "xgbTree", "gbm", "svmRadial", "nnet")

resultados <- list()

for (modelo in model_list) {
  cat("Entrenando:", modelo, "\n")
  fit <- train(
    as.formula(paste(target, "~ .")),
    data = trainData,
    method = modelo,
    trControl = control,
    preProcess = c("center", "scale"),
    metric = "MAPE"
  )
  resultados[[modelo]] <- fit
}

# Comparar resultados
resamples <- resamples(resultados)
summary(resamples)
bwplot(resamples)

