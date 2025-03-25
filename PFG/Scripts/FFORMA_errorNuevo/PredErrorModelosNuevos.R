# FFORMA SCRIPT: PREDICT ERROR OF EACH MODEL GIVEN A SET OF FEATURES

library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'car', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', 
               "randomForest", "mice", "mltools", "zoo") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

library(scales)
library(stringr)
library(skimr)

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

target <- "mean_mape"

library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3verse)
library(paradox)
library(xgboost)
library(kknn)

features <- setdiff(
  names(df), 
  c("id", "dia", "real", grep("_pred", names(df), value = TRUE), grep("_mape", names(df), value = TRUE), target)
)

dataset_modelo <- df[, c(features, target), with = FALSE]
dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))]
dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) if (is.character(x)) as.factor(x) else x)]
dataset_modelo <- dataset_modelo[, lapply(.SD, function(x) {
  if (is.character(x) || is.factor(x)) {
    as.numeric(as.factor(x))
  } else {
    x
  }
})]

task <- TaskRegr$new(id = "predict_mape", backend = dataset_modelo, target = target)
learners <- list(
  lrn("regr.ranger", id = "RandomForest"),
  lrn("regr.xgboost", id = "XGBoost"),
  lrn("regr.svm", id = "SVM"),
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

