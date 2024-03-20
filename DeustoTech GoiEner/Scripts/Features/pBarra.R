library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "purrr") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}


# Leer

folder <- "Resultados/PrediccionErrorNew"

# Definir una función para buscar archivos por patrón en todas las carpetas de modelos
buscar_archivos_por_modelo <- function(folder, pattern) {
  # Obtener una lista de todas las subcarpetas dentro de la carpeta principal
  subcarpetas <- list.dirs(folder, recursive = FALSE)
  
  # Inicializar una lista para guardar los resultados
  archivos_modelo <- character()
  
  # Iterar sobre cada subcarpeta para buscar archivos que coincidan con el patrón
  for (subcarpeta in subcarpetas) {
    archivos_encontrados <- list.files(subcarpeta, pattern = pattern, recursive = TRUE, full.names = TRUE)
    archivos_modelo <- c(archivos_modelo, archivos_encontrados)
  }
  
  return(archivos_modelo)
}

model_files <- list(
  # Buscar archivos por cada tipo de modelo
  lm <- buscar_archivos_por_modelo(folder, "_lm_.*\\.csv$"),
  rf <- buscar_archivos_por_modelo(folder, "_rf_.*\\.csv$"),
  gbm <- buscar_archivos_por_modelo(folder, "_gbm_.*\\.csv$")
  #svm <- buscar_archivos_por_modelo(folder, "_svm_.*\\.csv$"),
  #nn <- buscar_archivos_por_modelo(folder, "_nn_.*\\.csv$")
)


# Función para leer archivos y combinarlos en un único dataframe
combinar_archivos_en_df <- function(archivos_modelo) {
  # Inicializar una lista para almacenar dataframes
  lista_de_dfs <- lapply(archivos_modelo, fread)
  #combined <- lista_de_dfs %>% bind_rows() %>% group_by(ID)
  #combined <- combined %>% 
  #  distinct(ID, .keep_all = TRUE)
  combined <- Reduce(function(x, y) merge(x, y, by = "ID", all = T), lista_de_dfs[1:10])
  return(combined)
  
  }
  


lm_df <- combinar_archivos_en_df(model_files[[1]])
fwrite(lm_df, "Resultados/PrediccionErrorNew/combined_lm.csv")
rf_df <- combinar_archivos_en_df(model_files[[2]])
fwrite(rf_df, "Resultados/PrediccionErrorNew/combined_rf.csv")
gbm_df <- combinar_archivos_en_df(model_files[[3]])
fwrite(gbm_df, "Resultados/PrediccionErrorNew/combined_gbm.csv")
#svm_df <- combinar_archivos_en_df(model_files[[4]])
#fwrite(svm_df, "Resultados/PrediccionErrorNew/combined_svm.csv")
#nn_df <- combinar_archivos_en_df(model_files[[5]])
#fwrite(nn_df, "Resultados/PrediccionErrorNew/combined_nn.csv")


#columns_to_merge_by <- c("ID", "Real_Arima", "Real_Ensemble", "Real_ETS", "Real_Media", "Real_Naive", "Real_NN", "Real_SN", "Real_SVM")

combinedPreds <- Reduce(function(x, y) merge(x, y, by = "ID"), list(lm_df, rf_df, gbm_df))
fwrite(combinedPreds, "Resultados/PrediccionErrorNew/combinedPreds.csv")


#LEER ARCHIVOS PARA PBARRA
allFeats <- fread("NUEVOS DATOS/combined_data.csv")
combinedPreds <- fread("Resultados/PrediccionErrorNew/combinedPreds.csv")
feats <- allFeats 
feats$ID <- feats$id
feats <- feats %>% select(-id, -V1_error)

colsY <- grep("\\.y$", names(combinedPreds), value = TRUE)
combinedPreds <- combinedPreds[, !(names(combinedPreds) %in% colsY), with = FALSE]
colsX <- grep("\\.x$", names(combinedPreds), value = TRUE)
combinedPreds <- combinedPreds[, !(names(combinedPreds) %in% colsX), with = FALSE]

datosCombinados <- merge(combinedPreds, feats, by = "ID")
#fwrite(datosCombinados, "datosParaPBarra.csv")
#datosCombinados <- fread("datosParaPBarra.csv")


#CALCULAR P BARRA

modelosC <- c("mean", "rw", "naive", "simple", "lr", "ann", "svm", "arima", "ses", "ens")
modelosP <- c("lm", "rf", "gbm")
features <- c("tarifa")


columnasPBarra <- paste("PBarra", rep(modelosP, each = length(features)), rep(features, times = length(modelosP)), sep = "_")
pb <- data.frame(matrix(ncol = length(columnasPBarra), nrow = nrow(datosCombinados)))
names(pb) <- columnasPBarra
pb$ID <- datosCombinados$ID
pb$Real_mean <- datosCombinados$Real_mean
pb$Real_rw <- datosCombinados$Real_rw
pb$Real_naive <- datosCombinados$Real_naive
pb$Real_simple <- datosCombinados$Real_simple
pb$Real_lr <- datosCombinados$Real_lr
pb$Real_ann <- datosCombinados$Real_ann
pb$Real_svm <- datosCombinados$Real_svm
pb$Real_arima <- datosCombinados$Real_arima
pb$Real_ses <- datosCombinados$Real_ses
pb$Real_ens <- datosCombinados$Real_ens


#BUCLE QUE HACE PBARRA
for (i in 1:nrow(datosCombinados)) {
  for (modeloP in modelosP) {
    for (feature in features) {
      # Restablecer numerador y denominador para cada combinación de modeloP y feature
      numerador <- 0
      denominador <- 0
      for (modeloC in modelosC) {
        
        #en vez de predicted column hay que poner el mape de verdad
        predicted_column <- paste("Predicted", modeloC, feature, modeloP, sep = "_")
        pred_median_column <- paste(modeloC, "_pred", sep = "")
        
        predicted_value <- datosCombinados[i, ..predicted_column, with = FALSE][[1]]
        pred_median_value <- datosCombinados[i, ..pred_median_column, with = FALSE][[1]]
        
        # Sumar al denominador, ignorando NA
        if (!is.na(predicted_value)) {
          denominador <- denominador + predicted_value
        }
        
        # Sumar al numerador, solo si ambos valores no son NA
        if (!is.na(predicted_value) && !is.na(pred_median_value)) {
          numerador <- numerador + (predicted_value * pred_median_value)
        }
      }
      
      pBarra_name <- paste("PBarra", modeloP, feature, sep = "_")
      
      # Asignar el valor calculado de pBarra en pb
      if(denominador != 0) {
        pb[i, pBarra_name] <- numerador / denominador
      } else {
        pb[i, pBarra_name] <- NA  # Asignar NA si el denominador es cero
      }
    }
  }
}


for (i in 1:nrow(datosCombinados)) {
      # Restablecer numerador y denominador para cada combinación de modeloP y feature
      numerador <- 0
      denominador <- 0
      for (modeloC in modelosC) {
        
        #en vez de predicted column hay que poner el mape de verdad
        predicted_column <- paste(modeloC, "_error", sep = "")
        pred_median_column <- paste(modeloC, "_pred", sep = "")
        
        predicted_value <- datosCombinados[i, ..predicted_column, with = FALSE][[1]]
        pred_median_value <- datosCombinados[i, ..pred_median_column, with = FALSE][[1]]
        
        # Sumar al denominador, ignorando NA
        if (!is.na(predicted_value)) {
          denominador <- denominador + predicted_value
        }
        
        # Sumar al numerador, solo si ambos valores no son NA
        if (!is.na(predicted_value) && !is.na(pred_median_value)) {
          numerador <- numerador + (predicted_value * pred_median_value)
        }
      }
      
      pBarra_name <- paste("PBarra_errorMape")
      
      # Asignar el valor calculado de pBarra en pb
      if(denominador != 0) {
        pb[i, pBarra_name] <- numerador / denominador
      } else {
        pb[i, pBarra_name] <- NA  # Asignar NA si el denominador es cero
      }
}


fwrite(pb, "Resultados/pBarras.csv")



#AÑADIR ENSEMBLE
pBarra_df <- fread("Resultados/pBarras.csv")

# Calculamos PBarra_Ensemble para cada conjunto de features
features <- c("tarifa")
modelos <- c("lm", "rf", "gbm")

for(feature in features){
  columnas <- paste0("PBarra_", modelos, "_", feature) # Nombres de las columnas actuales para la feature
  pBarra_df[, paste0("PBarra_Ensemble_", feature) := rowMeans(.SD, na.rm = TRUE), .SDcols = columnas] # Calcula la media y añade la nueva columna
}

fwrite(pBarra_df, "Resultados/pBarras.csv", row.names = FALSE)


#CALCULAR MAPE
pBarra_df <- fread("Resultados/pBarras.csv")

calculate_mape <- function(actual, predicted) {
  error <- abs((actual - predicted) / actual) * 100
  ifelse(is.nan(error) | is.infinite(error), 0, error)  # Manejo de divisiones por cero
}

pBarra_df <- as.data.frame(pBarra_df)

pbarra_columns <- grep("PBarra", names(pBarra_df), value=TRUE)
for (col in pbarra_columns) {
  pBarra_df[paste0(col, "_MAPE")] <- calculate_mape(pBarra_df$real_mediana, pBarra_df[[col]])
}

pBarra_df <- as.data.table(pBarra_df)

fwrite(pBarra_df, 'Resultados/pBarrasMAPE.csv', row.names = FALSE)

pBarrasMAPE <- fread("Resultados/pBarrasMAPE.csv")

summary(pBarrasMAPE)





summaryPredsFeats <- fread("Resultados/CUPS/SummaryPredsFeats.csv")

df_list <- c()

for (file in model_files){
  for (i in 1:length(file)) {
    df <- read.csv(file[i])
    
    # Extrae el nombre del modelo de la cadena entre "Pred_" y "_"
    nombre_modelo <- gsub(".*Pred_(.*?)_.*", "\\1", file[i])
    
    # Cambia el nombre de la columna "Real" a "RealNombreModelo"
    colname_a_cambiar <- "Real"
    nuevo_colname <- paste0(colname_a_cambiar, nombre_modelo)
    names(df)[names(df) == colname_a_cambiar] <- nuevo_colname
    print(paste("Nuevo nombre: ", nuevo_colname))
    # Agrega el dataframe modificado a la lista
    df_list <- append(df_list, list(df))
  }}

combined <- df_list %>% bind_rows() %>% group_by(ID)

combined <- combined %>% 
  distinct(ID, .keep_all = TRUE)


combined <- df_list %>% reduce(full_join, by = "ID")
colnames(combined)[colnames(combined) == "Real.x"] <- "Real"
combined <- combined %>%
  select(-starts_with("MAPE")) %>% select(c("ID","Real", starts_with("Predicted"))) 

combined <-  merge(combined, summaryPredsFeats, by = "ID", all=T)
  
fwrite(combined, "Resultados/PrediccionError/combinedPreds.csv")



# Calcular p barra 
{
  #combined <- read.csv("Resultados/PrediccionError/combinedPreds.csv")
  feats <- read.csv("Resultados/CUPS/combined_predFeats.csv")
  
  modelos <- c("rf", "lm", "svm", "nn", "gbm")
  featuresets <- c("habitos", "cluster","edificio", "socio", "consumo", "tarifa")
  
  mapes <- c("RealMedia", "RealArima", "RealEnsemble", "RealETS",
              "RealSVM", "RealNaive", "RealSN", "RealNN")
  modelosOG <- c("Media", "Naive", "Arima", "SN", "NN", "ETS", "SVM", "Ensenmble")
  

  # Unir los dataframes por la columna "ID"

  #sumMapes <- rowSums(combined[, mapes], na.rm = T)
  #combined <- combined[which(sumMapes != 0), ]
  #sumMapes <- sumMapes[which(sumMapes != 0)]
  
  df <- feats
  
  for (i in 1:nrow(df)){
    
    sumMape <- df$real_mediana
    
    for (modelo in modelos){ # lm, gbm, svm, rf y nn
      
      for (set in featuresets){
        
        weighted_sum <- 0
        
        for (modeloOG in modelosOG){
          pred_col <- paste("pred", modeloOG, "_mediana", sep = "") # MAL. PREDICCION DEL MODELO OG
          mape_col <- "real_mediana"
          if (pred_col %in% colnames(df) && mape_col %in% colnames(df)) { #&& !is.na(df[i, mape_col]) && !is.na(df[i, pred_col])) {
            weighted_sum <- weighted_sum + df[i, mape_col] * df[i, pred_col]
            }          # weighted_sum <- weighted_sum + ifelse(is.na(df[i, mape_col] * df[i, pred_col]), 0, df[i, mape_col] * df[i, pred_col])
        }
        
        
        
        pBarra_col <- paste("pBarra", set, modelo, sep = "_")
        df[i, pBarra_col] <- ifelse(sumMape[i] > 0, (1 / sumMape[i]) * weighted_sum[i], 0)
      }
    }
    
    
  }


  df <- df %>%
    mutate(across(starts_with("pBarra_"), ~na_if(.x, 0)))
  pbarras <- df %>% select(ID, starts_with("pBarra"))
  
  fwrite(pbarras, "pBarra")
 
  
  columnas_nuevas <- colnames(pbarras)[-which(colnames(pbarras) == "ID")]
  
  pbarra_existing_cols <- setdiff(intersect(names(feats), names(pbarras)), "ID")
  if (length(pbarra_existing_cols) > 0) {
    feats <- feats[, !(names(feats) %in% pbarra_existing_cols)]
  }
  
  #feats <- merge(feats, pbarras, by = "ID", all.x = TRUE, suffixes = c("", ""))
  feats <- merge(feats, pbarras, by = "ID", all.x = TRUE)
  
  
  fwrite(feats, "allFeatsNew.csv")
  
  # mape del pbarra
  
  feats <- read.csv("allFeatsNew.csv")
  pbarra_columns <- grep("^pBarra", names(feats), value = TRUE)
  mape_columns <- grep("^mape.*_mediana$", names(feats), value = TRUE)
  
  pbarra <- feats[which(!is.na(feats$mapeEnsemble_mediana)), c("ID", mape_columns, pbarra_columns), drop = FALSE]
  
  for (real in mape_columns) {
    # Iterar sobre las columnas "pBarra" y calcular el MAPE
    for (pbarra_col in pbarra_columns) {
      # Obtener el nombre de la columna de MAPE específica para la pareja actual
      mape_col_name <- paste("mapePbarra", sub("^pBarra_", "", pbarra_col), sub("^mape(.+)_mediana$", "\\1", real), sep = "_")
      
      # Calcular el MAPE para cada fila
      for (i in 1:nrow(pbarra)) {
        pbarra[i, mape_col_name] <- mape(pbarra[[real]][i], pbarra[[i, pbarra_col]]) * 100
      }
    }
  }
  
  # Filtrar columnas de result_df que empiezan por "mape"
  subset <- pbarra[, c("ID", grep("^mape", names(pbarra), value = TRUE))]
  
  # Fusionar feats y result_df_subset por la columna ID
  pbarra_existing_cols <- setdiff(intersect(names(feats), names(subset)), "ID")
  if (length(pbarra_existing_cols) > 0) {
    feats <- feats[, !(names(feats) %in% pbarra_existing_cols)]
  }
  feats <- merge(feats, subset, by = "ID", all.x = TRUE)
  
  fwrite(feats, "allFeatsNew.csv")
  
  
}

allFeatsNew <- fread("allFeatsNew.csv")
print(head(allFeatsNew))
