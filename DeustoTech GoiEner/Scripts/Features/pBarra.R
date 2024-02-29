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

folder <- "Resultados/PrediccionError/AllFeats"


# Lista de modelos y sus correspondientes archivos
model_files <- list(
  lm = list.files(folder, pattern = "_lm.csv$", recursive = TRUE, full.names = TRUE),
  rf = list.files(folder, pattern = "_rf.csv$", recursive = TRUE, full.names = TRUE),
  gbm = list.files(folder, pattern = "_gbm.csv$", recursive = TRUE, full.names = TRUE),
  nn = list.files(folder, pattern = "_nn.csv$", recursive = TRUE, full.names = TRUE),
  svm = list.files(folder, pattern = "_svm.csv$", recursive = TRUE, full.names = TRUE)
)


#CARGA RESULTADOS NUEVOS
folder <- "Resultados/PrediccionError/AllFeats/"

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
  gbm <- buscar_archivos_por_modelo(folder, "_gbm_.*\\.csv$"),
  svm <- buscar_archivos_por_modelo(folder, "_svm_.*\\.csv$"),
  nn <- buscar_archivos_por_modelo(folder, "_nn_.*\\.csv$")
)


# Función para leer archivos y combinarlos en un único dataframe
combinar_archivos_en_df <- function(archivos_modelo) {
  # Inicializar una lista para almacenar dataframes
  lista_de_dfs <- lapply(archivos_modelo, fread)
  
  combined <- lista_de_dfs %>% bind_rows() %>% group_by(ID)
  
  combined <- combined %>% 
    distinct(ID, .keep_all = TRUE)
  
  return(combined)
  
  }
  

lm_df <- combinar_archivos_en_df(lm)
fwrite(lm_df, "Resultados/PrediccionError/combined_lm.csv")
rf_df <- combinar_archivos_en_df(rf)
fwrite(lm_df, "Resultados/PrediccionError/combined_rf.csv")
gbm_df <- combinar_archivos_en_df(gbm)
fwrite(lm_df, "Resultados/PrediccionError/combined_gbm.csv")
svm_df <- combinar_archivos_en_df(svm)
fwrite(lm_df, "Resultados/PrediccionError/combined_svm.csv")
nn_df <- combinar_archivos_en_df(nn)
fwrite(lm_df, "Resultados/PrediccionError/combined_nn.csv")
colnames(lm_df)


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

fwrite(combined, "Resultados/PrediccionError/combinedPreds.csv")



# Calcular p barra 
{
  combined <- read.csv("Resultados/PrediccionError/combinedPreds.csv")
  feats <- read.csv("Resultados/CUPS/combined_predFeats.csv")
  
  modelos <- c("rf", "lm", "svm", "nn", "gbm")
  featuresets <- c("habitos", "cluster","edificio", "socio", "consumo", "tarifa")
  
  mapes <- c("RealMedia", "RealArima", "RealEnsemble", "RealETS",
              "RealSVM", "RealNaive", "RealSN", "RealNN")
  modelosOG <- c("Media", "Naive", "Arima", "SN", "NN", "ETS", "SVM", "Ensemble")
  

  # Unir los dataframes por la columna "ID"

  sumMapes <- rowSums(combined[, mapes], na.rm = T)
  combined <- combined[which(sumMapes != 0), ]
  sumMapes <- sumMapes[which(sumMapes != 0)]
  
  df <- combined
  
  for (i in 1:nrow(df)){
    
    sumMape <- sumMapes[i]
    
    for (modelo in modelos){ # lm, gbm, svm, rf y nn
      
      for (set in featuresets){
        
        weighted_sum <- 0
        
        for (modeloOG in modelosOG){
          pred_col <- paste("Predicted", modeloOG, set, modelo, sep = "_") # MAL. PREDICCION DEL MODELO OG
          mape_col <- paste("Real", modeloOG,sep = "")
          if (pred_col %in% colnames(df) && mape_col %in% colnames(df) && !is.na(df[i, mape_col]) && !is.na(df[i, pred_col])) {
            weighted_sum <- weighted_sum + df[i, mape_col] * df[i, pred_col]
          }          # weighted_sum <- weighted_sum + ifelse(is.na(df[i, mape_col] * df[i, pred_col]), 0, df[i, mape_col] * df[i, pred_col])
        }
        
        
        
        pBarra_col <- paste("pBarra", set, modelo, sep = "_")
        df[i, pBarra_col] <- ifelse(sumMape > 0, (1 / sumMape) * weighted_sum, 0)
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
