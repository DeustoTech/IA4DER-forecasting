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
  feats <- read.csv("featuresPredicciones_3.csv")
  
  modelos <- c("rf", "lm", "svm", "nn", "gbm")
  modelos <- c("rf", "lm", "gbm")
  featuresets <- c("habitos", "cluster","edificio", "socio", "consumo", "tarifa")
  
  mapes <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana",
              "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana", "mapeEnsemble_mediana")
  modelosOG <- c("Media", "Naive", "Arima", "SN", "NN", "ETS", "SVM", "Ensemble")
  
  
  columns_to_select <- c("ID", mapes)  # Excluimos la primera columna de combined que es "ID"

  # Unir los dataframes por la columna "ID"
  df <- merge(feats[columns_to_select], combined, by = "ID", all.x = TRUE)
  df<- df[df$ID %in% combined$ID, ]
  sumMapes <- rowSums(df[, mapes], na.rm = T)
  df <- df[which(sumMapes != 0), ]
  sumMapes <- sumMapes[which(sumMapes != 0)]
  
  for (i in 1:nrow(df)){
    
    sumMape <- sumMapes[i]
    
    for (modelo in modelos){ # lm, gbm, svm, rf y nn
      
      for (set in featuresets){
        
        weighted_sum <- 0
        
        for (modeloOG in modelosOG){
          pred_col <- paste("Predicted", modeloOG, set, modelo, sep = "_")
          mape_col <- paste("mape", modeloOG, "_mediana", sep = "")
          print(pred_col)
          print(mape_col)
          print(colnames(df))
          weighted_sum <- weighted_sum + ifelse(is.na(df[i, mape_col] * df[i, pred_col]), 0, df[i, mape_col] * df[i, pred_col])
           }
        
        pBarra_col <- paste("pBarra", set, modelo, sep = "_")
        df[i, pBarra_col] <- (1 / sumMape) * weighted_sum
      }
    }
    
    
  }

  
  for (i in 1:nrow(combined)){
    
  }
  
  pbarras <- df %>% select(ID, starts_with("pBarra"))
  
  fwrite(pbarras, "sumaPonderada.csv")
 
  
  columnas_nuevas <- colnames(pbarras)[-which(colnames(pbarras) == "ID")]
  feats <- merge(feats, pbarras, by = "ID", all.x = TRUE, suffixes = c("", ""))
  
  
  fwrite(feats, "featuresPredicciones_3.csv")
  
  # mape del pbarra
  

  # Seleccionar las columnas necesarias de feats
  feats_subset <- feats[, c("ID", grep("^pBarra", names(feats), value = TRUE))]
  
  # Unir feats_subset con la columna "Real" de combined por la columna "ID"
  result_df <- merge(feats_subset, combined[c("ID", "Real")], by = "ID")

  result_df <- result_df %>%
    select(-ends_with(".1"))
  
  # Obtener el nombre de las columnas "pBarra"
  pbarra_columns <- grep("^pBarra", names(result_df), value = TRUE)
  
  # Iterar sobre las columnas "pBarra" y calcular el MAPE
  for (pbarra_col in pbarra_columns) {
    mape_col_name <- paste("mapePbarra", sub("^pBarra_", "", pbarra_col), sep = "_")
    
    for(i in 1:nrow(result_df)){
      result_df[i, mape_col_name] <- mape(result_df$Real[i], result_df[[i,pbarra_col]]) * 100
    }
    
  }
  

  
  # Filtrar columnas de result_df que empiezan por "mape"
  result_df_subset <- result_df[, c("ID", grep("^mape", names(result_df), value = TRUE))]
  
  # Fusionar feats y result_df_subset por la columna ID
  feats <- merge(feats, result_df_subset, by = "ID", all.x = TRUE)
  
  fwrite(feats, "featuresPredicciones_3.csv")
  
  
}
