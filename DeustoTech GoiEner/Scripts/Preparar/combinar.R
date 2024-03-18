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


#COMBINAR LAS PREDICCIONES DE LAS SUMMARYS CON EL CSV ALLFEATS

#leer
{
  feats <- fread("allFeats.csv")
  
  summaryMedia <- fread("Resultados/CUPS/SummaryMedia.csv")
  summaryNaive <- fread("Resultados/CUPS/SummaryNaive.csv")
  summarySN <- fread("Resultados/CUPS/SummarySNaive.csv")
  summaryArima <- fread("Resultados/CUPS/SummaryArima.csv")
  summaryETS <- fread("Resultados/CUPS/SummaryETS.csv")
  summaryNN <- fread("Resultados/CUPS/SummaryNN.csv")
  summarySVM <- fread("Resultados/CUPS/SummarySVM.csv")
  #summaryEnsemble <- fead("Resultados/CUPS/SummaryEnsemble.csv")
  
  summaryPredsFeats <- fread("Resultados/CUPS/SummaryPredsFeats.csv")
  predFeats <- fread("Resultados/CUPS/predFeats.csv")
}

#combinar summarys de cad modelos en uno
{
# Lista de tus data frames
df_list <- list(summaryMedia, summaryNaive, summarySN, summaryArima, summaryETS, summaryNN, summarySVM)

# Función para renombrar columnas duplicadas excepto ID
rename_duplicated <- function(df, suffix) {
  cols <- colnames(df)
  # No renombrar el ID
  cols[cols != "ID"] <- paste0(cols[cols != "ID"], suffix)
  colnames(df) <- cols
  return(df)
}

modelos <- c("Media", "Naive", "SN", "Arima", "ETS", "NN", "SVM")

# Aplicamos la función para renombrar las columnas (excepto en el primero)
df_list_renamed <- lapply(seq_along(df_list), function(i) {
  if (i == 1) {
    df_list[[i]]  # No renombrar el primero
  } else {
    rename_duplicated(df_list[[i]], paste0("_", i))
  }
})
feats2 <- feats %>% select(-contains("pBarra"), -starts_with("mape"))
# Ahora combinamos usando reduce y full_join sin errores de duplicados
combined_df_feats <- reduce(df_list_renamed, full_join, by = "ID")

combined_bien <- combined_df_feats %>% select(ID, real_mediana, real_q1, real_q3, starts_with("mape"), starts_with("pred"))
colnames(combined_bien) <- sub("_[0-9]+$", "", names(combined_bien))

subset_combined_bien <- combined_bien %>%
  anti_join(summaryPredsFeats, by = "ID")

medianas <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana", "mapeETS_mediana", "mapeNN_mediana", "mapeSVM_mediana") 
q1 <- c("mapeMedia_q1", "mapeNaive_q1", "mapeSN_q1", "mapeArima_q1", "mapeETS_q1", "mapeNN_q1", "mapeSVM_q1") 
q3 <- c("mapeMedia_q3", "mapeNaive_q3", "mapeSN_q3", "mapeArima_q3", "mapeETS_q3", "mapeNN_q3", "mapeSVM_q3") 

predmedianas <- c("predMedia_Mediana", "predNaive_Mediana", "predSN_Mediana", "predArima_Mediana", "predETS_Mediana", "predNN_Mediana", "predSVM_Mediana") 
predq1 <- c("predMedia_q1", "predNaive_q1", "predSN_q1", "predArima_q1", "predETS_q1", "predNN_q1", "predSVM_q1") 
predq3 <- c("predMedia_q3", "predNaive_q3", "predSN_q3", "predArima_q3", "predETS_q3", "predNN_q3", "predSVM_q3") 

# Calcular la mediana por fila y añadir las nuevas columnas
subset_combined_bien2 <- subset_combined_bien %>%
  rowwise() %>%
  mutate(
    predEnsemble_mediana = median(c_across(all_of(predmedianas)), na.rm = T),
    predEnsemble_q1 = median(c_across(all_of(predq1)), na.rm = T),
    predEnsemble_q3 = median(c_across(all_of(predq3)), na.rm = T),
    mapeEnsemble_mediana = median(c_across(all_of(medianas)), na.rm = T),
    mapeEnsemble_q1 = median(c_across(all_of(q1)), na.rm = T),
    mapeEnsemble_q3 = median(c_across(all_of(q3)), na.rm = T)
  ) %>%
  ungroup()

subset_combined_bien2 <- subset_combined_bien2 %>%
  rename_with(~ gsub("Mediana", "mediana", ., ignore.case = TRUE), ends_with("Mediana"))



finalPred <- rbind(summaryPredsFeats, subset_combined_bien2, fill = T)
finalCombined <- merge(feats2, finalPred, by = "ID", all = T)
fwrite(finalCombined, "allFeats.csv")

}

#combinar summaryPredFeats con allFeats
{
  combined_predFeats <- merge(feats, summaryPredsFeats, by = "ID", all=T)
  fwrite(combined_predFeats, "Resultados/CUPS/combined_predFeats.csv")
}


feats2 <- feats %>% select(-contains("pBarra"), -ends_with(".x"))

nuevos_nombres <- sub(".y", "", names(feats2))
colnames(feats2) <- nuevos_nombres

#combinar csvs de cruz

load_and_process_files <- function(folder_path, type) {
  
  # Lista de archivos CSV en la carpeta
  file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Cargar y unir todos los archivos en un dataframe
  do.call(rbind, lapply(file_list, function(file_name) {
    data <- fread(file_name)
    
    # Extraer el ID del nombre del archivo
    id <- tools::file_path_sans_ext(basename(file_name))
    
    # Añadir columna ID
    data <- mutate(data, id = id)
    
    # Renombrar columnas para indicar tipo (predicción o error)
    if (type != "real") {
      colnames(data) <- ifelse(colnames(data) == "id", "id", paste0(colnames(data), "_", type))
    }
    
    return(data)
  }))
}

# Cargar y procesar archivos
mape_data <- load_and_process_files("NUEVOS DATOS/goi4_pst_mape", "error")
preds_data <- load_and_process_files("NUEVOS DATOS/goi4_pst_preds", "pred")

# Unir los dataframes por ID
combined_data <- merge(mape_data, preds_data, by = "id")

# Escribir el dataframe combinado en un nuevo archivo CSV
fwrite(combined_data, "NUEVOS DATOS/combined_data.csv")

# Nota: Este código asume que las estructuras de los archivos son consistentes y que se pueden unir directamente.

