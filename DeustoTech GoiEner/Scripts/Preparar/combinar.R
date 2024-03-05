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

# Aplicamos la función para renombrar las columnas (excepto en el primero)
df_list_renamed <- lapply(seq_along(df_list), function(i) {
  if (i == 1) {
    df_list[[i]]  # No renombrar el primero
  } else {
    rename_duplicated(df_list[[i]], paste0("_", i))
  }
})

# Ahora combinamos usando reduce y full_join sin errores de duplicados
combined_df_feats <- reduce(df_list_renamed, full_join, by = "ID")
combined_df_feats <- merge(feats, combined_df_feats, by = "ID", all=T)
fwrite(combined_df_feats, "Resultados/CUPS/combined_df_feats.csv")

}

#combinar summaryPredFeats con allFeats
{
  combined_predFeats <- merge(feats, summaryPredsFeats, by = "ID", all=T)
  fwrite(combined_predFeats, "Resultados/CUPS/combined_predFeats.csv")
}



