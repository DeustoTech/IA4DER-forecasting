library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

CUPS <- fread("Predicciones.csv")
CUPS$ID <- basename(CUPS$ID)



result_df <- CUPS %>%
  group_by(ID = CUPS$ID) %>%
  summarise(
    
    
    
    Median_MAPE_media = median(Media_mape, na.rm = TRUE),
    Q1_MAPE_media = quantile(Media_mape, 0.25, na.rm = TRUE),
    Q3_MAPE_media = quantile(Media_mape, 0.75, na.rm = TRUE),
    
    Median_MAPE_naive = median(Naive_mape, na.rm = TRUE),
    Q1_MAPE_naive = quantile(Naive_mape, 0.25, na.rm = TRUE),
    Q3_MAPE_naive = quantile(Naive_mape, 0.75, na.rm = TRUE),
    
    Median_MAPE_snaive = median(SNaive_mape, na.rm = TRUE),
    Q1_MAPE_snaive = quantile(SNaive_mape, 0.25, na.rm = TRUE),
    Q3_MAPE_snaive = quantile(SNaive_mape, 0.75, na.rm = TRUE),
    
    Median_MAPE_arima = median(Arima_mape, na.rm = TRUE),
    Q1_MAPE_arima = quantile(Arima_mape, 0.25, na.rm = TRUE),
    Q3_MAPE_arima = quantile(Arima_mape, 0.75, na.rm = TRUE),
    
    Median_MAPE_ets = median(ETS_mape, na.rm = TRUE),
    Q1_MAPE_ets = quantile(ETS_mape, 0.25, na.rm = TRUE),
    Q3_MAPE_ets = quantile(ETS_mape, 0.75, na.rm = TRUE),
    
    Median_MAPE_nn = median(NN_mape, na.rm = TRUE),
    Q1_MAPE_nn = quantile(NN_mape, 0.25, na.rm = TRUE),
    Q3_MAPE_nn = quantile(NN_mape, 0.75, na.rm = TRUE),
    
    Median_MAPE_svm = median(SMV_pred, na.rm = TRUE),
    Q1_MAPE_svm = quantile(SMV_pred, 0.25, na.rm = TRUE),
    Q3_MAPE_svm = quantile(SMV_pred, 0.75, na.rm = TRUE),
    
    Median_MAPE_ensemble = median(Ensemble_mape, na.rm = TRUE),
    Q1_MAPE_ensemble = quantile(Ensemble_mape, 0.25, na.rm = TRUE),
    Q3_MAPE_ensemble = quantile(Ensemble_mape, 0.75, na.rm = TRUE)
    
   
    
    
  )

fwrite(result_df, file = "Resultados/CUPS/SummaryPreds.csv", col.names = T, row.names = F)




mediaCUPS <- fread("Resultados/CUPS/ResultadosCUPS_Media.csv")
mediaCUPS$ID <- basename(mediaCUPS$ID)
naiveCUPS <- fread("Resultados/CUPS/ResultadosCUPS_Naive.csv")
naiveCUPS$ID <- basename(naiveCUPS$ID)
sNaiveCUPS <- fread("Resultados/CUPS/ResultadosCUPS_SNaive.csv")
sNaiveCUPS$ID <- basename(sNaiveCUPS$ID)
arimaCUPS <- fread("Resultados/CUPS/ResultadosCUPS_Arima.csv")
arimaCUPS$ID <- basename(arimaCUPS$ID)
etsCUPS <- fread("Resultados/CUPS/ResultadosCUPS_ETS.csv")
etsCUPS$ID <- basename(etsCUPS$ID)
nnCUPS <- fread("Resultados/CUPS/ResultadosCUPS_NN.csv")
nnCUPS$ID <- basename(nnCUPS$ID)
svmCUPS <- fread("Resultados/CUPS/ResultadosCUPS_SVM_2.csv")
svmCUPS$ID <- basename(svmCUPS$ID)



mediaL <- fread("Resultados/L/ResultadosL_Media.csv")
mediaL$ID <- basename(mediaL$ID)
naiveL <- fread("Resultados/L/ResultadosL_Naive.csv")
naiveL$ID <- basename(naiveL$ID)
sNaiveL <- fread("Resultados/L/ResultadosL_SNaive.csv")
sNaiveL$ID <- basename(sNaiveL$ID)
arimaL <- fread("Resultados/L/ResultadosL_Arima.csv")
arimaL$ID <- basename(arimaL$ID)
etsL <- fread("Resultados/L/ResultadosL_ETS.csv")
etsL$ID <- basename(etsL$ID)
nnL <- fread("Resultados/L/ResultadosL_NN.csv")
nnL$ID <- basename(nnL$ID)
svmL <- fread("Resultados/L/ResultadosL_SVM.csv")
svmL$ID <- basename(svmL$ID)


mediaCT <- fread("Resultados/CT/ResultadosCT_Media.csv")
mediaCT$ID <- basename(mediaCT$ID)
naiveCT <- fread("Resultados/CT/ResultadosCT_Naive.csv")
naiveCT$ID <- basename(naiveCT$ID)
sNaiveCT <- fread("Resultados/CT/ResultadosCT_SNaive.csv")
sNaiveCT$ID <- basename(sNaiveCT$ID)
arimaCT <- fread("Resultados/CT/ResultadosCT_Arima.csv")
arimaCT$ID <- basename(arimaCT$ID)
etsCT <- fread("Resultados/CT/ResultadosCT_ETS.csv")
etsCT$ID <- basename(etsCT$ID)
nnCT <- fread("Resultados/CT/ResultadosCT_NN.csv")
nnCT$ID <- basename(nnCT$ID)
svmCT <- fread("Resultados/CT/ResultadosCT_SVM.csv")
svmCT$ID <- basename(svmCT$ID)



result_df <- svmCUPS %>%
  group_by(ID = svmCUPS$ID) %>%
  summarise(
    Median_MAPE = median(SVM_mape, na.rm = TRUE),
    Q1_MAPE = quantile(SVM_mape, 0.25, na.rm = TRUE),
    Q3_MAPE = quantile(SVM_mape, 0.75, na.rm = TRUE),
  
  )

fwrite(result_df, file = "Resultados/CUPS/SummarySVM.csv", col.names = T, row.names = F)




summaryMedia <- fread("Resultados/CUPS/SummaryMedia.csv")
summaryNaive <- fread("Resultados/CUPS/SummaryNaive.csv")
summarySN <- fread("Resultados/CUPS/SummarySNaive.csv")
summarySN$ID <- basename(summarySN$ID)
summaryArima <- fread("Resultados/CUPS/SummaryArima.csv")
summaryNN <- fread("Resultados/CUPS/SummaryNN.csv")
summaryETS <- fread("Resultados/CUPS/SummaryETS.csv")
summarySVM <- fread("Resultados/CUPS/SummarySVM.csv")

c("mapeMedia_mediana","mapeNaive_mediana","mapeSN_mediana",
"mapeArima_mediana","mapeETS_mediana","mapeSVM_mediana","mapeNN_mediana",
"mapeEnsemble_mediana","mapeMedia_q1","mapeNaive_q1","mapeSN_q1","mapeArima_q1","mapeETS_q1",
"mapeSVM_q1","mapeNN_q1","mapeEnsemble_q1","mapeMedia_q3","mapeNaive_q3","mapeSN_q3","mapeArima_q3",
"mapeETS_q3","mapeSVM_q3","mapeNN_q3","mapeEnsemble_q3")



# Lista de dataframes
df_list <- list(
  summaryMedia, summaryNaive, summarySN, summaryArima, summaryNN, summaryETS, summarySVM
)

combined <- df_list %>% reduce(inner_join, by = "ID")

colnames(combined)[colnames(combined) == "mapeETS_q3.y"] <- "mapeETS_q3"



library(matrixStats)

# Crear las nuevas columnas
combined$mapeEnsemble_mediana <- apply(combined[, grep("mape.*_mediana", names(combined))], 1, median, na.rm = TRUE)
combined$mapeEnsemble_q1 <- apply(combined[, grep("mape.*_q1", names(combined))], 1, median, na.rm = TRUE)
combined$mapeEnsemble_q3 <- apply(combined[, grep("mape.*_q3", names(combined))], 1, median, na.rm = TRUE)


combined$mapeEnsemble_mediana <- numeric()
combined$mapeEnsemble_q1 <- numeric()
combined$mapeEnsemble_q3 <- numeric()


for (i in 1:nrow(combined)){

  combined$mapeEnsemble_mediana[i] <- median(c(combined$mapeMedia_mediana[i],combined$mapeNaive_mediana[i], 
                                               combined$mapeSN_mediana[i], combined$mapeETS_mediana[i], combined$mapeNN_mediana[i],
                                               combined$mapeArima_mediana[i], combined$mapeSVM_mediana[i]
                                               
                                               ), na.rm = TRUE)
 
  combined$mapeEnsemble_q1[i] <- median(c(combined$mapeMedia_q1[i], combined$mapeNaive_q1[i], 
                                          combined$mapeSN_q1[i], combined$mapeETS_q1[i], combined$mapeNN_q1[i],
                                          combined$mapeArima_q1[i], combined$mapeSVM_q1[i]
                                          
  ), na.rm = TRUE)
  
  combined$mapeEnsemble_q3[i] <- median(c(combined$mapeMedia_q3[i], combined$mapeNaive_q3[i], 
                                          combined$mapeSN_q3[i], combined$mapeETS_q3[i], combined$mapeNN_q3[i],
                                          combined$mapeArima_q3[i], combined$mapeSVM_q3[i]
                                          
  ), na.rm = TRUE)
  
  
}

fwrite(combined, file = "Resultados/CUPS/SummaryPredsNuevo.csv")



