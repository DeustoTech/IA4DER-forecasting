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

CUPS <- fread("Resultados/CUPS/PredFeats.csv")
CUPS$ID <- basename(CUPS$ID)



result_df <- CUPS %>%
  group_by(ID = CUPS$ID) %>%
  summarise(
    
    real_mediana = median(Real, na.rm = TRUE),
    real_q1 = quantile(Real, 0.25, na.rm = TRUE),
    real_q3 = quantile(Real, 0.75, na.rm = TRUE), 

    predMedia_mediana = median(Media_pred, na.rm = TRUE),
    predMedia_q1 = quantile(Media_pred, 0.25, na.rm = TRUE),
    predMedia_q3 = quantile(Media_pred, 0.75, na.rm = TRUE), 
    
    predNaive_mediana = median(Naive_pred, na.rm = TRUE),
    predNaive_q1 = quantile(Naive_pred, 0.25, na.rm = TRUE),
    predNaive_q3 = quantile(Naive_pred, 0.75, na.rm = TRUE), 
    
    predSN_mediana = median(SNaive_pred, na.rm = TRUE),
    predSN_q1 = quantile(SNaive_pred, 0.25, na.rm = TRUE),
    predSN_q3 = quantile(SNaive_pred, 0.75, na.rm = TRUE), 
   
    predArima_mediana = median(Arima_pred, na.rm = TRUE),
    predArima_q1 = quantile(Arima_pred, 0.25, na.rm = TRUE),
    predArima_q3 = quantile(Arima_pred, 0.75, na.rm = TRUE), 
    
    predETS_mediana = median(ETS_pred, na.rm = TRUE),
    predETS_q1 = quantile(ETS_pred, 0.25, na.rm = TRUE),
    predETS_q3 = quantile(ETS_pred, 0.75, na.rm = TRUE), 
    
    predNN_mediana = median(NN_pred, na.rm = TRUE),
    predNN_q1 = quantile(NN_pred, 0.25, na.rm = TRUE),
    predNN_q3 = quantile(NN_pred, 0.75, na.rm = TRUE),
    
    predSVM_mediana = median(SMV_pred, na.rm = TRUE),
    predSVM_q1 = quantile(SMV_pred, 0.25, na.rm = TRUE),
    predSVM_q3 = quantile(SMV_pred, 0.75, na.rm = TRUE),
    
    predEnsenmble_mediana = median(Ensenmble_pred, na.rm = TRUE),
    predEnsenmble_q1 = quantile(Ensenmble_pred, 0.25, na.rm = TRUE),
    predEnsenmble_q3 = quantile(Ensenmble_pred, 0.75, na.rm = TRUE),
    
    mapeMedia_mediana = median(Media_mape, na.rm = TRUE),
    mapeMedia_q1 = quantile(Media_mape, 0.25, na.rm = TRUE),
    mapeMedia_q3 = quantile(Media_mape, 0.75, na.rm = TRUE),
    
    mapeNaive_mediana = median(Naive_mape, na.rm = TRUE),
    mapeNaive_q1 = quantile(Naive_mape, 0.25, na.rm = TRUE),
    mapeNaive_q3 = quantile(Naive_mape, 0.75, na.rm = TRUE),
    
    mapeSN_mediana = median(SNaive_mape, na.rm = TRUE),
    mapeSN_q1 = quantile(SNaive_mape, 0.25, na.rm = TRUE),
    mapeSN_q3 = quantile(SNaive_mape, 0.75, na.rm = TRUE),
    
    mapeArima_mediana = median(Arima_mape, na.rm = TRUE),
    mapeArima_q1 = quantile(Arima_mape, 0.25, na.rm = TRUE),
    mapeArima_q3 = quantile(Arima_mape, 0.75, na.rm = TRUE),
    
    mapeETS_mediana = median(ETS_mape, na.rm = TRUE),
    mapeETS_q1 = quantile(ETS_mape, 0.25, na.rm = TRUE),
    mapeETS_q3 = quantile(ETS_mape, 0.75, na.rm = TRUE),
    
    mapeNN_mediana = median(NN_mape, na.rm = TRUE),
    mapeNN_q1 = quantile(NN_mape, 0.25, na.rm = TRUE),
    mapeNN_q3 = quantile(NN_mape, 0.75, na.rm = TRUE),
    
    mapeSVM_mediana = median(SMV_mape, na.rm = TRUE),
    mapeSVM_q1 = quantile(SMV_mape, 0.25, na.rm = TRUE),
    mapeSVM_q3 = quantile(SMV_mape, 0.75, na.rm = TRUE),
    
    mapeEnsemble_mediana = median(Ensenmble_mape, na.rm = TRUE),
    mapeEnsemble_q1 = quantile(Ensenmble_mape, 0.25, na.rm = TRUE),
    mapeEnsemble_q3 = quantile(Ensenmble_mape, 0.75, na.rm = TRUE)
    
  )

fwrite(result_df, file = "Resultados/CUPS/SummaryPredsFeats.csv", col.names = T, row.names = F)




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
svmCUPS <- fread("Resultados/CUPS/ResultadosCUPS_SVM.csv")
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
  group_by(ID) %>%
  summarise(
    
    real_mediana = median(Real, na.rm = TRUE),
    real_q1 = quantile(Real, 0.25, na.rm = TRUE),
    real_q3 = quantile(Real, 0.75, na.rm = TRUE), 
    
    pred_Mediana = median(Predicted, na.rm = T),
    pred_q1 = quantile(Predicted, 0.25, na.rm = TRUE),
    pred_q3 = quantile(Predicted, 0.75, na.rm = TRUE),
    
    mape_mediana = median(MAPE, na.rm = TRUE),
    mape_q1 = quantile(MAPE, 0.25, na.rm = TRUE),
    mape_q3 = quantile(MAPE, 0.75, na.rm = TRUE),
    
    sMape_mediana = median(sMAPE, na.rm = TRUE),
    sMape_q1 = quantile(sMAPE, 0.25, na.rm = TRUE),
    sMape_q3 = quantile(sMAPE, 0.75, na.rm = TRUE),
    
    rmse_mediana = median(RMSE, na.rm = TRUE),
    rmse_q1 = quantile(RMSE, 0.25, na.rm = TRUE),
    rmse_q3 = quantile(RMSE, 0.75, na.rm = TRUE)
    
    
  )

modelo <- svmCUPS$Modelo[1]

result_df <- result_df %>%
  rename_with(~paste0("pred",modelo, "_", .), starts_with("pred_")) %>%
  rename_with(~paste0("mape",modelo, "_", .), starts_with("mape_")) %>%
  rename_with(~paste0("sMape",modelo, "_", .), starts_with("sMape_")) %>%
  rename_with(~paste0("rmse",modelo, "_", .), starts_with("rmse_"))


result_df <- result_df %>%
  rename_with(~sub("_[^_]+_", "_", .), -ID)




fwrite(result_df, file = "Resultados/CUPS/SummaryNN.csv", col.names = T, row.names = F)






