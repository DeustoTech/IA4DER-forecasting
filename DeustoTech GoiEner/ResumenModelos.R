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


mediaCUPS <- fread("Resultados/CUPS/ResultadosCUPS_Media.csv")
naiveCUPS <- fread("Resultados/CUPS/ResultadosCUPS_Naive.csv")
sNaiveCUPS <- fread("Resultados/CUPS/ResultadosCUPS_SNaive.csv")
arimaCUPS <- fread("Resultados/CUPS/ResultadosCUPS_Arima.csv")
etsCUPS <- fread("Resultados/CUPS/ResultadosCUPS_ETS.csv")
nnCUPS <- fread("Resultados/CUPS/ResultadosCUPS_NN.csv")
svmCUPS <- fread("Resultados/CUPS/ResultadosCUPS_SVM.csv")


mediaL <- fread("Resultados/L/ResultadosL_Media.csv")
naiveL <- fread("Resultados/L/ResultadosL_Naive.csv")
sNaiveL <- fread("Resultados/L/ResultadosL_SNaive.csv")
arimaL <- fread("Resultados/L/ResultadosL_Arima.csv")
etsL <- fread("Resultados/L/ResultadosL_ETS.csv")
nnL <- fread("Resultados/L/ResultadosL_NN.csv")
svmL <- fread("Resultados/L/ResultadosL_SVM.csv")

mediaCT <- fread("Resultados/CT/ResultadosCT_Media.csv")
naiveCT <- fread("Resultados/CT/ResultadosCT_Naive.csv")
sNaiveCT <- fread("Resultados/CT/ResultadosCT_SNaive.csv")
arimaCT <- fread("Resultados/CT/ResultadosCT_Arima.csv")
etsCT <- fread("Resultados/CT/ResultadosCT_ETS.csv")
nnCT <- fread("Resultados/CT/ResultadosCT_NN.csv")
svmCT <- fread("Resultados/CT/ResultadosCT_SVM.csv")


result_df <- mediaCUPS %>%
  group_by(ID) %>%
  summarise(
    Median_MAPE = median(MAPE, na.rm = TRUE),
    Q1_MAPE = quantile(MAPE, 0.25, na.rm = TRUE),
    Q3_MAPE = quantile(MAPE, 0.75, na.rm = TRUE),
    
    Median_sMAPE = median(sMAPE, na.rm = TRUE),
    Q1_sMAPE = quantile(sMAPE, 0.25, na.rm = TRUE),
    Q3_sMAPE = quantile(sMAPE, 0.75, na.rm = TRUE),
    
    Median_RMSE = median(RMSE, na.rm = TRUE),
    Q1_RMSE = quantile(RMSE, 0.25, na.rm = TRUE),
    Q3_RMSE = quantile(RMSE, 0.75, na.rm = TRUE),
    
    
  )

fwrite(result_df, file = "Resultados/CUPS/SummaryMedia.csv", col.names = T, row.names = F)
