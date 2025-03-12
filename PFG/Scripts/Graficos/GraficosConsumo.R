library(foreach)
library(doParallel)
librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', "utils", "gridExtra", "grid", 
               "gtable") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

modelosBaseRed <- fread("NUEVOS DATOS/DATOS ERROR NUEVO/preds_MAPE_RMSE_reducido.csv")
modelosFforma <- fread("NuevosResultados/PrediccionErrorNuevo/PrediccionMAPE/pBarrasMAPE.csv")
modelosBaseRed$ID <- modelosBaseRed$id
modelosBaseRed$Real <- modelosBaseRed$real
modelosBaseRed <- modelosBaseRed %>% select(ID, Real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred)
modelosFforma <- modelosFforma %>% select(ID, Real, PBarra_lm_tarifa, PBarra_rf_tarifa, PBarra_gbm_tarifa, PBarra_nn_tarifa, PBarra_svm_tarifa, PBarra_Ensemble_tarifa, PBarra_errorMape)



modelosFforma_clean <- modelosFforma %>%
  distinct(ID, .keep_all = TRUE)
modelosBaseRed_clean <- modelosBaseRed %>%
  filter(ID %in% modelosFforma_clean$ID)
all(modelosBaseRed_clean$ID == modelosFforma_clean$ID)
all(modelosBaseRed_clean$Real == modelosFforma_clean$Real)

datos_grafico <- modelosBaseRed_clean %>%
  select(ID, Real, mean_pred, rw_pred, naive_pred, simple_pred, lr_pred, ann_pred, svm_pred, arima_pred, ses_pred, ens_pred) %>%
  pivot_longer(cols = -c(ID), names_to = "Modelo", values_to = "Prediccion") %>%
  bind_rows(
    modelosFforma_clean %>%
      select(ID, Real, PBarra_lm_tarifa, PBarra_rf_tarifa, PBarra_gbm_tarifa, PBarra_nn_tarifa, PBarra_svm_tarifa, PBarra_Ensemble_tarifa) %>%
      pivot_longer(cols = -c(ID, Real), names_to = "Modelo", values_to = "Prediccion")
  )

primeros_ids <- head(modelosBaseRed_clean$ID, 2)
datos_filtrados <- datos_grafico %>%
  filter(ID %in% primeros_ids)

ggplot(datos_filtrados, aes(x = ID, y = Prediccion, color = Modelo, group = Modelo)) +
  geom_line() +
  geom_line(aes(y = Real, group = 1), color = "black", linetype = "dashed") +
  labs(title = "Comparación de Predicciones vs Real (Primeros 5 IDs)",
       x = "ID",
       y = "Consumo (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

