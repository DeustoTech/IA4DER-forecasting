library(foreach)
library(doParallel)

# R script to compute some features of CUPS. Includes: seasonal aggregates,
# mapes of each model, statistical descriptors, best model, solar tariffs
# and some metadata.


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", 
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture') 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}



folder <- "TransformersV2/"
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)
metadata_file <- fread("metadata.csv")


N <- csv_files[!grepl("-CT\\.csv$", csv_files) & !grepl("-L\\.csv$", csv_files)]
CT <- csv_files[grepl("-CT\\.csv$", csv_files)]
L <- csv_files[grepl("-L\\.csv$", csv_files)]

summaryPreds_CUPS <- fread("Resultados/CUPS/SummaryPreds.csv")
summaryPreds_CUPS$ID <- basename(summaryPreds_CUPS$ID)

summaryMedia_CUPS <- fread("Resultados/CUPS/SummaryMedia.csv")
summaryMedia_CUPS$ID <- basename(summaryMedia_CUPS$ID)

summaryNaive_CUPS <- fread("Resultados/CUPS/SummaryNaive.csv")
summaryNaive_CUPS$ID <- basename(summaryNaive_CUPS$ID)

summarysNaive_CUPS <- fread("Resultados/CUPS/SummarySNaive.csv") 
summarysNaive_CUPS$ID <- basename(summarysNaive_CUPS$ID)


summaryArima_CUPS <- fread("Resultados/CUPS/SummaryArima.csv")
summaryArima_CUPS$ID <- basename(summaryArima_CUPS$ID)

summaryETS_CUPS <- fread("Resultados/CUPS/SummaryETS.csv")
summaryETS_CUPS$ID <- basename(summaryETS_CUPS$ID)

summaryNN_CUPS <- fread("Resultados/CUPS/SummaryNN.csv")
summaryNN_CUPS$ID <- basename(summaryNN_CUPS$ID)

summarySVM_CUPS <- fread("Resultados/CUPS/SummarySVM.csv")
summarySVM_CUPS$ID <- basename(summarySVM_CUPS$ID)

#summaryEnsemble_CUPS <- fread("Resultados/CUPS/SummaryEnsemble.csv")
#summaryEnsemble_CUPS$ID <- basename(summaryEnsemble_CUPS$ID)

# Agregar el prefijo 'folder' a las rutas en N, CT y L
N <- paste(folder, N, sep = "")
CT <- paste(folder, CT, sep = "")
L <- paste(folder, L, sep = "")

horas <- data.frame(
  hora = 0:23,
  TARIFA_2.0 = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano", "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  ),
  TARIFA_SOLAR = c(
    "valle", "valle", "valle", "valle", "valle", "valle", "valle", "valle",
    "llano", "llano",
    "solar pico", "solar pico", "solar pico", "solar pico",
    "solar llano", "solar llano",
    "llano", "llano",
    "pico", "pico", "pico", "pico",
    "llano", "llano"
  )
)
MC  <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error
#p
# Precios de electricidad

# TD : Peajes €/kWh
# CS : Cargos Sistema €/kWh
# CG : Coste Gestión €/MWh
# L : Precio energía libre €/kWh

TD_p1 <- 0.029098
TD_p2 <- 0.019794
TD_p3 <- 0.00098

CS_p1 <- 0.043893
CS_p2 <- 0.008779
CS_p3 <- 0.002195

CG_p1 <- 36.115695
CG_p2 <- 36.115695
CG_p3 <- 36.115695
  
L_p1 <- 0.204321
L_p2 <- 0.200549
L_p3 <- 0.185471

model_names <- c("Media", "Naive", "SNaive", "Arima", "ETS", "SVM", "NN", "Ensemble")


Feats <- foreach(NAME = N,
             .combine = rbind,
             .errorhandling = "remove", .options.future = list(packages = librerias)) %dofuture% { 
               
               csv_actual <- fread(NAME)
               
               
               if ("time" %in% colnames(csv_actual)) {
                 # Cambiar el nombre de la columna a "timestamp". SOLO PARA LOS -L y -CT
                 colnames(csv_actual)[colnames(csv_actual) == "time"] <- "timestamp"
                 csv_actual$imputed <- 0
                 csv_actual <- csv_actual %>% select(timestamp, kWh, imputed)
               }
               
               
               a <- csv_actual %>%
                 mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")) %>%
                 mutate(Hora = hour(timestamp))
               
               a$week_day <- wday(a$timestamp)
               
               a$season <- case_when(
                 month(a$timestamp) %in% c(12, 1, 2) ~ "winter",
                 month(a$timestamp) %in% c(3, 4, 5) ~ "spring",
                 month(a$timestamp) %in% c(6, 7, 8) ~ "summer",
                 month(a$timestamp) %in% c(9, 10, 11) ~ "autum"
               )
               
               a$day_period <- cut(
                 hour(a$timestamp),
                 breaks = c(0, 4, 8, 12, 16, 20, 24),
                 labels = c("0-4", "5-8", "9-12", "13-16", "17-20", "21-24"),
                 include.lowest = TRUE
               )
              
               
               laborable <- a %>% filter(week_day %in% c(1, 2, 3, 4, 5))
               finde <- a %>% filter(week_day %in% c(6, 7))
               
               features_semana <- laborable %>%
                 group_by(season, day_period) %>%
                 summarise(kWhTotal = sum(kWh),
                           kWhMax = max(kWh)) %>%
                 pivot_wider(names_from = c(season, day_period), values_from = c(kWhTotal, kWhMax))
               
               features_fin_de_semana <- finde %>%
                 group_by(season) %>%
                 summarise(kWhTotal = sum(kWh),
                           kWhMax = max(kWh))%>%
                 pivot_wider(names_from = season, values_from = c(kWhTotal, kWhMax))
               
               
               colnames(features_fin_de_semana) <- paste(colnames(features_fin_de_semana), "finde", sep = "_")
               
               T2.0_VALLE <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "valle"]])
               T2.0_LLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "llano"]])
               T2.0_PICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_2.0 == "pico"]])
               
               T_SOLAR_LLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "llano"]])
               T_SOLAR_PICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "pico"]])
               T_SOLAR_SPICO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "solar pico"]])
               T_SOLAR_SLLANO <- sum(a$kWh[a$Hora %in% horas$hora[horas$TARIFA_SOLAR == "solar llano"]])
                 
               LENGTH <- length(a$kWh)
               
               QQ     <- as.numeric(quantile(a$kWh,c(0,0.25,0.5,0.75,1),na.rm=T))
               
               nombre     <- tools::file_path_sans_ext(NAME)
               ID1 <-  sub("TransformersV2/", "", nombre)

               
               metadatos <- filter(metadata_file, user ==  ID1)
               
               POT_NOM <- max(metadatos$p1, metadatos$p2, metadatos$p3, metadatos$p4, metadatos$p5, metadatos$p6, na.rm = T)
               ECDF   <- ecdf(a$kWh)(MC*POT_NOM) 
               
             
               summaryPreds <- filter(summaryPreds_CUPS, ID == ID1)
               
               summaryMedia <-  summaryPreds %>% select(ID, Median_MAPE_media, Q1_MAPE_media, Q3_MAPE_media)
               summaryNaive <- summaryPreds %>% select(ID, Median_MAPE_naive,Q1_MAPE_naive,Q3_MAPE_naive)
               summarysNaive <- summaryPreds %>% select(ID, Median_MAPE_snaive,Q1_MAPE_snaive,Q3_MAPE_snaive)
               summaryArima <- summaryPreds %>% select(ID, Median_MAPE_arima,Q1_MAPE_arima,Q3_MAPE_arima)
               summaryETS <- summaryPreds %>% select(ID, Median_MAPE_ets,Q1_MAPE_ets,Q3_MAPE_ets)
               summaryNN <- summaryPreds %>% select(ID, Median_MAPE_nn,Q1_MAPE_nn,Q3_MAPE_nn)
               summarySVM <- summaryPreds %>% select(ID, Median_MAPE_svm,Q1_MAPE_svm,Q3_MAPE_svm)
               summaryEnsemble <- summaryPreds %>% select(ID, Median_MAPE_ensemble,Q1_MAPE_ensemble,Q3_MAPE_ensemble)

               
               aux <- data.frame(
                 ID=     ID1,
                 LENGTH= LENGTH,
                 ZERO=   sum(a$kWh==0)/LENGTH,
                 IMPUTED=sum(a$issue)/LENGTH,
                 AVG=    mean(a$kWh,na.rm=T),
                 SD=     sd(a$kWh,na.rm=T),
                 MIN=    QQ[1],
                 Q1=     QQ[2],
                 MEDIAN= QQ[3],
                 Q3=     QQ[4],
                 MAX=    QQ[5],
                 TOTAL = sum(a$kWh, na.rm = T),
                 VAR = var(a$kWh, na.rm = T),
                 
                 POT_1 = metadatos$p1,
                 POT_2 = metadatos$p2,
                 POT_3 = metadatos$p3,
                 POT_4 = metadatos$p4,
                 POT_5 = metadatos$p5,
                 POT_6 = metadatos$p6,
                 POT_NOM = POT_NOM,
                 
                 MC25=   ECDF[1],
                 MC50=   ECDF[2],
                 MC80=   ECDF[3],
                 MC90=   ECDF[4],
                 MC95=   ECDF[5],
                 
                 P_T2.0_VALLE = T2.0_VALLE,
                 P_T2.0_LLANO = T2.0_LLANO,
                 P_T2.0_PICO = T2.0_PICO,
                 P_T_SOLAR_PICO = T_SOLAR_PICO,
                 P_T_SOLAR_LLANO = T_SOLAR_LLANO,
                 P_T_SOLAR_SPICO = T_SOLAR_SPICO,
                 P_T_SOLAR_SLLANO = T_SOLAR_SLLANO,
                 
                 zip_code = metadatos$zip_code,
                 cnae = metadatos$cnae,
                 municipality = metadatos$municipality,
                 contracted_tariff = metadatos$contracted_tariff,
                 self_consumption_type = metadatos$self_consumption_type,
                 
                 
                 # Si no hay summary para ese ID, pone a NA
                 
                 mapeMedia_mediana = if (nrow(summaryMedia) == 0) NA else summaryMedia$Median_MAPE,
                 mapeNaive_mediana = if (nrow(summaryNaive) == 0) NA else summaryNaive$Median_MAPE,
                 mapeSN_mediana = if (nrow(summarysNaive) == 0) NA else summarysNaive$Median_MAPE,
                 mapeArima_mediana = if (nrow(summaryArima) == 0) NA else summaryArima$Median_MAPE,
                 mapeETS_mediana = if (nrow(summaryETS) == 0) NA else summaryETS$Median_MAPE,
                 mapeSVM_mediana = if (nrow(summarySVM) == 0) NA else summarySVM$Median_MAPE,
                 mapeNN_mediana = if (nrow(summaryNN) == 0) NA else summaryNN$Median_MAPE,
                 mapeEnsemble_mediana = if (nrow(summaryEnsemble) == 0) NA else summaryEnsemble$Median_MAPE,
                   
                 mapeMedia_q1 = if (nrow(summaryMedia) == 0) NA else summaryMedia$Q1_MAPE,
                 mapeNaive_q1 = if (nrow(summaryNaive) == 0) NA else summaryNaive$Q1_MAPE,
                 mapeSN_q1 = if (nrow(summarysNaive) == 0) NA else summarysNaive$Q1_MAPE,
                 mapeArima_q1 = if (nrow(summaryArima) == 0) NA else summaryArima$Q1_MAPE,
                 mapeETS_q1 = if (nrow(summaryETS) == 0) NA else summaryETS$Q1_MAPE,
                 mapeSVM_q1 = if (nrow(summarySVM) == 0) NA else summarySVM$Q1_MAPE,
                 mapeNN_q1 = if (nrow(summaryNN) == 0) NA else summaryNN$Q1_MAPE,
                 mapeEnsemble_q1 = if (nrow(summaryEnsemble) == 0) NA else summaryEnsemble$Q1_MAPE,
                 
                 mapeMedia_q3 = if (nrow(summaryMedia) == 0) NA else summaryMedia$Q3_MAPE,
                 mapeNaive_q3 = if (nrow(summaryNaive) == 0) NA else summaryNaive$Q3_MAPE,
                 mapeSN_q3 = if (nrow(summarysNaive) == 0) NA else summarysNaive$Q3_MAPE,
                 mapeArima_q3 = if (nrow(summaryArima) == 0) NA else summaryArima$Q3_MAPE,
                 mapeETS_q3 = if (nrow(summaryETS) == 0) NA else summaryETS$Q3_MAPE,
                 mapeSVM_q3 = if (nrow(summarySVM) == 0) NA else summarySVM$Q3_MAPE,
                 mapeNN_q3 = if (nrow(summaryNN) == 0) NA else summaryNN$Q3_MAPE,
                 mapeEnsemble_q3 = if (nrow(summaryEnsemble) == 0) NA else summaryEnsemble$Q3_MAPE,
                 
                 P1_PICO_PRECIO = T2.0_PICO * TD_p1 +
                                   T2.0_PICO * CS_p1 + 
                                   T2.0_PICO * L_p1,
                 
                 P2_LLANO_PRECIO = T2.0_LLANO * TD_p2 +
                                   T2.0_LLANO * CS_p2 + 
                                   T2.0_LLANO * L_p2,
                 
                 P3_VALLE_PRECIO = T2.0_VALLE * TD_p3 +
                                  T2.0_VALLE * CS_p3 + 
                                  T2.0_VALLE * L_p3
                 
                 
                           )
               
               median_values <- c(
                 aux$mapeMedia_mediana,
                 aux$mapeNaive_mediana,
                 aux$mapeSN_mediana,
                 aux$mapeArima_mediana,
                 aux$mapeETS_mediana,
                 aux$mapeSVM_mediana,
                 aux$mapeNN_mediana,
                 aux$mapeEnsemble_mediana
               )
               

               min_index <- which.min(median_values)
               if (length(min_index) == 0) {
                 best_model <- NA
               } else {
                 best_model <- model_names[min_index]
               }

               aux <- cbind(aux, best_model, features_semana, features_fin_de_semana)
             }

write.csv(Feats,file="featuresPredicciones.csv",row.names = F)




# Graphs

B <- read.csv("featuresPredicciones.csv")

boxplot(B$P_T2.0_VALLE,B$P_T2.0_LLANO,B$P_T2.0_PICO,B$P_T_SOLAR_PICO,
        B$P_T_SOLAR_LLANO,B$P_T_SOLAR_SPICO, B$P_T_SOLAR_SLLANO,outline=F )

colnames(B)

boxplot(B$POT_NOM[B$POT_NOM <= quantile(B$POT_NOM, 0.75) & B$POT_NOM >= quantile(B$POT_NOM, 0.10)])

summary(data.frame(B$P_T2.0_VALLE,B$P_T2.0_LLANO,B$P_T2.0_PICO,B$P_T_SOLAR_PICO,
                   B$P_T_SOLAR_LLANO,B$P_T_SOLAR_SPICO, B$P_T_SOLAR_SLLANO))


B$POT_NOM <- max(B$POT_1, B$POT_2, B$POT_3, B$POT_4, B$POT_5, B$POT_6, na.rm = T)

plot(ecdf(B$ZERO))
plot(ecdf(B$IMPUTED))
plot(ecdf(B$MAX/B$POT_NOM))
ecdf(B$MAX/B$POT_NOM)(0.8)

histogram(B$POT_NOM[B$POT_NOM <= 10])

boxplot(B$P1_PICO_PRECIO, B$P2_LLANO_PRECIO, B$P3_VALLE_PRECIO, 
        names = c("P1 PICO", "P2 LLANO", "P3 VALLE"),
        col = c("lightblue", "lightgreen", "lightcoral"), main = "Boxplot de las columnas",
        xlab = "Columnas", ylab = "Valor", ylim = c(min(B), quantile(B, 0.75)) )


df_longPrecio <- gather(B, key = "Columna", value = "Valor", P1_PICO_PRECIO, P2_LLANO_PRECIO, P3_VALLE_PRECIO)

# Crea un boxplot combinado con ggplot2
ggplot(df_longPrecio, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot de los Precios por periodo", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_longPrecio$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral"))

###
preds <- fread("Predicciones.csv")
colnames(preds)

df_mediana <- gather(preds, key = "Columna", value = "Valor",
                     Media_mape,
                     Naive_mape,
                     SNaive_mape,
                     Arima_mape,
                     ETS_mape, 
                     NN_mape,
                     SMV_mape,
                     Ensemble_mape, na.rm = TRUE)

ggplot(df_mediana, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot de MAPE mediana por modelos", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_mediana$Valor, 0.85)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7", "#FF91C1", "#D54E8E"))


df_longMediana <- gather(B, key = "Columna", value = "Valor", 
                         mapeMedia_mediana, 
                         mapeNaive_mediana,
                         mapeSN_mediana,
                         mapeArima_mediana,
                         mapeETS_mediana, 
                         mapeNN_mediana,
                         mapeSVM_mediana,
                         mapeEnsemble_mediana,na.rm = TRUE)

# Crea un boxplot combinado con ggplot2
ggplot(df_longMediana, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot de MAPE mediana por modelos", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_longMediana$Valor, 0.90)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7", "#FF91C1", "#D54E8E"))


df_longQ1 <- gather(B, key = "Columna", value = "Valor", 
                         mapeMedia_q1, 
                         mapeNaive_q1,
                         mapeSN_q1,
                         mapeArima_q1,
                         mapeETS_q1, 
                         mapeNN_q1,
                         mapeSVM_q1,
                         mapeEnsemble_q1, na.rm = TRUE)

# Crea un boxplot combinado con ggplot2
ggplot(df_longQ1, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot de MAPE Q1 por modelos", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_longQ1$Valor, 0.90)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7", "#FF91C1", "#D54E8E"))

df_longQ3 <- gather(B, key = "Columna", value = "Valor", 
                    mapeMedia_q3, 
                    mapeNaive_q3,
                    mapeSN_q3,
                    mapeArima_q3,
                    mapeETS_q3, 
                    mapeNN_q3,
                    mapeEnsemble_q3,
                    mapeSVM_q3, na.rm = TRUE)

# Crea un boxplot combinado con ggplot2
ggplot(df_longQ3, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot de MAPE Q3 por modelos", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_longQ3$Valor, 0.90)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7", "#FF91C1", "#D54E8E"))



########## GRAFICOS CON LAS AGGREGATIONS ############

df_invierno <- gather(B, key = "Columna", value = "Valor", 
                      kWhTotal_winter_0.4, 
                      kWhTotal_winter_5.8,
                      kWhTotal_winter_9.12,
                      kWhTotal_winter_13.16,
                      kWhTotal_winter_17.20, 
                      kWhTotal_winter_21.24, na.rm = TRUE)

ggplot(df_invierno, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo total por rango de horas en Invierno", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_invierno$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))

df_invierno_max <- gather(B, key = "Columna", value = "Valor", 
                      kWhMax_winter_0.4, 
                      kWhMax_winter_5.8,
                      kWhMax_winter_9.12,
                      kWhMax_winter_13.16,
                      kWhMax_winter_17.20, 
                      kWhMax_winter_21.24, na.rm = TRUE)

ggplot(df_invierno_max, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo maximo por rango de horas en Invierno", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_invierno_max$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))



df_verano <- gather(B, key = "Columna", value = "Valor", 
                      kWhTotal_summer_0.4, 
                      kWhTotal_summer_5.8,
                      kWhTotal_summer_9.12,
                      kWhTotal_summer_13.16,
                      kWhTotal_summer_17.20, 
                      kWhTotal_summer_21.24, na.rm = TRUE)

ggplot(df_verano, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo total por rango de horas en verano", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_verano$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))

df_verano_max <- gather(B, key = "Columna", value = "Valor", 
                          kWhMax_summer_0.4, 
                          kWhMax_summer_5.8,
                          kWhMax_summer_9.12,
                          kWhMax_summer_13.16,
                          kWhMax_summer_17.20, 
                          kWhMax_summer_21.24, na.rm = TRUE)

ggplot(df_verano_max, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo maximo por rango de horas en verano", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_verano_max$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))


df_primavera <- gather(B, key = "Columna", value = "Valor", 
                    kWhTotal_spring_0.4, 
                    kWhTotal_spring_5.8,
                    kWhTotal_spring_9.12,
                    kWhTotal_spring_13.16,
                    kWhTotal_spring_17.20, 
                    kWhTotal_spring_21.24, na.rm = TRUE)

ggplot(df_primavera, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo total por rango de horas en primavera", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_primavera$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))

df_primavera_max <- gather(B, key = "Columna", value = "Valor", 
                        kWhMax_spring_0.4, 
                        kWhMax_spring_5.8,
                        kWhMax_spring_9.12,
                        kWhMax_spring_13.16,
                        kWhMax_spring_17.20, 
                        kWhMax_spring_21.24, na.rm = TRUE)

ggplot(df_primavera_max, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo maximo por rango de horas en primavera", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_primavera_max$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))


df_otono <- gather(B, key = "Columna", value = "Valor", 
                       kWhTotal_autum_0.4, 
                       kWhTotal_autum_5.8,
                       kWhTotal_autum_9.12,
                       kWhTotal_autum_13.16,
                       kWhTotal_autum_17.20, 
                       kWhTotal_autum_21.24, na.rm = TRUE)

ggplot(df_otono, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo total por rango de horas en otoño", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_otono$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))

df_otono_max <- gather(B, key = "Columna", value = "Valor", 
                           kWhMax_autum_0.4, 
                           kWhMax_autum_5.8,
                           kWhMax_autum_9.12,
                           kWhMax_autum_13.16,
                           kWhMax_autum_17.20, 
                           kWhMax_autum_21.24, na.rm = TRUE)

ggplot(df_otono_max, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo maximo por rango de horas en otoño", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_otono_max$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574","#B38FE2", "#91FFF7"))


df_finde <- gather(B, key = "Columna", value = "Valor", 
                   kWhTotal_winter_finde, 
                   kWhTotal_spring_finde,
                   kWhTotal_summer_finde,
                   kWhTotal_autum_finde, na.rm = TRUE)

ggplot(df_finde, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo total del fin de semana por estacion", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_finde$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574"))

df_finde_max <- gather(B, key = "Columna", value = "Valor", 
                   kWhMax_winter_finde, 
                   kWhMax_spring_finde,
                   kWhMax_summer_finde,
                   kWhMax_autum_finde, na.rm = TRUE)

ggplot(df_finde_max, aes(x = Columna, y = Valor, fill = Columna)) +
  geom_boxplot() +
  labs(title = "Boxplot del consumo maximo del fin de semana por estacion", x = "Columnas", y = "Valor") +
  ylim(0, quantile(df_finde_max$Valor, 0.75)) +  # Establece el límite superior del eje y
  scale_fill_manual(values = c("#9FA6FA", "#88FA8F", "#94929C", "#FCD574"))


