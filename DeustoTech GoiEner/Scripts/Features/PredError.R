# Script para predecir el error que comete cada modelo 
# a partir de las features 

library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm', "randomForest", "mice") 

foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

{
# Cargar ficheros y constantes

folder <- "TransformersV2/"
# Lista de archivos CSV en la carpeta extraída
csv_files <- list.files(folder, pattern = ".csv$", recursive = T, full.names = F)
metadata_file <- fread("metadata.csv")

# csv_files <- csv_files[201:length(csv_files)]

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
}
#EJECUTAR
model_names <- c("Media", "Naive", "SN", "Arima", "ETS", "SVM", "NN", "Ensemble")

# Target: columna que vamos a predecir: error mediano de cada modelo
target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana",
            "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana", "mapeEnsemble_mediana")

# Carga fichero con todas las features
{
feats <- read.csv("featuresPredicciones_3.csv") # Features y predicciones nuestras
feats2 <- read.csv("features_complete2.csv") # Features de cruz
feats_complete <- fread("feats-complete.csv") # Cuestionario
}

feats <- read.csv("allFeatures.csv")


# sets de features a usar 
{
# features sobre la tarifa
tarifa <- c("tarifa.tarifa_atr_re","p1_kw","p2_kw", "p3_kw","p4_kw", "p5_kw", "p6_kw",  
            "cups.direccion_cp",  "cnae") 

# features sobre el consumo
consumo <- c("cups.direccion_cp",  "kWhTotal_autum_9.12"  , "kWhTotal_autum_13.16",  "kWhTotal_autum_17.20",
             "kWhTotal_autum_21.24" , "kWhTotal_spring_0.4"  , "kWhTotal_spring_5.8" , 
  "kWhTotal_spring_9.12" , "kWhTotal_spring_13.16" ,"kWhTotal_spring_17.20", "kWhTotal_spring_21.24", "kWhTotal_summer_0.4"  , "kWhTotal_summer_5.8"  ,
  "kWhTotal_summer_9.12" , "kWhTotal_summer_13.16", "kWhTotal_summer_17.20", "kWhTotal_summer_21.24", "kWhTotal_winter_0.4"  , "kWhTotal_winter_5.8"  ,
  "kWhTotal_winter_9.12" , "kWhTotal_winter_13.16", "kWhTotal_winter_17.20", "kWhTotal_winter_21.24", "kWhMax_autum_0.4"     , "kWhMax_autum_5.8"   ,  
  "kWhMax_autum_9.12"    , "kWhMax_autum_13.16"   , "kWhMax_autum_17.20"  ,  "kWhMax_autum_21.24"  ,  "kWhMax_spring_0.4"    , "kWhMax_spring_5.8"  ,  
  "kWhMax_spring_9.12"  ,  "kWhMax_spring_13.16" ,  "kWhMax_spring_17.20"  , "kWhMax_spring_21.24"  , "kWhMax_summer_0.4"   ,  "kWhMax_summer_5.8"  ,  
  "kWhMax_summer_9.12"  ,  "kWhMax_summer_13.16" ,  "kWhMax_summer_17.20"  , "kWhMax_summer_21.24"  , "kWhMax_winter_0.4"    , "kWhMax_winter_5.8"  ,  
  "kWhMax_winter_9.12"  ,  "kWhMax_winter_13.16"  , "kWhMax_winter_17.20"  , "kWhMax_winter_21.24" ,  "kWhTotal_autum_finde" , "kWhTotal_spring_finde",
  "kWhTotal_summer_finde" ,"kWhTotal_winter_finde", "kWhMax_autum_finde"  ,  "kWhMax_spring_finde"  , "kWhMax_summer_finde" ,  "kWhMax_winter_finde"  )

# features sobre las descripcion del edificio
edificio <- c("cups.direccion_cp", "cnae", "main_residence" ,"secondary_residence" ,"energy_accumulators", "shop_office", "common_area",
              "electric_heat", "electric_kitchen","heat_pump","electric_vehicle", "dwelling_type","dwelling_age", "certificate" ,
              "climate", "locality_size", "size")

# features de descripcion socioeconómica
socio <- c("cups.direccion_cp","dwelling_type","dwelling_age","rent", "indepedent_adults","indepedent_adults_avg_age","independent_women_adult",
           "dependent_people", "dependent_people_avg_age", "dependent_women" ,"annual_savings" ,"heating_cost", "fuel_cost", "electrical_cost",
           "climate_awareness","climate_change_actions","energy_tansition_knowledge","citizen_role",
           "energy_community", "education_level", "POWER_TARGET", "TotalEnergyBudget" )

# features habitos de la CUP
habitos <- c("cups.direccion_cp", "main_residence", "secondary_residence", "energy_accumulators", "shop_office", "common_area", "office_hours", "electric_heat" ,                    
 "electric_kitchen","heat_pump","electric_vehicle","same_pattern_weekends","weekday_breakfast","weekday_lunch", "weekday_dinner", "weekday_sleep" ,                    
  "weekend_breakfast","weekend_lunch","weekend_dinner","weekend_sleep", "autumn", "winter" ,"spring","summer" ,"long_holidays","weekends", "people_at_home" )

cluster <- c("cups.direccion_cp", "cluster")

# columnas categoricas
categoricas <- c("tarifa.tarifa_atr_ref","cups.direccion_cp", "cnae", "main_residence", "secondary_residence" ,"energy_accumulators", "shop_office", "common_area", 
                 "electric_heat", "electric_kitchen","heat_pump","electric_vehicle", "same_pattern_weekends", "autumn", "winter" ,"spring","summer",
                 "long_holidays","weekends","dwelling_type","people_at_home", "dwelling_age", "certificate" ,"climate", "locality_size",
                 "size","annual_savings", "climate_change_actions", "energy_community", "citizen_role", "educational_level", "cluster")

}
for (col in colnames(feats)){
  if (col %in% categoricas){
    feats[[col]] <- as.factor(feats[[col]])
  }
}
#HASTA AQUI

# SOLO PARA COMBINAR LOS CSV
{
# features del csv de cruz que ya tenemos nosotros, no las cogemos

dontSelect <- c("P_T2.0_VALLE" ,"P_T2.0_LLANO", "P_T2.0_PICO","P_T_SOLAR_PICO","P_T_SOLAR_LLANO","P_T_SOLAR_SPICO","P_T_SOLAR_SLLANO","MAX", "MEDIAN", "cnae")
feats2.1 <- feats2 %>% select(-dontSelect) # no cogemos esas columnas


combined <- merge(feats, feats2.1, by = "ID", all.x = T)
fwrite(combined, "allFeatures.csv")
}

#columnas cuestionario
{
cols_cuest <- feats_complete %>% select(matches("^Q\\d"))
cols_cuest$ID <- feats_complete$file


# Solo las que han respondido al cuestionario entero
cuest <- cols_cuest[complete.cases(cols_cuest) & rowSums(!is.na(cols_cuest)) > 1]
cuest <- left_join(cuest, feats3[, c("ID", target)], by = "ID")
}

# Columnas y limipiar cuestionario
{
#descripcion socieconomica
descSE <- c("Q6_2_X2...Es.la", "Q6_15_X15...Cu", "Q6_16_X16...Cu", # Categoricas
            "Q6_17_X17...Cu", "Q6_18_X18...Cu", "Q6_19_X19...Cu","Q6_8_X8...Cunt" , 
            "Q6_9_X9...Cul.", "Q6_10_X10...Cu","Q6_11_X11...Cu", 
            "Q6_12_X12...Cu", "Q6_13_X13...Cu", "Q6_14_X14..Nive", "Q6_20_X20...Cu", 
            "Q6_21_X21...Has", "Q6_22_X22...Cm", "Q6_24_X24...Con",  "Q6_25_X25...Sab", 
            "Q6_99_X.Conside", "Q6_27_X27...En.") # numericas

#descripcion del edificio
descEd <- c("Q3_1_X1..El.sum", "Q3_2_X2...Cul." , "Q6_1_X1...En.qu", "Q6_3_X3...Cul.", 
            "Q6_4_X4...En.qu", "Q6_5_X5...Dispo", "Q6_6_X6...En.qu", "Q6_7_X7..Tamao") # Todas categoricas

#las costumbres de la gente
descCG <- c("Q1_1_X1...Cul.", # rango 0-10
            "Q1_2_Aumentar.l", "Q1_2_Incentivar","Q1_2_Mejorar.la",  "Q1_2_Reducir.la", "Q1_2_Todas.las." , # Binarias
            "Q1_5_Ajustar.el",  "Q1_5_Buscar.inf", "Q1_5_Cambiar.la", "Q1_5_Cambiar.lo", "Q1_5_Cambiar.mi", # Binarias
            "Q1_5_Cambiar.mi.1","Q1_5_Ninguna","Q1_5_Otro", "Q1_5_Reducir.el","Q1_5_Usar.la.fu", # Binarias
            "Q1_3_X3...Qu.o",  # Categorica
            # "Q1_99_X.Han.afe", # Son varias respuestas separadas por ; para cada CUP
            "Q1_6_Desconoca", "Q1_6_La.rutina.","Q1_6_Mi.horario" , "Q1_6_Ninguno" ,"Q1_6_No.quiero." ,    
            "Q1_6_Otro", "Q1_6_Priorizo.e",

            "Q1_7_X7...Han.a", # Categorica
            # "Q1_98_X8...Ha.b",  # Son varias respuestas separadas por ; para cada CUP
            "Q2_1_X1...Le.ha", # Categorica
            "Q2_2_X2...Le.ha",  "Q2_3_X3...Le.ha",  # rango 0-10
            "Q3_99_X.Hay.alg", # Categorica
            "Q3_98_X.Est.la" , # Categorica
            "Q4_1_X1..Elija.", "Q5_1_X1..Elija." # categoricas
            )

# Vector con las variables categóricas (incluyendo binarias)
categoricas <- c("Q6_2_X2...Es.la", "Q6_15_X15...Cu", "Q6_16_X16...Cu", 
                 "Q6_17_X17...Cu", "Q6_18_X18...Cu", "Q6_19_X19...Cu","Q6_8_X8...Cunt" , 
                 "Q6_9_X9...Cul.", "Q6_10_X10...Cu","Q6_11_X11...Cu", 
                 "Q6_12_X12...Cu", "Q6_13_X13...Cu", "Q6_14_X14..Nive", "Q6_20_X20...Cu", 
                 "Q6_21_X21...Has", "Q6_22_X22...Cm", "Q6_24_X24...Con",  "Q6_25_X25...Sab", 
                 "Q6_99_X.Conside", "Q6_27_X27...En.",
                 "Q3_1_X1..El.sum", "Q3_2_X2...Cul." , "Q6_1_X1...En.qu", "Q6_3_X3...Cul.", 
                 "Q6_4_X4...En.qu", "Q6_5_X5...Dispo", "Q6_6_X6...En.qu", "Q6_7_X7..Tamao",
                 "Q1_1_X1...Cul.", "Q1_2_Aumentar.l", "Q1_2_Incentivar","Q1_2_Mejorar.la",  
                 "Q1_2_Reducir.la", "Q1_2_Todas.las.", "Q1_5_Ajustar.el",  "Q1_5_Buscar.inf", 
                 "Q1_5_Cambiar.la", "Q1_5_Cambiar.lo", "Q1_5_Cambiar.mi", "Q1_5_Cambiar.mi.1",
                 "Q1_5_Ninguna","Q1_5_Otro", "Q1_5_Reducir.el","Q1_5_Usar.la.fu", "Q1_3_X3...Qu.o",
                 "Q1_6_Desconoca", "Q1_6_La.rutina.","Q1_6_Mi.horario" , "Q1_6_Ninguno" ,"Q1_6_No.quiero." ,    
                 "Q1_6_Otro", "Q1_6_Priorizo.e", "Q1_7_X7...Han.a", "Q2_1_X1...Le.ha", 
                 "Q2_2_X2...Le.ha",  "Q2_3_X3...Le.ha", "Q3_99_X.Hay.alg", "Q3_98_X.Est.la", 
                 "Q4_1_X1..Elija.", "Q5_1_X1..Elija.")

# Vector con las variables numéricas y de rango
numericas <- c("Q1_99_X.Han.afe", "Q1_98_X8...Ha.b", "Q2_1_X1...Le.ha", 
                       "Q2_2_X2...Le.ha", "Q2_3_X3...Le.ha")


# Definir las columnas categóricas
columnas <- c("descSE", "descEd", "descCG")

# EJECUTAR PARA NO TENER PROBLEMAS CON CATEGORICAS
for (col in colnames(cuest)){
  if (col %in% categoricas){
    cuest[[col]] <- as.factor(cuest[[col]])
  }
}

}



{
allFeatures <- c( # Lista de todas las columnas
  "ID", "LENGTH", "ZERO", "IMPUTED", "AVG",
  "SD", "MIN", "Q1", "MEDIAN", "Q3",
  "MAX", "TOTAL", "VAR", "POT_1", "POT_2",
  "POT_3", "POT_4", "POT_5", "POT_6", "POT_NOM",
  "MC25", "MC50", "MC80", "MC90", "MC95",
  "P_T2.0_VALLE", "P_T2.0_LLANO", "P_T2.0_PICO", "P_T_SOLAR_PICO", "P_T_SOLAR_LLANO",
  "P_T_SOLAR_SPICO", "P_T_SOLAR_SLLANO", "zip_code", "cnae", "municipality",
  "contracted_tariff", "self_consumption_type", "mapeMedia_mediana", "mapeNaive_mediana",
  "mapeSN_mediana", "mapeArima_mediana", "mapeETS_mediana", "mapeSVM_mediana", "mapeNN_mediana",
  "mapeEnsemble_mediana", "mapeMedia_q1", "mapeNaive_q1", "mapeSN_q1", "mapeArima_q1", "mapeETS_q1",
  "mapeSVM_q1", "mapeNN_q1", "mapeEnsemble_q1", "mapeMedia_q3", "mapeNaive_q3", "mapeSN_q3", "mapeArima_q3",
  "mapeETS_q3", "mapeSVM_q3", "mapeNN_q3", "mapeEnsemble_q3", "P1_PICO_PRECIO", "P2_LLANO_PRECIO",
  "P3_VALLE_PRECIO", "kWhTotal_autum_0.4", "kWhTotal_autum_5.8", "kWhTotal_autum_9.12", "kWhTotal_autum_13.16",
  "kWhTotal_autum_17.20", "kWhTotal_autum_21.24", "kWhTotal_spring_0.4", "kWhTotal_spring_5.8", "kWhTotal_spring_9.12",
  "kWhTotal_spring_13.16", "kWhTotal_spring_17.20", "kWhTotal_spring_21.24", "kWhTotal_summer_0.4", "kWhTotal_summer_5.8",
  "kWhTotal_summer_9.12", "kWhTotal_summer_13.16", "kWhTotal_summer_17.20", "kWhTotal_summer_21.24", "kWhTotal_winter_0.4",
  "kWhTotal_winter_5.8", "kWhTotal_winter_9.12", "kWhTotal_winter_13.16", "kWhTotal_winter_17.20", "kWhTotal_winter_21.24",
  "kWhMax_autum_0.4", "kWhMax_autum_5.8", "kWhMax_autum_9.12", "kWhMax_autum_13.16", "kWhMax_autum_17.20", "kWhMax_autum_21.24",
  "kWhMax_spring_0.4", "kWhMax_spring_5.8", "kWhMax_spring_9.12", "kWhMax_spring_13.16", "kWhMax_spring_17.20", "kWhMax_spring_21.24",
  "kWhMax_summer_0.4", "kWhMax_summer_5.8", "kWhMax_summer_9.12", "kWhMax_summer_13.16", "kWhMax_summer_17.20", "kWhMax_summer_21.24",
  "kWhMax_winter_0.4", "kWhMax_winter_5.8", "kWhMax_winter_9.12", "kWhMax_winter_13.16", "kWhMax_winter_17.20", "kWhMax_winter_21.24",
  "kWhTotal_autum_finde", "kWhTotal_spring_finde", "kWhTotal_summer_finde", "kWhTotal_winter_finde", "kWhMax_autum_finde",
  "kWhMax_spring_finde", "kWhMax_summer_finde", "kWhMax_winter_finde"
)



s1 <- c( # All stational feats
  "kWhTotal_autum_0.4", "kWhTotal_autum_5.8", "kWhTotal_autum_9.12", "kWhTotal_autum_13.16",
  "kWhTotal_autum_17.20", "kWhTotal_autum_21.24", "kWhTotal_spring_0.4", "kWhTotal_spring_5.8", "kWhTotal_spring_9.12",
  "kWhTotal_spring_13.16", "kWhTotal_spring_17.20", "kWhTotal_spring_21.24", "kWhTotal_summer_0.4", "kWhTotal_summer_5.8",
  "kWhTotal_summer_9.12", "kWhTotal_summer_13.16", "kWhTotal_summer_17.20", "kWhTotal_summer_21.24", "kWhTotal_winter_0.4",
  "kWhTotal_winter_5.8", "kWhTotal_winter_9.12", "kWhTotal_winter_13.16", "kWhTotal_winter_17.20", "kWhTotal_winter_21.24",
  "kWhMax_autum_0.4", "kWhMax_autum_5.8", "kWhMax_autum_9.12", "kWhMax_autum_13.16", "kWhMax_autum_17.20", "kWhMax_autum_21.24",
  "kWhMax_spring_0.4", "kWhMax_spring_5.8", "kWhMax_spring_9.12", "kWhMax_spring_13.16", "kWhMax_spring_17.20", "kWhMax_spring_21.24",
  "kWhMax_summer_0.4", "kWhMax_summer_5.8", "kWhMax_summer_9.12", "kWhMax_summer_13.16", "kWhMax_summer_17.20", "kWhMax_summer_21.24",
  "kWhMax_winter_0.4", "kWhMax_winter_5.8", "kWhMax_winter_9.12", "kWhMax_winter_13.16", "kWhMax_winter_17.20", "kWhMax_winter_21.24"
  
)

s2 <- c("AVG", "SD", "MIN", "Q1", "MEDIAN", "Q3", "MAX", "TOTAL", "VAR")

s3 <- c("POT_1", "POT_2",  
        "MC25", "MC50", "MC80", "MC90", "MC95","P_T2.0_VALLE", "P_T2.0_LLANO",
        "P_T2.0_PICO", "P_T_SOLAR_PICO", "P_T_SOLAR_LLANO")

}



# Función para realizar regresión y generar resultados
regresion_model_feats <- function(model_type, target_variable, trainIndex) {
    
  # TODO limpiar columnas
  
    modelo <- gsub("^mape|_mediana$", "", target_variable)
    
    columns <- list(consumo, habitos, socio, edificio, cluster)
    columns_names <- c("consumo", "habitos", "socio", "edificio", "cluster")
    results_list <- list()
    name_i = 1
    for (set in columns) {
      col_name <- columns_names[name_i]
      print(paste("Procesando columnas del set", col_name))
  
      datos <- feats[, c(set, "ID", target_variable), drop = FALSE]
      
      datos <- datos[which(!is.na(datos[[target_variable]])), ] %>% as.data.frame()

      sets_limpios <- limpiarColumnas(trainIndex, set, target_variable, datos)
      testID <- sets_limpios$testSet %>% select(ID)
      
      trainSet <- sets_limpios$trainSet %>% select(-ID)
      testSet <- sets_limpios$testSet %>% select(-ID)

      # trainSet <- datos[trainIndex, ] %>% select(-ID)
      log_variable <- paste("log", target_variable, sep = "_")
      trainSet[[log_variable]] <- log(trainSet[[target_variable]] + 1)

      # testSet <- datos[-trainIndex, ] %>% select(-ID)
      testSet[[log_variable]] <- log(testSet[[target_variable]] + 1)
  
      print(paste("Trainset: ", nrow(trainSet), "filas. Testset: ", nrow(testSet), "filas."))
      
      if (model_type == "lm") {
        # Regresión Lineal
        model <- lm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet, na.action = na.roughfix)
        predicciones_log <- exp(predict(model, newdata = testSet)) - 1
        
      } else if (model_type == "rf") {
        # Random Forest
        # print(names(trainSet))
        model <- randomForest(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet, na.action = na.roughfix)
        predicciones_log <- exp(predict(model, newdata = testSet)) - 1
      } else if (model_type == "gbm") {
        # Gradient Boosting
        model <- gbm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSet)
        predicciones_log <- exp(predict(model, newdata = testSet, n.trees = 100)) - 1
      } else if (model_type == "svm"){
        # SVM
        model <- tune(e1071::svm, as.formula(paste(log_variable, "~ . - ", target_variable)),
                                                  data = trainSet, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
        predicciones_log <- exp(predict(model$best.model, newdata = testSet)) - 1
  
      } else if (model_type == "nn"){
        
        # Neural Network
        model <- nnet(
          as.formula(paste(log_variable, "~ . - ", target_variable)),
          data = trainSet,
          size = 1
        )
        # print(predict(model, newdata = testSet))
        predicciones_log <- exp(predict(model, newdata = testSet)) - 1
        
      }
  
      namePred <- paste("Predicted", modelo, col_name, model_type, sep = "_")
      nameMAPE <- paste("MAPE", modelo, col_name, model_type, sep = "_")
  
      results_list[[namePred]] <- predicciones_log
     
      for (i in 1:nrow(testSet)) {
        results_list[[nameMAPE]][i] <- mape(testSet[i, target_variable], predicciones_log[i]) * 100
      }
      name_i = name_i + 1
     
    }

    resultados <- data.frame(
      ID = testID,
      Real = testSet[[target_variable]],
      results_list
    )
    write.csv(resultados, file = paste("Resultados/PrediccionError/AllFeats/Pred_", modelo, "_", model_type, ".csv", sep = ""), row.names = F)
   
    
    return(resultados)
}


  
regression_model_cuest <- function(model_type, target_variable, descSE_columns, descEd_columns, descCG_columns, trainIndexCuest){
  
  modelo <- gsub("^mape|_mediana$", "", target_variable)
  
  columns_descSE <- append(descSE_columns, target_variable)
  columns_descEd <- append(descEd_columns, target_variable)
  columns_descCG <- append(descCG_columns, target_variable)
  columnsDesc <- list(descSE_columns, descEd_columns, descCG_columns)
  results_list <- list()
  

  columnsDesc <- list(descSE, descCG, descEd)
  testSetCuest <- NULL
  trainSetCuest <- NULL
  i = 1

  for (colsDesc in (columnsDesc)) {
   

    sets_limpios <- limpiarColumnas(trainIndexCuest, colsDesc, target_variable)

    trainSetCuest <- sets_limpios$trainSet
    testSetCuest <- sets_limpios$testSet
    
    

    log_variable <- paste("log", target_variable, sep = "_")
    trainSetCuest[[log_variable]] <- log(trainSetCuest[[target_variable]] + 1)
    testSetCuest[[log_variable]] <- log(testSetCuest[[target_variable]] + 1)
   

    testID <- cuest[-trainIndexCuest] %>% select(ID)


    if (model_type == "lm") {
      # Regresión Lineal
      model <- lm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSetCuest)
      predicciones_log <- exp(predict(model, newdata = testSetCuest)) - 1
    } else if (model_type == "rf") {
      # Random Forest
      model <- randomForest(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSetCuest)
      predicciones_log <- exp(predict(model, newdata = testSetCuest)) - 1
    } else if (model_type == "gbm") {
      # Gradient Boosting
      model <- gbm(as.formula(paste(log_variable, "~ . - ", target_variable)), data = trainSetCuest)
      predicciones_log <- exp(predict(model, newdata = testSetCuest)) - 1
    } else if (model_type == "svm"){
      # SVM
      model <- tune(e1071::svm, as.formula(paste(log_variable, "~ . - ", target_variable)),
                    data = trainSetCuest, ranges = list(gamma = 10^(-3:2), cost = 10^(-4:4)))
      predicciones_log <- exp(predict(model$best.model, newdata = testSetCuest)) - 1
    } else if (model_type == "nn"){
      # Neural Network
      
      model <- nnet(
        as.formula(paste(log_variable, "~ . - ", target_variable)),
        data = trainSetCuest,
        size = 2
      )
      predicciones_log <- exp(predict(model, newdata = testSetCuest)) - 1
    }


    namePred <- paste("Predicted", modelo, columnas[i], model_type, sep = "_")
    nameMAE <- paste("MAE", modelo, columnas[i], sep = "_")

    i = i + 1


    print(namePred)
    results_list[[namePred]] <- predicciones_log
    results_list[[nameMAE]] <- abs(predicciones_log - testSetCuest[[target_variable]])
  }
  resultadosCuest <- data.frame(
    ID = testID,
    Real = testSetCuest[[target_variable]],
    results_list
  )



  # Como el cuestionario tiene menos observaciones para el testset, lo guardarmos en arhcivos distintos
  write.csv(resultadosCuest, file = paste("Resultados/PrediccionError/Cuest/Cuest_Pred_", modelo, "_", model_type, ".csv", sep = ""))
  return(resultadosCuest)
}
  
# Lista de modelos
modelos <- c("lm", "rf", "gbm", "svm", "nn")
# modelos <- c("lm", "svm")

# Lista de variables objetivo
target <- c("mapeMedia_mediana", "mapeNaive_mediana", "mapeSN_mediana", "mapeArima_mediana", 
            "mapeETS_mediana", "mapeNN_mediana", "mapeSVM_mediana", "mapeEnsemble_mediana")



set.seed(0)
index <- 0.75
cuest_nrow <- nrow(cuest)
feats_nrow <- nrow(feats %>% filter(!is.na(mapeSVM_mediana)))
# trainIndex <- sample(1:feats_nrow, index * feats_nrow) 
# trainIndexCuest <- sample(1:cuest_nrow, index * cuest_nrow)

limpiarColumnas <- function(trainIndex, colsDesc, target, dataset) {
  
  resultados <- list()
  
  #colsDesc <- colsDesc[colsDesc != "cups.direccion_cp"]

  cleanSet <- dataset %>% select(all_of(colsDesc), !!sym(target), ID)
  trainSet <- cleanSet[0, ]
  
  for (col in colsDesc){
    if (col %in% categoricas){
      niveles <- unique(cleanSet[[col]])
      for (nivel in niveles) {
        observacion <- cleanSet %>% filter(cleanSet[[col]] == nivel) %>% slice(1)
        trainSet <- bind_rows(trainSet, observacion)
        }
        print(paste("TrainSet tiene todos los niveles de", col))
    }
  }
  cleanSet2 <- anti_join(cleanSet, trainSet) # filas que todavia no hemos añadido al trainset
  clean_nrow <- nrow(cleanSet2)
  trainIndexClean <- sample(1:clean_nrow, index * clean_nrow)
  
  trainSet2 <- cleanSet2[trainIndexClean, ]
  trainSet <- bind_rows(trainSet, trainSet2)
  testSet <- cleanSet2[-trainIndexClean, ]
  
  for (col in colsDesc){
    if (col %in% categoricas){
      if (length(levels(trainSet[[col]])) <= 2 || length(levels(testSet[[col]])) <=  2){
        cat(paste("Eliminando la columna", col, "debido a dos o menos niveles.\n"))
        trainSet[[col]] <- NULL
        testSet[[col]] <- NULL
      }
    }
    else {
      # Imputación para variables numéricas
      valueToImpute <- median(trainSet[[col]], na.rm = TRUE) # O usar mean según sea apropiado
      trainSet[[col]][is.na(trainSet[[col]])] <- valueToImpute
      testSet[[col]][is.na(testSet[[col]])] <- valueToImpute
    }
  }
  
  return(list(trainSet = trainSet, testSet = testSet))
}


# Regresion con columnas de las features
for (modelo in modelos) {
  for (variable in target) {
    regresion_model_feats(modelo, variable, trainIndex)
  }
}

# Regresion con columnas del cuestionario
for (modelo in modelos) {
  for (variable in target) {
    regression_model_cuest(modelo, variable, descSE, descEd, descCG, trainIndexCuest)
  }
}








# CLASIFICACION
cat_columns <- c("municipality", "contracted_tariff", "best_model", "ID", "zip_code", "cnae", "self_consumption_type")
num_columns <- setdiff(names(feats3), cat_columns)

for(col in num_columns) {
  if(col %in% names(feats3)) {
    feats3[[col]] <- ifelse(is.na(feats3[[col]]), median(feats3[[col]], na.rm = TRUE), feats3[[col]])
  }
}

for(col in cat_columns) {
  mode_value <- calc_mode(feats3[[col]][!is.na(feats3[[col]])])
  feats3[[col]][is.na(feats3[[col]])] <- mode_value
}


clasificacion_model <- function(model_type, s1, s2, s3) {
  
  # seleccionamos solo las filas que tengan variable de respuesta  (best model)
  datos_clasificacion <- feats3[which(!is.na(feats3$best_model)), ]
  datos_clasificacion <- subset(datos_clasificacion, best_model != "")
  set.seed(0)
  index <- 0.75
  target <- datos_clasificacion$best_model
  
  columns_s1 <- append(s1, "best_model")
  columns_s2 <- append(s2, "best_model")
  columns_s3 <- append(s3, "best_model")
  
  columns <- list(columns_s1, columns_s2, columns_s3)
  names(columns) <- c("s1", "s2", "s3")
  results_list <- list()
  
  for (cols in names(columns)) {
    
    col_name <- cols
    col <- columns[[cols]]
    datos <- datos_clasificacion[, col]
    datos$ID <- datos_clasificacion$ID
    
    trainIndex <- sample(1:nrow(datos), index * nrow(datos))
    trainset <- datos[trainIndex, ] %>% select(-ID)
    testset <- datos[-trainIndex, ] %>% select(-ID)
    
    
    if (model_type == "svm") {
      modelo_clasificacion <- svm(as.factor(best_model) ~ ., data = trainset, probability = TRUE)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, probability = T)
      
      probabilidades <- attr(predicciones_clasificacion, "probabilities")
      clase_con_mayor_probabilidad <- apply(probabilidades, 1, which.max)
      nombres_de_clases <- colnames(probabilidades)
      predicciones_clasificacion <- nombres_de_clases[clase_con_mayor_probabilidad]
      
    } else if (model_type == "gbm") {
      modelo_clasificacion <- gbm(as.factor(best_model) ~ ., data = trainset, n.trees = 100)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "response")
      
      probabilidades_promedio <- apply(predicciones_clasificacion, c(1,2), mean)
      predicciones_clasificacion <- apply(probabilidades_promedio, 1, function(x) names(x)[which.max(x)])

    } else if (model_type == "logistic") {
      modelo_clasificacion <- multinom(best_model ~ ., data = trainset)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "probs")
      
      probabilidades_promedio <- apply(predicciones_clasificacion, c(1,2), mean)
      predicciones_clasificacion <- apply(probabilidades_promedio, 1, function(x) names(x)[which.max(x)])
      
    } else if (model_type == "rf") {
      modelo_clasificacion <- randomForest(as.factor(best_model) ~ ., data = trainset, ntree = 100, probability = T)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testset, type = "prob")
      
      probabilidades_promedio <- apply(predicciones_clasificacion, c(1,2), mean)
      predicciones_clasificacion <- apply(probabilidades_promedio, 1, function(x) names(x)[which.max(x)])
     }
    
    namePred <- paste("Predicted", model_type, col_name, sep = "_")
    results_list[[namePred]] <- predicciones_clasificacion
    correct_predictions <- predicciones_clasificacion == testset$best_model
    nameErrorRate <- paste("Error", model_type, col_name, sep = "_")
    results_list[[nameErrorRate]] <- as.numeric(correct_predictions)
    
  }
  
  # Crear un nuevo conjunto de resultados
  resultados_clasificacion <- data.frame(
    ID = datos$ID[-trainIndex],
    Real = testset$best_model,
    results_list
  )
  
  # Escribir el CSV final
  fwrite(resultados_clasificacion, file = paste("Resultados/PrediccionClasificacion/Clasif_", model_type, ".csv", sep = ""))
  
  return(resultados_clasificacion)
}

# Definir modelos
#modelos_clasificacion <- c( "gbm", "logistic", "svm", "rf")
modelos_clasificacion <- c("rf")

for (modelo_clasificacion in modelos_clasificacion) {
  clasificacion_model(modelo_clasificacion, s1, s2, s3)
} 



clasification_model_cuest <- function(model_type, target, descSE_columns, descEd_columns, descCG_columns){
  
  results_list <- list()
  
  columnsDesc <- list(
    descSE = descSE_columns,
    descEd = descEd_columns,
    descCG = descCG_columns
  )
  
  target_values <- feats3[[target]]
  

  for (group in names(columnsDesc)) {
    
    colsDesc <- columnsDesc[[group]]
    sets_limpios <- limpiarColumnas(trainIndexCuest, unlist(colsDesc), target)
    
    trainSetCuest <- sets_limpios$trainSet
    trainTargetValues <- target_values[trainIndexCuest]
    trainSetCuest[[target]] <- trainTargetValues
    
    
    
    # Preparar testSetCuest
    testSetCuest <- sets_limpios$testSet
    # Necesitamos definir testIndexCuest correctamente
    testIndexCuest <- setdiff(1:nrow(cuest), trainIndexCuest)
    testTargetValues <- target_values[testIndexCuest]
    testSetCuest[[target]] <- testTargetValues
    
    testID <- cuest[-trainIndexCuest] %>% select(ID)

    if (model_type == "svm") {
      # Random Forest para clasificación
      modelo_clasificacion <- svm(as.factor(best_model) ~ ., data = trainSetCuest, probability = T)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testSetCuest, type = "response")
    } else if (model_type == "gbm") {
      # Gradient Boosting para clasificación
      modelo_clasificacion <- gbm(as.factor(best_model) ~ ., data = trainSetCuest, n.trees = 100)
      predicciones_clasificacion <- predict.gbm(modelo_clasificacion, newdata = testSetCuest, type = "response")
      
      probabilidades_promedio <- apply(predicciones_clasificacion, c(1,2), mean)
      clases_predichas <- apply(probabilidades_promedio, 1, function(x) names(x)[which.max(x)])
      predicciones_clasificacion <- as.factor(clases_predichas)
    } else if (model_type == "logistic") {
      # Regresión Logística para clasificación
      modelo_clasificacion <- multinom(best_model ~ ., data = trainSetCuest)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testSetCuest)
    } else if (model_type == "rf") {
      modelo_clasificacion <- randomForest(as.factor(best_model) ~ ., data = trainSetCuest, ntree = 100)
      predicciones_clasificacion <- predict(modelo_clasificacion, newdata = testSetCuest)
    }
    
    namePred <- paste("Predicted", model_type, group, sep = "_")
    results_list[[namePred]] <- predicciones_clasificacion
    correct_predictions <- predicciones_clasificacion == testSetCuest[[target]]
    nameErrorRate <- paste("Error", model_type, group, sep = "_")
    results_list[[nameErrorRate]] <- as.numeric(correct_predictions)
    
  }
  resultadosCuest <- data.frame(
    ID = testID,
    Real = testSetCuest[[target]],
    results_list
  )
  
  
  
  # Como el cuestionario tiene menos observaciones para el testset, lo guardarmos en arhcivos distintos
  write.csv(resultadosCuest, file = paste("Resultados/PrediccionClasificacion/Cuest/Cuest_Clasif_", model_type, ".csv", sep = ""))
  return(resultadosCuest)
}


# Definir modelos
modelos_clasificacion <- c( "rf", "gbm", "logistic", "svm")


set.seed(0)
index <- 0.75
cuest_nrow <- nrow(cuest)
feats_nrow <- nrow(feats3 %>% filter(!is.na(best_model)))
trainIndex <- sample(1:feats_nrow, index * feats_nrow) # 214 porque son las que no son NA
trainIndexCuest <- sample(1:cuest_nrow, index * cuest_nrow)

for (modelo_clasificacion in modelos_clasificacion) {
  clasification_model_cuest(modelo_clasificacion, "best_model",descSE, descEd, descCG)
} 


