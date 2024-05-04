library(caret)
library(stringr)

FEATURES <- c("ENTROPY","AVG","SD","VAR","Q1","MEDIAN","Q3","TOTAL",
              "T2.0_LLANO","T2.0_VALLE","T_SOLAR_PICO","T_SOLAR_SPICO",
              "T2.0_PICO","T_SOLAR_SLLANO")
PERIODS  <- c("Daily","Monthly","Weekly")
KPI      <- c("AVR","Q2","ECDFA","ECDFQ","OUT","DT","RT","ENS")


r  <- read.csv("SOLAR/Variation/HasPV.csv")[,c("ID","hasPV")]
r  <- r[order(r$ID),]
r$hasPV <- factor(r$hasPV)
LEVELS  <- levels(r$hasPV)
rn <- sample(LEVELS,length(r$hasPV),replace=TRUE)
ra <- confusionMatrix(rn, r$hasPV)

overall_accuracy <- ifelse(is.data.frame(ra), ra$overall["Accuracy"], ra["Accuracy"])
precision <- ifelse(is.data.frame(ra), ra$byClass["Precision"], ra["Precision"])


cat("Features","Period","KPI","Hyper","Accuracy","Specificity","Kappa","#","\n",file="resultados.csv",sep=",")
cat(NA, NA, "random", NA, overall_accuracy, precision, NA, 0, "\n", file = "resultados.csv", append = TRUE, sep = ",")

  for (f in FEATURES) {
    for (p in PERIODS)
    {
      data <- read.csv(paste0("results","/forecast-",p,"-",f,".csv"))
      data <- data[data$CUPS %in% r$ID,]

      DIFF <- setdiff(r$ID,data$CUPS)
      ZZ   <- data.frame(DIFF, as.data.frame(t(rep(NA,length(names(data))-1))))
      names(ZZ) <- names(data)
      data <- rbind(data,ZZ)

      data <- data[order(data$CUPS),]
      for (k in KPI)
      {
        conf <- confusionMatrix(factor(ifelse(data[,k]>0,1,0),levels=LEVELS),r$hasPV)
        print(conf)
        cat(f,p,k,z,conf$overall[1],conf$byClass[2],conf$overall[2],length(DIFF),"\n",file="resultados.csv",append=TRUE,sep=",")
      }
    }
  }


#AJUSTAR CODIGO PARA LAS CARPETAS CON HIPERPARAMETROS
library(caret)
library(stringr)

# Define las carpetas de resultados
carpetas_resultados <- list.dirs(path = "results", recursive = FALSE)

# Características y otras variables
FEATURES <- c("ENTROPY", "AVG", "SD", "VAR", "Q1", "MEDIAN", "Q3", "TOTAL",
              "T2.0_LLANO", "T2.0_VALLE", "T_SOLAR_PICO", "T_SOLAR_SPICO",
              "T2.0_PICO", "T_SOLAR_SLLANO")
PERIODS  <- c("Daily", "Monthly", "Weekly")
KPI      <- c("AVR", "Q2", "ECDFA", "ECDFQ", "OUT", "DT", "RT", "ENS")

# Leer datos de referencia

r  <- read.csv("SOLAR/Variation/HasPV.csv")[,c("ID","hasPV")]
r  <- r[order(r$ID),]
r$hasPV <- factor(r$hasPV)
LEVELS  <- levels(r$hasPV)
rn <- sample(LEVELS,length(r$hasPV),replace=TRUE)
ra <- confusionMatrix(rn, r$hasPV)

overall_accuracy <- ifelse(is.data.frame(ra), ra$overall["Accuracy"], ra["Accuracy"])
precision <- ifelse(is.data.frame(ra), ra$byClass["Precision"], ra["Precision"])


# Crear el archivo de resultados
cat("Features", "Period", "KPI", "Hyper", "Accuracy", "Specificity", "Kappa", "#", "\n", 
    file = "resultados.csv", sep = ",")

cat(NA, NA, "random", NA, overall_accuracy, precision, NA, 0, "\n", 
    file = "resultados.csv", append = TRUE, sep = ",")

# Iterar sobre cada carpeta de resultados
for (carpeta in carpetas_resultados) {
  z <- basename(carpeta)
  for (f in FEATURES) {
    for (p in PERIODS) {
      archivo <- paste0(carpeta, "/forecast-", p, "-", f, ".csv")
      if (file.exists(archivo)) {
        data <- read.csv(archivo)
        data <- data[data$CUPS %in% r$ID, ]
        
        # Añadir registros que faltan
        DIFF <- setdiff(r$ID, data$CUPS)
        ZZ   <- data.frame(DIFF, as.data.frame(t(rep(NA, length(names(data)) - 1))))
        names(ZZ) <- names(data)
        data <- rbind(data, ZZ)
        
        data <- data[order(data$CUPS), ]
        for (k in KPI) {
          pred <- factor(ifelse(data[, k] > 0, 1, 0), levels = LEVELS)
          conf <- confusionMatrix(pred, r$hasPV)
          print(conf)
          
          cat(f, p, k, z, conf$overall[1], conf$byClass[2], conf$overall[2], length(DIFF), "\n",
              file = "resultados.csv", append = TRUE, sep = ",")
        }
      }
    }
  }
}
