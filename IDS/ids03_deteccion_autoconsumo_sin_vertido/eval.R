library(caret)
library(stringr)

RESULTS  <- c("-A0.01-Q50","-A0.01-Q75","-A0.05-Q50","-A0.05-Q75")
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
for (z in RESULTS)
  for (f in FEATURES)
    for (p in PERIODS)
    {
      data <- read.csv(paste0("results",z,"/forecast-",p,"-",f,".csv"))
      data <- data[data$CUPS %in% r$ID,]

      DIFF <- setdiff(r$ID,data$CUPS)
      ZZ   <- data.frame(DIFF, as.data.frame(t(rep(NA,length(names(data))-1))))
      names(ZZ) <- names(data)
      data <- rbind(data,ZZ)

      data <- data[order(data$CUPS),]
      for (k in KPI)
      {
        conf <- confusionMatrix(data=factor(ifelse(data[,k]>0,1,0),levels=LEVELS),reference=r$hasPV)
        cat(f,p,k,z,conf$overall[1],conf$byClass[2],conf$overall[2],length(DIFF),"\n",file="resultados.csv",append=TRUE,sep=",")
      }
    }


#AJUSTAR CODIGO PARA LAS CARPETAS CON HIPERPARAMETROS

library(caret)

# No necesitas RESULTS ahora, porque los nombres de las carpetas han cambiado
FEATURES <- c("ENTROPY", "AVG", "SD", "VAR", "Q1", "MEDIAN", "Q3", "TOTAL",
              "T2.0_LLANO", "T2.0_VALLE", "T_SOLAR_PICO", "T_SOLAR_SPICO",
              "T2.0_PICO", "T_SOLAR_SLLANO")
PERIODS <- c("Daily", "Monthly", "Weekly")
KPI <- c("AVR", "Q2", "ECDFA", "ECDFQ", "OUT", "DT", "RT", "ENS")

# Asumiendo que ALPHA y QMAX varían en el mismo rango que el script de forecast
ALPHAS <- seq(0.01, 0.05, by = 0.01)
QMAXS <- seq(0.70, 0.90, by = 0.1)

r <- read.csv("SOLAR/Variation/HasPV.csv")[, c("ID", "hasPV")]
r <- r[order(r$ID), ]
r$hasPV <- factor(r$hasPV)
LEVELS <- levels(r$hasPV)

cat("Features", "Period", "KPI", "Alpha", "QMAX", "Accuracy", "Specificity", "Kappa", "#", "\n", file = "resultados.csv", sep = ",")
for (ALPHA in ALPHAS) {
  for (QMAX in QMAXS) {
    for (f in FEATURES) {
      for (p in PERIODS) {
        file_path <- sprintf("results/ALPHA_%0.2f_QMAX_%0.2f/forecast-%s-%s.csv", ALPHA, QMAX, p, f)
          data <- read.csv(file_path)
          data <- data[data$CUPS %in% r$ID,]
          
          DIFF <- setdiff(r$ID, data$CUPS)
          if (length(DIFF) > 0) {
            ZZ <- data.frame(DIFF, as.data.frame(matrix(NA, nrow = length(DIFF), ncol = length(names(data)) - 1)))
            names(ZZ) <- names(data)
            data <- rbind(data, ZZ)
          }
          
          data <- data[order(data$CUPS),]
          for (k in KPI) {
            conf <- confusionMatrix(data = factor(ifelse(data[, k] > 0, "1", "0"), levels = c("0", "1")), reference = r$hasPV)
            cat(f, p, k, ALPHA, QMAX, conf$overall['Accuracy'], conf$byClass['Specificity'], conf$Kappa, length(DIFF), "\n", file = "resultados.csv", append = TRUE, sep = ",")
          }
      }
    }
  }
}

