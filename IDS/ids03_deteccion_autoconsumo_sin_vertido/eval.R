library(caret)

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
ra <- confusionMatrix(data=factor(rn,levels=LEVELS),reference=r$hasPV)

cat("Features","Period","KPI","Hyper","Accuracy","Specificity","Kappa","#","\n",file="resultados.csv",sep=",")
cat(NA,NA,"random",NA,ra$overall[1],ra$byClass[2],ra$overall[2],0,"\n",file="resultados.csv",append=TRUE,sep=",")
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
