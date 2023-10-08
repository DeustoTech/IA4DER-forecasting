library(data.table)
library(vtable)
library(zoo)
library(forecast)
library(neuralnet)
library(e1071)
library(TSSVM)
library(doFuture)
library(matrixStats)
library(PMCMRplus)
library(rcompanion)
library(multcompView)
#library(progressr)
plan(multisession)
#handlers(global = TRUE)

TRAIN_LIMIT <- 0.75  ### length of the training period
COMPLETE    <- 0.10  ### amount of data imputed allowed in the dataset
SAMPLE      <- 2000  ### number of cups to assess

FILES       <- list.files(path="post_cooked/",pattern="*.csv")
CT          <- list.files(path="post_cooked/",pattern="*-CT.csv")

MODELS      <- c("mean","rw","snaive","simple","lr","ann","svm","arima","ses","ens")
KPI         <- c("model","length","zeros","imputed","mean","sd","min","q1","q2","q3","max")
LO          <- 2*length(MODELS)

dir.create("results/mape",showWarnings = F, recursive = T)
dir.create("results/rmse",showWarnings = F, recursive = T)

B <- foreach(NAME = union(sample(FILES,SAMPLE),sample(CT,SAMPLE))) %dofuture% {

  a <- fread(paste("post_cooked/",NAME,sep=""))
  r <- zoo(a$kWh, order.by = a$time)
  
  LENGTH  <- length(a$kWh)
  ZEROS   <- sum(a$kWh==0)/LENGTH
  IMPUTED <- sum(a$issue)/LENGTH

  ZZ   <- floor(0.75*(length(r)/24))    #### training days
  RMSE <- MAPE <- data.frame(matrix(ncol = length(MODELS), nrow = (floor(length(r)/24)-ZZ-1)))
  p    <- data.frame(matrix(ncol = length(MODELS), nrow = 24))
  colnames(p) <- colnames(RMSE) <- colnames(MAPE) <- MODELS
  
  if( IMPUTED < COMPLETE ) {                 ### If we have enough non imputed values in the dataset,
                                             ### we continue with the assessment
    ARMA <- arimaorder(auto.arima(r))        ### ÑAPA: get hyperparameters of arima model taking the full serie
    
    #### METER AQUI LA PREDICCION DE LOS MODELOS COMPLETOS

    ##  NN    <- nnetar(rt)
    ##  SVM   <- ARSVM(rt,h=24)

    for (i in ZZ:(floor(length(r)/24)-1))    ### time series cross validation
    {
      rt     <-            window(r,                       end=index(r)[i*24])
      rv     <- as.numeric(window(r,start=index(r)[i*24+1],end=index(r)[i*24+24]))

      #### TRAINING
      p["mean"]  <- rep(mean(rt),24)
      p["rw"]    <- as.numeric(window(r,start=index(r)[i*24-23],      end=index(r)[i*24]))
      p["snaive"]<- as.numeric(window(r,start=index(r)[i*24-24*6-23], end=index(r)[i*24-24*6]))
      p["simple"]<- rowMeans(data.frame(
                    a=(as.numeric(window(r,start=index(r)[i*24-24*6-23], end=index(r)[i*24-24*6]))), 
                    b=(as.numeric(window(r,start=index(r)[i*24-24*13-23],end=index(r)[i*24-24*13]))),
                    c=(as.numeric(window(r,start=index(r)[i*24-24*20-23],end=index(r)[i*24-24*20])))),na.rm=T)

      TRAINSET        <- merge(rt,lag(rt,-24))
      names(TRAINSET) <- c("real","past")
    
      LM    <- lm(real~past,data=TRAINSET)
      ARIMA <- Arima(rt,order=ARMA)
      ES    <- ses(rt,h=24)
      ##NN    <- neuralnet(real~past,data=TRAINSET,hidden=24,linear.output = FALSE)
      ##SVM   <- neuralnet(real~past,data=TRAINSET,hidden=24,linear.output = FALSE)

      p["lr"]    <- forecast(LM,rv)$mean
      p["arima"] <- forecast(ARIMA,h=24)$mean
      p["ses"]   <- forecast(ES,h=24)$mean
    ##  p["ann"]   <- as.numeric(forecast(NN,h=24)$mean)
    ##  p["svm"]   <- as.numeric(forecast(SVM,h=24))
      p["svm"]   <- p["ann"] <- p["rw"]
      p["ens"]   <- rowMedians(as.matrix(p),na.rm=T)
      
      #### VALIDATION
      aux     <- rv != 0
      for(j in MODELS)
      {
        MAPE[i-ZZ,j] <- 100*median(ifelse(sum(aux)!=0,abs(rv[aux]-p[aux,j])/rv[aux],NA))
        RMSE[i-ZZ,j] <- sqrt(median((rv-p[,j])^2))
      }
    }
  }

  cat(KPI,"\n",sep=",",file=paste("results/mape/kpi-mape-",NAME,sep=""))
  cat(KPI,"\n",sep=",",file=paste("results/rmse/kpi-rmse-",NAME,sep=""))
  for(j in MODELS)
  {
    cat(j,LENGTH,ZEROS,IMPUTED,
        mean(MAPE[,j],na.rm=T),sd(MAPE[,j],na.rm=T),
        quantile(MAPE[,j],c(0,0.25,0.5,0.75,1),na.rm=T), 
        "\n",sep=",", file=paste("results/mape/kpi-mape-",NAME,sep=""),append=T)
    cat(j,LENGTH,ZEROS,IMPUTED,
        mean(RMSE[,j],na.rm=T),sd(RMSE[,j],na.rm=T),
        quantile(RMSE[,j],c(0,0.25,0.5,0.75,1),na.rm=T),
        "\n",sep=",", file=paste("results/rmse/kpi-rmse-",NAME,sep=""),append=T)
  }
}

ALL <- list.files(path="results/mape/",pattern="*.csv")
KCT <- list.files(path="results/mape/",pattern="*CT.csv")

RCT <- foreach(NAME = KCT,.combine=rbind) %dofuture% {
  fread(paste("results/mape/",NAME,sep=""),select=KPI)
}

RCU <- foreach(NAME = setdiff(ALL,KCT),.combine=rbind) %dofuture% {
  fread(paste("results/mape/",NAME,sep=""),select=KPI)
}

EVAL <- function(R)
{
  setDT(R)
  R$model <- as.factor(R$model)
  print(R[,as.list(summary(q2)), by = "model"])
  boxplot(q2~model,data=R,outline=F,ylab="MAPE (%)")
  print(kruskalTest(q2~model,data=R))
  multiple <- kwAllPairsNemenyiTest(q2~model,data=R)
  print(multcompLetters(fullPTable(multiple$p.value)))
  plot(multiple)
}

EVAL(RCT)
EVAL(RCU)

#R <- fread("kpi.csv")
# cat("cups","model","kpi","length","zeros","imputed","mean","sd","min","q1","q2","q3","max",     "\n",sep=",",file="kpi.csv")
#     cat(NAME,j,"mape",LENGTH,ZEROS,IMPUTED,mean(MAPE[,j],na.rm=T),sd(MAPE[,j],na.rm=T),quantile(MAPE[,j],c(0,0.25,0.5,0.75,1),na.rm=T),"\n",sep=",",file="kpi.csv",append=T)
#     cat(NAME,j,"rmse",LENGTH,ZEROS,IMPUTED,mean(RMSE[,j],na.rm=T),sd(RMSE[,j],na.rm=T),quantile(RMSE[,j],c(0,0.25,0.5,0.75,1),na.rm=T),"\n",sep=",",file="kpi.csv",append=T)


#
#COUNTRY <- AGE <- GENDER <- numeric(length(names(F[,-c(1:3)])))
#
#for (i in names(F[,-c(1:3)]))
#{
#   GENDER[i]  <- t.test(as.formula(paste(as.name(i),"ID202",sep="~")),data=F)$p.value
#   AGE[i]     <- t.test(as.formula(paste(as.name(i),"ID137",sep="~")),data=F)$p.value
#   COUNTRY[i] <- t.test(as.formula(paste(as.name(i),"ID300",sep="~")),data=F)$p.value
#}
