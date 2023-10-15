library(data.table)
library(vtable)
library(zoo)
library(forecast)
library(neuralnet)
library(e1071)
library(doFuture)
library(matrixStats)
library(PMCMRplus)
library(rcompanion)
library(multcompView)
library(boot)

plan(multisession)

TEST        <- TRUE  ### si estoy haciendo test
SAMPLE      <- 50    ### number of elements to assess per each type
COMPLETE    <- 0.10  ### amount of data imputed allowed in the dataset
TRAIN_LIMIT <- 0.75  ### length of the training period
F_DAYS      <- 7     ### number of days to forecast for STLF
T_DAYS      <- 21    ### number of days to use in the training widnow for AI methods for STLF

#### aqui podria leer lo que se ha generado en results y no recalcular de nuevo

ALL    <- list.files(path="post_cooked/",pattern="*.csv")
OTHERS <- list.files(path="post_cooked/",pattern="-")

CUPS   <- setdiff(ALL,OTHERS)
CT     <- list.files(path="post_cooked/",pattern="*-CT.csv")
LINE   <- list.files(path="post_cooked/",pattern="*-LINE.csv")

FILES  <- union(sample(CT,SAMPLE),sample(LINE,SAMPLE))
FILES  <- union(sample(CUPS,SAMPLE),FILES)

MODELS <- c("mean","rw","naive","simple","lr","ann","svm","arima","ses","ens")
KPI    <- c("model","length","zeros","imputed","mean","sd","min","q1","q2","q3","max")
LO     <- 2*length(MODELS)

dir.create("results/forecast",showWarnings = F, recursive = T)
dir.create("results/test",    showWarnings = F, recursive = T)

B <- foreach(NAME = FILES,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  a <- fread(paste("post_cooked/",NAME,sep=""))
  r <- zoo(a$kWh, order.by = a$time)

  if (TEST) ####### ÑAPA hasta tener los datos finales acorto una semana la serie temporal
  {
    a    <- a[1:(length(a[[1]])-24*F_DAYS)]
    real <- window(r,start=index(r)[length(r)-24*F_DAYS+1])
    r    <- window(r,end=index(r)[length(r)-24*F_DAYS])
  } else {
    #real <- fread()
    real <- window(r,start=index(r)[length(r)-24*F_DAYS+1])
  }

  TRAIN_DAYS  <- floor(TRAIN_LIMIT*(length(r)/24))    #### training days
  LENGTH      <- length(a$kWh)
  ZEROS       <- sum(a$kWh==0)/LENGTH
  IMPUTED     <- sum(a$issue)/LENGTH

  TKPI        <- data.frame(matrix(ncol = length(MODELS), nrow = 2))
  f           <- data.frame(matrix(ncol = length(MODELS), nrow = 24*F_DAYS))
  colnames(f) <- colnames(TKPI) <- MODELS
  
  ### If we have enough non zero, non imputed values in the dataset,
  ### then we continue with the assessment
  if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE))
  {
    f["mean"]  <- as.numeric(rep(mean(r),24*F_DAYS))
    f["rw"]    <- rep(as.numeric(window(r,start=index(r)[length(r)-23])),F_DAYS)
    f["naive"]<- as.numeric(window(r,   start=index(r)[length(r)-24*6-23], end=index(r)[length(r)-24*( 6-F_DAYS-1)]))
    f["simple"]<- rowMeans(data.frame(
                  a=(as.numeric(window(r,start=index(r)[length(r)-24*6 -23],end=index(r)[length(r)-24*( 6-F_DAYS-1)]))),
                  b=(as.numeric(window(r,start=index(r)[length(r)-24*13-23],end=index(r)[length(r)-24*(15-F_DAYS-1)]))),
                  c=(as.numeric(window(r,start=index(r)[length(r)-24*20-23],end=index(r)[length(r)-24*(22-F_DAYS-1)])))),
                  na.rm=T)

    rr              <- merge(r,lag(r,-24))
    TRAINSET        <- window(rr,start=index(r)[length(r)-24*T_DAYS-23])
    PREDICT         <- data.frame(past=as.numeric(window(r,start=index(r)[length(r)-24*F_DAYS+1])))
    names(TRAINSET) <- c("real","past")

    LM     <- lm(real~past,data=TRAINSET)
    ARIMA  <- auto.arima(r)
    ES     <- ses(r,h=24*F_DAYS)
    NN     <- nnetar(r)
    SVM    <- tune(svm,real~past,data=TRAINSET,ranges=list(elsilon=seq(0,1,0.2), cost=seq(1,100,10)))

    f["lr"]    <- try(as.numeric(forecast(LM,PREDICT)$mean),TRUE)
    f["arima"] <- try(as.numeric(forecast(ARIMA,h=24*F_DAYS)$mean),TRUE)
    f["ses"]   <- try(as.numeric(forecast(ES,h=24*F_DAYS)$mean),TRUE)
    f["ann"]   <- try(as.numeric(forecast(NN,h=24*F_DAYS)$mean),TRUE)
    f["svm"]   <- try(as.numeric(predict(SVM$best.model,PREDICT)),TRUE)
    f["ens"]   <- rowMedians(as.matrix(f),na.rm=T)
  
    write.csv(f, file=paste("results/forecast/forecast-",NAME,".csv",sep=""),row.names=F)

    aux  <- real != 0
    for(j in MODELS)
    {
      TKPI[1,j] <- 100*median(ifelse(sum(aux)!=0,abs(real[aux]-f[aux,j])/real[aux],NA))
      TKPI[2,j] <- sqrt(median((real-f[,j])^2))
    }
    
    rownames(TKPI) <- c("mape","rmse")
    write.csv(TKPI,file=paste("results/test/kpi-test-",NAME,".csv",sep=""))
  } ### if that test if the time series has data
}   ### foreach

ALL    <- list.files(path="results/test/",pattern="*.csv")
CT     <- list.files(path="results/test/",pattern="*-CT.csv")
LINE   <- list.files(path="results/test/",pattern="*-LINE.csv")
CUPS   <- setdiff(ALL,union(CT,LINE))

RCT <- foreach(NAME = CT,.combine=rbind) %dofuture% {
  fread(paste("results/test/",NAME,sep=""))
}

RLI <- foreach(NAME = LINE,.combine=rbind) %dofuture% {
  fread(paste("results/test/",NAME,sep=""))
}

RCU <- foreach(NAME = CUPS,.combine=rbind) %dofuture% {
  fread(paste("results/test/",NAME,sep=""))
}

EVAL <- function(R,K,TYPE)
{
  RR <- na.omit(as.matrix(R[R$V1==K,..MODELS]))
  rownames(RR) <- 1:nrow(RR)

  write.csv(as.data.frame(apply(RR, 2, summary)),
            file=paste("stlf-",TYPE,"-",K,".csv",sep=""))
  BOOT <- boot(data=RR,statistic=function(data,i) colMedians(data[i,],na.rm=T),R=100)

  pdf(file=paste("stlf-",TYPE,"-",K,".pdf",sep=""))
    #boxplot(RR,outline=F,ylab="MAPE (%)")
    print(friedman.test(RR))
    multiple <- frdAllPairsNemenyiTest(RR)
    print(multcompLetters(fullPTable(multiple$p.value)))
    plot(multiple)
    boxplot(BOOT$t,names=MODELS,ylab="Confidence Interval over the Median")
  dev.off()
}

EVAL(RCT,"mape","CT")
EVAL(RLI,"mape","LINE")
EVAL(RCU,"mape","CUPS")

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
