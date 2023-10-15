library(data.table)
library(vtable)
library(zoo)
library(xts)
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

FORECAST <- function(R,TT,KPI,N)
{
  f              <- data.frame(matrix(ncol = length(MODELS), nrow = F_DAYS))
  MAPE  <- RMSE  <- data.frame(matrix(ncol = length(MODELS), nrow = 1))
  colnames(MAPE) <- colnames(RMSE) <- colnames(f) <- MODELS

  f["mean"]  <- as.numeric(rep(mean(R),F_DAYS))
  f["rw"]    <- rep(as.numeric(window(R,start=index(R)[length(R)])),F_DAYS)
  f["naive"] <- as.numeric(window(R,    start=index(R)[length(R)-F_DAYS+1]))

  TR        <- merge(R,lag(R,-1))
  PREDICT   <- data.frame(past=f["naive"][[1]])
  names(TR) <- c("real","past")

  LM     <- lm(real~past,data=TR)
  ARIMA  <- auto.arima(R)
  ES     <- ses(R,h=F_DAYS)
  NN     <- nnetar(R)
  SVM    <- tune(svm,real~past,data=TR,ranges=list(elsilon=seq(0,1,0.2), cost=seq(1,100,10)))

  f["lr"]    <- try(as.numeric(forecast(LM,PREDICT)$mean),TRUE)
  f["arima"] <- try(as.numeric(forecast(ARIMA,h=F_DAYS)$mean),TRUE)
  f["ses"]   <- try(as.numeric(forecast(ES,h=F_DAYS)$mean),TRUE)
  f["ann"]   <- try(as.numeric(forecast(NN,h=F_DAYS)$mean),TRUE)
  f["svm"]   <- try(as.numeric(predict(SVM$best.model,PREDICT)),TRUE)
  f["ens"]   <- rowMedians(as.matrix(f),na.rm=T)

  write.csv(f, file=paste("mtlf/forecast/forecast-",N,".csv",sep=""),row.names=F)

  aux  <- TT != 0
  for (j in MODELS)
  {
    MAPE[1,j] <- 100*median(ifelse(sum(aux)!=0,abs(TT[aux]-f[aux,j])/TT[aux],NA))
    RMSE[1,j] <- sqrt(median((TT-f[,j])^2))
  }

  write.csv(MAPE,file=paste("mtlf/mape/mape-test-",KPI,"-",N,".csv",sep=""))
  write.csv(RMSE,file=paste("mtlf/rmse/rmse-test-",KPI,"-",N,".csv",sep=""))
}

TEST        <- TRUE
SAMPLE      <- 500     ### number of elements to assess per each type
COMPLETE    <- 0.10    ### amount of data imputed allowed in the dataset
TRAIN_LIMIT <- 0.75    ### length of the training period
F_DAYS      <- 7*4*3   ### number of days to forecast for MTLF

ALL    <- list.files(path="post_cooked/",pattern="*.csv")
OTHERS <- list.files(path="post_cooked/",pattern="-")

CUPS   <- setdiff(ALL,OTHERS)
CT     <- list.files(path="post_cooked/",pattern="*-CT.csv")
LINE   <- list.files(path="post_cooked/",pattern="*-LINE.csv")

FILES  <- union(sample(CT,SAMPLE),sample(LINE,SAMPLE))
FILES  <- union(sample(CUPS,SAMPLE),FILES)

MODELS <- c("mean","rw","naive","lr","ann","svm","arima","ses","ens")
KPI    <- c("min","max","sum")

dir.create("mtlf/forecast",showWarnings = F, recursive = T)
dir.create("mtlf/mape",    showWarnings = F, recursive = T)
dir.create("mtlf/rmse",    showWarnings = F, recursive = T)

B <- foreach(NAME = FILES,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  a <- fread(paste("post_cooked/",NAME,sep=""))
  r <- zoo(a$kWh, order.by = a$time)
  d <- merge(apply.daily(r,min),apply.daily(r,max),apply.daily(r,sum))
  colnames(d) <- KPI

  if (TEST) ####### ÑAPA hasta tener los datos finales acorto una semana la serie temporal
  {
    a  <- a[1:(nrow(a)-F_DAYS*24)]
    rr <- window(d,start=index(d)[nrow(d)-F_DAYS+1])
    d  <- window(d,end=index(d)[nrow(d)-F_DAYS])
  } else {
    #real <- fread()
    rr <- window(d,start=index(d)[nrow(d)-F_DAYS+1])
  }

  TRAIN_DAYS <- floor(TRAIN_LIMIT*nrow(d))    #### training days
  LENGTH     <- nrow(a)
  ZEROS      <- sum(a$kWh==0)/LENGTH
  IMPUTED    <- sum(a$issue)/LENGTH

  ### If we have enough non zero, non imputed values in the dataset,
  ### then we continue with the assessment
  if((IMPUTED < COMPLETE) | (ZEROS < COMPLETE))
  {
    FORECAST(d$min,rr$min,"min",NAME)
    FORECAST(d$max,rr$max,"max",NAME)
    FORECAST(d$sum,rr$sum,"sum",NAME)
  } ### if that test if the time series has data
}   ### foreach


ALL    <- list.files(path="mtlf/mape/",pattern="*.csv")
CT     <- list.files(path="mtlf/mape/",pattern="*-CT.csv")
LINE   <- list.files(path="mtlf/mape/",pattern="*-LINE.csv")
CUPS   <- setdiff(ALL,union(CT,LINE))

RCT <- foreach(NAME = CT,.combine=rbind) %dofuture% {
  fread(paste("mtlf/mape/",NAME,sep=""))
}

RLI <- foreach(NAME = LINE,.combine=rbind) %dofuture% {
  fread(paste("mtlf/mape/",NAME,sep=""))
}

RCU <- foreach(NAME = CUPS,.combine=rbind) %dofuture% {
  fread(paste("mtlf/mape/",NAME,sep=""))
}

EVAL <- function(R,TYPE)
{
  RR <- na.omit(as.matrix(R[,..MODELS]))
  rownames(RR) <- 1:nrow(RR)

  write.csv(as.data.frame(apply(RR, 2, summary)),
            file=paste("mtlf-",TYPE,"-mape.csv",sep=""))
  BOOT <- boot(data=RR,statistic=function(data,i) colMedians(data[i,],na.rm=T),R=100)

  pdf(file=paste("mtlf-",TYPE,"-mape.pdf",sep=""))
    print(friedman.test(RR))
    multiple <- frdAllPairsNemenyiTest(RR)
    print(multcompLetters(fullPTable(multiple$p.value)))
    plot(multiple)
    boxplot(BOOT$t,names=MODELS,ylab="Confidence Interval over the Median")
  dev.off()
}

EVAL(RCT,"CT")
EVAL(RLI,"LINE")
EVAL(RCU,"CUPS")
