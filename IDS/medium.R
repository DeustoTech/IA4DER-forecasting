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
library(stringr)

plan(multisession)

TEST        <- T
SAMPLE      <- 200     ### number of elements to assess per each type
COMPLETE    <- 0.10    ### amount of data imputed allowed in the dataset
TRAIN_LIMIT <- 0.75    ### length of the training period
F_DAYS      <- 7*4*3   ### number of days to forecast for MTLF

MODELS      <- c("mean","rw","naive","lr","ann","svm","arima","ses","ens")
TYPES       <- c("CUPS","LINE","CT")
KPIS        <- c("min","max","sum")

CUPS <- Sys.glob(paths="post_cooked/CUPS/*")
LINE <- Sys.glob(paths="post_cooked/LINE/*")
CT   <- Sys.glob(paths="post_cooked/CT/*")
ALL  <- union(sample(CUPS,SAMPLE),sample(LINE,SAMPLE))
ALL  <- union(ALL,sample(CT,SAMPLE))

# ALL  <- Sys.glob(paths="post_cooked/*/*")

for (TY in TYPES)
  for (KP in KPIS)
  {
    dir.create(paste("mtlf/forecast/",TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/mape/",    TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/rmse/",    TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/monc/",    TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/monp/",    TY,KP,sep="/"),showWarnings = F, recursive = T)
  }

FORECAST <- function(R,TT,KPI,FILE,TYPE)
{
  f     <- data.frame(matrix(ncol = length(MODELS), nrow = F_DAYS))
  MAPE  <- RMSE  <- MONC <- MONP <- data.frame(matrix(ncol = length(MODELS), nrow = 1))
  colnames(MAPE) <- colnames(RMSE) <- colnames(MONC) <- colnames(MONP) <- colnames(f) <- MODELS

  f["mean"]  <- as.numeric(rep(mean(R),F_DAYS))
  f["rw"]    <- rep(as.numeric(window(R,start=index(R)[length(R)])),F_DAYS)
  f["naive"] <- as.numeric(window(R,    start=index(R)[length(R)-F_DAYS+1]))

  TR        <- merge(R,lag(R,-1))
  PREDICT   <- data.frame(past=f["naive"][[1]])
  names(TR) <- c("real","past")

  tryCatch({LM     <- lm(real~past,data=TR)},warning=function(w){},error=function(e) {print(e)},finally = {})
  tryCatch({ARIMA  <- auto.arima(R)},        warning=function(w){},error=function(e) {print(e)},finally = {})
  tryCatch({ES     <- ses(R,h=F_DAYS)},      warning=function(w){},error=function(e) {print(e)},finally = {})
  tryCatch({NN     <- nnetar(R)},            warning=function(w){},error=function(e) {print(e)},finally = {})
  tryCatch({SVM    <- tune(svm,real~past,data=TR,ranges=list(elsilon=seq(0,1,0.2), cost=seq(1,100,10)))}, warning=function(w) {},error=function(e) {print(e)},finally = {})

  tryCatch({f["lr"]    <- as.numeric(forecast(LM,PREDICT)$mean)},      warning=function(w) {},error = function(e) {print(e)},finally = {})
  tryCatch({f["arima"] <- as.numeric(forecast(ARIMA,h=F_DAYS)$mean)},  warning=function(w) {},error = function(e) {print(e)},finally = {})
  tryCatch({f["ses"]   <- as.numeric(forecast(ES,h=F_DAYS)$mean)},     warning=function(w) {},error = function(e) {print(e)},finally = {})
  tryCatch({f["ann"]   <- as.numeric(forecast(NN,h=F_DAYS)$mean)},     warning=function(w) {},error = function(e) {print(e)},finally = {})
  tryCatch({f["svm"]   <- as.numeric(predict(SVM$best.model,PREDICT))},warning=function(w) {},error = function(e) {print(e)},finally = {})
  f["ens"]   <- rowMedians(as.matrix(f),na.rm=T)

  write.csv(f, file=paste("mtlf/forecast",TYPE,KPI,FILE,sep="/"), row.names=F)

  MC   <- quantile(TT,seq(0.6,1,0.01),na.rm=T)
  aux  <- TT != 0
  aux2 <- MC != 0
  for (j in MODELS)
  {
    MAPE[1,j] <- 100*median(ifelse(sum(aux)!=0,abs(TT[aux]-f[aux,j])/TT[aux],NA),na.rm=T)
    RMSE[1,j] <- sqrt(median((TT-f[,j])^2,na.rm=T))
    MONC[1,j] <- dist(rbind(MC,quantile(f[,j],seq(0.6,1,0.01),na.rm=T)))[1]
    MONP[1,j] <- 100*median(ifelse(sum(aux2)!=0,abs(MC[aux2]-quantile(f[,j],seq(0.6,1,0.01),na.rm=T)[aux2])/MC[aux2],NA),na.rm=T)
  }

  write.csv(MAPE,file=paste("mtlf/mape",TYPE,KPI,FILE,sep="/"))
  write.csv(RMSE,file=paste("mtlf/rmse",TYPE,KPI,FILE,sep="/"))
  write.csv(MONC,file=paste("mtlf/monc",TYPE,KPI,FILE,sep="/"))
  write.csv(MONP,file=paste("mtlf/monp",TYPE,KPI,FILE,sep="/"))
}

B <- foreach(NAME = ALL,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  a <- fread(NAME)
  r <- zoo(a$kWh, order.by = a$time)
  d <- merge(apply.daily(r,min),apply.daily(r,max),apply.daily(r,sum))
  colnames(d) <- KPIS

  FILE <- strsplit(NAME,"/")[[1]][3]
  TYPE <- strsplit(NAME,"/")[[1]][2]

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
    FORECAST(d$min,rr$min,"min",FILE,TYPE)
    FORECAST(d$max,rr$max,"max",FILE,TYPE)
    FORECAST(d$sum,rr$sum,"sum",FILE,TYPE)
  } ### if that test if the time series has data
}   ### foreach

EVAL <- function(ERROR,TYPE,KPI)
{
  R <- foreach(NAME = Sys.glob(paste("mtlf",ERROR,TYPE,KPI,"*.csv",sep="/")),.combine=rbind) %dofuture% {
    if (!grepl("summary",NAME,fixed = TRUE)) fread(NAME)
  }

  RR <- na.omit(as.matrix(R[,..MODELS]))
  rownames(RR) <- 1:nrow(RR)

  write.csv(as.data.frame(apply(RR, 2, summary)),
            file=paste("mtlf/",ERROR,TYPE,KPI,"summary.csv",sep="/"))
  BOOT <- boot(data=RR,statistic=function(data,i) colMedians(data[i,],na.rm=T),R=100)

  sink(paste("mtlf/",ERROR,TYPE,KPI,"p-values.txt",sep="/"))
    print(BOOT)
    print(friedman.test(RR))
    multiple <- frdAllPairsNemenyiTest(RR)
    print(multcompLetters(fullPTable(multiple$p.value)))
  sink()

  pdf(file=paste("mtlf/",ERROR,TYPE,KPI,"summary.pdf",sep="/"))
    plot(multiple)
    boxplot(BOOT$t,names=MODELS,ylab="Confidence Interval over the Median")
  dev.off()
}

for (TY in TYPES)
  for (KP in KPIS)
  {
    EVAL("mape",TY,KP)
    EVAL("rmse",TY,KP)
    EVAL("monc",TY,KP)
    EVAL("monp",TY,KP)
  }

R <- foreach(NAME = Sys.glob("mtlf/*/*/*/summary.csv"),.combine=rbind) %dofuture% {
  aux   <- fread(NAME)
  ERROR <- strsplit(NAME,"/")[[1]][2]
  TY    <- strsplit(NAME,"/")[[1]][3]
  KP    <- strsplit(NAME,"/")[[1]][4]
  
  return(data.frame(ERROR,TY,KP,aux[3,-1]))
}

write.csv(R,file="mtlf/summary.csv",row.names=F)
