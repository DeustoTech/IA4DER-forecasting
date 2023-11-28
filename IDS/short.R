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
library(stringr)
library(arrow)

plan(multisession)

TEST        <- T     ### si estoy haciendo test
SAMPLE      <- 200   ### number of elements to assess per each type
COMPLETE    <- 0.10  ### amount of data imputed allowed in the dataset
TRAIN_LIMIT <- 0.75  ### length of the training period
F_DAYS      <- 7     ### number of days to forecast for STLF
T_DAYS      <- 21    ### number of days to use in the training widnow for AI methods for STLF
MC          <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error
MCNAMES     <- sapply(MC,function(q) { paste(100*q,"%",sep="")})
MCTARGET    <- MCNAMES[1]

MODELS      <- c("mean","rw","naive","simple","lr","ann","svm","arima","ses","ens")
TYPES       <- c("CUPS","CGP","LBT","CT","TR")

CUPS <- Sys.glob(paths="post_cooked/CUPS/*")
CGP  <- Sys.glob(paths="post_cooked/CGP/*")
LBT  <- Sys.glob(paths="post_cooked/LBT/*")
CT   <- Sys.glob(paths="post_cooked/CT/*")
TR   <- Sys.glob(paths="post_cooked/TR/*")

ALL  <- union(sample(CUPS,SAMPLE),sample(CGP,SAMPLE))
ALL  <- union(ALL,sample(LBT,SAMPLE))

if (SAMPLE < length(CT)) { ALL  <- union(ALL,sample(CT,SAMPLE))
} else                   { ALL  <- union(ALL,CT) }

if (SAMPLE < length(TR)) { ALL  <- union(ALL,sample(TR,SAMPLE))
} else                   { ALL  <- union(ALL,TR) }

LIM  <- fread("features.csv")

# ALL  <- Sys.glob(paths="post_cooked/*/*")
ALL <- Sys.glob(paths="post_cooked/LBT/*")

for (TY in TYPES)
{
  dir.create(paste("stlf/forecast/",TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/mape/",    TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/rmse/",    TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/time/",    TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/mca/",     TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/mcb/",     TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/riska/",   TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/riskb/",   TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/mase/",    TY,sep="/"),showWarnings = F, recursive = T)
}

B <- foreach(NAME = ALL,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  a <- fread(NAME)
  r <- zoo(a$kWh, order.by = a$time)

  FILE <- strsplit(NAME,"/")[[1]][3]
  TYPE <- strsplit(NAME,"/")[[1]][2]
  ID   <- tools::file_path_sans_ext(FILE)
  
  POT_NOM <- LIM$POT_NOM[LIM$ID == ID]
  POT_EST <- LIM$POT_EST[LIM$ID == ID]
  
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

  f     <- data.frame(matrix(ncol = length(MODELS), nrow = 24*F_DAYS))
  MASE  <- MAPE  <- RMSE  <- TIME    <- data.frame(matrix(ncol = length(MODELS), nrow = 1))
  RISKA <- RISKB  <- MCA  <- MCB     <- data.frame(matrix(ncol = length(MODELS), nrow = length(MC)))
  colnames(MASE)  <- colnames(MAPE)  <- colnames(RMSE) <- colnames(TIME) <- colnames(f) <- MODELS
  colnames(RISKA) <- colnames(RISKB) <- colnames(MCA)  <- colnames(MCB)  <- MODELS
  rownames(RISKA) <- rownames(RISKB) <- rownames(MCA)  <- rownames(MCB)  <- MCNAMES
  
  ### If we have enough non zero, non imputed values in the dataset,
  ### then we continue with the assessment
  if( (IMPUTED < COMPLETE) & (ZEROS < COMPLETE))
  {
    f["mean"]  <- as.numeric(rep(mean(r),24*F_DAYS))
    f["rw"]    <- rep(as.numeric(window(r,start=index(r)[length(r)-23])),F_DAYS)
    f["naive"] <- as.numeric(window(r,   start=index(r)[length(r)-24*6-23], end=index(r)[length(r)-24*( 6-F_DAYS-1)]))
    f["simple"]<- rowMeans(data.frame(
                  a=(as.numeric(window(r,start=index(r)[length(r)-24*6 -23],end=index(r)[length(r)-24*( 6-F_DAYS-1)]))),
                  b=(as.numeric(window(r,start=index(r)[length(r)-24*13-23],end=index(r)[length(r)-24*(15-F_DAYS-1)]))),
                  c=(as.numeric(window(r,start=index(r)[length(r)-24*20-23],end=index(r)[length(r)-24*(22-F_DAYS-1)])))),
                  na.rm=T)

    rr              <- merge(r,lag(r,-24))
    TRAINSET        <- window(rr,start=index(r)[length(r)-24*T_DAYS-23])
    PREDICT         <- data.frame(past=as.numeric(window(r,start=index(r)[length(r)-24*F_DAYS+1])))
    names(TRAINSET) <- c("real","past")

    tryCatch({LM     <- lm(real~past,data=TRAINSET)},warning=function(w) {},error=function(e) {print(e)},finally = {})
    tryCatch({ARIMA  <- auto.arima(r)},              warning=function(w) {},error=function(e) {print(e)},finally = {})
    tryCatch({ES     <- ses(r,h=24*F_DAYS)},         warning=function(w) {},error=function(e) {print(e)},finally = {})
    tryCatch({NN     <- nnetar(r)},                  warning=function(w) {},error=function(e) {print(e)},finally = {})
    tryCatch({SVM    <- tune(svm,real~past,data=TRAINSET,ranges=list(elsilon=seq(0,1,0.2), cost=seq(1,100,10)))},warning=function(w) {},error=function(e) {print(e)},finally = {})

    tryCatch({f["lr"]    <- as.numeric(forecast(LM,PREDICT)$mean)},       warning=function(w) {},error= function(e) {print(e)},finally = {})
    tryCatch({f["arima"] <- as.numeric(forecast(ARIMA,h=24*F_DAYS)$mean)},warning=function(w) {},error= function(e) {print(e)},finally = {})
    tryCatch({f["ses"]   <- as.numeric(forecast(ES,h=24*F_DAYS)$mean)},   warning=function(w) {},error= function(e) {print(e)},finally = {})
    tryCatch({f["ann"]   <- as.numeric(forecast(NN,h=24*F_DAYS)$mean)},   warning=function(w) {},error= function(e) {print(e)},finally = {})
    tryCatch({f["svm"]   <- as.numeric(predict(SVM$best.model,PREDICT))}, warning=function(w) {},error= function(e) {print(e)},finally = {})
    f["ens"]   <- rowMedians(as.matrix(f),na.rm=T)
  
    write.csv(f, file=paste("stlf/forecast",TYPE,FILE,sep="/"), row.names=F)

    ## QQR  <- ecdf(real)
    aux     <- real != 0
    auxmase <- median(abs(real-f[,"naive"]))
    
    auxtime <- numeric(F_DAYS)
    for (D in 1:F_DAYS)
      auxtime[D] <- which.max(real[(24*D-23):(24*D)]) -1
    
    for(j in MODELS)
    {
      MAPE[1,j] <- 100*median(ifelse(sum(aux)!=0,abs(real[aux]-f[aux,j])/real[aux],NA),na.rm=T)
      RMSE[1,j] <- sqrt(median((real-f[,j])^2,na.rm=T))
      MASE[1,j] <- median(abs(real-f[,j]))/auxmase
    
      aux_f     <- numeric(F_DAYS)
      for (D in 1:F_DAYS)
        aux_f[D]<- ifelse(!anyNA(f[(24*D-23):(24*D),j]),which.max(f[(24*D-23):(24*D),j])-1,NA)
      
      TIME[1,j] <- median(abs(auxtime-aux_f),na.rm=T)
  
      RISKA[,j] <- (max(real) < POT_NOM*MC) == (max(f[,j]) < POT_NOM*MC)
      RISKB[,j] <- (max(real) < POT_EST*MC) == (max(f[,j]) < POT_EST*MC)
      
      QQF       <- ecdf(f[,j])
      MCA[,j]   <- QQF(MC*POT_NOM) ## QQR(MC*POT_NOM)-QQF(MC*POT_NOM)
      MCB[,j]   <- QQF(MC*POT_EST) ## QQR(MC*POT_EST)-QQF(MC*POT_EST)
    }
    
    write.csv(MAPE, file=paste("stlf/mape", TYPE,FILE,sep="/"))
    write.csv(RMSE, file=paste("stlf/rmse", TYPE,FILE,sep="/"))
    write.csv(TIME, file=paste("stlf/time", TYPE,FILE,sep="/"))
    write.csv(MCA,  file=paste("stlf/mca",  TYPE,FILE,sep="/"))
    write.csv(MCB,  file=paste("stlf/mcb",  TYPE,FILE,sep="/"))
    write.csv(RISKA,file=paste("stlf/riska",TYPE,FILE,sep="/"))
    write.csv(RISKB,file=paste("stlf/riskb",TYPE,FILE,sep="/"))
    write.csv(MASE, file=paste("stlf/mase", TYPE,FILE,sep="/"))
  } ### if that test if the time series has data
}   ### foreach

EVAL <- function(ERROR,TYPE)
{
  R <- foreach(NAME = Sys.glob(paste("stlf",ERROR,TYPE,"*.csv",sep="/")),.combine=rbind) %dofuture% {
    if (!grepl("summary",NAME,fixed = TRUE)) fread(NAME)
  }
  
  if (!grepl("risk",ERROR))
  {
    RR <- as.matrix(as.data.frame(na.omit(as.matrix(R[,..MODELS]))))
    rownames(RR) <- 1:nrow(RR)

    write.csv(as.data.frame(apply(RR, 2, summary)),
              file=paste("stlf",ERROR,TYPE,"summary.csv",sep="/"))

    BOOT <- boot(data=as.matrix(RR),statistic=function(data,i) colMedians(data[i,],na.rm=T),R=100)

    sink(paste("stlf/",ERROR,TYPE,"p-values.txt",sep="/"))
      print(BOOT)
      print(friedman.test(RR))
      multiple <- frdAllPairsNemenyiTest(RR)
      print(multcompLetters(fullPTable(multiple$p.value)))
    sink()

    pdf(file=paste("stlf/",ERROR,TYPE,"summary.pdf",sep="/"))
      plot(multiple)
      boxplot(BOOT$t,names=MODELS,ylab="Confidence Interval over the Median")
    dev.off()
  } else {
    RR <- as.matrix(as.data.frame(na.omit(as.matrix(R[R$V1==MCTARGET,..MODELS]))))
    rownames(RR) <- 1:nrow(RR)

    write.csv(t(apply(RR, 2, function(x) {sum(x,na.rm=T)/length(x) } )),
              file=paste("stlf",ERROR,TYPE,"summary.csv",sep="/"),row.names=F)
  }
}

for (TY in TYPES)
{
  EVAL("mape", TY)
  EVAL("rmse", TY)
  EVAL("time", TY)
  EVAL("mca",  TY)
  EVAL("mcb",  TY)
  EVAL("riska",TY)
  EVAL("riskb",TY)
  EVAL("mase", TY)
}

R <- foreach(NAME = Sys.glob("stlf/*/*/summary.csv"),.combine=rbind) %dofuture% {
  ERROR <- strsplit(NAME,"/")[[1]][2]
  TY    <- strsplit(NAME,"/")[[1]][3]
  
  if (!grepl("risk",ERROR)) { return(data.frame(ERROR,TY,fread(NAME)[3,-1]))  }
  else  {                     return(data.frame(ERROR,TY,fread(NAME)))  }
}

write.csv(R,file="stlf/summary.csv",row.names=F)


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
