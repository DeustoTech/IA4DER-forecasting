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

SAMPLE      <- 200     ### number of elements to assess per each type
COMPLETE    <- 0.10    ### amount of data imputed allowed in the dataset
TRAIN_LIMIT <- 0.75    ### length of the training period
F_DAYS      <- 7*4*3   ### number of days to forecast for MTLF
MC          <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error
MCNAMES     <- sapply(MC,function(q) { paste(100*q,"%",sep="")})
MCTARGET    <- MCNAMES[1]

MODELS      <- c("mean","rw","naive","lr","ann","svm","arima","ses","ens")
TYPES       <- c("CUPS","CGP","LBT","CUA","TR","CT")
KPIS        <- c("min","max","sum")

######### Take the time serie already computed only by the SUM KPI (assume the others are already complete also)
FCUPS <- FCGP  <- FLBT  <- FCUA  <- FTR  <- FCT <- character()
if (length(Sys.glob(paths="mtlf/forecast/CUPS/sum/*")) != 0)
{
  FCUPS <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="mtlf/forecast/CUPS/sum/*"),"/")),nrow=5)[5,]),warning=function(w){},error=function(e){},finally={})
  FCGP  <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="mtlf/forecast/CGP/sum/*") ,"/")),nrow=5)[5,]),warning=function(w){},error=function(e){},finally={})
  FLBT  <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="mtlf/forecast/LBT/sum/*") ,"/")),nrow=5)[5,]),warning=function(w){},error=function(e){},finally={})
  FCUA  <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="mtlf/forecast/CUA/sum/*") ,"/")),nrow=5)[5,]),warning=function(w){},error=function(e){},finally={})
  FTR   <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="mtlf/forecast/TR/sum/*")  ,"/")),nrow=5)[5,]),warning=function(w){},error=function(e){},finally={})
  FCT   <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="mtlf/forecast/CT/sum/*")  ,"/")),nrow=5)[5,]),warning=function(w){},error=function(e){},finally={})
}

ECUPS <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CUPS/*"),"/")),nrow=3)[3,])
ECGP  <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CGP/*") ,"/")),nrow=3)[3,])
ELBT  <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/LBT/*") ,"/")),nrow=3)[3,])
ECUA  <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CUA/*") ,"/")),nrow=3)[3,])
ETR   <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/TR/*")  ,"/")),nrow=3)[3,])
ECT   <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CT/*")  ,"/")),nrow=3)[3,])

CUPS   <- paste0("post_cooked/CUPS/",setdiff(ECUPS,FCUPS),".csv",sep="")
CGP    <- paste0("post_cooked/CGP/", setdiff(ECGP,FCGP),  ".csv",sep="")
LBT    <- paste0("post_cooked/LBT/", setdiff(ELBT,FLBT),  ".csv",sep="")
CUA    <- paste0("post_cooked/CUA/", setdiff(ECUA,FCUA),  ".csv",sep="")
TR     <- paste0("post_cooked/TR/",  setdiff(ETR ,FTR ),  ".csv",sep="")
CT     <- paste0("post_cooked/CT/",  setdiff(ECT ,FCT ),  ".csv",sep="")

ALL  <- union(sample(CUPS,SAMPLE),sample(CGP,SAMPLE))
ALL  <- union(ALL,sample(LBT,SAMPLE))

if (SAMPLE < length(CUA)){ ALL  <- union(ALL,sample(CUA,SAMPLE))
                          } else                   { ALL  <- union(ALL,CUA) }

if (SAMPLE < length(TR)) { ALL  <- union(ALL,sample(TR,SAMPLE))
                          } else                   { ALL  <- union(ALL,TR) }

if (SAMPLE < length(CT)) { ALL  <- union(ALL,sample(CT,SAMPLE))
                          } else                   { ALL  <- union(ALL,CT) }

LIM  <- fread("features.csv")

# ALL  <- Sys.glob(paths="post_cooked/*/*")

for (TY in TYPES)
  for (KP in KPIS)
  {
    dir.create(paste("mtlf/forecast/",TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/mape/",    TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/rmse/",    TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/mca/",     TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/mcb/",     TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/riska/",   TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/riskb/",   TY,KP,sep="/"),showWarnings = F, recursive = T)
    dir.create(paste("mtlf/mase/",    TY,KP,sep="/"),showWarnings = F, recursive = T)
  }

## R   train data (real)
## TT  test data
## KPI kpi to calculate (character)
## FILE file name (not path) where to output (character)
## TYPE type of the time serie (CT,TR,CUPS,etc.)
## POT_NOM "nominal power" of the device that the time series measure
## POT_EST "estimated nominal power" of the device that the time series measure
FORECAST <- function(R,TT,KPI,FILE,TYPE,POT_NOM,POT_EST)
{
  f     <- data.frame(matrix(ncol = length(MODELS), nrow = F_DAYS))
  MAPE  <- RMSE  <- MASE <- data.frame(matrix(ncol = length(MODELS), nrow = 1))
  RISKA <- RISKB <- MCA  <- MCB  <- data.frame(matrix(ncol = length(MODELS), nrow = length(MC)))
    
  colnames(MAPE)  <- colnames(RMSE)  <- colnames(MASE) <- colnames(f)   <- MODELS
  colnames(RISKA) <- colnames(RISKB) <- colnames(MCA)  <- colnames(MCB) <- MODELS
  rownames(RISKA) <- rownames(RISKB) <- rownames(MCA)  <- rownames(MCB) <- MCNAMES
  
  f["real"]  <- TT
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
  f["ens"]             <- rowMedians(as.matrix(f),na.rm=T)

  write.csv(f, file=paste("mtlf/forecast",TYPE,KPI,FILE,sep="/"), row.names=F)

  ##QQR  <- ecdf(TT)
  aux     <- TT != 0
  auxmase <- median(abs(TT-f[,"naive"]))
  for (j in MODELS)
  {
    MAPE[1,j] <- 100*median(abs(TT[aux]-f[aux,j])/TT[aux],na.rm=T)
    RMSE[1,j] <- sqrt(median((TT-f[,j])^2,na.rm=T))
    MASE[1,j] <- median(abs(TT-f[,j]))/auxmase

    RISKA[,j] <- (max(TT) < POT_NOM*MC) == (max(f[,j]) < POT_NOM*MC)
    RISKB[,j] <- (max(TT) < POT_EST*MC) == (max(f[,j]) < POT_EST*MC)

    QQF       <- ecdf(f[,j])
    MCA[,j]   <- QQF(MC*POT_NOM) ## QQR(MC*POT_NOM)-QQF(MC*POT_NOM)
    MCB[,j]   <- QQF(MC*POT_EST) ## QQR(MC*POT_EST)-QQF(MC*POT_EST)
  }

  write.csv(MAPE, file=paste("mtlf/mape", TYPE,KPI,FILE,sep="/"))
  write.csv(RMSE, file=paste("mtlf/rmse", TYPE,KPI,FILE,sep="/"))
  write.csv(MCA,  file=paste("mtlf/mca",  TYPE,KPI,FILE,sep="/"))
  write.csv(MCB,  file=paste("mtlf/mcb",  TYPE,KPI,FILE,sep="/"))
  write.csv(RISKA,file=paste("mtlf/riska",TYPE,KPI,FILE,sep="/"))
  write.csv(RISKB,file=paste("mtlf/riskb",TYPE,KPI,FILE,sep="/"))
  write.csv(MASE, file=paste("mtlf/mase", TYPE,KPI,FILE,sep="/"))
}

B <- foreach(NAME = ALL,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  a <- fread(NAME)
  r <- zoo(a$kWh, order.by = a$time)
  d <- merge(xts::apply.daily(r,min),xts::apply.daily(r,max),xts::apply.daily(r,sum))
  colnames(d) <- KPIS

  FILE <- strsplit(NAME,"/")[[1]][3]
  TYPE <- strsplit(NAME,"/")[[1]][2]
  ID      <- tools::file_path_sans_ext(FILE)
  
  POT_NOM <- LIM$POT_NOM[LIM$ID == ID]
  POT_EST <- LIM$POT_EST[LIM$ID == ID]

  ### Cojo los datos reales para CT y LBT y acorto la serie en otro caso
  if (TYPE %in% c("CT","LBT"))
  {
    rr <- fread(paste0("mtlf/test/",TYPE,"/",ID,".csv",sep=""))
    rs <- zoo(rr$SUM_VAL_AI/1000,order.by=rr$DIA_LECTURA)
    rM <- zoo(rr$MAX_VAL_AI/1000,order.by=rr$DIA_LECTURA)
    rm <- zoo(rr$MIN_VAL_AI/1000,order.by=rr$DIA_LECTURA)
    rr <- merge(rm,rM,rs)
    names(rr) <- KPIS
  } else {
    a  <- a[1:(nrow(a)-F_DAYS*24)]
    rr <- window(d,start=index(d)[nrow(d)-F_DAYS+1])
    d  <- window(d,end=index(d)[nrow(d)-F_DAYS])
  }

  TRAIN_DAYS <- floor(TRAIN_LIMIT*nrow(d))    #### training days
  LENGTH     <- nrow(a)
  ZEROS      <- sum(a$kWh==0)/LENGTH
  IMPUTED    <- sum(a$issue)/LENGTH

  ### If we have enough non zero, non imputed values in the dataset,
  ### then we continue with the assessment
  if((IMPUTED < COMPLETE) & (ZEROS < COMPLETE))
  {
    FORECAST(d$min,rr$min,"min",FILE,TYPE,POT_NOM,POT_EST)
    FORECAST(d$max,rr$max,"max",FILE,TYPE,POT_NOM,POT_EST)
    FORECAST(d$sum,rr$sum,"sum",FILE,TYPE,POT_NOM,POT_EST)
  } ### if that test if the time series has data
}   ### foreach

EVAL <- function(ERROR,TYPE,KPI)
{
  R <- foreach(NAME = Sys.glob(paste("mtlf",ERROR,TYPE,KPI,"*.csv",sep="/")),.combine=rbind) %dofuture% {
    if (!grepl("summary",NAME,fixed = TRUE)) fread(NAME)
  }

  if (!grepl("risk",ERROR))
  {
    RR <- na.omit(as.matrix(R[,..MODELS]))
    rownames(RR) <- 1:nrow(RR)

    write.csv(as.data.frame(apply(RR, 2, summary)),
              file=paste("mtlf",ERROR,TYPE,KPI,"summary.csv",sep="/"))

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
  } else {
    RR <- as.matrix(as.data.frame(na.omit(as.matrix(R[R$V1==MCTARGET,..MODELS]))))
    rownames(RR) <- 1:nrow(RR)

    write.csv(t(apply(RR, 2, function(x) {sum(x,na.rm=T)/length(x) } )),
              file=paste("mtlf",ERROR,TYPE,KPI,"summary.csv",sep="/"),row.names=F)
  }
}

for (TY in TYPES)
  for (KP in KPIS)
  {
    EVAL("mape", TY,KP)
    EVAL("rmse", TY,KP)
#     EVAL("mca",  TY,KP)
#     EVAL("mcb",  TY,KP)
#     EVAL("riska",TY,KP)
#     EVAL("riskb",TY,KP)
    EVAL("mase", TY,KP)
  }

R <- foreach(NAME = Sys.glob("mtlf/*/*/*/summary.csv"),.combine=rbind) %dofuture% {
  ERROR <- strsplit(NAME,"/")[[1]][2]
  TY    <- strsplit(NAME,"/")[[1]][3]
  KP    <- strsplit(NAME,"/")[[1]][4]

  if (!grepl("risk",ERROR)) { return(data.frame(ERROR,TY,KP,fread(NAME)[3,-1]))  }
  else  {                     return(data.frame(ERROR,TY,KP,fread(NAME)))  }
}

write.csv(R,file="mtlf/summary.csv",row.names=F)
