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

SAMPLE       <- 400   ### number of elements to assess per each type
FIXED_TEST   <- F     ### control if we wanted to test only on a fixed date or in random dates
COMPLETE     <- 0.10  ### amount of data imputed allowed in the dataset
TRAIN_LIMIT  <- 0.75  ### length of the training period
F_DAYS       <- 7     ### number of days to forecast for STLF
T_DAYS       <- 21    ### number of days to use in the training widnow for AI methods for STLF
MIN_TRAINING <- (T_DAYS+F_DAYS*2)*24
MC           <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error
MCNAMES      <- sapply(MC,function(q) { paste(100*q,"%",sep="")})
MCTARGET     <- MCNAMES[1]
MODELS       <- c("mean","rw","naive","simple","lr","ann","svm","arima","ses","ens")
TYPES        <- c("CUPS","CGP","LBT","CUA","TR","CT","SOLAR")

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

LIM  <- fread("features.csv",select=c("ID","POT_NOM","POT_EST"))
DONE <- Sys.glob(paths="stlf/forecast/*/*")
ALL  <- Sys.glob(paths="post_cooked/*/*")
ALL  <- sample(ALL)

# {
#   FCUPS <- FCGP  <- FLBT  <- FCUA  <- FTR  <- FCT <- FSOL <- character()
#   if (length(Sys.glob(paths="stlf/forecast/CUPS/*")) != 0)
#   {
#     FCUPS <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="stlf/forecast/CUPS/*") ,"/")),nrow=4)[4,]),warning=function(w){},error=function(e){},finally={})
#     FCGP  <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="stlf/forecast/CGP/*")  ,"/")),nrow=4)[4,]),warning=function(w){},error=function(e){},finally={})
#     FLBT  <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="stlf/forecast/LBT/*")  ,"/")),nrow=4)[4,]),warning=function(w){},error=function(e){},finally={})
#     FCUA  <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="stlf/forecast/CUA/*")  ,"/")),nrow=4)[4,]),warning=function(w){},error=function(e){},finally={})
#     FTR   <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="stlf/forecast/TR/*")   ,"/")),nrow=4)[4,]),warning=function(w){},error=function(e){},finally={})
#     FCT   <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="stlf/forecast/CT/*")   ,"/")),nrow=4)[4,]),warning=function(w){},error=function(e){},finally={})
#     FSOL  <- tryCatch(tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="stlf/forecast/SOLAR/*"),"/")),nrow=4)[4,]),warning=function(w){},error=function(e){},finally={})
#   }
# 
#   ECUPS <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CUPS/*") ,"/")),nrow=3)[3,])
#   ECGP  <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CGP/*")  ,"/")),nrow=3)[3,])
#   ELBT  <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/LBT/*")  ,"/")),nrow=3)[3,])
#   ECUA  <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CUA/*")  ,"/")),nrow=3)[3,])
#   ETR   <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/TR/*")   ,"/")),nrow=3)[3,])
#   ECT   <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/CT/*")   ,"/")),nrow=3)[3,])
#   ESOL  <- tools::file_path_sans_ext(matrix(unlist(strsplit(Sys.glob(paths="post_cooked/SOLAR/*"),"/")),nrow=3)[3,])
# 
#   CUPS   <- paste0("post_cooked/CUPS/", setdiff(ECUPS,FCUPS),".csv",sep="")
#   CGP    <- paste0("post_cooked/CGP/",  setdiff(ECGP,FCGP),  ".csv",sep="")
#   LBT    <- paste0("post_cooked/LBT/",  setdiff(ELBT,FLBT),  ".csv",sep="")
#   CUA    <- paste0("post_cooked/CUA/",  setdiff(ECUA,FCUA),  ".csv",sep="")
#   TR     <- paste0("post_cooked/TR/",   setdiff(ETR ,FTR ),  ".csv",sep="")
#   CT     <- paste0("post_cooked/CT/",   setdiff(ECT ,FCT ),  ".csv",sep="")
#   SOLAR  <- paste0("post_cooked/SOLAR/",setdiff(ESOL,FSOL),  ".csv",sep="")

#   ALL  <- union(sample(CUPS,SAMPLE),sample(CGP,SAMPLE))
#   ALL  <- union(ALL,sample(LBT,SAMPLE))
# 
#   if (SAMPLE < length(CUA)){ ALL  <- union(ALL,sample(CUA,SAMPLE))
#   } else                   { ALL  <- union(ALL,CUA) }
# 
#   if (SAMPLE < length(TR)) { ALL  <- union(ALL,sample(TR,SAMPLE))
#   } else                   { ALL  <- union(ALL,TR) }
# 
#   if (SAMPLE < length(CT)) { ALL  <- union(ALL,sample(CT,SAMPLE))
#   } else                   { ALL  <- union(ALL,CT) }
# rm(CGP,CUPS,ECGP,ECUPS)
# gc()

# CT: edxKl+t2YUOUV1679x4MEuUKWTy0sdqJMaOqa2NNgm933TCP+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g==
#NAME <- "post_cooked/CT/edxKl+t2YUOUV1679x4MEuUKWTy0sdqJMaOqa2NNgm9.csv"
# LBT: +/qa24rrKT7vCTmmuxS2y+UKWTy0sdqJMaOqa2NNgm933TCP+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g==
#NAME <- "post_cooked/LBT/+\\qa24rrKT7vCTmmuxS2y+UKWTy0sdqJMaOqa2NNgm9.csv"

B <- foreach(NAME = ALL,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  FILE <- strsplit(NAME,"/")[[1]][3]
  TYPE <- strsplit(NAME,"/")[[1]][2]
  ID   <- tools::file_path_sans_ext(FILE)

  ### If we have not carried out the forecasting yet
  if (!paste0("stlf/forecast/",TYPE,"/",FILE) %in% DONE)
  {
    a <- fread(NAME)
    if (length(names(a)) == 4) names(a) <- c("time","kWh","VAL_AE","AUTO")
    if (!FIXED_TEST) a <- a[1:min(sample(MIN_TRAINING:length(a$kWh),1),length(a$kWh)),]
    
    LENGTH  <- length(a$kWh)
    ZEROS   <- sum(a$kWh==0)/LENGTH
    IMPUTED <- sum(a$issue)/LENGTH
    
    ### If we have enough data which is non zero and non imputed values in the time serie,
    ### then we continue with the assessment
    if( (IMPUTED < COMPLETE) & (ZEROS < COMPLETE) & (LENGTH > MIN_TRAINING))
    {
      POT_NOM <- LIM$POT_NOM[LIM$ID == ID]
      POT_EST <- LIM$POT_EST[LIM$ID == ID]
      
  #   ### Cojo los datos reales para CT y LBT y acorto la serie en otro caso
  #   if (TYPE %in% c("CT","LBT"))
  #   {
  #     real <- fread(paste0("stlf/test/",TYPE,"/",ID,".csv",sep=""))
  #     real <- zoo(real$SUM_VAL_AI/1000,order.by=real$DIA_LECTURA)
  #   } else {
 
      r    <- zoo(a$kWh, order.by = seq(from = as.POSIXct(first(a$time),tz="CET"),
                                        to   = as.POSIXct(last(a$time), tz="CET"), by="hour"))
      real <- window(r,start=index(r)[length(r)-24*F_DAYS+1])
      r    <- window(r,end=index(r)[length(r)-24*F_DAYS])
      rm(a)
  #   }

      f     <- data.frame(matrix(ncol = length(MODELS), nrow = 24*F_DAYS))
      MASE  <- MAPE  <- RMSE  <- TIME    <- data.frame(matrix(ncol = length(MODELS), nrow = 1))
      RISKA <- RISKB  <- MCA  <- MCB     <- data.frame(matrix(ncol = length(MODELS), nrow = length(MC)))
      colnames(MASE)  <- colnames(MAPE)  <- colnames(RMSE) <- colnames(TIME) <- colnames(f) <- MODELS
      colnames(RISKA) <- colnames(RISKB) <- colnames(MCA)  <- colnames(MCB)  <- MODELS
      rownames(RISKA) <- rownames(RISKB) <- rownames(MCA)  <- rownames(MCB)  <- MCNAMES

      f["real"]  <- real
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
      tryCatch({NN     <- nnetar(window(r,start=index(r)[length(r)-24*T_DAYS-23]))},                              warning=function(w) {},error=function(e) {print(e)},finally = {})
      tryCatch({SVM    <- tune(svm,real~past,data=TRAINSET,ranges=list(elsilon=seq(0,1,0.2),cost=seq(1,100,10)))},warning=function(w) {},error=function(e) {print(e)},finally = {})

      tryCatch({f["lr"]    <- as.numeric(forecast(LM,PREDICT)$mean)},       warning=function(w) {},error= function(e) {print(e)},finally = {})
      tryCatch({f["arima"] <- as.numeric(forecast(ARIMA,h=24*F_DAYS)$mean)},warning=function(w) {},error= function(e) {print(e)},finally = {})
      tryCatch({f["ses"]   <- as.numeric(forecast(ES,h=24*F_DAYS)$mean)},   warning=function(w) {},error= function(e) {print(e)},finally = {})
      tryCatch({f["ann"]   <- as.numeric(forecast(NN,h=24*F_DAYS)$mean)},   warning=function(w) {},error= function(e) {print(e)},finally = {})
      tryCatch({f["svm"]   <- as.numeric(predict(SVM$best.model,PREDICT))}, warning=function(w) {},error= function(e) {print(e)},finally = {})
      f["ens"]             <- rowMedians(as.matrix(f),na.rm=T)

      write.csv(f, file=paste("stlf/forecast",TYPE,FILE,sep="/"), row.names=F)

      ## QQR  <- ecdf(real)
      aux     <- real != 0
      auxmase <- median(abs(real-f[,"naive"]))

      auxtime <- numeric(F_DAYS)
      for (D in 1:F_DAYS)
        auxtime[D] <- which.max(real[(24*D-23):(24*D)]) -1

      for(j in MODELS)
      {
        MAPE[1,j] <- 100*median(abs(real[aux]-f[aux,j])/real[aux],na.rm=T)
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
  }   ### if that test if we have already done the forecast
  return(NAME)
}     ### foreach

EVAL <- function(ERROR,TYPE)
{
  R <- foreach(NAME = Sys.glob(paste("stlf",ERROR,TYPE,"*.csv",sep="/")),.combine=rbind) %dofuture% {
    if (!grepl("summary",NAME,fixed = TRUE)) fread(NAME,select=MODELS)
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
#   EVAL("mca",  TY)
#   EVAL("mcb",  TY)
#   EVAL("riska",TY)
#   EVAL("riskb",TY)
  EVAL("mase", TY)
}

R <- foreach(NAME = Sys.glob("stlf/*/*/summary.csv"),.combine=rbind) %dofuture% {
  ERROR <- strsplit(NAME,"/")[[1]][2]
  TY    <- strsplit(NAME,"/")[[1]][3]
  
  if (!grepl("risk",ERROR)) { return(data.frame(ERROR,TY,fread(NAME)[3,-1]))  }
  else  {                     return(data.frame(ERROR,TY,fread(NAME)))  }
}

write.csv(R,file="stlf/summary.csv",row.names=F)
