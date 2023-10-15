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

plan(multisession)

TRAIN_LIMIT <- 0.75  ### length of the training period
COMPLETE    <- 0.10  ### amount of data imputed allowed in the dataset
SAMPLE      <- 50    ### number of elements to assess per each type
S_DAYS      <- 7     ### number of days to forecast for STLF
L_DAYS      <- 4*5   ### number of days to forecast for MTLF

#### aqui podria leer lo que se ha generado en results y no recalcular de nuevo

ALL    <- list.files(path="post_cooked/",pattern="*.csv")
OTHERS <- list.files(path="post_cooked/",pattern="-")
CUPS   <- setdiff(ALL,OTHERS)
CT     <- list.files(path="post_cooked/",pattern="*-CT.csv")
LINES  <- list.files(path="post_cooked/",pattern="*-LINES.csv")

FILES  <- union(sample(CT,SAMPLE),sample(LINES,SAMPLE))
FILES  <- union(sample(CUPS,SAMPLE),FILES)

MODELS <- c("mean","rw","snaive","simple","lr","ann","svm","arima","ses","ens")
KPI    <- c("model","length","zeros","imputed","mean","sd","min","q1","q2","q3","max")
LO     <- 2*length(MODELS)

dir.create("results/mape",    showWarnings = F, recursive = T)
dir.create("results/rmse",    showWarnings = F, recursive = T)
dir.create("results/forecast",showWarnings = F, recursive = T)
dir.create("results/test",    showWarnings = F, recursive = T)

B <- foreach(NAME = FILES,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  a <- fread(paste("post_cooked/",NAME,sep=""))
  r <- zoo(a$kWh, order.by = a$time)
  d <- merge(apply.daily(r,min),apply.daily(r,max),apply.daily(r,sum))
  colnames(d) <- c("min","max","sum")

  LENGTH  <- length(a$kWh)
  ZEROS   <- sum(a$kWh==0)/LENGTH
  IMPUTED <- sum(a$issue)/LENGTH

  TRAIN_DAYS  <- floor(0.75*(length(r)/24))    #### training days
  RMSE  <- MAPE  <- data.frame(matrix(ncol = length(MODELS), nrow = (floor(length(r)/24)-TRAIN_DAYS-1)))
  TKPI  <- data.frame(matrix(ncol = length(MODELS), nrow = 2))
  
  p <- f <- data.frame(matrix(ncol = length(MODELS), nrow = 24))
  colnames(p) <- colnames(f) <- colnames(RMSE) <- colnames(MAPE) <- colnames(TKPI) <- MODELS
  
  ### If we have enough non zero, non imputed values in the dataset,
  ### then we continue with the assessment
  if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE)) {

    f["mean"]  <- as.numeric(rep(mean(r),24))
    f["rw"]    <- as.numeric(window(r,start=index(r)[length(r)-23]))
    f["snaive"]<- as.numeric(window(r,start=index(r)[length(r)-24*6-23], end=index(r)[length(r)-24*6]))
    f["simple"]<- rowMeans(data.frame(
                  a=(as.numeric(window(r,start=index(r)[length(r)-24*6 -23],end=index(r)[length(r)-24*6] ))), 
                  b=(as.numeric(window(r,start=index(r)[length(r)-24*13-23],end=index(r)[length(r)-24*13]))),
                  c=(as.numeric(window(r,start=index(r)[length(r)-24*20-23],end=index(r)[length(r)-24*20])))),
                  na.rm=T)

    rr              <- merge(r,lag(r,-24))
    TRAINSET        <- window(rr,start=index(r)[length(r)-24*20-23])
    PREDICT         <- data.frame(past=as.numeric(window(r,start=index(r)[length(r)-23])))
    names(TRAINSET) <- c("real","past")
      
    LM     <- lm(real~past,data=TRAINSET)
    ARIMA  <- auto.arima(r)
    ES     <- ses(r,h=24)
    NN     <- nnetar(r)
    SVM    <- tune(svm,real~past,data=TRAINSET,ranges=list(elsilon=seq(0,1,0.2), cost=seq(1,100,10)))

    f["lr"]    <- as.numeric(forecast(LM,PREDICT)$mean)
    f["arima"] <- as.numeric(forecast(ARIMA,h=24)$mean)
    f["ses"]   <- as.numeric(forecast(ES,h=24)$mean)
    f["ann"]   <- as.numeric(forecast(NN,h=24)$mean)
    f["svm"]   <- as.numeric(predict(SVM$best.model,PREDICT))
    f["ens"]   <- rowMedians(as.matrix(f),na.rm=T)
  
    write.csv(f, file=paste("results/forecast/forecast-",NAME,".csv",sep=""),row.names=F)
    
    ###  TESTSET with real hidden values 
    #real <- fread()
    real <- as.numeric(window(r,start=index(r)[length(r)-23]))   ####### ÑAPA hasta tener los datos finales
    aux  <- real != 0
    for(j in MODELS)
    {
      TKPI[1,j] <- 100*median(ifelse(sum(aux)!=0,abs(real[aux]-f[aux,j])/real[aux],NA))
      TKPI[2,j] <- sqrt(median((real-f[,j])^2))
    }
    
    rownames(TKPI) <- c("mape","rmse")
    write.csv(TKPI,file=paste("results/test/kpi-test-",NAME,".csv",sep=""))

    HARIMA <- arimaorder(ARIMA)              ### ÑAPA: get hyperparameters of arima model taking the full serie
    HSVM   <- SVM$best.parameters
    
    for (i in TRAIN_DAYS:(floor(length(r)/24)-1))    ### time series cross validation
    {
      rt <-                 window(r,start=index(r)[i*24-24*7*4*2+1],end=index(r)[i*24])
      rv <- data.frame(past=window(r,start=index(r)[i*24+1],        end=index(r)[i*24+24]))

result = tryCatch({
        #### TRAINING
      p["mean"]  <- as.numeric(rep(mean(rt),24))
      p["rw"]    <- as.numeric(window(r,start=index(r)[i*24-23],      end=index(r)[i*24]))
      p["snaive"]<- as.numeric(window(r,start=index(r)[i*24-24*6-23], end=index(r)[i*24-24*6]))
      p["simple"]<- rowMeans(data.frame(
                    a=(as.numeric(window(r,start=index(r)[i*24-24*6-23], end=index(r)[i*24-24*6]))), 
                    b=(as.numeric(window(r,start=index(r)[i*24-24*13-23],end=index(r)[i*24-24*13]))),
                    c=(as.numeric(window(r,start=index(r)[i*24-24*20-23],end=index(r)[i*24-24*20])))),na.rm=T)

      rr              <- merge(rt,lag(rt,-24))
      TRAINSET        <- window(rr,start=index(rr)[25])
      names(TRAINSET) <- c("real","past")
    
      LM    <- lm(real~past,data=TRAINSET)
      ARIMA <- Arima(rt,order=HARIMA)
      ES    <- ses(rt,h=24)
      NN    <- neuralnet(real~past,data=TRAINSET,hidden=24,linear.output=F)
      SVM   <- svm(      real~past,data=TRAINSET,elsilon=HSVM[[1]],cost=HSVM[[2]],linear.output=F)

      p["lr"]    <- as.numeric(forecast(LM,rv)$mean)
      p["arima"] <- as.numeric(forecast(ARIMA,h=24)$mean)
      p["ses"]   <- as.numeric(forecast(ES,h=24)$mean)
      p["ann"]   <- as.numeric(predict(NN,rv))
      p["svm"]   <- as.numeric(predict(SVM,rv))
      p["ens"]   <- rowMedians(as.matrix(p),na.rm=T)
}, warning = {
}, error = function(e) { (c("la he pificado en",NAME,e))  
}, finally = {}
)
      #### VALIDATION
      rv  <- as.numeric(unlist(rv))
      aux <- rv != 0
      for(j in MODELS)
      {
        MAPE[i-TRAIN_DAYS,j] <- 100*median(ifelse(sum(aux)!=0,abs(rv[aux]-p[aux,j])/rv[aux],NA))
        RMSE[i-TRAIN_DAYS,j] <- sqrt(median((rv-p[,j])^2))
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

EVAL <- function(R,NAME)
{
  setDT(R)
  R$model <- as.factor(R$model)
  print(R[,as.list(summary(q2)), by = "model"])
  write.csv(RCT[,as.list(summary(q2)), by = "model"],row.names=F,file=paste("results",NAME,".csv",sep=""))
  boxplot(q2~model,data=R,outline=F,ylab="MAPE (%)")
  print(kruskalTest(q2~model,data=R))
  multiple <- kwAllPairsNemenyiTest(q2~model,data=R)
  print(multcompLetters(fullPTable(multiple$p.value)))
  plot(multiple)
}

EVAL(RCT,"CT")
EVAL(RCU,"CUPS")

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
