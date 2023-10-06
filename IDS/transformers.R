library(data.table)
library(vtable)
library(zoo)
library(forecast)
library(doFuture)
library(matrixStats)
library(PMCMRplus)
library(rcompanion)
library(multcompView)
#library(progressr)
plan(multisession)
#handlers(global = TRUE)

TRAIN_LIMIT <- 0.75  ### length of the training period
SAMPLE      <- 20000 ### number of cups to assess
TRANSFORMERS<- 100   ### number of transformers to create
CUPS_PER    <- 1000  ### number of supply points per transformer

FILES       <- list.files(path="post_cooked/",pattern="*.csv")
MODELS      <- c("mean","rw","snaive","simple","lr","ann","svm","arima","ses","ens")
LO          <- 2*length(MODELS)

CT <- foreach(i = 1:TRANSFORMERS) %dofuture% {
  CUPS <- sample(FILES,CUPS_PER)

  a <- fread(paste("post_cooked/",CUPS[1],sep=""))
  z <- a$kWh
  for (j in CUPS[-1])
    z <- z + fread(paste("post_cooked/",j,sep=""))$kWh

  return(zoo(z, order.by = a$time))
}

R <- foreach(r = CT,.combine = 'rbind') %dofuture% {

  ZZ   <- floor(0.75*(length(r)/24))    #### training days
  RMSE <- MAPE <- data.frame(matrix(ncol = length(MODELS), nrow = (floor(length(r)/24)-ZZ-1)))
  p    <- data.frame(matrix(ncol = length(MODELS), nrow = 24))
  colnames(p) <- colnames(RMSE) <- colnames(MAPE) <- MODELS
  
  ARMA <- arimaorder(auto.arima(r))        ### ÑAPA: get hyperparameters of arima model taking the full serie

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

    tr <- merge(rt,lag(rt,-24))
    names(tr) <- c("r","x")
    
    LM    <- lm(r~x,data=tr)
    ARIMA <- Arima(rt,order=ARMA)
    ES    <- ses(rt,h=24)  
  ##  NN    <- nnetar(rt)
  ##  SVM   <- ARSVM(rt,h=24)

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
  
  k<-1
  OUT <- data.frame(cups=character(LO),model=character(LO),kpi=character(LO),
                    mean=numeric(LO),sd=numeric(LO),min=numeric(LO),
                    q1=numeric(LO),q2=numeric(LO),q3=numeric(LO),max=numeric(LO))
  for(j in MODELS)
  {
    OUT[k,]   <- c(NAME,j,"mape",mean(MAPE[,j],na.rm=T),sd(MAPE[,j],na.rm=T),quantile(MAPE[,j],c(0,0.25,0.5,0.75,1),na.rm=T))
    OUT[k+1,] <- c(NAME,j,"rmse",mean(RMSE[,j],na.rm=T),sd(RMSE[,j],na.rm=T),quantile(RMSE[,j],c(0,0.25,0.5,0.75,1),na.rm=T))
    k<-k+2
  }
  return(OUT)
}

fwrite(R,file="kpi-ct.csv")
setDT(R)
R[,2:3]         <- R[,lapply(.SD,as.factor),.SDcols=names(R)[2:3]]
R[,4:length(R)] <- R[,lapply(.SD,as.numeric),.SDcols=names(R)[4:length(R)]]
R[,as.list(summary(q2)), by = "model"]
boxplot(q2~model,data=R[R$kpi=="mape",],outline=F,ylab="MAPE (%)")
boxplot(q2~model,data=R[R$kpi=="rmse",],outline=F,ylab="RMSE (kWh)")
kruskalTest(q2~model,data=R[R$kpi=="mape",])

multiple <- kwAllPairsNemenyiTest(q2~model,data=R[R$kpi=="mape",])
multcompLetters(fullPTable(multiple$p.value))
plot(multiple)

