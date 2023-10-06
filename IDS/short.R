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
COMPLETE    <- 0.10  ### amount of data imputed allowed in the dataset
SAMPLE      <- 20    ### number of cups to assess

FILES       <- list.files(path="post_cooked/",pattern="*.csv")
MODELS      <- c("mean","rw","snaive","simple","lr","ann","svm","arima","ses","ens")
LO          <- 2*length(MODELS)

R <- foreach(NAME = sample(FILES,SAMPLE),.combine = 'rbind') %dofuture% {

  a <- fread(paste("post_cooked/",NAME,sep=""))
  r <- zoo(a$kWh, order.by = a$time)
  
  LENGTH  <- length(a$kWh)
  ZEROS   <- sum(a$kWh==0)/length(LENGTH)
  IMPUTED <- sum(a$issue)/length(LENGTH)

  ZZ   <- floor(0.75*(length(r)/24))    #### training days
  RMSE <- MAPE <- data.frame(matrix(ncol = length(MODELS), nrow = (floor(length(r)/24)-ZZ-1)))
  p    <- data.frame(matrix(ncol = length(MODELS), nrow = 24))
  colnames(p) <- colnames(RMSE) <- colnames(MAPE) <- MODELS
  
  if( IMPUTED > COMPLETE ) {     ### If we have enough non imputed values in the dataset,
                                 ### we continue with the assessment
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
  }
  
  k<-1
  OUT <- data.frame(cups=character(LO),model=character(LO),kpi=character(LO),
                    length=numeric(LO),zeros=numeric(LO),imputed=numeric(LO),
                    mean=numeric(LO),sd=numeric(LO),min=numeric(LO),
                    q1=numeric(LO),q2=numeric(LO),q3=numeric(LO),max=numeric(LO))
  for(j in MODELS)
  {
    OUT[k,]   <- c(NAME,j,"mape",LENGTH,ZEROS,IMPUTED, mean(MAPE[,j],na.rm=T),sd(MAPE[,j],na.rm=T), quantile(MAPE[,j],c(0,0.25,0.5,0.75,1),na.rm=T))
    OUT[k+1,] <- c(NAME,j,"rmse",LENGTH,ZEROS,IMPUTED, mean(RMSE[,j],na.rm=T),sd(RMSE[,j],na.rm=T), quantile(RMSE[,j],c(0,0.25,0.5,0.75,1),na.rm=T))
    k<-k+2
  }
  return(OUT)
}

fwrite(R,file="kpi-cups.csv")
setDT(R)
R[,2:3]  <- R[,lapply(.SD,as.factor),.SDcols=names(R)[2:3]]
R[,4:length(R)] <- R[,lapply(.SD,as.numeric),.SDcols=names(R)[4:length(R)]]
R[,as.list(summary(q2)), by = "model"]
boxplot(q2~model,data=R[R$kpi=="mape",],outline=F,ylab="MAPE (%)")
boxplot(q2~model,data=R[R$kpi=="rmse",],outline=F,ylab="RMSE (kWh)")
kruskalTest(q2~model,data=R[R$kpi=="mape",])

multiple <- kwAllPairsNemenyiTest(q2~model,data=R[R$kpi=="mape",])
multcompLetters(fullPTable(multiple$p.value))
plot(multiple)

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
