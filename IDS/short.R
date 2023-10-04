library(data.table)
library(vtable)
library(zoo)
library(forecast)
library(doFuture)
#library(progressr)

plan(multisession)
#handlers(global = TRUE)

TRAIN_LIMIT <- 0.75  ### length of the training period
COMPLETE    <- 0.75  ### amount of 0 allowed in the dataset
SAMPLE      <- 2000   ### number of cups to assess

MODELS      <- c("mean","rw","snaive","simple","lr","ann","svm","arima","ses","ens")

cat("cups","model","kpi","mean","sd","min","q1","q2","q3","max",     "\n",sep=",",file="kpi.csv")

RESULTS <- foreach(NAME = sample(list.files(path="post_cooked/",pattern="*.csv",recursive=T),SAMPLE),
        .combine = 'rbind') %dofuture% {

  print(NAME)
  a <- fread(paste("post_cooked/",NAME,sep=""))
  r <- zoo(a$kWh, order.by = a$time)

  ZZ   <- floor(0.75*(length(r)/24))    #### training days
  RMSE <- MAPE <- data.frame(matrix(ncol = length(MODELS), nrow = (floor(length(r)/24)-ZZ-1)))
  p    <- data.frame(matrix(ncol = length(MODELS), nrow = 24))
  colnames(p) <- colnames(RMSE) <- colnames(MAPE) <- MODELS
  
  if( (sum(r!=0)/length(r)) > COMPLETE ) {   ### If we have enough non zero values in the dataset, 
                                             ### we continue with the assessment
    ARMA <- arimaorder(auto.arima(r))        ### ÑAPA: get hyperparameters of arima model takin the full serie

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
      p["ens"]   <- rowMeans(p,na.rm=T)
      
      #### VALIDATION
      aux     <- rv != 0
      for(j in MODELS)
      {
        MAPE[i-ZZ,j] <- median(ifelse(sum(aux)!=0,abs(rv[aux]-p[aux,j])/rv[aux],NA))
        RMSE[i-ZZ,j] <- sqrt(median((rv-p[,j])^2))
      }
    }
  }
  
  for(j in MODELS)
  {
    cat(NAME,j,"mape",mean(MAPE[,j],na.rm=T),sd(MAPE[,j],na.rm=T),quantile(MAPE[,j],c(0,0.25,0.5,0.75,1),na.rm=T),"\n",sep=",",file="kpi.csv",append=T)
    cat(NAME,j,"rmse",mean(RMSE[,j],na.rm=T),sd(RMSE[,j],na.rm=T),quantile(RMSE[,j],c(0,0.25,0.5,0.75,1),na.rm=T),"\n",sep=",",file="kpi.csv",append=T)
  }

#  c(NAME,apply(MAPE,2,median,na.rm=T))
}

z       <- fread("kpi.csv")
#z$model <- factor(z$model)
#z$kpi   <- factor(z$kpi)
z[,as.list(summary(q2)), by = model]
boxplot(q2~model*kpi,data=z[z$kpi=="mape",],outline=F)
