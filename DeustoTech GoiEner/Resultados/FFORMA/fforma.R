library(data.table)
library(caret)
library(doFuture)

registerDoFuture()
plan(multicore)

dir.create(file.path("./", "error"), showWarnings = FALSE)
dir.create(file.path("./", "model"), showWarnings = FALSE)
dir.create(file.path("./", "fig"),   showWarnings = FALSE)

ERRORS <- c("mean_mape","rw_mape","naive_mape","simple_mape","lr_mape","ann_mape","svm_mape","arima_mape","ses_mape","ens_mape",
            "mean_rmse","rw_rmse","naive_rmse","simple_rmse","lr_rmse","ann_rmse","svm_rmse","arima_rmse","ses_rmse","ens_rmse")
PRED   <- c("dia","hora","p1","p2","p3","p4","p5","p6","zip_code","cnae","contracted_tariff")

df <- data.table::fread("allMetadata_errorAne.csv")

## clean data
df[sapply(df, is.infinite)] <- NA
df$cnae <- floor(df$cnae/1000)
df$p    <- pmax(df$p1,df$p2,df$p3,df$p4,df$p5,df$p6,na.rm=TRUE)
df <- as.data.frame(df)

## Remove outlayers
for (i in ERRORS)
{
  z <- as.numeric(quantile(df[,i],c(0.33,0.66),na.rm=T))
  df[df[,i]>=z[2]+1.5*(z[2]-z[1]) & !is.na(df[,i]),i] <- NA
}

df <- data.table::as.data.table(df)

## Model
SAMPLE <- 100
pdf(paste("fig/",SAMPLE,"-scaterplots.pdf"))
for (e in ERRORS)
{
  f    <- reformulate(termlabels = c("dia","hora","p","zip_code","cnae"), response = e)
  tc   <- trainControl(method = "cv")
  grid <- expand.grid(
            C = c(0.01,0.1,1,10,100),      # Regularization parameter
            sigma = c(0.01,0.1,1,10,100)   # Kernel parameter (for radial basis function)
          )
  n <- train(
        f,
        data      = df[sample(.N,SAMPLE)],
        na.action = na.omit,
        preProc   = c("range", "zv" , "nzv", "corr"), #"center", "scale"
        method    = "svmRadial",
        tuneGrid  = grid,
        trControl = tc
      )

  write.csv(n$results,row.names=F,file=paste("error/",SAMPLE,"-",e,".csv",sep=""))
  plot(n$finalModel@ymatrix,n$finalModel@fitted,main=e)

  saveRDS(n, paste("model/",SAMPLE,"-",e,".rds",sep="")) ##my_model <- readRDS("FILE.rds")
}
dev.off()
