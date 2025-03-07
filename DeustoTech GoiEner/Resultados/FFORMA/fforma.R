library(data.table)
library(caret)
library(doFuture)
library(scales)
library(stringr)

registerDoFuture()
plan(multisession)

dir.create(file.path("./", "error"), showWarnings = FALSE)
dir.create(file.path("./", "model"), showWarnings = FALSE)
dir.create(file.path("./", "fig"),   showWarnings = FALSE)

MAPE   <- c("mean_mape","rw_mape","naive_mape","simple_mape","lr_mape","ann_mape","svm_mape","arima_mape","ses_mape","ens_mape")
RMSE   <- c("mean_rmse","rw_rmse","naive_rmse","simple_rmse","lr_rmse","ann_rmse","svm_rmse","arima_rmse","ses_rmse","ens_rmse")
BASE   <- c("mean_pred","rw_pred","naive_pred","simple_pred","lr_pred","ann_pred","svm_pred","arima_pred","ses_pred","ens_pred")
PRED   <- c("dia","hora","p","zip_code","cnae") # "contracted_tariff"
ERRORS <- c(MAPE, RMSE)

df <- fread("allMetadata_errorAne.csv")

## clean data
df[sapply(df, is.infinite)] <- NA
df[is.na(cnae),     cnae:=9]
df[is.na(zip_code), zip_code:=0]

df[,p:=rescale(pmax(df$p1,df$p2,df$p3,df$p4,df$p5,df$p6,na.rm=TRUE))] # by default rescale to to = c(0, 1)

df$contracted_tariff <- factor(df$contracted_tariff)
df$cnae     <- factor(floor(df$cnae/1000))
df$zip_code <- factor(floor(df$zip_code/1000))
df$pf       <- factor(floor(df$p*100))
df$dia      <- factor(df$dia)
df$hora     <- factor(df$hora)

## Remove outlayers
for (e in ERRORS)  # Replace outliers with NA
{
  z <- as.numeric(quantile(df[,..e],c(0.25,0.75),na.rm=T))
  df[,(e):=ifelse(get(e)>z[2]+1.5*(z[2]-z[1]), NA, get(e))]
}

## Scale MAPE to [0,1] as it seems caret is not doing a good job
mmape <- max(df[,..MAPE],na.rm=T)
for (e in MAPE) df[,(e):=get(e)/mmape]

## Model
SAMPLE <- "100000"
train  <- df[sample(.N,as.numeric(SAMPLE)),c(..PRED,..ERRORS)]
test   <- df[sample(.N,as.numeric(SAMPLE)),c(..PRED,..BASE,"real")]
rm(df)

pdf(paste("fig/",SAMPLE,"-train-scaterplots.pdf"))
for (e in ERRORS)
{
  f    <- reformulate(termlabels = PRED, response = e)
  tc   <- trainControl(method = "adaptive_cv")
  grid <- expand.grid(
            C     = c(c(1,5) %o% 10^(-3:-2)),   # Regularization parameter
            sigma = c(c(1,5) %o% 10^(-3:-2))    # Kernel parameter (for radial basis function)
          )
  n    <- train(
            f,
            data      = train,
            na.action = na.omit,
            preProc   = c("zv" , "nzv", "corr"), # "center", "scale", "range"
            method    = "svmRadial",
            tuneGrid  = grid,
            trControl = tc
      )

  write.csv(n$results,row.names=F,file=paste("error/",SAMPLE,"-",e,".csv",sep=""))
  plot(n$finalModel@ymatrix,n$finalModel@fitted,main=e)

  saveRDS(n, paste("model/",SAMPLE,"-",e,".rds",sep="")) ##my_model <- readRDS("FILE.rds")
}
dev.off()
plan(sequential)

pm <- pr <- data.table(matrix(ncol = length(BASE), nrow = as.numeric(SAMPLE)))
colnames(pm) <- colnames(pr) <- BASE
for (e in BASE)
{
  mm <- readRDS(paste("model/",SAMPLE,"-",str_replace(e,"pred","mape"),".rds",sep=""))
  mr <- readRDS(paste("model/",SAMPLE,"-",str_replace(e,"pred","rmse"),".rds",sep=""))
  pm[,(e):=as.data.table(predict(mm, newdata = test))]
  pr[,(e):=as.data.table(predict(mr, newdata = test))]
}
fwrite(pm,"prediction-mape.csv")
fwrite(pr,"prediction-rmse.csv")
test[, fforma_svm_mape := rowSums(test[, ..BASE] * pm) / rowSums(pm)]
test[, fforma_svm_rmse := rowSums(test[, ..BASE] * pr) / rowSums(pr)]

z <- test$real != 0
cat("RMSE FFORMA SVM MAPE","RMSE FFORMA SVM RMSE", "MAPE FFORMA SVM MAPE","MAPE FFORMA SVM RMSE","\n",  
      sep=",",file=paste(SAMPLE,"-resultados.csv",sep=""))
cat(sqrt(mean((test$real - test$fforma_svm_mape)*(test$real - test$fforma_svm_mape))),
    sqrt(mean((test$real - test$fforma_svm_rmse)*(test$real - test$fforma_svm_rmse))),
    mean(abs(test$real[z] - test$fforma_svm_mape[z])/test$real[z]),
    mean(abs(test$real[z] - test$fforma_svm_rmse[z])/test$real[z]),"\n",
      sep=",",file=paste(SAMPLE,"-resultados.csv",sep=""),append=T)

pdf(paste("fig/",SAMPLE,"-test-scaterplots.pdf"))
  plot(test$real,test$fforma_svm_mape,main="MAPE errors")
  plot(test$real,test$fforma_svm_rmse,main="RMSE errors")
dev.off()
