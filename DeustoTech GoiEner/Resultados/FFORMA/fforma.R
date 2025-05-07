library(data.table)
library(caret)
library(doFuture)
library(scales)
library(stringr)
library(skimr)

registerDoFuture()
plan(multicore)

dir.create(file.path("./", "error"), showWarnings = FALSE)
dir.create(file.path("./", "model"), showWarnings = FALSE)
dir.create(file.path("./", "fig"),   showWarnings = FALSE)

MAPE   <- c("mean_mape","rw_mape","naive_mape","simple_mape","lr_mape","ann_mape","svm_mape","arima_mape","ses_mape","ens_mape")
RMSE   <- c("mean_rmse","rw_rmse","naive_rmse","simple_rmse","lr_rmse","ann_rmse","svm_rmse","arima_rmse","ses_rmse","ens_rmse")
BASE   <- c("mean_base","rw_base","naive_base","simple_base","lr_base","ann_base","svm_base","arima_base","ses_base","ens_base")
FF_WM  <- stringr::str_replace(BASE,"base","fforma_svn_mape_weight")
FF_WR  <- stringr::str_replace(BASE,"base","fforma_svn_rmse_weight")
PRED   <- c("dia","hora","p","zip_code","cnae") # "contracted_tariff"
ERRORS <- c(MAPE, RMSE)

df <- fread("allMetadata_errorAne.csv")
#df <- fread("allMetadataDEF.csv") #carpeta donde lo guarda ane

## clean data
names(df) <- stringr::str_replace(names(df),"pred","base")

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
SAMPLE <- "10000"
train  <- df[,c(..PRED,..ERRORS)][sample(.N,as.numeric(SAMPLE))]
test   <- df[,c(..PRED,..BASE,"real")] #[sample(.N,2*as.numeric(SAMPLE))]

med_base_mape_weight <- 1/matrixStats::colMedians(as.matrix(df[,..MAPE]),na.rm=T)
med_base_rmse_weight <- 1/matrixStats::colMedians(as.matrix(df[,..RMSE]),na.rm=T)

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

  saveRDS(n, paste("model/",SAMPLE,"-",e,".rds",sep=""))
}
dev.off()
plan(sequential)

plan(multisession, workers=5)
rm(train)

pre_m <- foreach (e=BASE,.combine=cbind) %dofuture% { 
  setNames(data.table(1/predict(readRDS(paste("model/",SAMPLE,"-",stringr::str_replace(e,"base","mape"),".rds",sep="")), newdata = test)),
          (stringr::str_replace(e,"base","fforma_svm_mape_weight")))
}

fwrite(pre_m,file="fforma_svm_mape_error_prediction.csv")

test[, fforma_svm_mape_base := rowSums(test[, ..BASE] * pre_m / rowSums(pre_m,na.rm=T),na.rm=T)]
test[, fforma_min_mape_base := rowSums(test[, ..BASE] * med_base_mape_weight / sum(med_base_mape_weight,na.rm=T),na.rm=T)]


for (modeloP in ModelosP)
{
  test[, (paste("fforma_",modeloP,"_mape_base",sep="")) :=
         rowSums(test[, ..BASE] * pre_m[modeloP] / rowSums(pre_m[modeloP],na.rm=T),na.rm=T)]
}


for (e in c(BASE,"fforma_svm_mape_base","fforma_min_mape_base"))
{
  i <- paste(e,"_mape",sep="")
  test[,(i) := as.matrix(abs(test[,"real"] - test[,..e])/test[,"real"])]
}

pdf(paste("fig/",SAMPLE,"-test-boxplot.pdf"),width=20)
  boxplot(test[,19:30],outline=F,names=stringr::str_split_i(names(test[,19:30]),"_",i=1))
dev.off()

pdf(paste("fig/",SAMPLE,"-test-scaterplots.pdf"))
  plot(test$real,test$fforma_svm_mape,main="MAPE errors")
  plot(test$real,test$fforma_min_mape,main="MIN errors")
dev.off()

test[sapply(test, is.infinite)] <- NA
skim(test)

# pre_r <- foreach (e=BASE,.combine=cbind) %dofuture% { 
#   setNames(data.table(1/predict(readRDS(paste("model/",SAMPLE,"-",stringr::str_replace(e,"base","rmse"),".rds",sep="")), newdata = test)),
#           (stringr::str_replace(e,"base","fforma_svm_rmse_weight")))
# }
# 
# fwrite(pre_r,file="fforma_svm_rmse_error_prediction.csv")
# 
# test[, fforma_svm_rmse_base := rowSums(test[, ..BASE] * pre_r,na.rm=T) / rowSums(pre_r,na.rm=T)]
# test[, fforma_min_rmse_base := rowSums(test[, ..BASE] * med_base_rmse_weight,na.rm=T) / sum(med_base_rmse_weight,na.rm=T)]
