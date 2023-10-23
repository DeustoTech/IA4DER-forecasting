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

TEST        <- TRUE  ### si estoy haciendo test
SAMPLE      <- 5     ### number of elements to assess per each type
COMPLETE    <- 0.10  ### amount of data imputed allowed in the dataset
TRAIN_LIMIT <- 0.75  ### length of the training period
F_DAYS      <- 7     ### number of days to forecast for STLF
T_DAYS      <- 21    ### number of days to use in the training widnow for AI methods for STLF
MODELS      <- c("mean","rw","naive","simple","lr","ann","svm","arima","ses","ens")
TYPES       <- c("CUPS","LINE","CT")

CUPS <- Sys.glob(paths="post_cooked/CUPS/*")
LINE <- Sys.glob(paths="post_cooked/LINE/*")
CT   <- Sys.glob(paths="post_cooked/CT/*")
ALL  <- union(sample(CUPS,SAMPLE),sample(LINE,SAMPLE))
ALL  <- union(ALL,sample(CT,SAMPLE))

# ALL  <- Sys.glob(paths="post_cooked/*/*")

for (TY in TYPES)
{
  dir.create(paste("stlf/forecast/",TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/mape/",    TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/rmse/",    TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/time/",    TY,sep="/"),showWarnings = F, recursive = T)
  dir.create(paste("stlf/effe/",    TY,sep="/"),showWarnings = F, recursive = T)
}

ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

cups   <- ARROW2DF("./inputdata/data/anm_ids01_punto_suministro")
cgp    <- ARROW2DF("./inputdata/data/anm_ids01_cgp")
linea  <- ARROW2DF("./inputdata/data/anm_ids01_linea_bt")
cuadro <- ARROW2DF("./inputdata/data/anm_ids01_cuadro_bt")
pos    <- ARROW2DF("./inputdata/data/anm_ids01_pos_trafo")
ct     <- ARROW2DF("./inputdata/data/anm_ids01_ct")
trafo  <- ARROW2DF("./inputdata/data/anm_ids01_trafo")

ROSETA <- merge(cups,  cgp,   by.x="COD_SIC_SIGRID",    by.y="ID_CAJA")
ROSETA <- merge(ROSETA,linea, by.x="ID_PADRE_LINEA_BT", by.y="G3E_FID")
ROSETA <- merge(ROSETA,cuadro,by.x="ID_PADRE_CUADRO_BT",by.y="G3E_FID")
ROSETA <- merge(ROSETA,trafo, by.x="ID_PADRE_POS_TRAFO",by.y="ID_PADRE_POS_TRAFO")

cgp$ID_CAJA <- str_replace(cgp$ID_CAJA, fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
cgp$ID_CAJA <- str_replace(cgp$ID_CAJA, fixed("/"),fixed("\\"))

ROSETA$ID_USUARIO_INST_PADRE <- str_replace_all(ROSETA$ID_USUARIO_INST_PADRE, fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
ROSETA$ID_USUARIO_INST_PADRE <- str_replace_all(ROSETA$ID_USUARIO_INST_PADRE, fixed("/"),fixed("\\"))

LIM <- data.frame(ID=character(),POT_NOM=numeric())
for (z in unique(ROSETA$ID_USUARIO_INST_PADRE))
  LIM[z,"POT_NOM"] <- ROSETA$POT_NOMI_TRAFO[which(ROSETA$ID_USUARIO_INST_PADRE == z)][1]

LIM$ID <- row.names(LIM)
row.names(LIM) <- NULL

LIM <- rbind(LIM,data.frame(ID=cups$CUPS,  POT_NOM=cups$VAL_POT_AUTORIZADA/1000))
LIM <- rbind(LIM,data.frame(ID=cgp$ID_CAJA,POT_NOM=(cgp$COD_INT_NOMI_FUSIB * cgp$COD_TENS_SUMI)))
ID  <- tools::file_path_sans_ext(matrix(unlist(strsplit(ALL,"/")),nrow=3)[3,])

LIM <- LIM[LIM$ID %in% ID,]

B <- foreach(NAME = ALL,
             .options.future = list(seed = TRUE),
             .errorhandling = "remove") %dofuture% {

  a <- fread(NAME)
  r <- zoo(a$kWh, order.by = a$time)

  FILE <- strsplit(NAME,"/")[[1]][3]
  TYPE <- strsplit(NAME,"/")[[1]][2]
  ID   <- tools::file_path_sans_ext(FILE)
  
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
  MAPE  <- RMSE  <- TIME <- EFFE <- data.frame(matrix(ncol = length(MODELS), nrow = 1))
  colnames(MAPE) <- colnames(RMSE) <- colnames(TIME) <- colnames(EFFE) <- colnames(f) <- MODELS
  
  ### If we have enough non zero, non imputed values in the dataset,
  ### then we continue with the assessment
  if( (IMPUTED < COMPLETE) | (ZEROS < COMPLETE))
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

    aux  <- real != 0
    for(j in MODELS)
    {
      MAPE[1,j] <- 100*median(ifelse(sum(aux)!=0,abs(real[aux]-f[aux,j])/real[aux],NA),na.rm=T)
      RMSE[1,j] <- sqrt(median((real-f[,j])^2,na.rm=T))
      
      aux_real  <- aux_f <- numeric(F_DAYS)
      for (D in 1:F_DAYS)
      {
        aux_real[D] <-                                      which.max(real[(24*D-23):(24*D)])  -1
        aux_f[D]    <- ifelse(!anyNA(f[(24*D-23):(24*D),j]),which.max(f[   (24*D-23):(24*D),j])-1,NA)
      }
      
      TIME[1,j] <- median(abs(aux_real-aux_f),na.rm=T)
      EFFE[1,j] <- sum(f[,j] >= 0.8*LIM$POT_NOM[LIM$ID == ID] )
    }
    
    write.csv(MAPE,file=paste("stlf/mape",TYPE,FILE,sep="/"))
    write.csv(RMSE,file=paste("stlf/rmse",TYPE,FILE,sep="/"))
    write.csv(TIME,file=paste("stlf/time",TYPE,FILE,sep="/"))
    write.csv(EFFE,file=paste("stlf/effe",TYPE,FILE,sep="/"))
  } ### if that test if the time series has data
}   ### foreach

EVAL <- function(ERROR,TYPE)
{
  R <- foreach(NAME = Sys.glob(paste("stlf",ERROR,TYPE,"*.csv",sep="/")),.combine=rbind) %dofuture% {
    if (!grepl("summary",NAME,fixed = TRUE)) fread(NAME)
  }
  
  RR <- na.omit(as.matrix(R[,..MODELS]))
  rownames(RR) <- 1:nrow(RR)

  write.csv(as.data.frame(apply(RR, 2, summary)),
            file=paste("stlf",ERROR,TYPE,"summary.csv",sep="/"))
  BOOT <- boot(data=RR,statistic=function(data,i) colMedians(data[i,],na.rm=T),R=100)

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
}

for (TY in TYPES)
{
  EVAL("mape",TY)
  EVAL("rmse",TY)
  EVAL("time",TY)
  EVAL("effe",TY)
}

R <- foreach(NAME = Sys.glob("stlf/*/*/summary.csv"),.combine=rbind) %dofuture% {
  aux   <- fread(NAME)
  ERROR <- strsplit(NAME,"/")[[1]][2]
  TY    <- strsplit(NAME,"/")[[1]][3]
  
  return(data.frame(ERROR,TY,aux[3,-1]))
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
