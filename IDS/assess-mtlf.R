library(data.table)
library(stringr)

F_DAYS <- 7*4*3   ### number of days to forecast for MTLF
# MC    <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error

CLEAN_ID <- function(X)
{
  X <- str_replace(X,fixed("33TCP+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
  X <- str_replace_all(X,fixed("/"),fixed("\\"))
  return(X)
}

col1 <- fread("collaborator1.csv",    select = c("G3E_FID_CT","G3E_FID_CGP","G3E_FID_LBT","FEC_LECTURA","VAL_AI"))
col2 <- fread("collaborator2_mp.csv", select = c("G3E_FID_CT","G3E_FID_LBT","FEC_LECTURA","VAL_AI_MAX"))

ENG  <- fread("enganches.csv")
ENG$ID_USUARIO <- CLEAN_ID(ENG$ID_USUARIO)
ENG$G3E_FID_CT <- CLEAN_ID(ENG$G3E_FID_CT)

col1$G3E_FID_CT  <- CLEAN_ID(col1$G3E_FID_CT  )
col1$G3E_FID_CGP <- CLEAN_ID(col1$G3E_FID_CGP )
col1$G3E_FID_LBT <- CLEAN_ID(col1$G3E_FID_LBT )

col2$G3E_FID_CT  <- CLEAN_ID(col2$G3E_FID_CT  )
col2$G3E_FID_LBT <- CLEAN_ID(col2$G3E_FID_LBT )

LBT <- Sys.glob(paths="mtlf/test/LBT/*")
cat("ID,MAPE1,RMSE1,MAPE2,RMSE2,\n",file="mtlf/assess-LBT.csv",sep=",")
for (i in LBT)
{
  FILE   <- strsplit(i,"/")[[1]][4]
  TYPE   <- strsplit(i,"/")[[1]][3]
  ID     <- tools::file_path_sans_ext(FILE)

  r      <- fread(paste0("mtlf/test/LBT/",ID,".csv",sep=""),select="MAX_VAL_AI")[[1]]

  p1     <- col1[col1$G3E_FID_LBT == ID,]
  p1     <- p1[!grepl(":", as.character(p1$FEC_LECTURA)),]
  p2     <- col2[col2$G3E_FID_LBT == ID,]

  aux    <- numeric(F_DAYS)
  for (x in unique(p1$G3E_FID_CGP))
    aux  <- aux + p1[p1$G3E_FID_CGP == x,"VAL_AI"][[1]][1:F_DAYS]

  p1     <- aux[1:F_DAYS]
  p2     <- p2[1:F_DAYS,"VAL_AI_MAX"][[1]]

  aux    <- r != 0
  MAPE1  <- 100*median(abs(r[aux]-p1[aux])/r[aux])
  MAPE2  <- 100*median(abs(r[aux]-p2[aux])/r[aux])
  RMSE1  <- sqrt(median((r-p1)^2,na.rm=T))
  RMSE2  <- sqrt(median((r-p2)^2,na.rm=T))

  cat(ID,MAPE1,RMSE1,MAPE2,RMSE2,"\n",file="mtlf/assess-LBT.csv",sep=",",append=T)
}

CT <- Sys.glob(paths="mtlf/test/CT/*")
cat("ID,MAPE1,RMSE1,MAPE2,RMSE2,\n",file="mtlf/assess-CT.csv",sep=",")
for (i in CT)
{
  FILE   <- strsplit(i,"/")[[1]][4]
  TYPE   <- strsplit(i,"/")[[1]][3]
  ID     <- tools::file_path_sans_ext(FILE)

  r      <- fread(paste0("mtlf/test/CT/",ID,".csv",sep=""),select="MAX_VAL_AI")[[1]]

  p1     <- col1[col1$G3E_FID_CT == ID,]
  p2     <- col2[col2$G3E_FID_CT == ID,]

  aux <- numeric(F_DAYS)
  for (x in unique(p1$G3E_FID_CGP))
    aux <- aux + p1[p1$G3E_FID_CGP == x,"VAL_AI"][[1]][1:F_DAYS]
  p1 <- aux

  aux <- numeric(F_DAYS)
  for (x in unique(p2$G3E_FID_LBT))
    aux <- aux + p2[p2$G3E_FID_LBT == x,"VAL_AI_MAX"][[1]][1:F_DAYS]
  p2 <- aux

  aux     <- r != 0
  MAPE1  <- 100*median(abs(r[aux]-p1[aux])/r[aux])
  MAPE2  <- 100*median(abs(r[aux]-p2[aux])/r[aux])
  RMSE1  <- sqrt(median((r-p1)^2,na.rm=T))
  RMSE2  <- sqrt(median((r-p2)^2,na.rm=T))

  cat(ID,MAPE1,RMSE1,MAPE2,RMSE2,"\n",file="mtlf/assess-CT.csv",sep=",",append=T)
}

aLBT <- read.csv("mtlf/assess-LBT.csv")
aCT  <- read.csv("mtlf/assess-CT.csv")

summary(aLBT[-1])
summary(aCT[-1])
