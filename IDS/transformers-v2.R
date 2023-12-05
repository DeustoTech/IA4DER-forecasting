library(data.table)
library(zoo)
library(forecast)
library(doFuture)
library(stringr)

plan(multisession)

PEGA <- function(CUPS)
{
  N_SAMPLES <- length(CUPS)
  a <- data.frame(time=seq(ISOdate(2021,06,01,00,00,00),length.out=24*457,by="hour"),
                  kWh=double(24*457),
                  issue=double(24*457))
  for (j in CUPS)
  {
    tryCatch({
      z <- fread(paste("post_cooked/CUPS/",j,".csv",sep=""),data.table=F)
      a$kWh   <- a$kWh   + z$kWh
      a$issue <- a$issue/N_SAMPLES + z$issue/N_SAMPLES
    },
             warning = function(w) {},
             error   = function(e) {print(j)},
             finally = {}
    )
  }
  return(a)
}

CLEAN_ID <- function(X)
{
  X <- str_replace(X,fixed("33TCP+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
  X <- str_replace_all(X,fixed("/"),fixed("\\"))
  return(X)
}

dir.create("post_cooked/CT", showWarnings = F, recursive = T)
dir.create("post_cooked/TR", showWarnings = F, recursive = T)
dir.create("post_cooked/CUA",showWarnings = F, recursive = T)
dir.create("post_cooked/LBT",showWarnings = F, recursive = T)
dir.create("post_cooked/CGP",showWarnings = F, recursive = T)

dir.create("test/CT",        showWarnings = F, recursive = T)
dir.create("test/TR",        showWarnings = F, recursive = T)
dir.create("test/CUA",       showWarnings = F, recursive = T)
dir.create("test/LBT",       showWarnings = F, recursive = T)
dir.create("test/CGP",       showWarnings = F, recursive = T)

#ROSETA <- fread("cups_per_lines_ct-v2.csv",header=T)
ROSETA <- fread("roseta-v2.csv")

CGP <- CLEAN_ID(unique(ROSETA$COD_SIC_SIGRID))
LBT <- CLEAN_ID(unique(ROSETA$ID_PADRE_LINEA_BT))
CUA <- CLEAN_ID(unique(ROSETA$ID_PADRE_CUADRO_BT))
TR  <- CLEAN_ID(unique(ROSETA$ID_PADRE_POS_TRAFO))
CT  <- CLEAN_ID(unique(ROSETA$ID_PADRE_CT))

ROSETA$COD_SIC_SIGRID      <- CLEAN_ID(ROSETA$COD_SIC_SIGRID)
ROSETA$ID_PADRE_LINEA_BT   <- CLEAN_ID(ROSETA$ID_PADRE_LINEA_BT)
ROSETA$ID_PADRE_CUADRO_BT  <- CLEAN_ID(ROSETA$ID_PADRE_CUADRO_BT)
ROSETA$ID_PADRE_POS_TRAFO  <- CLEAN_ID(ROSETA$ID_PADRE_POS_TRAFO)
ROSETA$ID_PADRE_CT         <- CLEAN_ID(ROSETA$ID_PADRE_CT)

z <- foreach(i = CT) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_CT == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/CT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = TR) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_POS_TRAFO == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/TR/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = CUA) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_CUADRO_BT == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/CUA/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = LBT) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_LINEA_BT == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/LBT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = CGP) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$COD_SIC_SIGRID == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/CGP/",i,".csv",sep=""),dateTimeAs="write.csv")
}

############### TEST ###############

F_DAYS <- 7     ### number of days to forecast for STLF

ENG <- fread("enganches.csv")
ENG$ID_USUARIO <- CLEAN_ID(ENG$ID_USUARIO)
ENG$G3E_FID_CT <- CLEAN_ID(ENG$G3E_FID_CT)

cp <- fread("cp.csv", select = c("ID_LINEA_BT","ID_USUARIO","DIA_LECTURA","SUM_VAL_AI"))
cp$ID_LINEA_BT <- CLEAN_ID(cp$ID_LINEA_BT)
cp$ID_USUARIO  <- CLEAN_ID(cp$ID_USUARIO)
cp$DIA_LECTURA <- as.POSIXct(as.character(cp$DIA_LECTURA),format="%Y%m%d%H%M%S",tz="GMT")

z <- foreach(i = LBT) %dofuture% {
  FILE <- strsplit(i,"/")[[1]][3]
  TYPE <- strsplit(i,"/")[[1]][2]
  ID   <- tools::file_path_sans_ext(FILE)

  r  <- cp[    cp$ID_LINEA_BT == ID,]
  r  <- r[order(r$DIA_LECTURA),]

  fwrite(r[1:(F_DAYS*24),c("DIA_LECTURA","SUM_VAL_AI")],
         file=paste0("test/LBT/",ID,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = CT) %dofuture% {
  FILE  <- strsplit(i,"/")[[1]][3]
  TYPE  <- strsplit(i,"/")[[1]][2]
  ID    <- tools::file_path_sans_ext(FILE)

  r     <- cp[cp$ID_USUARIO == ENG$ID_USUARIO[ENG$G3E_FID_CT == ID][1],]
  r     <- r[order(r$DIA_LECTURA),]

  aux   <- numeric(F_DAYS*24)
  for (j in unique(r$ID_LINEA_BT))
    aux <- aux + as.numeric(r[r$ID_LINEA_BT == j,"SUM_VAL_AI"][[1]][1:(F_DAYS*24)])

  fwrite(data.frame(DIA_LECTURA=unique(r$DIA_LECTURA)[1:(F_DAYS*24)],
                    SUM_VAL_AI=aux),
         file=paste0("test/CT/",ID,".csv",sep=""),dateTimeAs="write.csv")
}


