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

dir.create("post_cooked/TR", showWarnings = F, recursive = T)
dir.create("post_cooked/CT", showWarnings = F, recursive = T)
dir.create("post_cooked/LBT",showWarnings = F, recursive = T)
dir.create("post_cooked/CGP",showWarnings = F, recursive = T)

dir.create("test/TR",        showWarnings = F, recursive = T)
dir.create("test/CT",        showWarnings = F, recursive = T)
dir.create("test/LBT",       showWarnings = F, recursive = T)
dir.create("test/CGP",       showWarnings = F, recursive = T)

#ROSETA <- fread("cups_per_lines_ct-v2.csv",header=T)
ROSETA <- fread("roseta.csv")

CGP <- CLEAN_ID(unique(ROSETA$COD_SIC_SIGRID))
LBT <- CLEAN_ID(unique(ROSETA$ID_PADRE_LINEA_BT))
CT  <- CLEAN_ID(unique(ROSETA$ID_PADRE_CUADRO_BT))
TR  <- CLEAN_ID(unique(ROSETA$ID_PADRE_POS_TRAFO))

ROSETA$COD_SIC_SIGRID      <- CLEAN_ID(ROSETA$COD_SIC_SIGRID    )
ROSETA$ID_PADRE_LINEA_BT   <- CLEAN_ID(ROSETA$ID_PADRE_LINEA_BT )
ROSETA$ID_PADRE_CUADRO_BT  <- CLEAN_ID(ROSETA$ID_PADRE_CUADRO_BT)
ROSETA$ID_PADRE_POS_TRAFO  <- CLEAN_ID(ROSETA$ID_PADRE_POS_TRAFO)

z <- foreach(i = TR) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_POS_TRAFO == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/TR/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = CT) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_CUADRO_BT == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/CT/",i,".csv",sep=""),dateTimeAs="write.csv")
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

