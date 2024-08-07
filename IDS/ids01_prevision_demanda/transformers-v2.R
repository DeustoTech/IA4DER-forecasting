library(data.table)
library(zoo)
library(forecast)
library(doFuture)
library(stringr)
library(arrow)

plan(multisession)

ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

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

dir.create("stlf/test/CT",   showWarnings = F, recursive = T)
dir.create("stlf/test/TR",   showWarnings = F, recursive = T)
dir.create("stlf/test/CUA",  showWarnings = F, recursive = T)
dir.create("stlf/test/LBT",  showWarnings = F, recursive = T)
dir.create("stlf/test/CGP",  showWarnings = F, recursive = T)

dir.create("mtlf/test/CT",   showWarnings = F, recursive = T)
dir.create("mtlf/test/TR",   showWarnings = F, recursive = T)
dir.create("mtlf/test/CUA",  showWarnings = F, recursive = T)
dir.create("mtlf/test/LBT",  showWarnings = F, recursive = T)
dir.create("mtlf/test/CGP",  showWarnings = F, recursive = T)

#ROSETA <- fread("roseta-v2.csv")

cups   <- ARROW2DF("./inputdata/data/anm_ids01_punto_suministro")
cgp    <- ARROW2DF("./inputdata/data/anm_ids01_cgp")
linea  <- ARROW2DF("./inputdata/data/anm_ids01_linea_bt")
cuadro <- ARROW2DF("./inputdata/data/anm_ids01_cuadro_bt")
pos    <- ARROW2DF("./inputdata/data/anm_ids01_pos_trafo")
#trafo  <- ARROW2DF("./inputdata/data/anm_ids01_trafo")
ct     <- ARROW2DF("./inputdata/data/anm_ids01_ct")

ROSETA <- merge(cups,  cgp,   by.x="COD_SIC_SIGRID",    by.y="ID_CAJA")
ROSETA <- merge(ROSETA,linea, by.x="ID_PADRE_LINEA_BT", by.y="G3E_FID")
ROSETA <- merge(ROSETA,cuadro,by.x="ID_PADRE_CUADRO_BT",by.y="G3E_FID")
ROSETA <- merge(ROSETA,pos,   by.x="ID_PADRE_POS_TRAFO",by.y="G3E_FID")
#ROSETA <- merge(ROSETA,trafo, by.x="ID_PADRE_POS_TRAFO",by.y="G3E_FID")
ROSETA <- merge(ROSETA,ct,    by.x="ID_PADRE_CT",       by.y="G3E_FID")

ROSETA$COD_SIC_SIGRID      <- CLEAN_ID(ROSETA$COD_SIC_SIGRID)
ROSETA$ID_PADRE_LINEA_BT   <- CLEAN_ID(ROSETA$ID_PADRE_LINEA_BT)
ROSETA$ID_PADRE_CUADRO_BT  <- CLEAN_ID(ROSETA$ID_PADRE_CUADRO_BT)
ROSETA$ID_PADRE_POS_TRAFO  <- CLEAN_ID(ROSETA$ID_PADRE_POS_TRAFO)
ROSETA$ID_PADRE_CT         <- CLEAN_ID(ROSETA$ID_PADRE_CT)

z <- foreach(i = unique(ROSETA$ID_PADRE_CT)) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_CT == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/CT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = unique(ROSETA$ID_PADRE_POS_TRAFO)) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_POS_TRAFO == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/TR/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = unique(ROSETA$ID_PADRE_CUADRO_BT)) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_CUADRO_BT == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/CUA/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = unique(ROSETA$ID_PADRE_LINEA_BT)) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$ID_PADRE_LINEA_BT == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/LBT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = unique(ROSETA$COD_SIC_SIGRID)) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$COD_SIC_SIGRID == i]
  a <- PEGA(CUPS)
  fwrite(a,file=paste("post_cooked/CGP/",i,".csv",sep=""),dateTimeAs="write.csv")
}

############### TEST stlf ###############
library(arrow)
library(dplyr)

ds <- open_dataset("./inputdata/universidad_deusto/anm_ids01_test_lec_horaria_val/")

set_io_thread_count(2)
write_dataset(dataset = ds %>% select(c("cups","fec_lectura","val_ai")),
  path = "test",
  hive_style = F,
  max_partitions = 2048,
  partitioning = "cups",
  format = "csv"
  )

ds %>% 
  group_by(cups) %>% 
    select(c("cups","fec_lectura","val_ai")) %>%
      write_dataset(  
        path = "test",
        hive_style = F,
        max_partitions = 80000,
        partitioning = "cups",
        format = "csv"
      )

CUPS <- 
z <- foreach(i = unique(ROSETA$CUPS)) %dofuture% {
  
  
  fwrite(a,file=paste("post_cooked/CGP/",i,".csv",sep=""),dateTimeAs="write.csv")
}

scan_builder <- ds$NewScan()
  scan_builder$Project(c("cups"))
  
  scanner <- scan_builder$Finish()
a <- scanner$ToTable()

scan_builder <- ds$NewScan()
scan_builder$Project(c("fec_lectura","val_ai"))
#scan_builder$Filter(Expression$field_ref("cups") == i)
scanner <- scan_builder$Finish()
a <- scanner$ToTable()

############### TEST stlf ###############

F_DAYS <- 7     ### number of days to forecast for STLF

ENG <- fread("enganches.csv")
ENG$ID_USUARIO <- CLEAN_ID(ENG$ID_USUARIO)
ENG$G3E_FID_CT <- CLEAN_ID(ENG$G3E_FID_CT)

cp <- fread("cp.csv", select = c("ID_LINEA_BT","ID_USUARIO","DIA_LECTURA","SUM_VAL_AI"))
cp$ID_LINEA_BT <- CLEAN_ID(cp$ID_LINEA_BT)
cp$ID_USUARIO  <- CLEAN_ID(cp$ID_USUARIO)
cp$DIA_LECTURA <- as.POSIXct(as.character(cp$DIA_LECTURA),format="%Y%m%d%H%M%S",tz="GMT")

z <- foreach(i = LBT) %dofuture% {
#   FILE <- strsplit(i,"/")[[1]][3]
#   TYPE <- strsplit(i,"/")[[1]][2]
#   ID   <- tools::file_path_sans_ext(FILE)

  r  <- cp[    cp$ID_LINEA_BT == i,]
  r  <- r[order(r$DIA_LECTURA),]

  fwrite(r[1:(F_DAYS*24),c("DIA_LECTURA","SUM_VAL_AI")],
         file=paste0("stlf/test/LBT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = CT) %dofuture% {
#   FILE  <- strsplit(i,"/")[[1]][3]
#   TYPE  <- strsplit(i,"/")[[1]][2]
#   ID    <- tools::file_path_sans_ext(FILE)

  r     <- cp[cp$ID_USUARIO == ENG$ID_USUARIO[ENG$G3E_FID_CT == i][1],]
  r     <- r[order(r$DIA_LECTURA),]

  aux   <- numeric(F_DAYS*24)
  for (j in unique(r$ID_LINEA_BT))
    aux <- aux + as.numeric(r[r$ID_LINEA_BT == j,"SUM_VAL_AI"][[1]][1:(F_DAYS*24)])

  fwrite(data.frame(DIA_LECTURA=unique(r$DIA_LECTURA)[1:(F_DAYS*24)],
                    SUM_VAL_AI=aux),
         file=paste0("stlf/test/CT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

############### TEST mtlf ###############

F_DAYS <- 7*4*3     ### number of days to forecast for STLF

ENG <- fread("enganches.csv")
ENG$ID_USUARIO <- CLEAN_ID(ENG$ID_USUARIO)
ENG$G3E_FID_CT <- CLEAN_ID(ENG$G3E_FID_CT)

mp <- fread("mp.csv", select = c("ID_LINEA_BT","ID_USUARIO","DIA_LECTURA",
                                 "SUM_VAL_AI","MAX_VAL_AI","MIN_VAL_AI"))
mp$ID_LINEA_BT <- CLEAN_ID(mp$ID_LINEA_BT)
mp$ID_USUARIO  <- CLEAN_ID(mp$ID_USUARIO)
mp$DIA_LECTURA <- as.POSIXct(as.character(mp$DIA_LECTURA),format="%Y%m%d",tz="GMT")

z <- foreach(i = LBT) %dofuture% {
#   FILE <- strsplit(i,"/")[[1]][3]
#   TYPE <- strsplit(i,"/")[[1]][2]
#   ID   <- tools::file_path_sans_ext(FILE)

  r  <- mp[    mp$ID_LINEA_BT == i,]
  r  <- r[order(r$DIA_LECTURA),]

  fwrite(r[1:(F_DAYS),c("DIA_LECTURA","SUM_VAL_AI","MAX_VAL_AI","MIN_VAL_AI")],
         file=paste0("mtlf/test/LBT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

z <- foreach(i = CT) %dofuture% {
#   FILE  <- strsplit(i,"/")[[1]][3]
#   TYPE  <- strsplit(i,"/")[[1]][2]
#   ID    <- tools::file_path_sans_ext(FILE)

  r  <- mp[mp$ID_USUARIO == ENG$ID_USUARIO[ENG$G3E_FID_CT == i][1],]
  r  <- r[order(r$DIA_LECTURA),]

  as <- aM <- am <- numeric(F_DAYS)
  for (j in unique(r$ID_LINEA_BT))
  {
    as <- as + as.numeric(r[r$ID_LINEA_BT == j,"SUM_VAL_AI"][[1]][1:F_DAYS])
    aM <- aM + as.numeric(r[r$ID_LINEA_BT == j,"MAX_VAL_AI"][[1]][1:F_DAYS])
    am <- am + as.numeric(r[r$ID_LINEA_BT == j,"MIN_VAL_AI"][[1]][1:F_DAYS])
  }

  fwrite(data.frame(DIA_LECTURA=unique(r$DIA_LECTURA)[1:F_DAYS],
                    SUM_VAL_AI=as,
                    MAX_VAL_AI=aM,
                    MIN_VAL_AI=am),
         file=paste0("mtlf/test/CT/",i,".csv",sep=""),dateTimeAs="write.csv")
}


