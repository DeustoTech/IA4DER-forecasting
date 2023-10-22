library(data.table)
library(zoo)
library(forecast)
library(doFuture)

plan(multisession)

dir.create("post_cooked/CT",  showWarnings = F, recursive = T)
dir.create("post_cooked/LINE",showWarnings = F, recursive = T)

ROSETA <- fread("cups_per_lines_ct-v2.csv",header=T)

foreach(i = unique(ROSETA$CT)) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$CT == i]
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

  fwrite(a,file=paste("post_cooked/CT/",i,".csv",sep=""),dateTimeAs="write.csv")
}

foreach(i = unique(ROSETA$CGP)) %dofuture% {
  CUPS <- ROSETA$CUPS[ROSETA$CGP == i]
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

  fwrite(a,file=paste("post_cooked/LINE/",i,".csv",sep=""),dateTimeAs="write.csv")
}
