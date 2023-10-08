library(data.table)
library(zoo)
library(forecast)
library(doFuture)

plan(multisession)

TRANSFORMERS<- 2000  ### number of transformers to create
CUPS_PER    <- 1000  ### number of supply points per transformer

ALL <- list.files(path="post_cooked/",pattern="*.csv")
CT  <- list.files(path="post_cooked/",pattern="*CT.csv")

FILES <- setdiff(ALL,CT)

CT <- foreach(i = 1:TRANSFORMERS,.options.future = list(seed = TRUE)) %dofuture% {
  CUPS <- sample(FILES,sample(floor(CUPS_PER/10):CUPS_PER,1))

  a <- data.frame(time=seq(ISOdate(2021,06,01,00,00,00),length.out=24*457,by="hour"), 
             kWh=double(24*457),
             issue=double(24*457))
  
  for (j in CUPS)
  {
    z <- fread(paste("post_cooked/",j,sep=""),data.table=F)
    a$kWh   <- a$kWh   + z$kWh
    a$issue <- a$issue/CUPS_PER + z$issue/CUPS_PER
  }

  fwrite(a,file=paste("post_cooked/",i,"-CT.csv",sep=""),dateTimeAs="write.csv")
}


