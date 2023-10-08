library(data.table)
library(zoo)
library(forecast)
library(doFuture)

TRANSFORMERS<- 2000  ### number of transformers to create
CUPS_PER    <- 1000  ### number of supply points per transformer
FILES       <- list.files(path="post_cooked/",pattern="*.csv")

CT <- foreach(i = 1:TRANSFORMERS,.options.future = list(seed = TRUE)) %dofuture% {
  CUPS <- sample(FILES,CUPS_PER)

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


