library(data.table)
library(zoo)
library(forecast)
library(doFuture)

plan(multisession)

create_time_series <- function(NUMBER_TS,UNIVERSE_TS,N_SAMPLES,TYPE)
{
  foreach(i = 1:NUMBER_TS,.options.future = list(seed = TRUE)) %dofuture% {
    CUPS <- sample(UNIVERSE_TS,N_SAMPLES[i])

    a <- data.frame(time=seq(ISOdate(2021,06,01,00,00,00),length.out=24*457,by="hour"),
                    kWh=double(24*457),
                    issue=double(24*457))

    for (j in CUPS)
    {
      z <- fread(paste("post_cooked/",j,sep=""),data.table=F)
      a$kWh   <- a$kWh   + z$kWh
      a$issue <- a$issue/N_SAMPLES[i] + z$issue/N_SAMPLES[i]
    }

    fwrite(a,file=paste("post_cooked/",i,"-",TYPE,".csv",sep=""),dateTimeAs="write.csv")
  }
}

ALL    <- list.files(path="post_cooked/",pattern="*.csv")
OTHERS <- list.files(path="post_cooked/",pattern="-")
FILES  <- setdiff(ALL,OTHERS)

N_CT          <- 2000 ### number of transformes to create
N_LINES       <- 2000 ### number of lines to create
CUPS_PER_CT   <- 1000 ### number of supply points per transformer
CUPS_PER_LINE <- 50   ### number of supply points per line
SAMPLES_CT    <- sapply(rep(0,N_CT),   function(X) {sample(floor(CUPS_PER_CT/10): CUPS_PER_CT,1)})
SAMPLES_LINE  <- sapply(rep(0,N_LINES),function(X) {sample(floor(CUPS_PER_LINE/3):CUPS_PER_LINE,1)})

z <- create_time_series(N_CT,   FILES,SAMPLES_CT,  "CT")
z <- create_time_series(N_LINES,FILES,SAMPLES_LINE,"LINE")
