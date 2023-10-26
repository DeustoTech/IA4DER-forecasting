library(data.table)
library(zoo)
library(forecast)
library(doFuture)


out_dir <- "TransformersV2/" # carpeta de salida
ind_dir <- "TransformersV2/" # carpeta de entrada



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
      z <- fread(file.path(ind_dir, paste(j, sep = "")), data.table = FALSE) # Lee desde la carpeta input_dir
      a$kWh   <- a$kWh   + z$kWh
      # a$issue <- a$issue/N_SAMPLES[i] + z$issue/N_SAMPLES[i]
    }

    output_file <- file.path(out_dir, paste(i, "-", TYPE, ".csv", sep = ""))
    fwrite(a, file = output_file, dateTimeAs = "write.csv")
  }
}

ALL    <- list.files(path="post_cooked/",pattern="*.csv")
OTHERS <- list.files(path="post_cooked/",pattern="-")
FILES  <- setdiff(ALL,OTHERS)


ALL <- list.files(path = ind_dir,pattern="*.csv")
OTHERS <- list.files(path = ind_dir,pattern="-")
CT  <- list.files(path = ind_dir,pattern="*-CT.csv")
L  <- list.files(path = ind_dir,pattern="*-L.csv")
FILES  <- setdiff(ALL,OTHERS)
# FILES <- setdiff(ALL,c(CT, L))


set.seed(42)

N_CT          <- 2000 ### number of transformes to create
N_LINES       <- 2000 ### number of lines to create
CUPS_PER_CT   <- 1000 ### number of supply points per transformer
CUPS_PER_LINE <- 50   ### number of supply points per line
SAMPLES_CT    <- sapply(rep(0,N_CT),   function(X) {sample(floor(CUPS_PER_CT/10): CUPS_PER_CT,1)})
SAMPLES_LINE  <- sapply(rep(0,N_LINES),function(X) {sample(floor(CUPS_PER_LINE/3):CUPS_PER_LINE,1)})

z <- create_time_series(N_CT,   FILES,SAMPLES_CT,  "CT") # la - ya lo pone ello solo
z <- create_time_series(N_LINES,FILES,SAMPLES_LINE,"L")

