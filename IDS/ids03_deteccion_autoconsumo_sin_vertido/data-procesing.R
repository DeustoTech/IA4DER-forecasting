library(data.table)
library(arrow)
library(doFuture)
library(stringr)
library(zoo)
#library(lubridate)

plan(multisession)

dir.create("post_cooked/SOLAR", showWarnings = F, recursive = T)
dir.create("fig/SOLAR", showWarnings = F, recursive = T)

ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

base     <- "../data/ids03_deteccion_autoconsumo_sin_vertido/inputdata/data/"

VARS     <- c("FEC_LECTURA","VAL_AI","VAL_AE") # VAL_R1 VAL_R2 VAL_R3 VAL_R4

lec      <- ARROW2DF(paste0(base,"anm_ids03_aut_lec_horaria_val"))
aut      <- ARROW2DF(paste0(base,"anm_ids03_lec_horaria_val"))
punto    <- ARROW2DF(paste0(base,"anm_ids03_punto_suministro"))
contrato <- ARROW2DF(paste0(base,"anm_ids03_contrato"))
poliza   <- ARROW2DF(paste0(base,"anm_ids03_poliza"))

# intersect(punto$COD_PS,contrato$COD_PS)
# intersect(poliza$COD_CONTRATO,contrato$COD_CONTRATO)

ROSETA <- merge(punto,contrato,by="COD_PS")
ROSETA <- merge(ROSETA,poliza,by="COD_CONTRATO")

#NAME <- NAMES[1]
NAMES <- union(unique(aut$CUPS),unique(lec$CUPS))
B <- foreach(NAME = NAMES,
             .errorhandling = "remove",
             .combine=merge) %dofuture%
{
  a1 <- aut[aut$CUPS == NAME,VARS]
  a2 <- lec[lec$CUPS == NAME,VARS]

  a1 <- zooreg(a1[,-1],order.by=unique(as.POSIXct(a1$FEC_LECTURA)))
  a2 <- zooreg(a2[,-1],order.by=unique(as.POSIXct(a2$FEC_LECTURA)))

  col <- "Black"
  aux <- zooreg()
  if ((length(a1) != 0) & (length(a2) != 0))
  {
    a1$AUTO <- 0
    a2$AUTO <- 1

    fin <- last(index(a1))
    if(!last(index(a1)) < first(index(a2)))
      fin <- index(a2)[1]-60*60
    aux <- c(window(a1,end=fin),a2)
  } else { if (length(a1) != 0)
    {
      a1$AUTO <- 0
      aux     <- a1
      col     <- "red"
    } else if (length(a2) != 0)
    {
      a2$AUTO <- 1
      aux     <- a2
      col     <- "blue"
    }
  }

  fwrite(data.frame(timestamp=index(aux),VAL_AI=aux$VAL_AI,
                    VAL_AE=aux$VAL_AE, AUTO=aux$AUTO),
         file=paste("post_cooked/SOLAR/",NAME,".csv",sep=""),
         dateTimeAs="write.csv",row.names=F)
  pdf(paste("fig/SOLAR/",NAME,".pdf",sep=""))
    plot(aux,col=col,main=NAME)
  dev.off()

  aux$VAL_AI
}

names(B) <- NAMES
fwrite(data.frame(timestamp=index(B),B),
       file="all_solar.csv",dateTimeAs="write.csv",row.names=F)
pdf(width=100,height=100,file="solar_plots.pdf")
  plot(B)
dev.off()

### features
# longitud
# estadisticos básicos
# energia consumida y energia generada
# numero de 0 y NA
# fecha de cambio
# fecha de primera inyeccion
# entropia

