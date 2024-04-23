library(data.table)
library(arrow)
library(doFuture)
library(stringr)
library(zoo)
#library(lubridate)

plan(multisession)
options(future.globals.maxSize= 891289600)

dir.create("post_cooked/SOLAR", showWarnings = F, recursive = T)
dir.create("post_cooked/NOPV",  showWarnings = F, recursive = T)
dir.create("post_cooked/PV",    showWarnings = F, recursive = T)
dir.create("fig/SOLAR", showWarnings = F, recursive = T)

ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

base     <- "data/"

VARS     <- c("FEC_LECTURA","VAL_AI","VAL_AE") # VAL_R1 VAL_R2 VAL_R3 VAL_R4

lec      <- ARROW2DF(paste0(base,"anm_ids03_lec_horaria_val"))
aut      <- ARROW2DF(paste0(base,"anm_ids03_aut_lec_horaria_val"))

punto    <- ARROW2DF(paste0(base,"anm_ids03_punto_suministro"))
contrato <- ARROW2DF(paste0(base,"anm_ids03_contrato"))
poliza   <- ARROW2DF(paste0(base,"anm_ids03_poliza"))
#polizaut <- ARROW2DF(paste0(base,"anm_ids03_poliza_autoconsumo"))
pot      <- ARROW2DF(paste0(base,"anm_ids03_potencias_contratadas"))
gen      <- ARROW2DF(paste0(base,"anm_ids03_potencias_generacion"))

lec      <- lec[,c("CUPS",VARS)]
aut      <- aut[,c("CUPS",VARS)]

# intersect(punto$COD_PS,contrato$COD_PS)
# intersect(poliza$COD_CONTRATO,contrato$COD_CONTRATO)

poliza <- poliza[,c("COD_CONTRATO","COD_TARIF_IBDLA","TIP_PUNTO_MEDIDA")]
pot    <- pot[,   c("COD_CONTRATO","CAN_POT_CTD")]
gen    <- gen[,   c("COD_PS","POT_GRUPO")]

ROSETA <- merge(punto,contrato,by="COD_PS")
ROSETA <- merge(ROSETA,poliza, by="COD_CONTRATO")
ROSETA <- merge(ROSETA,pot,    by="COD_CONTRATO")
ROSETA <- merge(ROSETA,gen,    by="COD_PS")

#NAME <- NAMES[1]
NAMES <- union(unique(aut$CUPS),unique(lec$CUPS))
B <- foreach(NAME = NAMES,
             .errorhandling = "remove",
             .combine=merge) %dofuture%
{
  a1 <- lec[lec$CUPS == NAME,VARS]
  a2 <- aut[aut$CUPS == NAME,VARS]

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

    fwrite(data.frame(timestamp=index(a1),VAL_AI=a1$VAL_AI,
                      VAL_AE=a1$VAL_AE, AUTO=a1$AUTO),
          file=paste("post_cooked/NOPV/",NAME,".csv",sep=""),
          dateTimeAs="write.csv",row.names=F)
    fwrite(data.frame(timestamp=index(a2),VAL_AI=a2$VAL_AI,
                      VAL_AE=a2$VAL_AE, AUTO=a2$AUTO),
          file=paste("post_cooked/PV/",NAME,".csv",sep=""),
          dateTimeAs="write.csv",row.names=F)

  } else { if (length(a1) != 0)
    {
      a1$AUTO <- 0
      aux     <- a1
      col     <- "red"
      fwrite(data.frame(timestamp=index(a1),VAL_AI=a1$VAL_AI,
                        VAL_AE=a1$VAL_AE, AUTO=a1$AUTO),
            file=paste("post_cooked/NOPV/",NAME,".csv",sep=""),
            dateTimeAs="write.csv",row.names=F)
          
    } else if (length(a2) != 0)
    {
      a2$AUTO <- 1
      aux     <- a2
      col     <- "blue"
      fwrite(data.frame(timestamp=index(a2),VAL_AI=a2$VAL_AI,
                        VAL_AE=a2$VAL_AE, AUTO=a2$AUTO),
            file=paste("post_cooked/PV/",NAME,".csv",sep=""),
            dateTimeAs="write.csv",row.names=F)
    }
  }

  fwrite(data.frame(timestamp=index(aux),VAL_AI=aux$VAL_AI,
                    VAL_AE=aux$VAL_AE, AUTO=aux$AUTO),
         file=paste("post_cooked/SOLAR/",NAME,".csv",sep=""),
         dateTimeAs="write.csv",row.names=F)
  pdf(width=10,paste("fig/SOLAR/",NAME,".pdf",sep=""),)
    try(plot(aux,col=col,main=NAME))
  dev.off()

  aux$VAL_AI
}

names(B) <- NAMES
fwrite(data.frame(timestamp=index(B),B),
       file="all_solar.csv",dateTimeAs="write.csv",row.names=F)
pdf(width=100,height=100,file="solar_plots.pdf")
  plot(B[,001:100])
  plot(B[,101:200])
  plot(B[,201:300])
  plot(B[,301:400])
  plot(B[,401:500])
  plot(B[,501:600])
  plot(B[,601:700])
  plot(B[,701:800])
  plot(B[,801:900])
  plot(B[,901:983])
dev.off()

### features
# longitud
# estadisticos básicos
# energia consumida y energia generada
# numero de 0 y NA
# fecha de cambio
# fecha de primera inyeccion
# entropia

# library(arrow)
# library(dplyr)
# 
# ds <- open_dataset("./inputdata/universidad_deusto/anm_ids01_test_lec_horaria_val/")
# 
# write_dataset(dataset = ds %>% select(all_of(c("CUPS",VARS))),
# path = "test",
# hive_style = F,
# max_partitions = 2048,
# partitioning = "CUPS",
# format = "csv"
# )
