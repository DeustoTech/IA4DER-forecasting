library(data.table)
library(arrow)
library(doFuture)
library(stringr)
library(lubridate)

plan(multisession)

dir.create("post_cooked/SOLAR", showWarnings = F, recursive = T)

ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

VARS   <- c("FEC_LECTURA","VAL_AI","VAL_AE") # VAL_R1 VAL_R2 VAL_R3 VAL_R4

solar  <- ARROW2DF("../data/ids03_deteccion_autoconsumo_sin_vertido/inputdata/data/anm_ids03_aut_lec_horaria_val")
#solar  <- ARROW2DF("../data/ids03_deteccion_autoconsumo_sin_vertido/inputdata/data/anm_ids03_lec_horaria_val")

#solar  <- ARROW2DF("anm_ids03_lec_horaria_val")
#poliza <- ARROW2DF("anm_ids03_poliza_autoconsumo")

punto    <- ARROW2DF("anm_ids03_punto_suministro")
contrato <- ARROW2DF("anm_ids03_contrato")
poliza   <- ARROW2DF("anm_ids03_poliza")

# intersect(punto$COD_PS,contrato$COD_PS)
# intersect(poliza$COD_CONTRATO,contrato$COD_CONTRATO)

ROSETA <- merge(punto,contrato,by="COD_PS")
ROSETA <- merge(ROSETA,poliza,by="COD_CONTRATO")

NAMES <- unique(solar$CUPS)
B <- foreach(NAME = NAMES,
             .errorhandling = "remove",
             .combine=merge) %dofuture%
{
  aux <- solar[solar$CUPS == NAME,VARS]
  aux <- zooreg(aux[,c("VAL_AI","VAL_AE")],order.by=unique(aux$FEC_LECTURA))

  fwrite(data.frame(timestamp=index(aux),VAL_AI=aux$VAL_AI, VAL_AE=aux$VAL_AE),
         file=paste("post_cooked/SOLAR/",NAME,".csv",sep=""),
         dateTimeAs="write.csv",row.names=F)
  aux$VAL_AI
}

names(B) <- NAMES
fwrite(data.frame(timestamp=index(B),B),
       file="all_solar.csv",dateTimeAs="write.csv",row.names=F)
pdf(width=100,height=100,file="solar_plots.pdf")
  plot(B)
dev.off()

