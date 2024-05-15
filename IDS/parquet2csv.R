library(data.table)
library(arrow)
library(doFuture)
library(stringr)
library(zoo)

ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

#IDS1
dir.create("./CSV/ids01_prevision_demanda", showWarnings = F, recursive = T)
base    <- "./ids01_prevision_demanda/data/"
exclude <- c("anm_ids01_lec_horaria_val")
output  <- "./CSV/ids01_prevision_demanda/"

for (i in setdiff(dir(path = base),exclude))
  fwrite(ARROW2DF(paste0(base,i)),file=paste0(output,i,".csv",sep=""))

#IDS3
dir.create("./CSV/ids03_deteccion_autoconsumo_sin_vertido", showWarnings = F, recursive = T)
base    <- "./ids03_deteccion_autoconsumo_sin_vertido/data/"
exclude <- c("anm_ids03_lec_horaria_val","anm_ids03_aut_lec_horaria_val")
output  <- "./CSV/ids03_deteccion_autoconsumo_sin_vertido/"

for (i in setdiff(dir(path = base),exclude))
  fwrite(ARROW2DF(paste0(base,i)),file=paste0(output,i,".csv",sep=""))

#IDS4
dir.create("./CSV/ids04_descubrimiento_red", showWarnings = F, recursive = T)
base    <- "./ids04_descubrimiento_red/data/"
exclude <- "Eventos Telegestión.xlsx"
output  <- "./CSV/ids04_descubrimiento_red/"

for (i in setdiff(dir(path = base),exclude))
  fwrite(ARROW2DF(paste0(base,i)),file=paste0(output,i,".csv",sep=""))
