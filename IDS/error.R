library(arrow)
 
ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

cups   <- ARROW2DF("./inputdata/data/anm_ids01_punto_suministro")
cgp    <- ARROW2DF("./inputdata/data/anm_ids01_cgp")
linea  <- ARROW2DF("./inputdata/data/anm_ids01_linea_bt")
cuadro <- ARROW2DF("./inputdata/data/anm_ids01_cuadro_bt")
pos    <- ARROW2DF("./inputdata/data/anm_ids01_pos_trafo")
ct     <- ARROW2DF("./inputdata/data/anm_ids01_ct")
trafo  <- ARROW2DF("./inputdata/data/anm_ids01_trafo")

ROSETA <- merge(cups,  cgp,   by.x="COD_SIC_SIGRID",    by.y="ID_CAJA")
ROSETA <- merge(ROSETA,linea, by.x="ID_PADRE_LINEA_BT", by.y="G3E_FID")
ROSETA <- merge(ROSETA,cuadro,by.x="ID_PADRE_CUADRO_BT",by.y="G3E_FID")
ROSETA <- merge(ROSETA,trafo, by.x="ID_PADRE_POS_TRAFO",by.y="ID_PADRE_POS_TRAFO")



d    <- open_dataset(source="./anm_ids01_test_aggct_lec_horaria")
so   <- Scanner$create(d)
at   <- so$ToTable()
lec  <- as.data.frame(at)

lec$ID_USUARIO  <- str_replace(lec$ID_USUARIO,fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),"")
lec$ID_LINEA_BT <- str_replace(lec$ID_LINEA_BT,fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),"")

lec$ID_USUARIO  <- str_replace(lec$ID_USUARIO,fixed("/"),"\\")
lec$ID_LINEA_BT <- str_replace(lec$ID_LINEA_BT,fixed("/"),"\\")

fwrite(lec,"mtlf-test-values.csv",row.names=F)

d    <- open_dataset(source="./anm_ids01_punto_suministro")
so   <- Scanner$create(d)
at   <- so$ToTable()
cups <- as.data.frame(at)

d    <- open_dataset(source="./anm_ids01_cgp")
so   <- Scanner$create(d)
at   <- so$ToTable()
cgp  <- as.data.frame(at)

zzz <- merge(cups,cgp, by.x = "COD_SIC_SIGRID", by.y = "ID_CAJA")

yyy <- zzz[,c("CUPS","COD_SIC_SIGRID","ID_USUARIO_INST_PADRE")]
names(yyy) <- c("CUPS","CGP","CT")

