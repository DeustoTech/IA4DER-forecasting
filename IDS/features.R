library(data.table)
library(arrow)
library(doFuture)
library(stringr)

plan(multisession)

ALL  <- Sys.glob(paths="post_cooked/*/*")


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

cgp$ID_CAJA <- str_replace(cgp$ID_CAJA, fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
cgp$ID_CAJA <- str_replace(cgp$ID_CAJA, fixed("/"),fixed("\\"))

ROSETA$ID_USUARIO_INST_PADRE <- str_replace_all(ROSETA$ID_USUARIO_INST_PADRE, fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
ROSETA$ID_USUARIO_INST_PADRE <- str_replace_all(ROSETA$ID_USUARIO_INST_PADRE, fixed("/"),fixed("\\"))

LIM <- data.frame(ID=character(),POT_NOM=numeric())
for (z in unique(ROSETA$ID_USUARIO_INST_PADRE))
  LIM[z,"POT_NOM"] <- ROSETA$POT_NOMI_TRAFO[which(ROSETA$ID_USUARIO_INST_PADRE == z)][1]

LIM$ID <- row.names(LIM)
row.names(LIM) <- NULL

LIM <- rbind(LIM,data.frame(ID=cups$CUPS,  POT_NOM=cups$VAL_POT_AUTORIZADA/1000))
LIM <- rbind(LIM,data.frame(ID=cgp$ID_CAJA,POT_NOM=(cgp$COD_INT_NOMI_FUSIB * cgp$COD_TENS_SUMI)))
ID  <- tools::file_path_sans_ext(matrix(unlist(strsplit(ALL,"/")),nrow=3)[3,])

LIM <- LIM[LIM$ID %in% ID,]

B <- foreach(NAME = ALL,
             .combine = rbind,
             .errorhandling = "remove") %dofuture% { 
  a <- fread(NAME)
  
  FILE   <- strsplit(NAME,"/")[[1]][3]
  TYPE   <- strsplit(NAME,"/")[[1]][2]
  ID     <- tools::file_path_sans_ext(FILE)
  LENGTH <- length(a$kWh)
  POT    <- LIM$POT_NOM[LIM$ID == ID]
  QQ     <- as.numeric(quantile(a$kWh,c(0,0.25,0.5,0.75,1),na.rm=T))
  
  aux <- data.frame(
           ID=     FILE,
           TYPE=   TYPE,
           POT=    ifelse(length(POT)==0,NA,POT),
           LENGTH= LENGTH,
           ZERO=   sum(a$kWh==0)/LENGTH,
           IMPUTED=sum(a$issue)/LENGTH,
           AVG=    mean(a$kWh,na.rm=T),
           SD=     sd(a$kWh,na.rm=T),
           MIN=    QQ[1],
           Q1=     QQ[2],
           MEDIAN= QQ[3],
           Q3=     QQ[4],
           MAX=    QQ[5]
           )
}

write.csv(B,file="features.csv")

plot(ecdf(B$ZERO))
plot(ecdf(B$IMPUTED))
plot(ecdf(B$MAX/B$POT))
ecdf(B$MAX/B$POT)(0.8)
