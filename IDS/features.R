library(data.table)
library(arrow)
library(doFuture)
library(stringr)

plan(multisession)

ALL <- Sys.glob(paths="post_cooked/*/*")

MC  <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error

ARROW2DF <- function(SOURCE)
{
  d    <- open_dataset(source=SOURCE)
  so   <- Scanner$create(d)
  at   <- so$ToTable()
  return(as.data.frame(at))
}

CLEAN_ID <- function(X)
{
  X <- str_replace(X,fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
  X <- str_replace(X,fixed("/"),fixed("\\"))
  return(X)
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

cgp$ID_CAJA                  <- CLEAN_ID(cgp$ID_CAJA)
ROSETA$ID_USUARIO_INST_PADRE <- CLEAN_ID(ROSETA$ID_USUARIO_INST_PADRE)
cups$COD_SIC_SIGRID          <- CLEAN_ID(cups$COD_SIC_SIGRID)

LIM <- data.frame(ID=character(),POT_NOM=numeric())
for (z in unique(ROSETA$ID_USUARIO_INST_PADRE))
  LIM[z,"POT_NOM"] <- ROSETA$POT_NOMI_TRAFO[which(ROSETA$ID_USUARIO_INST_PADRE == z)][1]

LIM$ID <- row.names(LIM)
row.names(LIM) <- NULL

LIM <- rbind(LIM,data.frame(ID=cups$CUPS,  POT_NOM=cups$VAL_POT_AUTORIZADA/1000))
LIM <- rbind(LIM,data.frame(ID=cgp$ID_CAJA,POT_NOM=(cgp$COD_INT_NOMI_FUSIB * cgp$COD_TENS_SUMI)))
ID  <- tools::file_path_sans_ext(matrix(unlist(strsplit(ALL,"/")),nrow=3)[3,])

LIM     <- LIM[LIM$ID %in% ID,]

LIM_EST_CUPS        <- LIM[LIM$ID %in% cups$CUPS,]
LIM_EST_CUPS$TYPE   <- "CUPS"
names(LIM_EST_CUPS) <- c("ID","POT_EST","TYPE")

LIM_EST_LINE <- foreach(i = unique(cgp$ID_CAJA),
             .combine = rbind,
             .errorhandling = "remove") %dofuture% {
  data.frame(ID = i, POT_EST = sum(cups$VAL_POT_AUTORIZADA[cups$COD_SIC_SIGRID == i]/1000),TYPE="LINE")
}

LIM_EST_CT <- foreach(i = unique(ROSETA$ID_USUARIO_INST_PADRE),
             .combine = rbind,
             .errorhandling = "remove") %dofuture% {
  data.frame(ID = i, POT_EST = sum(cups$VAL_POT_AUTORIZADA[ROSETA$ID_USUARIO_INST_PADRE == i]/1000),TYPE="CT")
}

LIM_EST <- rbind(LIM_EST_CUPS,LIM_EST_LINE,LIM_EST_CT)
LIM     <- merge(LIM,LIM_EST,by="ID")

B <- foreach(NAME = ALL,
             .combine = rbind,
             .errorhandling = "remove") %dofuture% { 
  a <- fread(NAME)
  
  FILE   <- strsplit(NAME,"/")[[1]][3]
  TYPE   <- strsplit(NAME,"/")[[1]][2]
  ID     <- tools::file_path_sans_ext(FILE)
  LENGTH <- length(a$kWh)
  POT_NOM<- LIM$POT_NOM[LIM$ID == ID]
  POT_EST<- LIM$POT_EST[LIM$ID == ID]

  QQ     <- as.numeric(quantile(a$kWh,c(0,0.25,0.5,0.75,1),na.rm=T))
  ECDF   <- ecdf(a$kWh)(MC*POT_NOM)
 
  aux <- data.frame(
           ID=     ID,
           TYPE=   TYPE,
           POT_NOM=ifelse(length(POT_NOM)==0,NA,POT_NOM),
           POT_EST=ifelse(length(POT_EST)==0,NA,POT_EST),
           LENGTH= LENGTH,
           ZERO=   sum(a$kWh==0)/LENGTH,
           IMPUTED=sum(a$issue)/LENGTH,
           AVG=    mean(a$kWh,na.rm=T),
           SD=     sd(a$kWh,na.rm=T),
           MIN=    QQ[1],
           Q1=     QQ[2],
           MEDIAN= QQ[3],
           Q3=     QQ[4],
           MAX=    QQ[5],
           MC25=   ECDF[1],
           MC50=   ECDF[2],
           MC80=   ECDF[3],
           MC90=   ECDF[4],
           MC95=   ECDF[5]
           )
}

write.csv(B,file="features.csv",row.names = F)

plot(ecdf(B$ZERO))
plot(ecdf(B$IMPUTED))
plot(ecdf(B$MAX/B$POT_NOM))
ecdf(B$MAX/B$POT_NOM)(0.8)

plot(ecdf(B$MAX[B$TYPE=="CUPS"]/B$POT_NOM[B$TYPE=="CUPS"]))
plot(ecdf(B$MAX[B$TYPE=="LINE"] /B$POT_NOM[B$TYPE=="LINE"]))
plot(ecdf(B$MAX[B$TYPE=="CT"]  /B$POT_NOM[B$TYPE=="CT"]))

plot(ecdf(B$MAX[B$TYPE=="CUPS"]/B$POT_EST[B$TYPE=="CUPS"]))
plot(ecdf(B$MAX[B$TYPE=="LINE"] /B$POT_EST[B$TYPE=="LINE"]))
plot(ecdf(B$MAX[B$TYPE=="CT"]  /B$POT_EST[B$TYPE=="CT"]))

boxplot(B$POT_NOM[B$TYPE=="CUPS"],B$POT_EST[B$TYPE=="CUPS"],outline=F)
boxplot(B$POT_NOM[B$TYPE=="LINE"], B$POT_EST[B$TYPE=="LINE"],outline=F)
boxplot(B$POT_NOM[B$TYPE=="CT"],  B$POT_EST[B$TYPE=="CT"],outline=F)

boxplot(B$POT_NOM[B$TYPE=="CT"]/B$POT_EST[B$TYPE=="CT"])

summary(B$POT_NOM[B$TYPE=="CT"]/B$POT_EST[B$TYPE=="CT"])
summary(B$POT_NOM[B$TYPE=="LINE"]/B$POT_EST[B$TYPE=="LINE"])

boxplot(B$MC25,B$MC50,B$MC80,B$MC90,B$MC95,outline=F)
summary(data.frame(B$MC25,B$MC50,B$MC80,B$MC90,B$MC95))
