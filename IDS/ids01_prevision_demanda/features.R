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
  X <- str_replace(X,fixed("33TCP+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
  X <- str_replace_all(X,fixed("/"),fixed("\\"))
  return(X)
}

cups   <- ARROW2DF("./inputdata/data/anm_ids01_punto_suministro")
cgp    <- ARROW2DF("./inputdata/data/anm_ids01_cgp")
linea  <- ARROW2DF("./inputdata/data/anm_ids01_linea_bt")
cuadro <- ARROW2DF("./inputdata/data/anm_ids01_cuadro_bt")
pos    <- ARROW2DF("./inputdata/data/anm_ids01_pos_trafo")
trafo  <- ARROW2DF("./inputdata/data/anm_ids01_trafo")
ct     <- ARROW2DF("./inputdata/data/anm_ids01_ct")
punto  <- ARROW2DF("./inputdata/data/anm_ids01_punto_suministro")
contra <- ARROW2DF("./inputdata/data/anm_ids01_contrato")
pol    <- ARROW2DF("./inputdata/data/anm_ids01_poliza")
auto   <- ARROW2DF("./inputdata/data/anm_ids01_poliza_autoconsumo")
pot    <- ARROW2DF("./inputdata/data/anm_ids01_potencias_contratadas")

contra <- contra[,c("COD_CONTRATO","COD_PS","COD_CNAE")]
pol    <- pol[,c("COD_CONTRATO","COD_TARIF_IBDLA")]
auto   <- auto[,c("COD_CONTRATO","TIP_AUTOCONSUMO")]
pot    <- pot[,c("COD_CONTRATO","CAN_POT_CTD")]

ROSETA <- merge(cups,  cgp,   by.x="COD_SIC_SIGRID",    by.y="ID_CAJA")
ROSETA <- merge(ROSETA,linea, by.x="ID_PADRE_LINEA_BT", by.y="G3E_FID")
ROSETA <- merge(ROSETA,cuadro,by.x="ID_PADRE_CUADRO_BT",by.y="G3E_FID")
ROSETA <- merge(ROSETA,pos,   by.x="ID_PADRE_POS_TRAFO",by.y="G3E_FID")
ROSETA <- merge(ROSETA,trafo, by.x="ID_PADRE_POS_TRAFO",by.y="G3E_FID")
ROSETA <- merge(ROSETA,ct,    by.x="ID_PADRE_CT",       by.y="G3E_FID")
ROSETA <- merge(ROSETA,contra,by="COD_PS")
ROSETA <- merge(ROSETA,pol,   by="COD_CONTRATO")
#ROSETA <- merge(ROSETA,auto,  by="COD_CONTRATO")
ROSETA <- merge(ROSETA,pot,   by="COD_CONTRATO")

ROSETA <- ROSETA[!duplicated(ROSETA$CUPS),]

ROSETA$TIP_SUMINISTRO[ROSETA$TIP_SUMINISTRO == "  "] <- NA

ROSETA$COD_SIC_SIGRID        <- CLEAN_ID(ROSETA$COD_SIC_SIGRID)
ROSETA$ID_PADRE_LINEA_BT     <- CLEAN_ID(ROSETA$ID_PADRE_LINEA_BT)
ROSETA$ID_PADRE_CUADRO_BT    <- CLEAN_ID(ROSETA$ID_PADRE_CUADRO_BT)
ROSETA$ID_PADRE_POS_TRAFO    <- CLEAN_ID(ROSETA$ID_PADRE_POS_TRAFO)
ROSETA$ID_PADRE_CT           <- CLEAN_ID(ROSETA$ID_PADRE_CT)
ROSETA$COD_CNAE              <- substring(ROSETA$COD_CNAE,1,1)
ROSETA$COD_CNAE              <- factor(ROSETA$COD_CNAE)
ROSETA$COD_TARIF_IBDLA       <- factor(ROSETA$COD_TARIF_IBDLA)
ROSETA$COD_PROVINCIA         <- factor(ROSETA$COD_PROVINCIA)
ROSETA$TIP_SUMINISTRO        <- factor(ROSETA$TIP_SUMINISTRO)

ROSETA$CNAE  <- 1
ROSETA$TARIF <- 1
ROSETA$PROV  <- 1
ROSETA$SUM   <- 1

ACNAE  <- reshape(ROSETA[,c("CUPS","COD_CNAE","CNAE")],         idvar="CUPS", timevar="COD_CNAE",        direction="wide")
ATARIF <- reshape(ROSETA[,c("CUPS","COD_TARIF_IBDLA","TARIF")], idvar="CUPS", timevar="COD_TARIF_IBDLA", direction="wide")
APROV  <- reshape(ROSETA[,c("CUPS","COD_PROVINCIA","PROV")],    idvar="CUPS", timevar="COD_PROVINCIA",   direction="wide")
ASUM   <- reshape(ROSETA[,c("CUPS","TIP_SUMINISTRO","SUM")],    idvar="CUPS", timevar="TIP_SUMINISTRO",  direction="wide")

ROSETA <- merge(ROSETA,ACNAE)
ROSETA <- merge(ROSETA,ATARIF)
ROSETA <- merge(ROSETA,APROV)
ROSETA <- merge(ROSETA,ASUM)

ROSETA <- subset(ROSETA,select=-c(CNAE,TARIF,PROV,SUM))

LIM <- data.frame(ID=character(),POT_NOM=numeric())
for (z in unique(ROSETA$ID_PADRE_CT))
  LIM[z,"POT_NOM"] <- ROSETA$POT_NOMI_TRAFO[which(ROSETA$ID_PADRE_CT == z)][1]
LIM$ID <- row.names(LIM)
row.names(LIM) <- NULL

LIM <- rbind(LIM,data.frame(ID=cups$CUPS,  POT_NOM=cups$VAL_POT_AUTORIZADA/1000))
LIM <- rbind(LIM,data.frame(ID=cgp$ID_CAJA,POT_NOM=(cgp$COD_INT_NOMI_FUSIB * cgp$COD_TENS_SUMI)))

ID  <- tools::file_path_sans_ext(matrix(unlist(strsplit(ALL,"/")),nrow=3)[3,])
LIM <- LIM[LIM$ID %in% ID,]

VAR <- c("CUPS","CAN_POT_CTD","VAL_POT_AUTORIZADA",
         "COD_CNAE","COD_TARIF_IBDLA","COD_PROVINCIA","TIP_SUMINISTRO")

LCNAE   <- levels(ROSETA$COD_CNAE)
LTARIF  <- levels(ROSETA$COD_TARIF_IBDLA)
LPROV   <- levels(ROSETA$COD_PROVINCIA)
LSUM    <- levels(ROSETA$TIP_SUMINISTRO)
LASS    <- levels(factor(c("CUPS","CGP","LBT","CUA","TR","CT","SOLAR")))

LIM_EST_CUPS                     <- ROSETA[,VAR]
LIM_EST_CUPS                     <- LIM_EST_CUPS[LIM_EST_CUPS$CUPS %in% cups$CUPS,]
LIM_EST_CUPS$SHARP               <- 1
LIM_EST_CUPS$COD_CNAE            <- factor(LIM_EST_CUPS$COD_CNAE,       levels=LCNAE)
LIM_EST_CUPS$COD_TARIF_IBDLA     <- factor(LIM_EST_CUPS$COD_TARIF_IBDLA,levels=LTARIF)
LIM_EST_CUPS$COD_PROVINCIA       <- factor(LIM_EST_CUPS$COD_PROVINCIA,  levels=LPROV)
LIM_EST_CUPS$TIP_SUMINISTRO      <- factor(LIM_EST_CUPS$TIP_SUMINISTRO, levels=LSUM)
LIM_EST_CUPS$ASS                 <- factor("CUPS",                      levels=LASS)
LIM_EST_CUPS$CAN_POT_CTD         <- LIM_EST_CUPS$CAN_POT_CTD/1000
LIM_EST_CUPS$VAL_POT_AUTORIZADA  <- LIM_EST_CUPS$VAL_POT_AUTORIZADA/1000
names(LIM_EST_CUPS)              <- c("ID","POT_CON","POT_EST","CNAE","TARIF","PROV","SUM","SHARP","ASS")

LIM_EST_SOLAR                    <- fread("roseta-solar.csv")
POT_GRUPO                        <- data.frame(ID=LIM_EST_SOLAR$CUPS,POT_AUT=LIM_EST_SOLAR$POT_GRUPO/1000)
POT_GRUPO                        <- POT_GRUPO[!duplicated(POT_GRUPO$ID),]
LIM_EST_SOLAR                    <- LIM_EST_SOLAR[,c("CUPS","CAN_POT_CTD","VAL_POT_AUTORIZADA",
                                    "COD_CNAE", "COD_TARIF_IBDLA","TIP_SUMINISTRO")]
LIM_EST_SOLAR$SHARP              <- 1
LIM_EST_SOLAR$COD_CNAE           <- substring(LIM_EST_SOLAR$COD_CNAE,1,1)
LIM_EST_SOLAR$PROV               <- NA
LIM_EST_SOLAR$ASS                <- factor("CUPS", levels=LASS)
LIM_EST_SOLAR$CAN_POT_CTD        <- LIM_EST_SOLAR$CAN_POT_CTD/1000
LIM_EST_SOLAR$VAL_POT_AUTORIZADA <- LIM_EST_SOLAR$VAL_POT_AUTORIZADA/1000
names(LIM_EST_SOLAR)             <- c("ID","POT_CON","POT_EST","CNAE","TARIF","SUM","SHARP","PROV","ASS")
setcolorder(LIM_EST_SOLAR,names(LIM_EST_CUPS))
LIM_EST_CUPS                     <- rbindlist(list(LIM_EST_SOLAR ,LIM_EST_CUPS))
LIM_EST_CUPS                     <- merge(LIM_EST_CUPS,POT_GRUPO,by="ID",all=TRUE)
rm(LIM_EST_SOLAR,POT_GRUPO)

LIM_EST_CGP <- foreach(i = unique(ROSETA$COD_SIC_SIGRID),.combine = rbind,.errorhandling = "remove") %dofuture% {
  aux  <- unique(ROSETA$CUPS[ROSETA$COD_SIC_SIGRID == i])
  data.frame(ID = i, ASS="CGP", POT_EST = sum(LIM_EST_CUPS$POT_EST[LIM_EST_CUPS$ID %in% aux]),
                                POT_CON = sum(LIM_EST_CUPS$POT_CON[LIM_EST_CUPS$ID %in% aux]),
                                POT_AUT = sum(LIM_EST_CUPS$POT_AUT[LIM_EST_CUPS$ID %in% aux]),
                                SHARP   = length(aux))
}
LIM_EST_LBT <- foreach(i = unique(ROSETA$ID_PADRE_LINEA_BT),.combine = rbind,.errorhandling = "remove") %dofuture% {
  aux <- unique(ROSETA$CUPS[ROSETA$ID_PADRE_LINEA_BT == i])
  data.frame(ID = i, ASS="LBT", POT_EST = sum(LIM_EST_CUPS$POT_EST[LIM_EST_CUPS$ID %in% aux]),
                                POT_CON = sum(LIM_EST_CUPS$POT_CON[LIM_EST_CUPS$ID %in% aux]),
                                POT_AUT = sum(LIM_EST_CUPS$POT_AUT[LIM_EST_CUPS$ID %in% aux]),
                                SHARP   = length(aux))
}
LIM_EST_CUA <- foreach(i = unique(ROSETA$ID_PADRE_CUADRO_BT),.combine = rbind,.errorhandling = "remove") %dofuture% {
  aux <- unique(ROSETA$CUPS[ROSETA$ID_PADRE_CUADRO_BT == i])
  data.frame(ID = i, ASS="CUA", POT_EST = sum(LIM_EST_CUPS$POT_EST[LIM_EST_CUPS$ID %in% aux]),
                                POT_CON = sum(LIM_EST_CUPS$POT_CON[LIM_EST_CUPS$ID %in% aux]),
                                POT_AUT = sum(LIM_EST_CUPS$POT_AUT[LIM_EST_CUPS$ID %in% aux]),
                                SHARP   = length(aux))
}
LIM_EST_POS <- foreach(i = unique(ROSETA$ID_PADRE_POS_TRAFO),.combine = rbind,.errorhandling = "remove") %dofuture% {
  aux <- unique(ROSETA$CUPS[ROSETA$ID_PADRE_POS_TRAFO == i])
  data.frame(ID = i, ASS="TR",  POT_EST = sum(LIM_EST_CUPS$POT_EST[LIM_EST_CUPS$ID %in% aux]),
                                POT_CON = sum(LIM_EST_CUPS$POT_CON[LIM_EST_CUPS$ID %in% aux]),
                                POT_AUT = sum(LIM_EST_CUPS$POT_AUT[LIM_EST_CUPS$ID %in% aux]),
                                SHARP   = length(aux))
}
LIM_EST_CT <- foreach(i = unique(ROSETA$ID_PADRE_CT),.combine = rbind,.errorhandling = "remove") %dofuture% {
  aux <- unique(ROSETA$CUPS[ROSETA$ID_PADRE_CT == i])
  data.frame(ID = i, ASS="CT",  POT_EST = sum(LIM_EST_CUPS$POT_EST[LIM_EST_CUPS$ID %in% aux]),
                                POT_CON = sum(LIM_EST_CUPS$POT_CON[LIM_EST_CUPS$ID %in% aux]),
                                POT_AUT = sum(LIM_EST_CUPS$POT_AUT[LIM_EST_CUPS$ID %in% aux]),
                                SHARP   = length(aux))
}

LIM_EST <- rbind(LIM_EST_CUPS,LIM_EST_CGP,LIM_EST_LBT,LIM_EST_CUA,LIM_EST_POS,LIM_EST_CT,fill=TRUE)
LIM     <- merge(LIM_EST,LIM,by="ID",all=TRUE)

#rm(LIM_EST,LIM_EST_CUPS,LIM_EST_CGP,LIM_EST_LBT,LIM_EST_CUA,LIM_EST_POS,LIM_EST_CT)

B <- foreach(NAME = ALL,.combine = rbind,.errorhandling = "remove") %dofuture% {

  FILE   <- strsplit(NAME,"/")[[1]][3]
  ASS    <- strsplit(NAME,"/")[[1]][2]
  ID     <- tools::file_path_sans_ext(FILE)

  if(ASS %in% LASS)
  {
    a <- fread(NAME)
    if (length(names(a)) == 4) names(a) <- c("time","kWh","VAL_AE","AUTO")
    if (length(names(a)) == 3) names(a) <- c("time","kWh","issue")

    LENGTH <- length(a$kWh)
    POT_CON<- LIM$POT_CON[LIM$ID == ID]
    POT_EST<- LIM$POT_EST[LIM$ID == ID]
    POT_NOM<- LIM$POT_NOM[LIM$ID == ID]
    POT_AUT<- LIM$POT_AUT[LIM$ID == ID]
    SHARP  <- LIM$SHARP[  LIM$ID == ID]

    QQ     <- as.numeric(quantile(a$kWh,c(0,0.25,0.5,0.75,1),na.rm=T))
    #ECDF   <- ecdf(a$kWh)(MC*POT_NOM)

    aux <- data.frame(
            ID=     ID,
            ASS=    ASS,
            POT_CON=ifelse(length(POT_CON) == 0,NA,POT_CON),
            POT_EST=ifelse(length(POT_EST) == 0,NA,POT_EST),
            POT_NOM=ifelse(length(POT_NOM) == 0,NA,POT_NOM),
            POT_AUT=ifelse(length(POT_AUT) == 0,NA,POT_AUT),
            SHARP=  ifelse(length(SHARP)   == 0,NA,SHARP),
            LENGTH= LENGTH,
            ZERO=   sum(a$kWh==0)/LENGTH,
            IMPUTED=sum(a$issue)/LENGTH,
            ENERGY= sum(a$kWh,na.rm=TRUE),
            AVG=    mean(a$kWh,na.rm=TRUE),
            SD=     sd(a$kWh,na.rm=TRUE),
            MIN=    QQ[1],
            Q1=     QQ[2],
            MEDIAN= QQ[3],
            Q3=     QQ[4],
            MAX=    QQ[5]
  #            MC25=   ECDF[1],
  #            MC50=   ECDF[2],
  #            MC80=   ECDF[3],
  #            MC90=   ECDF[4],
  #            MC95=   ECDF[5]
            )
  } else { aux <- NULL }
}
f <- merge(B,LIM[,c("ID","CNAE","TARIF","SUM","PROV")],by.x="ID",by.y="ID",all=T)
f <- f[!duplicated(f$ID),]
fwrite(f,file="features.csv",row.names = F)

# plot(ecdf(B$ZERO))
# plot(ecdf(B$IMPUTED))
# plot(ecdf(B$MAX/B$POT_NOM))
# ecdf(B$MAX/B$POT_NOM)(0.8)
#
# plot(ecdf(B$MAX[B$ASS=="CUPS"]/B$POT_NOM[B$ASS=="CUPS"]))
# plot(ecdf(B$MAX[B$ASS=="LINE"]/B$POT_NOM[B$ASS=="LINE"]))
# plot(ecdf(B$MAX[B$ASS=="CT"]  /B$POT_NOM[B$ASS=="CT"]))
#
# plot(ecdf(B$MAX[B$ASS=="CUPS"]/B$POT_EST[B$ASS=="CUPS"]))
# plot(ecdf(B$MAX[B$ASS=="LINE"]/B$POT_EST[B$ASS=="LINE"]))
# plot(ecdf(B$MAX[B$ASS=="CT"]  /B$POT_EST[B$ASS=="CT"]))
#
# boxplot(B$POT_NOM[B$ASS=="CUPS"],B$POT_EST[B$ASS=="CUPS"],outline=F)
# boxplot(B$POT_NOM[B$ASS=="LINE"],B$POT_EST[B$ASS=="LINE"],outline=F)
# boxplot(B$POT_NOM[B$ASS=="CT"],  B$POT_EST[B$ASS=="CT"],outline=F)
#
# boxplot(B$POT_NOM[B$ASS=="CT"]/B$POT_EST[B$ASS=="CT"])
#
# summary(B$POT_NOM[B$ASS=="CT"]/B$POT_EST[B$ASS=="CT"])
# summary(B$POT_NOM[B$ASS=="LINE"]/B$POT_EST[B$ASS=="LINE"])
#
# boxplot(B$MC25,B$MC50,B$MC80,B$MC90,B$MC95,outline=F)
# summary(data.frame(B$MC25,B$MC50,B$MC80,B$MC90,B$MC95))
