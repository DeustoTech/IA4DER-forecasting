library(arrow)
library(data.table)
library(doFuture)
library(stringr)

plan(multisession)

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

######################################## CP ###################################

SN <- c("G3E_FID_LBT","FEC_LECTURA","VAL_AI")

## Reales
rs <- ARROW2DF("datos_test/anm_ids01_test_aggct_cp_lec_horaria")
rs <- rs[,c("ID_LINEA_BT","DIA_LECTURA","SUM_VAL_AI")]
names(rs) <- SN
rs$G3E_FID_LBT <- CLEAN_ID(rs$G3E_FID_LBT)
rs$FEC_LECTURA <- as.POSIXct(as.character(rs$FEC_LECTURA),format="%Y%m%d%H%M%S",tz="GMT")
#rs$FEC_LECTURA <- droplevels(cut(rs$FEC_LECTURA, breaks="hour"))
fwrite(rs,file="rs.csv")

### Colaborador 1
a1s <- ARROW2DF("resultados_colaborador_1/entregable_1.parquet")
a1s$G3E_FID_LBT <- CLEAN_ID(a1s$G3E_FID_LBT)
a1s$FEC_LECTURA <- as.POSIXct(a1s$FEC_LECTURA,format="%d/%m/%Y %H:%M")

AAA <- foreach(x=unique(a1s$G3E_FID_LBT),.combine=rbind,.errorhandling = "remove") %dofuture% {
  z <- a1s[a1s$G3E_FID_LBT == x,]
  d <- head(unique(z$FEC_LECTURA),7*24)
  aux <- numeric(7*24)
  for (i in unique(z$G3E_FID_CGP))
    aux <- aux + z[z$G3E_FID_CGP == i,"VAL_AI"]
  data.frame(G3E_FID_LBT=x,FEC_LECTURA=d,VAL_AI=aux)
}
fwrite(AAA[,SN],file="c1s.csv")

### Colaborador 2
system("xlsx2csv resultados_colaborador_2/corto_plazo_predicciones_2024.xlsx > c2s.csv")
c2s <- fread("c2s.csv",select=SN)
c2s$G3E_FID_LBT <- CLEAN_ID(c2s$G3E_FID_LBT)
fwrite(c2s,file="c2s.csv")

### Colaborador 3
c3s <- fread("resultados_colaborador_3/entregable_CP.csv",select=SN)
c3s$G3E_FID_LBT <- CLEAN_ID(c3s$G3E_FID_LBT)
fwrite(c3s,file="c3s.csv")

### Colaborador 4
c4s <- fread("resultados_colaborador_4/entregable_CP.csv",
             select=c("ID_PADRE_LINEA_BT","FEC_LECTURA","VAL_AI_MIN"),col.names=SN)
c4s$G3E_FID_LBT <- CLEAN_ID(c4s$G3E_FID_LBT)
fwrite(c4s,file="c4s.csv")

### Colaborador 5
c5s <- ARROW2DF("resultados_colaborador_5/entregable_CP/prediccion_LBT.parquet")
c5s <- c5s[,SN]
c5s$G3E_FID_LBT <- CLEAN_ID(c5s$G3E_FID_LBT)
fwrite(c5s,file="c5s.csv")

### Colaborador 6
system("xlsx2csv resultados_colaborador_6/entregable_CP.xlsx > c6s.csv")
c6s <- fread("c6s.csv",select=SN)
c6s$G3E_FID_LBT <- CLEAN_ID(c6s$G3E_FID_LBT)
fwrite(c6s,file="c6s.csv")

### Colaborador 7
c7s <- fread("resultados_colaborador_7/entregable_CP.csv",select=SN)
c7s$G3E_FID_LBT <- CLEAN_ID(c7s$G3E_FID_LBT)
fwrite(c7s,file="c7s.csv")

######################################## MP ###################################
MN <- c("G3E_FID_LBT","FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")

a1m <- ARROW2DF("resultados_colaborador_1/entregable_2.parquet")
a1m$G3E_FID_LBT <- CLEAN_ID(a1m$G3E_FID_LBT)
a1m$FEC_LECTURA <- as.Date(a1m$FEC_LECTURA,format="%Y/%m/%d")
                   #as.POSIXct(a1m$FEC_LECTURA,format="%d/%m/%Y")

AAA <- foreach(x=unique(a1m$G3E_FID_LBT),.combine=rbind,.errorhandling = "remove") %dofuture% {
  z <- a1m[a1m$G3E_FID_LBT == x,]
  d <- unique(z$FEC_LECTURA)
  auxM <- auxm <- numeric(length(d))
  for (i in unique(z$G3E_FID_CGP))
  {
    auxM <- auxM + z[z$G3E_FID_CGP == i,"VAL_AI_MAX"]
    auxm <- auxm + z[z$G3E_FID_CGP == i,"VAL_AI_MIN"]
  }
  data.frame(G3E_FID_LBT=x,FEC_LECTURA=d,VAL_AI_MAX=auxM,VAL_AI_MIN=auxm)
}
fwrite(AAA[,MN],file="c1m.csv")
