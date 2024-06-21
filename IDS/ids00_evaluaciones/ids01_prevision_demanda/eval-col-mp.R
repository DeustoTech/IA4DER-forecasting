library(arrow)
library(data.table)
library(stringr)
library(zoo)
library(lubridate)
library(doFuture)
library(skimr)

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

dir.create("fig/MP", showWarnings = FALSE)
DEBUG <- FALSE

MN <- c("G3E_FID_LBT","FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")

r0m <- fread("rm.csv" ,select=MN) # unsorted
c1m <- fread("c1m.csv",select=MN)
c2m <- fread("c2m.csv",select=MN)
c3m <- fread("c3m.csv",select=MN) # char
c4m <- fread("c4m.csv",select=MN) # char
c5m <- fread("c5m.csv",select=MN)
c6m <- fread("c6m.csv",select=MN) # char
c7m <- fread("c7m.csv",select=MN) # char

r0m <- r0m[order(r0m$FEC_LECTURA)]
c3m$FEC_LECTURA <- as.POSIXct(c3m$FEC_LECTURA,format="%Y-%m-%d")
c4m$FEC_LECTURA <- as.POSIXct(c4m$FEC_LECTURA,format="%Y-%m-%d")
c6m$FEC_LECTURA <- as.POSIXct(c6m$FEC_LECTURA,format="%m-%d-%y",tz="UTC")
c7m$FEC_LECTURA <- as.POSIXct(c7m$FEC_LECTURA,format="%Y-%m-%d")

LBT <- unique(r0m$G3E_FID_LBT)
if (DEBUG) cat("ID,MAPE1,MAPE2,MAPE3,MAPE4,MAPE5,MAPE6,MAPE7\n",file="assess-LBT-MP-debug.csv",sep=",")
EVALS <- foreach(i=LBT,.combine=rbind,.errorhandling = "remove") %dofuture% {

  ar <- head(r0m[r0m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)
  a1 <- head(c1m[c1m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)
  a2 <- head(c2m[c2m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)
  a3 <- head(c3m[c3m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)
  a4 <- head(c4m[c4m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)
  a5 <- head(c5m[c5m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)
  a6 <- head(c6m[c6m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)
  a7 <- head(c7m[c7m$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI_MAX","VAL_AI_MIN")],122)

  rM  <- zooreg(ar$VAL_AI_MAX,order.by=ar$FEC_LECTURA)
  pM1 <- zooreg(a1$VAL_AI_MAX,order.by=ar$FEC_LECTURA) # Faltan días y tiene muchos valores repetidos
  pM2 <- zooreg(a2$VAL_AI_MAX,order.by=ar$FEC_LECTURA)
  pM3 <- zooreg(a3$VAL_AI_MAX,order.by=ar$FEC_LECTURA)
  pM4 <- zooreg(a4$VAL_AI_MAX,order.by=ar$FEC_LECTURA)
  pM5 <- zooreg(a5$VAL_AI_MAX,order.by=ar$FEC_LECTURA) # Faltan días y tiene muchos valores repetidos
  pM6 <- zooreg(a6$VAL_AI_MAX,order.by=ar$FEC_LECTURA)
  pM7 <- zooreg(a7$VAL_AI_MAX,order.by=ar$FEC_LECTURA)

  rm  <- zooreg(ar$VAL_AI_MIN,order.by=ar$FEC_LECTURA)
  pm1 <- zooreg(a1$VAL_AI_MIN,order.by=ar$FEC_LECTURA) # Faltan días y tiene muchos valores repetidos
  pm2 <- zooreg(a2$VAL_AI_MIN,order.by=ar$FEC_LECTURA)
  pm3 <- zooreg(a3$VAL_AI_MIN,order.by=ar$FEC_LECTURA)
  pm4 <- zooreg(a4$VAL_AI_MIN,order.by=ar$FEC_LECTURA)
  pm5 <- zooreg(a5$VAL_AI_MIN,order.by=ar$FEC_LECTURA) # Faltan días y tiene muchos valores repetidos
  pm6 <- zooreg(a6$VAL_AI_MIN,order.by=ar$FEC_LECTURA)
  pm7 <- zooreg(a7$VAL_AI_MIN,order.by=ar$FEC_LECTURA)

  if (DEBUG)
  {
    pdf(file=paste0("fig/MP",i,".pdf"))
      try(plot(merge(rM,pM1,pM2,pM3,pM4,pM5,pM6,pM7,
                     rm,pm1,pm2,pm3,pm4,pm5,pm6,pm7)),silent=T)
    dev.off()
  }

  aux <- rM != 0
  MM1 <- 100*median(abs(rM[aux]-pM1[aux])/rM[aux],na.rm=T)
  MM2 <- 100*median(abs(rM[aux]-pM2[aux])/rM[aux],na.rm=T)
  MM3 <- 100*median(abs(rM[aux]-pM3[aux])/rM[aux],na.rm=T)
  MM4 <- 100*median(abs(rM[aux]-pM4[aux])/rM[aux],na.rm=T)
  MM5 <- 100*median(abs(rM[aux]-pM5[aux])/rM[aux],na.rm=T)
  MM6 <- 100*median(abs(rM[aux]-pM6[aux])/rM[aux],na.rm=T)
  MM7 <- 100*median(abs(rM[aux]-pM7[aux])/rM[aux],na.rm=T)

  aux <- rm != 0
  Mm1 <- 100*median(abs(rm[aux]-pm1[aux])/rm[aux],na.rm=T)
  Mm2 <- 100*median(abs(rm[aux]-pm2[aux])/rm[aux],na.rm=T)
  Mm3 <- 100*median(abs(rm[aux]-pm3[aux])/rm[aux],na.rm=T)
  Mm4 <- 100*median(abs(rm[aux]-pm4[aux])/rm[aux],na.rm=T)
  Mm5 <- 100*median(abs(rm[aux]-pm5[aux])/rm[aux],na.rm=T)
  Mm6 <- 100*median(abs(rm[aux]-pm6[aux])/rm[aux],na.rm=T)
  Mm7 <- 100*median(abs(rm[aux]-pm7[aux])/rm[aux],na.rm=T)

  out <- data.frame(ID=i,MM1=MM1,MM2=MM2,MM3=MM3,MM4=MM4,MM5=MM5,MM6=MM6,MM7=MM7,
                         Mm1=Mm1,Mm2=Mm2,Mm3=Mm3,Mm4=Mm4,Mm5=Mm5,Mm6=Mm6,Mm7=Mm7)

  if (DEBUG) write.table(out[-1],row.names=out$ID,col.names=NA,file="assess-LBT-MP-debug.csv",sep=",",append=TRUE)

  return(out)
}

fwrite(EVALS,"assess-LBT-MP.csv")
write.csv(skim(EVALS[,-1])[,-1],row.names=F,file="assess-LBT-MP-summary-NA.csv")
EVALS[is.na(EVALS)] <- 100
write.csv(skim(EVALS[,-1])[,-1],row.names=F,file="assess-LBT-MP-summary.csv")



