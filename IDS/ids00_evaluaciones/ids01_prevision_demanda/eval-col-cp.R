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

dir.create("fig/CP", showWarnings = FALSE)
DEBUG <- FALSE

SN <- c("G3E_FID_LBT","FEC_LECTURA","VAL_AI")

rs  <- fread("rs.csv" ,select=SN)
c1s <- fread("c1s.csv",select=SN)
c2s <- fread("c2s.csv",select=SN) # <- son char !
c3s <- fread("c3s.csv",select=SN)
c4s <- fread("c4s.csv",select=SN) # <- hay que quitarle un año
c5s <- fread("c5s.csv",select=SN)
c6s <- fread("c6s.csv",select=SN) # <- son char ! y hay que quitarle una hora
c7s <- fread("c7s.csv",select=SN) # <- son todas iguales (?)

rs  <- rs[order(rs$FEC_LECTURA)]
c2s$G3E_FID_LBT <- CLEAN_ID(c2s$G3E_FID_LBT)
c2s$FEC_LECTURA <- as.POSIXct(c2s$FEC_LECTURA,format="%d/%m/%Y %H:%M")

c4s$FEC_LECTURA <- c4s$FEC_LECTURA - lubridate::years(1)

c6s$G3E_FID_LBT <- CLEAN_ID(c6s$G3E_FID_LBT)
c6s$FEC_LECTURA <- as.POSIXct(c6s$FEC_LECTURA,format="%d/%m/%Y %H:%M") - lubridate::hours(1)

LBT <- unique(rs$G3E_FID_LBT)
if (DEBUG) cat("ID,MAPE1,MAPE2,MAPE3,MAPE4,MAPE5,MAPE6,MAPE7,TIME1,TIME2,TIME3,TIME4,TIME5,TIME6,TIME7\n",file="assess-LBT-CP-debug.csv",sep=",")
EVALS <- foreach(i=LBT,.combine=rbind,.errorhandling = "remove") %dofuture% {

  ar <- head(rs[  rs$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)
  a1 <- head(c1s[c1s$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)
  a2 <- head(c2s[c2s$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)
  a3 <- head(c3s[c3s$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)
  a4 <- head(c4s[c4s$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)
  a5 <- head(c5s[c5s$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)
  a6 <- head(c6s[c6s$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)
  a7 <- head(c7s[c7s$G3E_FID_LBT == i,c("FEC_LECTURA","VAL_AI")],24*7)

  r  <- zooreg(ar$VAL_AI,order.by=ar$FEC_LECTURA)
  p1 <- zooreg(a1$VAL_AI,order.by=ar$FEC_LECTURA)
  p2 <- zooreg(a2$VAL_AI,order.by=ar$FEC_LECTURA)
  p3 <- zooreg(a3$VAL_AI,order.by=ar$FEC_LECTURA)
  p4 <- zooreg(a4$VAL_AI,order.by=ar$FEC_LECTURA)
  p5 <- zooreg(a5$VAL_AI,order.by=ar$FEC_LECTURA)
  p6 <- zooreg(a6$VAL_AI,order.by=ar$FEC_LECTURA)
  p7 <- zooreg(a7$VAL_AI,order.by=ar$FEC_LECTURA)

  if (DEBUG)
  {
    pdf(file=paste0("fig/CP/",i,".pdf"))
      try(plot(merge(r,p1,p2,p3,p4,p5,p6,p7)),silent=T)
    dev.off()
  }

  aux   <- r != 0
  MAPE1 <- 100*median(abs(r[aux]-p1[aux])/r[aux],na.rm=T)
  MAPE2 <- 100*median(abs(r[aux]-p2[aux])/r[aux],na.rm=T)
  MAPE3 <- 100*median(abs(r[aux]-p3[aux])/r[aux],na.rm=T)
  MAPE4 <- 100*median(abs(r[aux]-p4[aux])/r[aux],na.rm=T)
  MAPE5 <- 100*median(abs(r[aux]-p5[aux])/r[aux],na.rm=T)
  MAPE6 <- 100*median(abs(r[aux]-p6[aux])/r[aux],na.rm=T)
  MAPE7 <- 100*median(abs(r[aux]-p7[aux])/r[aux],na.rm=T)

  atr <- at1 <- at2 <- at3 <- at4 <- at5 <- at6 <- at7 <- numeric(7)
  for (D in 1:7)
  {
    atr[D] <-                                       which.max(r[ (24*D-23):(24*D)]) - 1
    at1[D] <- ifelse(!anyNA(p1[(24*D-23):(24*D),j]),which.max(p1[(24*D-23):(24*D)]) - 1, NA)
    at2[D] <- ifelse(!anyNA(p2[(24*D-23):(24*D),j]),which.max(p2[(24*D-23):(24*D)]) - 1, NA)
    at3[D] <- ifelse(!anyNA(p3[(24*D-23):(24*D),j]),which.max(p3[(24*D-23):(24*D)]) - 1, NA)
    at4[D] <- ifelse(!anyNA(p4[(24*D-23):(24*D),j]),which.max(p4[(24*D-23):(24*D)]) - 1, NA)
    at5[D] <- ifelse(!anyNA(p5[(24*D-23):(24*D),j]),which.max(p5[(24*D-23):(24*D)]) - 1, NA)
    at6[D] <- ifelse(!anyNA(p6[(24*D-23):(24*D),j]),which.max(p6[(24*D-23):(24*D)]) - 1, NA)
    at7[D] <- ifelse(!anyNA(p7[(24*D-23):(24*D),j]),which.max(p7[(24*D-23):(24*D)]) - 1, NA)
  }

  TIME1 <- median(abs(atr-at1),na.rm=T)
  TIME2 <- median(abs(atr-at2),na.rm=T)
  TIME3 <- median(abs(atr-at3),na.rm=T)
  TIME4 <- median(abs(atr-at4),na.rm=T)
  TIME5 <- median(abs(atr-at5),na.rm=T)
  TIME6 <- median(abs(atr-at6),na.rm=T)
  TIME7 <- median(abs(atr-at7),na.rm=T)

  out <- data.frame(ID=i,M1=MAPE1,M2=MAPE2,M3=MAPE3,M4=MAPE4,M5=MAPE5,M6=MAPE6,M7=MAPE7,
                         T1=TIME1,T2=TIME2,T3=TIME3,T4=TIME4,T5=TIME5,T6=TIME6,T7=TIME7)

  if (DEBUG) write.table(out[-1],row.names=out$ID,col.names=NA,file="assess-LBT-CP-debug.csv",sep=",",append=TRUE)

  return(out)
}

fwrite(EVALS,"assess-LBT-CP.csv")
write.csv(skim(EVALS[,-1])[,-1],row.names=F,file="assess-LBT-CP-summary-NA.csv")
EVALS[is.na(EVALS)] <- 100
write.csv(skim(EVALS[,-1])[,-1],row.names=F,file="assess-LBT-CP-summary.csv")



