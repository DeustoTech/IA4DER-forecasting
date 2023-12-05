library(data.table)
library(stringr)
library(arrow)
library(doFuture)
library(zoo)

#options(future.globals.maxSize= 891289600)
plan(multisession)

F_DAYS <- 7
# MC     <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error
#
# ARROW2DF <- function(SOURCE)
# {
#   d    <- open_dataset(source=SOURCE)
#   so   <- Scanner$create(d)
#   at   <- so$ToTable()
#   return(as.data.frame(at))
# }

CLEAN_ID <- function(X)
{
  X <- str_replace(X,fixed("33TCP+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),fixed(""))
  X <- str_replace_all(X,fixed("/"),fixed("\\"))
  return(X)
}

# cp    <- ARROW2DF("./inputdata/datos_test/anm_ids01_test_aggct_cp_lec_horaria")
# mp    <- ARROW2DF("./inputdata/datos_test/anm_ids01_test_aggct_mp_lec_horaria")
# col1  <- ARROW2DF("./inputdata/resultados_colaborador_1/")
# 
# fwrite(cp,"cp.csv")
# fwrite(mp,"mp.csv")
# fwrite(col1,"collaborator1.csv")

#mp   <- fread("mp.csv")
#cp   <- fread("cp.csv",               select = c("ID_LINEA_BT","ID_USUARIO","DIA_LECTURA","SUM_VAL_AI"))
col1 <- fread("collaborator1.csv",    select = c("G3E_FID_CT","G3E_FID_CGP","G3E_FID_LBT","FEC_LECTURA","VAL_AI"))
col2 <- fread("collaborator2_cp.csv", select = c("G3E_FID_CT","G3E_FID_LBT","FEC_LECTURA","VAL_AI"))
#LIM  <- fread("features.csv",         select = c("ID","POT_NOM","POT_EST"))

ENG  <- fread("enganches.csv")
ENG$ID_USUARIO <- CLEAN_ID(ENG$ID_USUARIO)
ENG$G3E_FID_CT <- CLEAN_ID(ENG$G3E_FID_CT)

col1$G3E_FID_CT  <- CLEAN_ID(col1$G3E_FID_CT  )
col1$G3E_FID_CGP <- CLEAN_ID(col1$G3E_FID_CGP )
col1$G3E_FID_LBT <- CLEAN_ID(col1$G3E_FID_LBT )

col2$G3E_FID_CT  <- CLEAN_ID(col2$G3E_FID_CT  )
col2$G3E_FID_LBT <- CLEAN_ID(col2$G3E_FID_LBT )

# length(intersect(unique(col1$G3E_FID_CT), unique(cp$ID_USUARIO)))
# length(intersect(unique(col1$G3E_FID_LBT),unique(cp$ID_USUARIO)))
# length(intersect(unique(col1$G3E_FID_CGP),unique(cp$ID_USUARIO)))
# 
# length(intersect(unique(col1$G3E_FID_CT), unique(cp$ID_LINEA_BT)))
# length(intersect(unique(col1$G3E_FID_LBT),unique(cp$ID_LINEA_BT))) # <----
# length(intersect(unique(col1$G3E_FID_CGP),unique(cp$ID_LINEA_BT)))

# length(intersect(CLEAN_ID(unique(col1$G3E_FID_CGP)),
#                  tools::file_path_sans_ext(basename(CGP))))

LBT <- Sys.glob(paths="test/LBT/*")
B <- foreach(i = LBT,.combine=rbind) %do% {

  FILE <- strsplit(i,"/")[[1]][3]
  TYPE <- strsplit(i,"/")[[1]][2]
  ID   <- tools::file_path_sans_ext(FILE)

  p1 <- col1[col1$G3E_FID_LBT == ID,]
  p2 <- col2[col2$G3E_FID_LBT == ID,]

  r  <- fread(paste0("test/LBT/",ID,".csv",sep=""))
  r  <- zoo(r$SUM_VAL_AI,order.by=r$DIA_LECTURA)

  aux <- numeric(F_DAYS*24)
  for (x in unique(p1$G3E_FID_CGP))
    aux <- aux + p1[p1$G3E_FID_CGP == x,"VAL_AI"][1:(F_DAYS*24)][[1]]

  p1 <- aux
  p2 <- p2$VAL_AI[1:(F_DAYS*24)]
  #r  <- r$SUM_VAL_AI[1:(F_DAYS*24)]

#   POT_NOM <- LIM$POT_NOM[LIM$ID == CLEAN_ID(ID)]
#   POT_EST <- LIM$POT_EST[LIM$ID == CLEAN_ID(ID)]

#   plot(p[1:(24*7*7+1),"VAL_AI"])
#   plot(r$SUM_VAL_AI[1:(7*24)])
#   cbind(p[1:(7*24),],r[1:(7*24),])

  aux     <- r != 0
#  auxmase <- median(abs(real-f[,"naive"]))

  auxtime <- numeric(F_DAYS)
  for (D in 1:F_DAYS)
    auxtime[D] <- which.max(r[(24*D-23):(24*D)]) -1

  MAPE1  <- 100*median(ifelse(sum(aux)!=0,abs(r[aux]-p1[aux])/r[aux],NA),na.rm=T)
  MAPE2  <- 100*median(ifelse(sum(aux)!=0,abs(r[aux]-p2[aux])/r[aux],NA),na.rm=T)
  RMSE1  <- sqrt(median((r-p1)^2,na.rm=T))
  RMSE2  <- sqrt(median((r-p2)^2,na.rm=T))
#  MASE1  <- median(abs(r-p1))/auxmase
#  MASE2  <- median(abs(r-p2))/auxmase

  aux_p1 <- aux_p2 <- numeric(F_DAYS)
  for (D in 1:F_DAYS)
  {
    aux_p1[D] <- ifelse(!anyNA(p1[(24*D-23):(24*D)]),which.max(p1[(24*D-23):(24*D)])-1,NA)
    aux_p2[D] <- ifelse(!anyNA(p2[(24*D-23):(24*D)]),which.max(p2[(24*D-23):(24*D)])-1,NA)
  }
  TIME1  <- median(abs(auxtime-aux_p1),na.rm=T)
  TIME2  <- median(abs(auxtime-aux_p2),na.rm=T)

#   RISKA1 <- (max(r) < POT_NOM*MC) == (max(p1) < POT_NOM*MC)
#   RISKA2 <- (max(r) < POT_NOM*MC) == (max(p2) < POT_NOM*MC)
#   RISKB <- (max(r) < POT_EST*MC) == (max(p) < POT_EST*MC)
#
#   QQF   <- ecdf(p)
#   MCA   <- QQF(MC*POT_NOM) ## QQR(MC*POT_NOM)-QQF(MC*POT_NOM)
#   MCB   <- QQF(MC*POT_EST) ## QQR(MC*POT_EST)-QQF(MC*POT_EST)
#
  out1   <- data.frame(id=  ID,
                       mape1=MAPE1,
                       rmse1=RMSE1,
#                      mase1=MASE1,
                       time1=TIME1
#                        risk125=RISKA1[1],
#                        risk150=RISKA1[2],
#                        risk180=RISKA1[3],
#                        risk190=RISKA1[4],
#                        risk195=RISKA1[5]
                       )
  out2   <- data.frame(id=  ID,
                       mape2=MAPE2,
                       rmse2=RMSE2,
#                      mase2=MASE2,
                       time2=TIME2
#                        risk225=RISKA2[1],
#                        risk250=RISKA2[2],
#                        risk280=RISKA2[3],
#                        risk290=RISKA2[4],
#                        risk295=RISKA2[5]
                       )
  cbind(out1,out2)
}

CT <- Sys.glob(paths="test/CT/*")

C <- foreach(i = CT,.combine=rbind) %do% {

  FILE <- strsplit(i,"/")[[1]][3]
  TYPE <- strsplit(i,"/")[[1]][2]
  ID   <- tools::file_path_sans_ext(FILE)

  r  <- fread(paste0("test/CT/",ID,".csv",sep=""))
  r  <- zoo(r$SUM_VAL_AI,order.by=r$DIA_LECTURA)

  p1 <- col1[col1$G3E_FID_CT == ID,]
  p2 <- col2[col2$G3E_FID_CT == ID,]

  aux <- numeric(F_DAYS*24)
  for (x in unique(p1$G3E_FID_CGP))
    aux <- aux + p1[p1$G3E_FID_CGP == x,"VAL_AI"][1:(F_DAYS*24)][[1]]
  p1 <- aux

  aux <- numeric(F_DAYS*24)
  for (x in unique(p2$G3E_FID_LBT))
    aux <- aux + p2[p2$G3E_FID_LBT == x,"VAL_AI"][1:(F_DAYS*24)][[1]]
  p2 <- aux

#   POT_NOM <- LIM$POT_NOM[LIM$ID == CLEAN_ID(NAME)]
#   POT_EST <- LIM$POT_EST[LIM$ID == CLEAN_ID(NAME)]

  #   plot(p[1:(24*7*7+1),"VAL_AI"])
  #   plot(r$SUM_VAL_AI[1:(7*24)])
  #   cbind(p[1:(7*24),],r[1:(7*24),])

  aux     <- r != 0
  #  auxmase <- median(abs(real-f[,"naive"]))

  auxtime <- numeric(F_DAYS)
  for (D in 1:F_DAYS)
    auxtime[D] <- which.max(r[(24*D-23):(24*D)]) -1

  MAPE1  <- 100*median(ifelse(sum(aux)!=0,abs(r[aux]-p1[aux])/r[aux],NA),na.rm=T)
  MAPE2  <- 100*median(ifelse(sum(aux)!=0,abs(r[aux]-p2[aux])/r[aux],NA),na.rm=T)
  RMSE1  <- sqrt(median((r-p1)^2,na.rm=T))
  RMSE2  <- sqrt(median((r-p2)^2,na.rm=T))
  #  MASE1  <- median(abs(r-p1))/auxmase
  #  MASE2  <- median(abs(r-p2))/auxmase

  aux_p1 <- aux_p2 <- numeric(F_DAYS)
  for (D in 1:F_DAYS)
  {
    aux_p1[D] <- ifelse(!anyNA(p1[(24*D-23):(24*D)]),which.max(p1[(24*D-23):(24*D)])-1,NA)
    aux_p2[D] <- ifelse(!anyNA(p2[(24*D-23):(24*D)]),which.max(p2[(24*D-23):(24*D)])-1,NA)
  }
  TIME1  <- median(abs(auxtime-aux_p1),na.rm=T)
  TIME2  <- median(abs(auxtime-aux_p2),na.rm=T)

#   RISKA1 <- (max(r) < POT_NOM*MC) == (max(p1) < POT_NOM*MC)
#   RISKA2 <- (max(r) < POT_NOM*MC) == (max(p2) < POT_NOM*MC)
  #   RISKB <- (max(r) < POT_EST*MC) == (max(p) < POT_EST*MC)
  #
  #   QQF   <- ecdf(p)
  #   MCA   <- QQF(MC*POT_NOM) ## QQR(MC*POT_NOM)-QQF(MC*POT_NOM)
  #   MCB   <- QQF(MC*POT_EST) ## QQR(MC*POT_EST)-QQF(MC*POT_EST)
  #
  out1   <- data.frame(id=  ID,
                       mape1=MAPE1,
                       rmse1=RMSE1,
#                      mase1=MASE1,
                       time1=TIME1
#                        risk125=RISKA1[1],
#                        risk150=RISKA1[2],
#                        risk180=RISKA1[3],
#                        risk190=RISKA1[4],
#                        risk195=RISKA1[5]
                       )
  out2   <- data.frame(id=  ID,
                       mape2=MAPE2,
                       rmse2=RMSE2,
#                      mase2=MASE2,
                        time2=TIME2
#                        risk225=RISKA2[1],
#                        risk250=RISKA2[2],
#                        risk280=RISKA2[3],
#                        risk290=RISKA2[4],
#                        risk295=RISKA2[5]
                       )
  cbind(out1,out2)
}

