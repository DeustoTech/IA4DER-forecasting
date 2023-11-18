library(data.table)
library(stringr)
# library(arrow)
library(doFuture)

#options(future.globals.maxSize= 891289600)
plan(multisession)

F_DAYS <- 7
MC     <- c(0.25,0.5,0.8,0.90,0.95) ### quantiles to use in the monotona creciente error

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

# cp    <- ARROW2DF("./inputdata/datos_test/anm_ids01_test_aggct_cp_lec_horaria")
# mp    <- ARROW2DF("./inputdata/datos_test/anm_ids01_test_aggct_mp_lec_horaria")
# col1  <- ARROW2DF("./inputdata/resultados_colaborador_1/")
# 
# fwrite(cp,"cp.csv")
# fwrite(mp,"mp.csv")
# fwrite(col1,"collaborator1.csv")

cp   <- fread("cp.csv", select = c("ID_LINEA_BT","DIA_LECTURA","SUM_VAL_AI"))
#mp   <- fread("mp.csv")
col1 <- fread("collaborator1.csv", select = c("G3E_FID_LBT","FEC_LECTURA","VAL_AI"))
LIM  <- fread("features.csv", select = c("ID","POT_NOM","POT_EST"))

# length(intersect(unique(col1$G3E_FID_CT), unique(cp$ID_USUARIO)))
# length(intersect(unique(col1$G3E_FID_LBT),unique(cp$ID_USUARIO)))
# length(intersect(unique(col1$G3E_FID_CGP),unique(cp$ID_USUARIO)))
# 
# length(intersect(unique(col1$G3E_FID_CT), unique(cp$ID_LINEA_BT)))
# length(intersect(unique(col1$G3E_FID_LBT),unique(cp$ID_LINEA_BT))) # <----
# length(intersect(unique(col1$G3E_FID_CGP),unique(cp$ID_LINEA_BT)))

B <- foreach(NAME = unique(cp$ID_LINEA_BT),.combine=rbind) %dofuture% {

  p <- col1[col1$G3E_FID_LBT == NAME,]
  r <- cp[    cp$ID_LINEA_BT == NAME,]
  r <- r[order(r$DIA_LECTURA),]
  
  POT_NOM <- LIM$POT_NOM[LIM$ID == CLEAN_ID(NAME)]
  POT_EST <- LIM$POT_EST[LIM$ID == CLEAN_ID(NAME)]

  p <- p$VAL_AI[1:(F_DAYS*24)]
  r <- r$SUM_VAL_AI[1:(F_DAYS*24)]

#   plot(p[1:(24*7*7+1),"VAL_AI"])
#   plot(r$SUM_VAL_AI[1:(7*24)])
#   cbind(p[1:(7*24),],r[1:(7*24),])

  aux     <- r != 0
#  auxmase <- median(abs(real-f[,"naive"]))

  auxtime <- numeric(F_DAYS)
  for (D in 1:F_DAYS)
    auxtime[D] <- which.max(r[(24*D-23):(24*D)]) -1
  
  MAPE  <- 100*median(ifelse(sum(aux)!=0,abs(r[aux]-p[aux])/r[aux],NA),na.rm=T)
  RMSE  <- sqrt(median((r-p)^2,na.rm=T))
#  MASE  <- median(abs(r-p))/auxmase

  aux_f <- numeric(F_DAYS)
  for (D in 1:F_DAYS)
    aux_f[D] <- ifelse(!anyNA(p[(24*D-23):(24*D)]),which.max(p[(24*D-23):(24*D)])-1,NA)
  TIME  <- median(abs(auxtime-aux_f),na.rm=T)
  
  RISKA <- (max(r) < POT_NOM*MC) == (max(p) < POT_NOM*MC)
#   RISKB <- (max(r) < POT_EST*MC) == (max(p) < POT_EST*MC)
#   
#   QQF   <- ecdf(p)
#   MCA   <- QQF(MC*POT_NOM) ## QQR(MC*POT_NOM)-QQF(MC*POT_NOM)
#   MCB   <- QQF(MC*POT_EST) ## QQR(MC*POT_EST)-QQF(MC*POT_EST)
#   
  out   <- data.frame(id=  NAME,
                      mape=MAPE,
                      rmse=RMSE,
#                      mase=MASE,
                      time=TIME,
                      risk25=RISKA[1],
                      risk50=RISKA[2],
                      risk80=RISKA[3],
                      risk90=RISKA[4],
                      risk95=RISKA[5])
}
