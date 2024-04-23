sin <- read.csv("SOLAR/features_sin_autoconsumo_Trampa.csv")
con <- read.csv("SOLAR/features_con_autoconsumo_ConPV.csv")

# CAMBIOS DE ANE Y ASIER CON LAS SERIES TRAMPA
library(tidyr)
library(dplyr)

featsSin <- read.csv("SOLAR/featuresNoPV.csv")
fs1 <- featsSin[which(featsSin$ID %in% sin$ID),] %>% select(c(TIP_SUMINISTRO, COD_CNAE, COD_TARIF_IBDLA, ID))
sin <- merge(sin, fs1, by = "ID")

columnas_comunes <- intersect(names(sin), names(con))

sin <- sin[, columnas_comunes]
con <- con[, columnas_comunes]
# FINAL DE LOS CAMBIOS

ind <- sapply(sin, is.numeric)

pdf("boxplot.pdf")
cat("variable","avr","sd","min","Q1","Q2","Q3","max",
    "pv pre-post","pv suministro","pv tarifa","pv cnae","\n",
    file="p-valores.csv",sep=",")
for (i in names(sin)[ind])
{
  p <- try(wilcox.test(sin[,i],con[,i],paired=F)$p.value)
  boxplot(sin[,i],con[,i],outline=F,names=c("Sin","Con"),main=paste(i,p))

  k1 <- k2 <- k3 <- NA
  aux <- abs(con[,i]-sin[,i])
  if (sum(is.na(aux))/length(aux) <= 0.25)
  {
    k1 <- try(kruskal.test(aux~sin$TIP_SUMINISTRO)$p.value)
    k2 <- try(kruskal.test(aux~sin$COD_TARIF_IBDLA)$p.value)
    k3 <- try(kruskal.test(aux~substring(sin$COD_CNAE,1,1))$p.value)

    boxplot(aux~sin$TIP_SUMINISTRO,outline=F,main=paste(i,k1))
    boxplot(aux~sin$COD_TARIF_IBDLA,outline=F,main=paste(i,k2))
    boxplot(aux~substring(sin$COD_CNAE,1,1),outline=F,main=paste(i,k3))
  }
  cat(i,mean(aux,na.rm=T),sd(aux,na.rm=T),as.numeric(quantile(aux,c(0,0.25,0.5,0.75,1),na.rm=T)),
      p,k1,k2,k3,"\n",file="p-valores.csv",sep=",",append=T)
}
dev.off()

