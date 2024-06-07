library(caret)
library(plyr)
library(pheatmap)

TRAMPA <- c("0040.jpg","0970.jpg","1606.jpg","2167.jpg","2185.jpg","2208.jpg","2992.jpg")
BASE   <- ""

r  <- read.csv("datos_reales.csv")
t  <- read.csv("train.csv")
c1 <- read.csv("resultados_colaborador_1.1.csv")
c2 <- read.csv("resultados_colaborador_2.csv")
c3 <- read.csv("resultados_colaborador_3.csv")
c4 <- read.csv("resultados_colaborador_4.1.csv")
c5 <- read.csv("resultados_colaborador_5.csv")

W  <- r[r$Nombre %in% TRAMPA,]
NO <- t[is.na(t$Tipo.Material),"Nombre"]
T  <- r[ r$Nombre %in% NO, ]
T  <- T[!T$Nombre %in% TRAMPA,]
T  <- T[order(T$Nombre),]
T[T$Función.Normal   == 98,"Función.Normal"]   <- NA
T[T$Función.Especial == 98,"Función.Especial"] <- NA

rm(t)

ud1 <- read.csv("resultados_ud/Funcion_Especial_01_swin_inbalance_256_50.csv")
ud2 <- read.csv("resultados_ud/Funcion_Normal_01_swin_inbalance_256_50.csv")
ud3 <- read.csv("resultados_ud/Subtipo_Material_01_swin_inbalance_256_50.csv")
ud4 <- read.csv("resultados_ud/Tipo_Material_01_swin_inbalance_256_50.csv")

names(ud1)[2] <- "Función.Especial"
names(ud2)[2] <- "Función.Normal"
names(ud3)[2] <- "Subtipo.Material"
names(ud4)[2] <- "Tipo.Material"

ud <- merge(ud4,ud3,by="Nombre")
ud <- merge(ud, ud2,by="Nombre")
ud <- merge(ud, ud1,by="Nombre")

ud <- ud[ud$Nombre %in% T$Nombre,names(r)]
ud$Tipo.Material    <- factor(ud$Tipo.Material    )
ud$Subtipo.Material <- factor(ud$Subtipo.Material )
ud$Función.Normal   <- factor(ud$Función.Normal   )
ud$Función.Especial <- factor(ud$Función.Especial )

levels(ud$Tipo.Material)    <- list("1"="40", "20"="20", "30"="1", "40"="30")
levels(ud$Subtipo.Material) <- list(  "1"="210",  "10"="80" , "30"="30",  "80"="1"  ,
                                     "90"="150", "100"="90" ,"120"="100","140"="120",
                                    "150"="200", "200"="10" ,"210"="140")
levels(ud$Función.Normal)   <- list("1"="2", "2"="4", "3"="1", "4"="3", "98"=NA)
levels(ud$Función.Especial) <- list("1"="9", "2"="2", "3"="3", "4"="10", "5"="8",
                                    "7"="1", "8"="4", "9"="5", "10"="7","98"=NA)

c1 <- c1[,1:5]
names(c1) <- names(r)
c1 <- c1[c1$Nombre %in% T$Nombre,names(r)]
c1 <- c1[!duplicated(c1$Nombre),]
c1 <- rbind(c1,c("4070.jpg","NA","NA","NA","NA"))
c1$Subtipo.Material[c1$Subtipo.Material == 9999]  <- 80

c2 <- c2[c2$Nombre %in% T$Nombre,names(r)]

c3 <- c3[c3$Nombre %in% T$Nombre,c("Nombre","Tipo.Material_Pred","Subtipo.Material_Pred",
                                    "Función.Normal_Pred","Función.Especial_Pred")]
names(c3) <- names(r)
c3$Tipo.Material[c3$Tipo.Material       == 98]   <- NA
c3$Subtipo.Material[c3$Subtipo.Material == 998]  <- NA

c4 <- c4[c4$name   %in% T$Nombre,c("name","material_category_code","material_subcategory_code",
                                   "function_category_code","function_subcategory_code")]
names(c4) <- names(T)
c4 <- c4[!duplicated(c4$Nombre),]
c4$Tipo.Material[c4$Tipo.Material       == 210] <- 40
c4$Función.Especial[c4$Función.Especial == 0]   <- 98
c4$Función.Especial[c4$Función.Especial == 6]   <- 5

c5 <- c5[c5$Nombre %in% T$Nombre,names(r)]
c5 <- rbind(c5,c("0907.jpg","NA","NA","NA","NA"))

c1 <- c1[order(c1$Nombre),]
c2 <- c2[order(c2$Nombre),]
c3 <- c3[order(c3$Nombre),]
c4 <- c4[order(c4$Nombre),]
c5 <- c5[order(c5$Nombre),]

pdf()
cat("Caracteristica","Colaborador","Accuracy","Individual min", "Individual Q2","#","\n",file="resultados.csv",sep=",")
for (t in names(T)[-1])
{
  LEVELS <- levels(factor(T[,t]))
  rn <- sample(levels(factor(T[,t])),length(T[,t]),replace=TRUE)

  ra <- confusionMatrix(data=factor(rn,    levels=LEVELS),reference=factor(T[,t]))
  au <- confusionMatrix(data=factor(ud[,t],levels=LEVELS),reference=factor(T[T$Nombre %in% ud$Nombre,t]))
  a1 <- confusionMatrix(data=factor(c1[,t],levels=LEVELS),reference=factor(T[T$Nombre %in% c1$Nombre,t]))
  a2 <- confusionMatrix(data=factor(c2[,t],levels=LEVELS),reference=factor(T[T$Nombre %in% c2$Nombre,t]))
  a3 <- confusionMatrix(data=factor(c3[,t],levels=LEVELS),reference=factor(T[T$Nombre %in% c3$Nombre,t]))
  a4 <- confusionMatrix(data=factor(c4[,t],levels=LEVELS),reference=factor(T[T$Nombre %in% c4$Nombre,t]))
  a5 <- confusionMatrix(data=factor(c5[,t],levels=LEVELS),reference=factor(T[T$Nombre %in% c5$Nombre,t]))

  cat(t,"random",       ra$overall[1],min(ra$byClass[,6]),median(ra$byClass[,6]),length(rn),                "\n",file="resultados.csv",append=TRUE,sep=",")
  cat(t,"UDEUSTO",      au$overall[1],min(au$byClass[,6]),median(au$byClass[,6]),sum(T$Nombre == ud$Nombre),"\n",file="resultados.csv",append=TRUE,sep=",")
  cat(t,"colaborador_1",a1$overall[1],min(a1$byClass[,6]),median(a1$byClass[,6]),sum(T$Nombre == c1$Nombre),"\n",file="resultados.csv",append=TRUE,sep=",")
  cat(t,"colaborador_2",a2$overall[1],min(a2$byClass[,6]),median(a2$byClass[,6]),sum(T$Nombre == c2$Nombre),"\n",file="resultados.csv",append=TRUE,sep=",")
  cat(t,"colaborador_3",a3$overall[1],min(a3$byClass[,6]),median(a3$byClass[,6]),sum(T$Nombre == c3$Nombre),"\n",file="resultados.csv",append=TRUE,sep=",")
  cat(t,"colaborador_4",a4$overall[1],min(a4$byClass[,6]),median(a4$byClass[,6]),sum(T$Nombre == c4$Nombre),"\n",file="resultados.csv",append=TRUE,sep=",")
  cat(t,"colaborador_5",a5$overall[1],min(a5$byClass[,6]),median(a5$byClass[,6]),sum(T$Nombre == c5$Nombre),"\n",file="resultados.csv",append=TRUE,sep=",")

  pheatmap(au$table, display_numbers = au$table,
           color = colorRampPalette(c('white','red'))(100), legend=FALSE,
           cluster_rows = F, cluster_cols = F, fontsize_number = 15)

  pheatmap(au$table, display_numbers = TRUE,
           color = colorRampPalette(c('white','red'))(100), legend=FALSE,
           cluster_rows = F, cluster_cols = F, fontsize_number = 15)
}
dev.off()
