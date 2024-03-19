library(caret)

r  <- read.csv("datos_reales.csv")
c1 <- read.csv("resultados_colaborador_1.csv")
c2 <- read.csv("resultados_colaborador_2.csv")
c3 <- read.csv("resultados_colaborador_3.csv")
c4 <- read.csv("resultados_colaborador_4.1.csv")
c5 <- read.csv("resultados_colaborador_5.csv")

r  <- r[r$Nombre   %in% c5$Nombre,]
c1 <- c1[c1$Nombre %in% c5$Nombre,names(r)]
c2 <- c2[c2$Nombre %in% c5$Nombre,names(r)]
c3 <- c3[c3$Nombre %in% c5$Nombre,c("Nombre","Tipo.Material_Pred","Subtipo.Material_Pred",
                                    "Función.Normal_Pred","Función.Especial_Pred")]
names(c3) <- names(r)
c4 <- c4[c4$name %in% c5$Nombre,c("name","material_category_code","material_subcategory_code",
                                  "function_category_code","function_subcategory_code")]
names(c4) <- names(r)

c3$Tipo.Material[c3$Tipo.Material       == 98]   <- NA
c3$Subtipo.Material[c3$Subtipo.Material == 998]  <- NA

c4 <- c4[!duplicated(c4$Nombre),]
c4$Tipo.Material[c4$Tipo.Material       == 210] <- 40
c4$Función.Especial[c4$Función.Especial == 0]   <- 98
c4$Función.Especial[c4$Función.Especial == 6]   <- 5

cat("Colaborador","Accuracy","Individual","\n",file="resultados.csv",sep=",")
for (t in names(r)[-1])
{
  #a1 <- confusionMatrix(data=factor(c1[,t]),reference=factor(r[,t]))
  a2 <- confusionMatrix(data=factor(c2[,t]),reference=factor(r[,t]))
  a3 <- confusionMatrix(data=factor(c3[,t]),reference=factor(r[,t]))
  a4 <- confusionMatrix(data=factor(c4[,t]),reference=factor(r[,t]))
  a5 <- confusionMatrix(data=factor(c5[,t]),reference=factor(r[,t]))

  #cat("colaborador_1",a1$overall[1],min(a1$byClass[,6]),"\n",file="resultados.csv",append=T)
  cat("colaborador_2",a2$overall[1],min(a2$byClass[,6]),"\n",file="resultados.csv",append=T,sep=",")
  cat("colaborador_3",a3$overall[1],min(a3$byClass[,6]),"\n",file="resultados.csv",append=T,sep=",")
  cat("colaborador_4",a4$overall[1],min(a4$byClass[,6]),"\n",file="resultados.csv",append=T,sep=",")
  cat("colaborador_5",a5$overall[1],min(a5$byClass[,6]),"\n",file="resultados.csv",append=T,sep=",")
}

