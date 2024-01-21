Crear_Rmd<-function(Rscript,NombreRmd="goiEner.Rmd") {
  
  require(data.table)
  require(stringr)
  require(dplyr)
  
  codigo<-fread(Rscript,sep="\n",header = FALSE)$V1
  
  str_detect(codigo,"#")->titulos
  codigo==""->espacios_vacios
  cumsum(titulos)->bloque
  
  todo<-data.frame(codigo,titulos,espacios_vacios,bloque)
  todo$codigo<-as.character(todo$codigo)
  
  lista_bloques<-list()
  for (i in min(bloque):max(bloque)) {
    todo %>% filter(bloque==i)->segmento
    if (sum(segmento$titulo | segmento$espacios_vacios)==nrow(segmento)) {
      lista_bloques[[i]]<-c(segmento$codigo)
    } else {
      lista_bloques[[i]]<-c(segmento$codigo[1],
                            "```{r message=FALSE,warning=FALSE}",
                            segmento$codigo[-1],"```")
    }
  }
  
  cabecera<-c(
    "---",
    'title: "goiEner predicciones"',
    'author: "Asier Loinaz y Ane San Juan"',
    "output: ",
    "  html_document:",
    "    toc: TRUE",
    "    toc_float: TRUE",
    "    theme: cerulean",
    "---",
    "" ,
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE)",
    "```",
    ""
  )
  
  data.frame(bloques=c(cabecera,unlist(lista_bloques)))->codigo_corregido
  names(codigo_corregido)<-"# SCRIPT"
  
  nombre_archivo<-NombreRmd
  
  write.table(codigo_corregido,nombre_archivo,sep="\n",
              quote = FALSE,row.names = FALSE,col.names = FALSE)
  
  file.edit(nombre_archivo)
}


Crear_Rmd("goiEnerV3.R")
