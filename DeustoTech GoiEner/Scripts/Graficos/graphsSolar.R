library(foreach)
library(doParallel)


# añadir las librerias nuevas en este vector

librerias <- c("ggplot2", "lattice", "caret", "fpp3", "class",
               "lattice", "forecast", "Metrics", "fable", 
               "data.table", "xts", "future", "fable", "foreach", "doParallel", "RSNNS", "TTR", 
               'quantmod', 'caret', 'e1071', 'nnet', 'tools', 'doFuture', 'neuralnet', 'gbm',
               "randomForest", "mice", "tsfeatures", "gridExtra") 
foreach(lib = librerias) %do% {
  library(lib, character.only = TRUE)
}

antes <- fread("SOLAR/features_sin_autoconsumo.csv")
despues <- fread("SOLAR/features_con_autoconsumo.csv")

dontselect <- c("COD_CONTRATO", "COD_PS", "TIP_SUMINISTRO", "FEC_ALTA_PUN_SUM", "FEC_BAJA_PUN_SUM", 
              "COD_CLIENTE"       ,"TIP_CONTRATO", "FEC_ALTA_CONTRATO" ,"D_COD_POLIZA","COD_CNAE" 
              ,"COD_SOCIEDAD" ,  "COD_POLIZA"  ,"FEC_ENG_POLIZA"    ,"FEC_DGCHE_POLIZA"  ,"COD_TARIF_IBDLA"  ,     
             "TIP_EST_POLIZA"   ,"TIP_CUALIFICACION" ,"TIP_PUNTO_MEDIDA"  ,"ID", "INSTALLATION_TIMESTAMP")



antes <- antes %>% select(!all_of(dontselect))
despues <- despues %>% select(!all_of(dontselect))

antes$grupo <- "Antes"
despues$grupo <- "Despues"

combinado <- bind_rows(antes, despues)
folder <- "SOLAR/Graficos/"
archivo_pdf <- "SOLAR/graficos.pdf"
pdf(archivo_pdf, width = 10, height = 8)



for (columna in colnames(antes)) {
  if (is.numeric(antes[[columna]])) {
    # Crear boxplot usando ggplot2
    p <- ggplot(combinado, aes(x = grupo, y = !!as.name(columna))) +
      geom_boxplot() +
      labs(title = columna) + ylim(NA, quantile(combinado[[columna]], 0.8, na.rm = TRUE))

    plot(p)
    fileName <- paste(folder, gsub(" ", "_", columna), ".png", sep = "")
    print(paste("Guardando",fileName))

    # Descomentar la siguiente linea si se quiere guardar cada grafico en png
    # ggsave(fileName, plot = p, device = "png", width = 8, height = 6, units = "in")
    
    # Mostrar el gráfico
    # print(p)
    
  }
}

dev.off() # TODO probar para guardarlo en un pdf


