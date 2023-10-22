library(arrow)
 
d    <- open_dataset(source="./anm_ids01_test_aggct_lec_horaria")
so   <- Scanner$create(d)
at   <- so$ToTable()
lec  <- as.data.frame(at)

lec$ID_USUARIO  <- str_replace(lec$ID_USUARIO,fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),"")
lec$ID_LINEA_BT <- str_replace(lec$ID_LINEA_BT,fixed("+/G1i/4PIPVyZJkeRIas7gj6nbPBAjEfZ0td9g=="),"")

lec$ID_USUARIO  <- str_replace(lec$ID_USUARIO,fixed("/"),"\\")
lec$ID_LINEA_BT <- str_replace(lec$ID_LINEA_BT,fixed("/"),"\\")

fwrite(lec,"mtlf-test-values.csv",row.names=F)

d    <- open_dataset(source="./anm_ids01_punto_suministro")
so   <- Scanner$create(d)
at   <- so$ToTable()
cups <- as.data.frame(at)

d    <- open_dataset(source="./anm_ids01_cgp")
so   <- Scanner$create(d)
at   <- so$ToTable()
cgp  <- as.data.frame(at)

zzz <- merge(cups,cgp, by.x = "COD_SIC_SIGRID", by.y = "ID_CAJA")

yyy <- zzz[,c("CUPS","COD_SIC_SIGRID","ID_USUARIO_INST_PADRE")]
names(yyy) <- c("CUPS","CGP","CT")

