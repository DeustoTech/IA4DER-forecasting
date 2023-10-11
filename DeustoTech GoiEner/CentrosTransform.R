library(data.table)
library(zoo)
library(forecast)
library(doFuture)
library(doParallel)



path <- "imp_csv.zip" # path del zip
tempdir <- tempdir() # crea un directorio temporal. Cuando cierras R, se elimina

unzip(path, exdir = tempdir) # descomprime. Tarda un poco



set.seed(123) # Salen unos 200 transformadores y el resto lineas


# Número de centros a crear. 
num_out <- 5000

# Número de clientes por línea (entre 20 y 50) 
num_clients_per_line <- sample(20:50, num_out, replace = TRUE)

# Número de CUPS por centro de transformación (entre 100 y 1000)
num_cups_per_transformer <- sample(100:1000, num_out, replace = TRUE)

# Directorio donde se encuentran los archivos CSV de los clientes
# client_files <- list.files("", pattern = ".csv$", recursive = T, full.names = F)
ALL <- list.files(path="Transformers/",pattern="*.csv")
CT  <- list.files(path="Transformers/",pattern="*-CT.csv")
L  <- list.files(path="Transformers/",pattern="*-L.csv")

FILES <- setdiff(ALL,c(CT, L))


# Crear centros de transformación y líneas
generate_transformers <- function(i) {
  # Seleccionar un número de CUPS para este transformador
  num_cups <- num_cups_per_transformer[i]
  
  # Seleccionar un número de clientes para crear una línea
  num_clients_in_line <- num_clients_per_line[i]
  clients_in_line <- sample(FILES, num_clients_in_line)
  
  # Crear un centro de transformación con los CUPS seleccionados
  result <- data.frame(
    time=seq(ISOdate(2021,06,01,00,00,00),length.out=24*457,by="hour"),
    kWh = double(24 * 457)
  )
  
  for (client_file in clients_in_line) {
    serie <- fread(paste("Transformers/", client_file, sep = ""),data.table = FALSE)
    result$kWh <- result$kWh + serie$kWh
  }
  
  # Guardar el centro de transformación como CSV
  
  if (num_clients_in_line >= 50) { # Si tiene 100 o más clientes, es un transformador
    fileToWrite <- paste("Transformers/", i, "-CT.csv", sep = "")
  } else { # sino, es una linea
    fileToWrite <- paste("Transformers/", i, "-L.csv", sep = "")
  }
  fwrite(result, file = fileToWrite, dateTimeAs = "write.csv")
}

# Ejecutar la generación de centros de transformación en paralelo
plan(multisession)

resultFiles <- foreach(i = 1:num_out, .options.future = list(seed = TRUE)) %dofuture% {
  generate_transformers(i)
}











