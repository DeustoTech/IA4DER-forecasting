library(ggplot2)
library(lattice)
library(caret)
library(fpp3)
library(lattice)
library(forecast)
library(purrr)
library(data.table)
library(tidyverse)

L <- fread("ResultadosNuevosL.csv")
CT <- fread("ResultadosNuevosCT.csv")
CUPS <- fread("ResultadosNuevosCUPS.csv")

#graficos 

