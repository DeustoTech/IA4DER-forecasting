library(ggplot2)
library(lattice)
library(caret)
library(fpp3)
library(lattice)
library(forecast)
library(purrr)
library(data.table)
library(tidyverse)



data <- read.csv("SOLAR/RegrSolar.csv")
case1 <- data %>% filter(Train_test_Case == 1) # train t2, test t2
case2 <- data %>% filter(Train_test_Case == 2) # train t6, test t6
case3 <- data %>% filter(Train_test_Case == 3) # train t2, test t6
case4 <- data %>% filter(Train_test_Case == 4) # train t6, test t2

