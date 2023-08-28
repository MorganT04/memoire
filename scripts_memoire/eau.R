# Ici, je regarde un maximum de parametres concernant la temperature de l'eau (TempEau).

library("tidyverse")
library("ggplot2")

setwd("~/Documents/Universite/memoire2/data")
load("data.RData")
rm(list = grep(ls(), pattern = "TempEau|Posido", invert = T, value = T))
source("../r/exploratory_analysis/functions.R")

# idees :
# decoupe en fonction des mois
# zeitgeber tous les 11 ans ? 
# accumulation chaleur ? a partir de quand ? ou alors pic ?
# faire avec absence/presence de floraison, mettre un threshold et faire varier pour voir impact sur correlation

##################################
########## A. ANNEE 2012 #########
##################################

# Evolution temp annee 2012
JourTempEau$Date <- as.POSIXct(JourTempEau$Date)
subset(JourTempEau, subset = Date >= as.POSIXct("2012-01-01") & Date <= as.POSIXct("2012-12-31")) %>%
  ggplot(aes(x = Date, y = Temperature))+
  geom_line(aes(y = Pression_abs_kPa))+
  geom_point(size = 0.1)+
  geom_line(color="red")
  