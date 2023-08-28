library(tidyverse)
library(vistime)

setwd("~/Documents/Universite/memoire2/data")
load("data.RData")
source("../r/exploratory_analysis/functions.R")

###############################################
#### Floraisons ###############################
###############################################

#### 1. Visualisation floraisons 75-21

# dates avec NA
Posido77_20[is.na(Posido77_20$Moyenne), 1]
# entrer dates manuellement
Posido77_20_temps <- data.frame(event = c(rep("", 3)),
                                start = c("1975-01-01", "2003-01-01", "2015-01-01"),
                                end   = c("1999-12-31", "2012-12-31", "2021-12-31"),
                                group = c(rep("Floraisons", 3)),
                                color = c(rep("#0EEE81", 3)))
Posido77_20_temps$end[2] <- as.character(as.POSIXct(min(Posido12_13$Date))-1)
vistime(Posido77_20_temps)

#### 2. Ajouter floraisons 2012-2013 et 2022

# pas de "trou" dans les donnees
Posido12_13_22_temps <- data.frame(event = c("2012-2013", "2022"),
                                   start = as.character(c(min(Posido12_13$Date),
                                                          min(Posido22$Date))),
                                   end   = as.character(c(max(Posido12_13$Date),
                                                          max(Posido22$Date))),
                                   group = "Floraisons",
                                   color = rep("#1ECB77", 2))

Posido_temps <- rbind(Posido77_20_temps, Posido12_13_22_temps)

gg_vistime(Posido_temps)

#### Bonus : ajouter evenements flo massifs 
# recenser dans la litt les evenements massifs (tableau ?) ################################################
# comparer aux evenements massifs calvi

#### 3. Ajouter temp eau

sum(is.na(MoyJourTempEau$Temp_moy))
  # pas de NA pour la temperature de l'eau moyennee par jour
sum(is.na(TempEau$Temperature))
  # pas de NA pour la temperature de l'eau non moyennee non plus
MoyJourTempEau_temps <- data.frame(event = "",
                                   start = as.character(min(MoyJourTempEau$Date)),
                                   end   = as.character(max(MoyJourTempEau$Date)),
                                   group = "Temp. eau ",
                                   color = "#13D8E5")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps))

#### 4. Ajouter temp herbier

# Obtention bornes niveaux 10m et 10+2m et visualisation
MoyJourTempHerbier_10_12_temps <- temp_herbier_visu("10m", "10+2m", group = "Temp. herbier 10-12m",
                                                    color = "#24BBC4")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, MoyJourTempHerbier_10_12_temps))

# temp herbier 3-5m
MoyJourTempHerbier_3_5_temps <- temp_herbier_visu("3m", "3+2m", group = "Temp. herbier 3-5m")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, 
                 MoyJourTempHerbier_3_5_temps, MoyJourTempHerbier_10_12_temps))

# temp herbier 20-22m
MoyJourTempHerbier_20_22_temps <- temp_herbier_visu("20m", "20+2m", group = "Temp. herbier 20-22m")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, 
                 MoyJourTempHerbier_3_5_temps, MoyJourTempHerbier_10_12_temps,
                 MoyJourTempHerbier_20_22_temps))

# temp herbier 30-32m
MoyJourTempHerbier_30_32_temps <- temp_herbier_visu("30m", "30+2m", group = "Temp. herbier 30-32m")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, 
                 MoyJourTempHerbier_3_5_temps, MoyJourTempHerbier_10_12_temps,
                 MoyJourTempHerbier_20_22_temps, MoyJourTempHerbier_30_32_temps))

# temp herbier 36-38m
MoyJourTempHerbier_36_38_temps <- temp_herbier_visu("36m", "36+2m", group = "Temp. herbier lim. inf.")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, 
                 MoyJourTempHerbier_3_5_temps, MoyJourTempHerbier_10_12_temps,
                 MoyJourTempHerbier_20_22_temps, MoyJourTempHerbier_30_32_temps,
                 MoyJourTempHerbier_36_38_temps))

#### 5. Ajouter poissons
bornes <- as.character(unique(sort(c(Poissons$P2012_2016$DATE, 
                                     Poissons$P2017_2020$DATE, 
                                     Poissons$P2021_2022$DATE))))
Poissons_temps <- data.frame(event = "",
                             start = bornes,
                             end   = bornes,
                             group = "Sarpa salpa",
                             color = "#DF9547")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, 
                 MoyJourTempHerbier_3_5_temps, MoyJourTempHerbier_10_12_temps,
                 MoyJourTempHerbier_20_22_temps, MoyJourTempHerbier_30_32_temps,
                 MoyJourTempHerbier_36_38_temps, Poissons_temps))

#### 6. Ajouter salinite
bornes <- getDatesBornes(Salinite, VarNA = 6, Data.Value = "complete")
Salinite_temps <- data.frame(event = "",
                             start = bornes$start,
                             end   = bornes$end,
                             group = "Salinité",
                             color = "#B73196")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, 
                 MoyJourTempHerbier_3_5_temps, MoyJourTempHerbier_10_12_temps,
                 MoyJourTempHerbier_20_22_temps, MoyJourTempHerbier_30_32_temps,
                 MoyJourTempHerbier_36_38_temps, Poissons_temps,
                 Salinite_temps))

#### 7. Ajouter sels nutri
# creer vecteur de NAs pour lignes contenant > 1 NA
donneeOK <- apply(is.na(SelsNutri[, 3:7]), 1, sum) 
donneeOK[donneeOK!=0] <- NA
# puis trouver les bornes
bornes <- getDatesBornes(cbind(SelsNutri, donneeOK), VarNA = 8)
SelsNutri_temps <- data.frame(event = "",
                              start = bornes$start,
                              end   = bornes$end,
                              group = "Sels nutri",
                              color = "#11832D")

gg_vistime(rbind(Posido_temps, MoyJourTempEau_temps, 
                 MoyJourTempHerbier_3_5_temps, MoyJourTempHerbier_10_12_temps,
                 MoyJourTempHerbier_20_22_temps, MoyJourTempHerbier_30_32_temps,
                 MoyJourTempHerbier_36_38_temps, # Poissons_temps,
                 Salinite_temps, SelsNutri_temps),
           title = "Ligne du temps des différentes données exploitables")

