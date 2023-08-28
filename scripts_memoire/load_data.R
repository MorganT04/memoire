library("tidyverse")
library("readxl")
library("arules")

setwd("~/Documents/Universite/memoire2/data")
source("../r/exploratory_analysis/functions.R")

##################################
####### TEMP EAU ################# 
##################################

# NB : pressions de 2017-2023
TempEau <- readRDS("temp_eau.rds")
TempEau <- select(TempEau, select = -tssta_codesta) # enlever colonne inutile
colnames(TempEau)[3] <- "Pression_abs_kPa"

# resumer info par jour (notons que ce n'est que depuis les annees 2000 que > 1 val/jour)
  # simplifier dates
JourTempEau <- data.frame(Date = format(TempEau$daytime, format = "%Y-%m-%d"), 
                              TempEau[,2:3])

  # indice de la ligne (i-1) a partir duquel les dates sont multiples :
for(i in 2:length(JourTempEau$Date)){
  if(JourTempEau$Date[i-1]==JourTempEau$Date[i]){return(print(i-1))}
}

  # parcourir les dates a partir du 2000-04-19 (indice i-1 et plus)...
for(date in unique(JourTempEau[(i-1):dim(JourTempEau)[1], ]$Date)){
  if(date == "2000-04-19"){MoyJourTempEau <- data.frame(JourTempEau[1:(i-2), 1:2],
                                                        nT  = rep(1, i-2),
                                                        StT = rep(NA, i-2),
                                                        Pression_abs_kPa = JourTempEau[1:(i-2), 3],
                                                        nP  = rep(NA, i-2),
                                                        StP = rep(NA, i-2))} # dates avant 2000-04-19
  # ... calculer la temp et press moy par jour (avec n et St)...
  tempMoy  <- subset(JourTempEau, subset = Date==date, select = Temperature)[,1]
  nT       <- length(tempMoy[!is.na(tempMoy)]) # pas prendre NAs en compte pour n et st
  StT      <- sd(tempMoy, na.rm = T)
  tempMoy  <- mean(tempMoy, na.rm = T)
  pressMoy <- subset(JourTempEau, subset = Date==date, select = Pression_abs_kPa)[,1]
  nP       <- length(pressMoy[!is.na(pressMoy)])
  StP      <- sd(pressMoy, na.rm = T)
  pressMoy <- mean(pressMoy, na.rm = T)
  # ... puis generer le tableau avec les temp moy et press moy pour chaque jour
  MoyJourTempEau <- rbind(MoyJourTempEau, 
                              data.frame(Date = date, 
                                         Temperature = tempMoy,
                                         nT = nT,
                                         StT = StT,
                                         Pression_abs_kPa = pressMoy,
                                         nP = nP,
                                         StP = StP))
}
colnames(MoyJourTempEau)[c(2,5)] <- c("Temp_moy", "Press_moy_kPa")
rm(date, tempMoy, pressMoy, i, nT, nP, StT, StP)

# ajouter info du lieu (port de STARESO)
MoyJourTempEau <- data.frame(MoyJourTempEau, Location = as.factor("Stareso"))
# convertir date en format POSIXct
MoyJourTempEau$Date <- as.POSIXct(MoyJourTempEau$Date)

##################################
####### TEMP HERBIER #############
##################################
# et herbier+2m (et limite inf a 36m et 36m+2) 

TempHerbier <- readRDS("temp_herbier.rds")
colnames(TempHerbier)[2:3] <- c("Temperature", "Profondeur")
TempHerbier$Profondeur <- as.factor(TempHerbier$Profondeur)
count_na(TempHerbier)
TempHerbier <- drop_na(TempHerbier) # enlever les 78 dates inconnues
count_na(TempHerbier)

# resumer info par jour
  # simplifier dates
JourTempHerbier <- data.frame(Date = format(TempHerbier$Datetime, format = "%Y-%m-%d"), 
                              TempHerbier[,2:3])

  # parcourir les dates pour chaque profondeur...
for(prof in unique(JourTempHerbier$Profondeur)){
  if(prof==unique(JourTempHerbier$Profondeur)[1]){MoyJourTempHerbier <- data.frame()}
  for(date in unique(JourTempHerbier$Date)){
    # ... en recuperer toutes les temperatures et calculer la moyenne...
    tempMoy <- subset(JourTempHerbier, 
                      subset = Profondeur==prof & Date==date, 
                      select = Temperature)[,1]
    n       <- length(tempMoy[!is.na(tempMoy)]) # pas prendre NAs en compte pour n et st
    St      <- sd(tempMoy, na.rm = T)
    tempMoy <- mean(tempMoy)
    # ... puis generer le tableau avec les temp moy pour chaque jour a chaque profondeur
    MoyJourTempHerbier <- rbind(MoyJourTempHerbier, 
                                data.frame(Date = date, 
                                           Profondeur = prof,
                                           Temp_moy = tempMoy,
                                           n = n, St = St))
  }
}
rm(prof, date, tempMoy, n, St)
# convertir date en format POSIXct
MoyJourTempHerbier$Date <- as.POSIXct(MoyJourTempHerbier$Date)
# convertir les prof en facteurs
MoyJourTempHerbier$Profondeur <- as.factor(MoyJourTempHerbier$Profondeur)
count_na(MoyJourTempHerbier) # donnees manquantes pour beaucoup de dates

  # obtention du nombre de NAs par profondeur + proportion par profondeur et barplot
for(prof in levels(TempHerbier$Profondeur)[c(10,5,2,1,4,3,7,6,9,8)]){
  if(prof == levels(TempHerbier$Profondeur)[10]){nNA <- pNA <- vector(length = 10) ; i <- 1}
  nNA[i] <- sum(is.na(MoyJourTempHerbier$Temp_moy[MoyJourTempHerbier$Profondeur==prof]))
  pNA[i] <- nNA[i]/nrow(subset(MoyJourTempHerbier, subset = Profondeur==prof))
  i <- i+1
}
rm(prof, i)
par(mfrow=c(1,2))
barplot(nNA, names.arg = levels(TempHerbier$Profondeur)[c(10,5,2,1,4,3,7,6,9,8)],
        xlab = "Profondeur", ylab = "Nombre de NAs",
        main = "Nombre de NAs par niveau de profondeur",
        space = 0, col = "#85C1E9")
barplot(pNA*100, names.arg = levels(TempHerbier$Profondeur)[c(10,5,2,1,4,3,7,6,9,8)],
        xlab = "Profondeur", ylab = "Proportion de NAs [%]",
        main = "Proportion de NAs par niveau de profondeur",
        space = 0, col = "#85C1E9")
par(mfrow=c(1,1))
rm(nNA, pNA)

# for(depth in levels(TempHerbier$Profondeur)[c(10,5,2,1,4,3,7,6,9,8)]){
#   if(depth == levels(TempHerbier$Profondeur)[10]){
#     dat <- data.frame(Datetime = format(TempHerbier$Datetime, format = "%Y-%m-%d"), 
#                       TempHerbier[,2:3])
#     par(mfrow=c(2,5))
#   }
#   subset(dat, subset = Profondeur==depth, select = Datetime) %>%
#     unique() %>%
#     plot(main = depth, cex=0.01, ylim = c(min(dat$Datetime), max(dat$Datetime)))
# }
# rm(dat)



##################################
####### SALINITE #################
##################################
# (2 points, a 40 et 60m, avec leurs coord resp.)

Salinite <- readRDS("ProfilCTD_Point40-60m.rds")
Salinite$Site <- as.factor(Salinite$Site)
# discretiser les profondeurs pour avoir des intervalles
Salinite <- data.frame(Salinite[,1:3], 
                       Prof_group = discretize(Salinite$Profondeur, 
                                               method = "fixed", 
                                               breaks = c(0,2,4,9,11,19,21,29,31,
                                                          35,37,max(Salinite$Profondeur))), 
                       Salinite[,4:7])

# moyenner salinite par jour par site et par intervalle de profondeur
for(site in levels(Salinite$Site)){
  if(site==levels(Salinite$Site)[1]){Salinite_moy <- data.frame()}
  for(jour in unique(subset(Salinite, subset = Site==site, select = Date)[,1])){
    dat <- subset(Salinite, subset = Site == site & Date == jour)
    for(prof in unique(dat$Prof_group)){
      sali <- subset(dat, subset = Prof_group==prof, select = Salinite)[, 1]
      Sali_moy <- mean(sali, na.rm = T)
      n <- length(sali[!is.na(sali)])
      St <- sd(sali, na.rm = T)
      Salinite_moy <- rbind(Salinite_moy, 
                             data.frame(Date = jour,
                                        Site = site,
                                        Profondeur = prof,
                                        Sali_moy = Sali_moy,
                                        n = n,
                                        St = St))
    }
  }
}
attributes(Salinite_moy$Date) <- attributes(Salinite$Date)
rm(dat, site, jour, prof, sali, Sali_moy, n, St)


##################################
####### SELS NUTRITIFS ###########
##################################
# (µmol/l, POINT40)

SelsNutri <- select(readRDS("Nutriments_POINT40_2012-2022.rds"), select = -Unite)
SelsNutri <- data.frame(Date = SelsNutri$Date,
                        Site = as.factor(rep("POINT40", nrow(SelsNutri))),
                        SelsNutri[,-1])

##################################
####### FAUNE ICHTYOLOGIQUE ######
##################################

anneesPoi <- as.character(seq(2012, 2022))
for(annee in anneesPoi){
  Poissons <- rbind(read_xlsx("Suivi_poisson_2012-2022_REVELLATA.xlsx", sheet = annee), 
                    read_xlsx("Suivi_poisson_2012-2022_SPANO.xlsx", sheet = annee))
  assign(paste0("Poissons", annee), Poissons, envir = globalenv())
}

# assembler tableaux 2012-2016
for(annee in as.character(seq(2012, 2016))){
  if(annee=="2012"){Poissons2012_2016 <- data.frame()}
  Poissons2012_2016 <- rbind(Poissons2012_2016, get(paste0("Poissons", annee)))
}

# assembler tableaux 2017-2020
for(annee in as.character(seq(2017, 2020))){
  if(annee=="2017"){Poissons2017_2020 <- data.frame()}
  Poissons2017_2020 <- rbind(Poissons2017_2020, get(paste0("Poissons", annee)))
}

# assembler tableaux 2021-2022
Poissons2022 <- subset(Poissons2022, select = -HEURE)
Poissons2021 <- Poissons2021[, -dim(Poissons2021)[2]]
colnames(Poissons2021)[c(ncol(Poissons2021)-1, ncol(Poissons2021))] <- c("PROFONDEUR MEROU", 
                                                                         "COMPORTEMENT MEROU")
Poissons2021_2022 <- rbind(Poissons2021, Poissons2022)

Poissons <- list(P2012_2016 = Poissons2012_2016, 
                 P2017_2020 = Poissons2017_2020, 
                 P2021_2022 = Poissons2021_2022)


rm(list = c("anneesPoi", "annee", ls(pattern = "Poissons.")))
colnames(Poissons$P2012_2016)[6] <- c("NOM_LATIN")
colnames(Poissons$P2017_2020)[c(6, 
                                ncol(Poissons$P2017_2020)-1, 
                                ncol(Poissons$P2017_2020))] <- c("NOM_LATIN", 
                                                                "PROFONDEUR_MEROU", 
                                                                "COMPORTEMENT_MEROU")
colnames(Poissons$P2021_2022)[c(6, 
                                ncol(Poissons$P2021_2022)-1, 
                                ncol(Poissons$P2021_2022))] <- c("NOM_LATIN", 
                                                                 "PROFONDEUR_MEROU", 
                                                                 "COMPORTEMENT_MEROU")



Poissons$P2012_2016$SITE <- as.factor(Poissons$P2012_2016$SITE)
Poissons$P2017_2020$SITE <- as.factor(Poissons$P2017_2020$SITE)
Poissons$P2021_2022$SITE <- as.factor(Poissons$P2021_2022$SITE)

# on ne s'interesse qu'a Sarpa salpa :
Poissons$P2012_2016 <- subset(Poissons$P2012_2016, 
                              subset = NOM_LATIN=="Sarpa salpa")
Poissons$P2017_2020 <- subset(Poissons$P2017_2020, 
                              subset = NOM_LATIN=="Sarpa salpa", 
                              select = -c(OBSERVATEURS, PORTION, REPLICAT,
                                          PROFONDEUR_MEROU, COMPORTEMENT_MEROU))
Poissons$P2021_2022 <- subset(Poissons$P2021_2022, 
                              subset = NOM_LATIN=="Sarpa salpa", 
                              select = -c(OBSERVATEURS, PORTION, REPLICAT,
                                          PROFONDEUR_MEROU, COMPORTEMENT_MEROU))
# enlever les colonnes vides :
empty <- list(P2012_2016 = colSums(is.na(Poissons$P2012_2016))==nrow(Poissons$P2012_2016),
              P2017_2020 = colSums(is.na(Poissons$P2017_2020))==nrow(Poissons$P2017_2020),
              P2021_2022 = colSums(is.na(Poissons$P2021_2022))==nrow(Poissons$P2021_2022))
Poissons <- list(P2012_2016 = Poissons$P2012_2016[, !empty$P2012_2016],
                 P2017_2020 = Poissons$P2017_2020[, !empty$P2017_2020],
                 P2021_2022 = Poissons$P2021_2022[, !empty$P2021_2022])
rm(empty)

#### CE QUI SUIT SUR LES POISSONS SERAIT A REVOIR VU QUE J'AI TRANSFORME EN LISTE !!!
# moyenne nombre poissons par jour et par site
for(site in levels(Poissons$SITE)){
  if(site==levels(Poissons$SITE)[1]){SommeJourPoissons <- data.frame()}
  for(jour in unique(Poissons[Poissons$SITE==site,]$DATE)){
    dat <- subset(Poissons, subset = SITE==site & DATE==jour,
                  select = -c(DATE, SITE, NOM_LATIN))
    dat[is.na(dat)] <- 0
    nPoi <- sum(dat)
    attributes(jour) <- attributes(Poissons$DATE[1])
    SommeJourPoissons <- rbind(SommeJourPoissons, 
                               data.frame(Date = jour, Site = site, TotPoi = nPoi))
  }
}
rm(jour, nPoi, site, dat)

# evolution nombre poissons par site
ggplot(SommeJourPoissons, aes(x=Date, colour = Site)) +
  geom_line(aes(y = TotPoi, color = Site)) +
  scale_color_manual(values=c("green", "#9999CC"))

##################################
####### FLORAISON POSIDO #########
##################################

# 2022
Posido22 <- read_xlsx("Posidonia Data .xlsx")
colnames(Posido22) <- c("Period", "Transect", "Observer", "Date", "Depth_ft", 
                      "Meter_mark", "Shoots", "Flowers", "Flowers_pourc", 
                      "Quadrat_size", "Shoots_m2", "Flowers_m2", "Location")
Posido22 <- subset(Posido22, select = c("Date", "Depth_ft", "Flowers_pourc", 
                                    "Shoots_m2", "Flowers_m2", "Location"))
Posido22$Depth_ft <- round(Posido22$Depth_ft/3.281)
colnames(Posido22)[2] <- "Depth_m"
Posido22$Location <- as.factor(paste0("Stareso_", Posido22$Location))

subset(Posido22, subset = Depth_m==6) %>%
  ggplot(aes(x=Date))+
    geom_line(aes(y = Flowers_m2))

# 1977-2020
Posido77_20 <- read_xlsx("IF1977-2020.xlsx")
Posido77_20$Profondeur_m <- as.factor(Posido77_20$Profondeur_m)
  # obtenir un format dates (fin d'annees)
Posido77_20$Date <- as.POSIXct(paste(Posido77_20$Date, 12, 31, sep = "-"))

Posido77_20$n <- as.numeric(Posido77_20$n)
Posido77_20$Moyenne <- as.numeric(Posido77_20$Moyenne)
Posido77_20$St <- as.numeric(Posido77_20$St)

# generer la colonne annee a partir des dates (visualisation graphs)
Posido77_20 <- data.frame(Posido77_20[,1], 
                          Annee = as.integer(format(Posido77_20$Date, format = "%Y")),
                          Posido77_20[,2:7])

# 2012-2013
Posido12_13 <- read_xlsx("IF2012-2013.xlsx")

# save only the tables
save(list = ls(pattern = "^[A-Z]"), file = "data.RData")
