library("tidyverse")

setwd("~/Documents/Universite/memoire2/r")

# chargement des dataframes sur la temp eau et la posido + fonctions
load("../data/floraison_posido.RData")
load("../data/statEau_TempEau.RData")
source("exploratory_analysis/functions.R")

# on va faire les analyses sur toutes les annees sauf la periode 2008-2010.
# ainsi, on modifie le tableau de donnees contenant les IFs de facon a ce que la periode
# d'etude soit continue (plus facile pour programmer la fonction apres) et que les 3 annees
# retirees soient a la fin du dataframe (virtuellement 3008-3010).
# ces 3 annees serviront de verifiation du modele
# NB : il aurait ete plus propre de pouvoir choisir des periodes non continues dans la fonction...
Posido77_20[Posido77_20$Annee >= 2008 & Posido77_20$Annee <= 2010, ]$Annee <- c(3008, 3009, 3010)
Posido77_20[Posido77_20$Annee >= 2011 & 
              Posido77_20$Annee < 3008, ]$Annee <- Posido77_20[Posido77_20$Annee >= 2011 & 
                                                                 Posido77_20$Annee < 3008, ]$Annee - 3
Posido77_20 <- Posido77_20[order(Posido77_20$Annee), ]

# voir si temp min atteinte a un moment donne floraison
# voir si accumulation temp min donne floraison

####################################################
# EXACTITUDE MODELES TEMP MIN ATTEINTE =? FLORAISON
####################################################

## TOUTES LES SAISONS
for(i in 1:10){ # on cherche a trouver les coordonnees les meilleures 10 fois...
  if(i==1){
    coordmaxjustesse <- 0
    maxjustesse <- 0
    coordmaxjustesseAUT <- 0
    coordmaxjustesseETE <- 0
    coordmaxjustesseHIV <- 0
    coordmaxjustessePRI <- 0}
  justesse <- 0
  maxjustesse[i] <- 0
  n <- 0
  while (n < 1000) { # ... en faisant 1000 tests et en ne gardant que les meilleures coord a chaque fois
    var <- justesse_temp_sup(c(runif(1, min = 20, max = 26), runif(1, min = 23, max = 29),
                               runif(1, min = 11, max = 17), runif(1, min = 19, max = 25)), # aut, ete, hiv, print
                             period = c(1982, 2019)) # periode 1982-2007 et 2011-2022
    justesse <- var[1]
    if(justesse > maxjustesse[i]){
      maxjustesse[i] <- justesse
      coordmaxjustesseAUT[i] <- var[2]
      coordmaxjustesseETE[i] <- var[3]
      coordmaxjustesseHIV[i] <- var[4]
      coordmaxjustessePRI[i] <- var[5]
    }
    n <- n+1
    message(i)
  }
  if(i==10){
    print(cbind(maxjustesse, coordmaxjustesseAUT, coordmaxjustesseETE, coordmaxjustesseHIV, coordmaxjustessePRI))
    sup_max_all <- cbind(maxjustesse, coordmaxjustesseAUT, coordmaxjustesseETE, coordmaxjustesseHIV, coordmaxjustessePRI)
    CI_sup_max_all <- apply(cbind(maxjustesse, coordmaxjustesseAUT, coordmaxjustesseETE, coordmaxjustesseHIV, coordmaxjustessePRI),
                            MARGIN = 2, compute_CI)
  }
}

## ETES
for(i in 1:10){
  if(i==1){coordmaxjustesse <- NULL ; maxjustesse <- 0}
  justesse <- 0
  maxjustesse[i] <- 0
  n <- 0
  while (n < 1000) {
    var <- justesse_temp_sup(runif(1, min = 23, max = 29), seasons = "ete", period = c(1982, 2019))
    justesse <- var[1]
    if(justesse > maxjustesse[i]){
      maxjustesse[i] <- justesse
      coordmaxjustesse[i] <- var[2]
    }
    # print(var[2:5])
    n <- n+1
    message(i)
  }
  if(i==10){
    print(cbind(maxjustesse, coordmaxjustesse))
    # CIete <- compute_CI(coordmaxjustesse, a = 0.95)
    # CIetejustesse <- compute_CI(maxjustesse, a = 0.95)
    sup_max_ete <- cbind(maxjustesse, coordmaxjustesse)
    CI_sup_max_ete <- apply(cbind(maxjustesse, coordmaxjustesse), MARGIN = 2, compute_CI)
  }
}

## ETES + PRINTEMPS
for(i in 1:10){
  if(i==1){coordmaxjustesse <- NULL ; maxjustesse <- 0}
  justesse <- 0
  maxjustesse[i] <- 0
  n <- 0
  while (n < 1000) {
    var <- justesse_temp_sup(c(runif(1, min = 23, max = 29), runif(1, min = 19, max = 25)), 
                             seasons = c("ete", "printemps"), period = c(1982, 2019))
    justesse <- var[1]
    if(justesse > maxjustesse[i]){
      maxjustesse[i] <- justesse
      coordmaxjustesseETE[i] <- var[2]
      coordmaxjustessePRI[i] <- var[3]
    }
    # print(var[2:5])
    n <- n+1
    message(i)
  }
  if(i==10){
    print(cbind(maxjustesse, coordmaxjustesseETE, coordmaxjustessePRI))
    # CIete <- compute_CI(coordmaxjustesse, a = 0.95)
    # CIetejustesse <- compute_CI(maxjustesse, a = 0.95)
    sup_max_ete_print <- cbind(maxjustesse, coordmaxjustesseETE, coordmaxjustessePRI)
    CI_sup_max_ete_print <- apply(cbind(maxjustesse, coordmaxjustesseETE, coordmaxjustessePRI), MARGIN = 2, compute_CI)
  }
}

####################################################
# EXACTITUDE MODELES TEMP MIN NE DEPASSANT PAS SEUIL =? FLORAISON
####################################################
# hypothèse du mecanisme de detection de l'hiver

## HIVERS
for(i in 1:10){
  if(i==1){coordmaxjustesse <- NULL ; maxjustesse <- 0}
  justesse <- 0
  maxjustesse[i] <- 0
  n <- 0
  while (n < 1000) {
    var <- justesse_temp_inf(runif(1, min = 10, max = 15), seasons = "hiver", period = c(1982, 2019), prev = FALSE)
    justesse <- var[1]
    if(justesse > maxjustesse[i]){
      maxjustesse[i] <- justesse
      coordmaxjustesse[i] <- var[2]
    }
    # print(var[2:5])
    n <- n+1
    message(i)
  }
  if(i==10){
    print(cbind(maxjustesse, coordmaxjustesse))
    # CIete <- compute_CI(coordmaxjustesse, a = 0.95)
    # CIetejustesse <- compute_CI(maxjustesse, a = 0.95)
    hiv <- cbind(maxjustesse, coordmaxjustesse)
    CI_hiv <- apply(cbind(maxjustesse, coordmaxjustesse), MARGIN = 2, compute_CI)
  }
}

## HIVERS (TEMPERATURES DE L'ANNEE PRECEDANTE)
for(i in 1:10){
  if(i==1){coordmaxjustesse <- NULL ; maxjustesse <- 0}
  justesse <- 0
  maxjustesse[i] <- 0
  n <- 0
  while (n < 1000) {
    var <- justesse_temp_inf(runif(1, min = 10, max = 15), seasons = "hiver", period = c(1982, 2019), prev = TRUE)
    justesse <- var[1]
    if(justesse > maxjustesse[i]){
      maxjustesse[i] <- justesse
      coordmaxjustesse[i] <- var[2]
    }
    # print(var[2:5])
    n <- n+1
    message(i)
  }
  if(i==10){
    print(cbind(maxjustesse, coordmaxjustesse))
    # CIete <- compute_CI(coordmaxjustesse, a = 0.95)
    # CIetejustesse <- compute_CI(maxjustesse, a = 0.95)
    hiv_prev <- cbind(maxjustesse, coordmaxjustesse)
    CI_hiv_prev <- apply(cbind(maxjustesse, coordmaxjustesse), MARGIN = 2, compute_CI)
  }
}

####################################################
# EXACTITUDE MODELES AMPLITUDE HIVER ETE DEPASSE SEUIL =? FLORAISON
####################################################
# NB : l'amplitude est calculee selon mean(nJchauds)-mean(nJfroids)

## HIVERS ET ETES
for(i in 1:10){
  if(i==1){coordmaxjustesse <- NULL ; maxjustesse <- 0 ; nJchauds <- 1 ; nJfroids <- 1}
  justesse <- 0
  maxjustesse[i] <- 0
  n <- 0
  while (n < 10) {
    var <- justesse_temp_ampl(runif(1, min = 10, max = 30), # amplitude seuil
                              n1 = round(runif(1, min = 1, max = 20)), # nbre jours chauds
                              n2 = round(runif(1, min = 1, max = 20)), # nbre jours froids
                              seasons = c("ete","hiver"), period = c(1982, 2019))
    justesse <- var[1]
    if(justesse > maxjustesse[i]){
      maxjustesse[i] <- justesse
      coordmaxjustesse[i] <- var[2]
      nJchauds[i] <- var[3]
      nJfroids[i] <- var[4]
    }
    # print(var[2:5])
    n <- n+1
    message(i)
  }
  if(i==10){
    print(cbind(maxjustesse, coordmaxjustesse, nJchauds, nJfroids))
    # CIete <- compute_CI(coordmaxjustesse, a = 0.95)
    # CIetejustesse <- compute_CI(maxjustesse, a = 0.95)
    ampl_ete_hiv <- cbind(maxjustesse, coordmaxjustesse, nJchauds, nJfroids)
    CI_ampl_ete_hiv <- apply(cbind(maxjustesse, coordmaxjustesse, nJchauds, nJfroids), MARGIN = 2, compute_CI)
  }
}


# pareil mais avec temp froides automne ?


#####################################################
# EXACTITUDE FENETRE ALEATOIRE SUR SOMME TEMPERATURES
#####################################################

# indices de floraison
IFs <- Posido77_20[Posido77_20$Annee >= 1982 & Posido77_20$Annee <= 2019, c(2,4)]
# pool de dates -> on recupere les dates presentes dans toutes les annees
pool <- TempEauNorm0101Saison %>%
  mutate(Mois = as.factor(format(Date, format = "%m"))) %>%
  subset(subset = (Mois != "11" & Mois != "12")) %>%
  mutate(MoisJour = format(Date, format = "%m-%d")) %>%
  mutate(Annee = format(Date, format = "%Y")) %>%
  subset(subset = MoisJour != "02-29") %>%
  group_by(MoisJour) %>%
  summarise(n = n()) %>%
  subset(subset = n == 41, select = MoisJour) %>%
  unlist()

# donnees modifiees pour avoir une colonne MoisJour
data <- TempEauNorm0101Saison %>%
  subset(subset = (Annee != 2008 & Annee != 2009 & Annee != 2010)) %>%
  mutate(MoisJour = format(Date, format = "%m-%d"))



for(i in 1:10){
  if(i==1){start <- NULL ; end <- NULL ; maxjustesse <- 0 ; taille_fen <-  0 ; seuil <- 0 ; Jstart <- NULL ; Jend <- NULL}
  justesse <- 0
  maxjustesse[i] <- 0
  n <- 0
  while (n < 50000) {
    var <- simili_window(bound1 = round(runif(1, 1, length(pool))), bound2 = round(runif(1, 1, length(pool))))
    justesse <- var[1]
    if(justesse > maxjustesse[i]){
      maxjustesse[i] <- as.numeric(justesse)
      start[i] <- as.integer(var[2])
      end[i] <- as.integer(var[3])
      Jstart[i] <- var[4]
      Jend[i]   <- var[5]
      taille_fen[i] <- as.integer(var[6])
      seuil[i] <- as.numeric(var[7])
    }
    n <- n+1
    message(i)
  }
  if(i==10){
    print(data.frame(cbind(maxjustesse, start, end, Jstart, Jend, taille_fen, seuil)))
    # CIete <- compute_CI(coordmaxjustesse, a = 0.95)
    # CIetejustesse <- compute_CI(maxjustesse, a = 0.95)
    fenetre <- data.frame(maxjustesse, start, end, taille_fen, seuil)
    CI_fenetre <- apply(data.frame(maxjustesse, start, end, taille_fen, seuil), MARGIN = 2, compute_CI)
  }
}

data.frame(Justesse = round(fenetre$maxjustesse, 3), 
           Début = Jstart, 
           Fin = Jend, 
           Taille = fenetre$taille_fen,
           Seuil = round(fenetre$seuil, 3)) %>% write.csv(file = "exploratory_analysis/fenetre2.csv")

save.image(file = "exploratory_analysis/fenetre_all.RData")

# CI juste pour les meilleures valeurs de la fenetre
CI_fenetre_max <- apply(fenetre[fenetre$maxjustesse > 0.76, ], 2, compute_CI)

Posido77_20[Posido77_20$Annee == 3008 | Posido77_20$Annee == 3009 | Posido77_20$Annee == 3010, ] %>%
  mutate(Annee = Annee-1000) %>%
  reframe(IF_bool = Moyenne!=0)

# CI_fenetre_max$start$mean

TempEauNorm0101Saison %>%
  subset(subset = (Annee==2008 | Annee==2009 | Annee==2010)) %>%
  mutate(Mois = format(Date, format ="%m")) %>%
  filter(Mois=="04" | Mois=="05") -> data

data[c(10:31,71:92,132:153),] %>%
  group_by(Annee) %>%
  summarise(Temp_sum = sum(Temp_moy))
