library(tidyverse)
library(ggplot2)
library(grid)
library(cowplot)
library(ggbreak)

setwd("~/Documents/Universite/memoire2/data")
load("data.RData")
rm(list = grep(ls(), pattern = "Poissons", invert = T, value = T))
load("floraison_posido.RData")
source("../r/exploratory_analysis/functions.R")

# dire qu'on n'a pas assez de donnees pour retirer qque chose avec presence absence (voir IFs nas)

# Calcul du nombre de poissons moyen pour 2018-2020 sur la periode automne-hiver
Poissons$P2017_2020 %>%
  mutate(SAISON = saisons(DATE), ANNEE = as.integer(format(DATE, format = "%Y"))) %>%
  filter(SAISON == "automne" | SAISON == "hiver") %>%
  filter(SITE == "Revellata") -> Poissons1720

data.frame(Annee = Poissons1720$ANNEE, n = apply(Poissons1720[, c(4:6)], 1, sum)) %>%
  group_by(Annee) %>%
  summarise(PoissonsMoy = mean(n)) -> Poissons1720Moy

# Calcul du nombre de poissons moyen pour 2021-2022 sur la periode automne-hiver
Poissons$P2021_2022 %>%
  mutate(SAISON = saisons(DATE), ANNEE = as.integer(format(DATE, format = "%Y"))) %>%
  filter(SAISON == "automne" | SAISON == "hiver") %>%
  filter(SITE == "Revellata") -> Poissons2122

data.frame(Annee = Poissons2122$ANNEE, n = apply(Poissons2122[, c(4:30)], 1, sum, na.rm = T)) %>%
  group_by(Annee) %>%
  summarise(PoissonsMoy = mean(n)) -> Poissons2122Moy


rbind(data.frame(Annee = Poissons1720$ANNEE, n = apply(Poissons1720[, c(4:6)], 1, sum)),
      data.frame(Annee = Poissons2122$ANNEE, n = apply(Poissons2122[, c(4:30)], 1, sum, na.rm = T))) -> Poissons1822

Poissons1822 %>%
  group_by(Annee) %>%
  summarise(Max = max(n))

# Intervalles de confiance par annee
for(annee in unique(Poissons1822$Annee)){
  CI <- Poissons1822 %>% filter(Annee == annee) %>% subset(select = n) %>% unlist() %>% compute_CI()
  assign(paste0("CI_", annee), CI, envir = globalenv())
  rm(CI)
}

# moy <- c(CI_2018$mean[1], CI_2019$mean[1], CI_2020$mean[1], CI_2021$mean[1], CI_2022$mean[1])
# inf <- c(CI_2018$bounds[1], CI_2019$bounds[1], CI_2020$bounds[1], CI_2021$bounds[1], CI_2022$bounds[1])
# inf[inf<0] <- 0
# sup <- c(CI_2018$bounds[2], CI_2019$bounds[2], CI_2020$bounds[2], CI_2021$bounds[2], CI_2022$bounds[2])

# plot(x = unique(Poissons1822$Annee), y = moy)

cbind(rbind(Poissons1720Moy, Poissons2122Moy), Posido77_20[44:48,]$Moyenne)[, -1] %>%
  cor(method = "pearson")


max()

# parler des moyennes et de leurs dev enormes
# ajouter IF et faire cor pearson

