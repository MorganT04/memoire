library(tidyverse)
library(ggplot2)
library(grid)
library(cowplot)
library(ggbreak)

setwd("~/Documents/Universite/memoire2/r")

# chargement des dataframes sur la temp eau et la posido + fonctions
load("../data/floraison_posido.RData")
load("../data/statEau_TempEau.RData")
source("exploratory_analysis/functions.R")

# profils temp 
groupAnnee <- rep("2008-2010", nrow(TempEauNorm0101))
groupAnnee[format(TempEauNorm0101$Date, format = "%Y") < 2008] <- "1985-2007"
groupAnnee[format(TempEauNorm0101$Date, format = "%Y") > 2010] <- "2011-2019"
groupAnnee[format(TempEauNorm0101$Date, format = "%Y") > 2019] <- "2020-2022" # VOIR SI JE NE FAIS PAS LES 3 DERNIERES ANNEES !!
groupAnnee <- as.factor(groupAnnee)

P3 <- TempEauNorm0101 %>%
  cbind(groupAnnee) %>%
  filter(groupAnnee != "2008-2010") %>%
  filter(Date >= as.POSIXct("1985-01-01")) %>%
  mutate(Mois = format(Date, format="%m-%d")) %>%
  ggplot(aes(x = Mois, group = groupAnnee, color = groupAnnee)) +
  geom_smooth(aes(y = Temp_moy), alpha = 0.2) +
  geom_vline(aes(xintercept = 80), linetype = "dashed", alpha = 0.4) +
  geom_vline(aes(xintercept = 170), linetype = "dashed", alpha = 0.4) +
  geom_vline(aes(xintercept = 260), linetype = "dashed", alpha = 0.4) +
  geom_vline(aes(xintercept = 350), linetype = "dashed", alpha = 0.4) +
  # geom_vline(aes(xintercept = 12), linetype = "dashed", alpha = 0.3) +
  xlab("Saison") +
  ylab("Températures moyennes smooth [°C]") +
  theme_light() +
  labs(color = "Période") +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  geom_text(aes(x = 35, y = 12.3, label = "hiver"), color = "grey", size = 5) +
  geom_text(aes(x = 125, y = 12.3, label = "printemps"), color = "grey", size = 5) +
  geom_text(aes(x = 215, y = 12.3, label = "été"), color = "grey", size = 5) +
  geom_text(aes(x = 305, y = 12.3, label = "automne"), color = "grey", size = 5) +
  ylim(12, 27) +
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=14))

P3


