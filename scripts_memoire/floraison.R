library(tidyverse)
library(ggplot2)
library(grid)
library(cowplot)
library(ggbreak)

setwd("~/Documents/Universite/memoire2/data")
load("data.RData")
rm(list = grep(ls(), pattern = "Posido", invert = T, value = T))
source("../r/exploratory_analysis/functions.R")


####################################
#### Floraison 2012-13 #############
####################################

# Calcul intervalles de confiance pour une moyenne d'écart-type connu :
  # 1. n>=100 ? NON ! On utilise donc la loi de student avec n-1 degres de liberte
  # 2. Quantile a 95% de confiance pour 9 degres de liberte :
t <- qt(1-0.025, 9) # NB : si n > 100 --> t = Z = 2
  # 3. Calcul des intervalles :
inf <- Posido12_13$M-(t*(Posido12_13$s/sqrt(Posido12_13$n)))
sup <- Posido12_13$M+(t*(Posido12_13$s/sqrt(Posido12_13$n)))

ggplot(Posido12_13, aes(x = Date, y = M))+
  geom_bar(stat = "identity", color = "green", fill = "green")+
  geom_point(color = "black", size = 0.5)+
  geom_segment(aes(x=Date, y=M, xend=Date, yend=sup), 
               arrow = arrow(length=unit(1, 'mm')),
               color='black', lwd=0.5)+
  geom_segment(aes(x=Date, y=M, xend=Date, yend=inf), 
               arrow = arrow(length=unit(1, 'mm')),
               color='black', lwd=0.5)+
  theme_light()+
  ylab("IF moyen") +
  ggtitle("Evolution de l'indice de floraison moyennée par date pour l'année 2012-2013")




####################################
#### Floraison 1977-2021 ###########
####################################

# Ajout des floraisons 2022 au dataframe (se servir de profondeur 9m)
  # facteur de correction (voir plus bas)
round(8.12/9.33, 2)
  # multiplier les IFs par le facteur, en calculer la moyenne et l'ecart-type
Posido77_20 <- rbind(Posido77_20, 
                     Posido22 %>%
                       drop_na(Flowers_pourc) %>%
                       filter(Depth_m==9) %>%
                       summarise(Date = as.POSIXct("2022-12-31"),
                                 Annee = 2022,
                                 Profondeur_m = "10",
                                 Moyenne = mean(Flowers_pourc*0.87), 
                                 St = sd(Flowers_pourc*0.87), 
                                 n = n(), 
                                 Graine = "NA",
                                 Medit_fleur = "NA")
                     )


# Calcul intervalles de confiance pour une moyenne d'écart-type connu :
# 1. n>=100 ? NON ! On utilise donc la loi de student avec n-1 degres de liberte
# 2. Quantile a 95% de confiance pour 9 degres de liberte :
t <- qt(1-0.025, Posido77_20$n-1) # NB : si n > 100 --> t = Z = 2
# 3. Calcul des intervalles :
inf <- Posido77_20$Moyenne-(t*(Posido77_20$St/sqrt(Posido77_20$n)))
sup <- Posido77_20$Moyenne+(t*(Posido77_20$St/sqrt(Posido77_20$n)))


# Mise en evidence annee ou fleurs sur la mediterranee
subset(Posido77_20, Medit_fleur=='T') # annees 1994 et 2003
colors <- rep("green", dim(drop_na(Posido77_20, Moyenne))[1])
colors[which(drop_na(Posido77_20, Moyenne)$Medit_fleur=="T")] <- "red"

# graph
# Attention : utiliser annees et pas les dates sinon on dirait qu'il y a decallage
ggplot(Posido77_20, aes(x = Annee, y = Moyenne))+
  geom_bar(stat = "identity", color = colors, fill = colors)+
  geom_point(color = "black", size = 0.5)+
  geom_segment(aes(x=Annee, y=Moyenne, xend=Annee, yend=sup), 
               arrow = arrow(length=unit(1, 'mm')),
               color='black', lwd=0.5)+
  geom_segment(aes(x=Annee, y=Moyenne, xend=Annee, yend=inf), 
               arrow = arrow(length=unit(1, 'mm')),
               color='black', lwd=0.5)+
  theme_light()+
  xlab("Année") +
  scale_x_continuous(breaks = seq(1975, 2022), labels= seq(1975, 2022)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("IF moyen") +
  ggtitle("Evolution de l'indice de floraison moyenné par date de 1975 à 2022")


# Tendances linéaires variees
  # toutes annees
lm(Moyenne ~ Annee, 
   data = Posido77_20 %>%
     mutate(Annee = Annee -1975))$coefficients[2]
  # enlever toutes les annes vides du debut
lm(Moyenne ~ Annee, 
   data = Posido77_20 %>%
     filter(Annee >= 1994) %>%
     mutate(Annee = Annee -1994))$coefficients[2]
  # + enlever 1994
lm(Moyenne ~ Annee, 
   data = Posido77_20 %>%
     filter(Annee >= 1995) %>%
     mutate(Annee = Annee -1995))$coefficients[2]
  # + enlever 2003
lm(Moyenne ~ Annee, 
   data = Posido77_20 %>%
     filter(Annee >= 1995 & Annee != 2003) %>%
     mutate(Annee = seq(0, 26)))$coefficients[2]


####################################
#### Floraison 2022 ################
####################################

Posido22 %>%
  mutate(Depth_m = as.factor(Depth_m)) %>%
  drop_na(Flowers_pourc) %>%
  group_by(Date, Depth_m, Location) %>%
  summarise(IF_moy = mean(Flowers_pourc), n = n(), sd = sd(Flowers_pourc)) %>%
  mutate(fl_group = as.factor(0/IF_moy)) %>%
  ggplot(aes(x = Date, y = IF_moy, fill = Depth_m))+
  geom_bar(stat = "identity", position = "jitter")+
  geom_point(aes(y = 0, color = fl_group, alpha = fl_group)) +
  facet_grid(~ Depth_m)+
  ylab("Indice de floraison") +
  scale_alpha_manual(values = c(0,1), guide = "none") +
  scale_color_manual(values = c("black", "red"), guide = "none") +
  theme_light() +
  theme(legend.position="none") +
  ggtitle("Evolution de l'indice de floraison en 2022 en fonction de la profondeur")
  

# ggplot(Posido22, aes(x = Date, y = Flowers_pourc, fill = Depth_m))+
#   geom_bar(stat = "identity", position = "jitter")+
#   facet_grid(~ Depth_m)+
#   ggtitle("Evolution de l'indice de floraison en 2022")+
#   ylab("Indice de floraison")


# creation du tableau Posido22 conforme aux autres tableaux
Posido22conf <- Posido22 %>%
  mutate(Depth_m = as.numeric(Depth_m)) %>%
  drop_na(Flowers_pourc) %>%
  group_by(Date, Depth_m, Location) %>%
  summarise(IF_moy = mean(Flowers_pourc), n = n(), sd = sd(Flowers_pourc))

# mise en evidence du declin non lineaire et non constant entre les sites
FC2 <- Posido22conf %>%
  ggplot(aes(x = Depth_m, y = IF_moy, color = Location)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = 0.2) +
  scale_x_continuous(breaks = seq(6, 15, 1), labels= seq(6, 15, 1)) +
  # geom_vline(aes(xintercept = 10), linetype = "dashed") +
  theme_light() +
  xlab("Profondeur [m]") +
  ylab("IF moyen") +
  scale_color_discrete(labels = c("STARESO Nord", "STARESO Sud")) +
  labs(color = "Localisation") +
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=14),
        legend.position = "bottom")
 # NB : on voit que plus on est profond, plus la tendance est lineaire. Differences temp plus faibles ?

# montrer choix des valeurs a considerer pour le facteur de correction
FC1 <- Posido22conf %>%
  ggplot(aes(x = Depth_m, y = IF_moy, color = "black")) +
  geom_point() +
  geom_smooth(method = "loess", alpha = 0.2, color = "blue") +
  scale_x_continuous(breaks = seq(6, 15, 1), labels= seq(6, 15, 1)) +
  geom_vline(aes(xintercept = 10), linetype = "dotted", color = "red") +
  geom_hline(aes(yintercept = 8.12), linetype = "dotted", color = "red") +
  geom_hline(aes(yintercept = 9.33), linetype = "dotted", color = "orange") +
  geom_vline(aes(xintercept = 9), linetype = "dotted", color = "orange") +
  # scale_y_continuous(breaks = c(0, 8.12, 9.33, 10, 20, 30), c(0, 8.12, 9.33, 10, 20, 30)) +
  theme_light() +
  xlab("Profondeur [m]") +
  ylab("IF moyen") +
  scale_color_manual(values = "black", labels = "Indifférencié") +
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=14),
        legend.position = "bottom") +
  labs(color = "")

plot_grid(FC2, FC1, nrow = 1)

# facteur IF 9m vers 10m
round(8.12/9.33, 2)

save(list = ls(pattern = "Posido"), file = "floraison_posido.RData")

# 2. Lien temp flor
# 3. Refaire avec les temp a 9m juste pour les floraisons de la periode ou y a une telle temp

# IDEES POUR LIER FLORAISONS A TEMP :
## VOIR ACCUMULATION DE CHALEUR (A UNE CERTAINE PERIODE ?)
## VOIR EN TERME DE JOURS FROIDS ET JOURS CHAUDS S'IL N'Y A PAS UNE BALANCE
## COMPARER PROFILS ANNEES SANS FLO ET AVEC FLO
## UTILISER DIFFERENTES METRIQUES TEMP, ET VOIR CORR AVEC IF + ACP ?