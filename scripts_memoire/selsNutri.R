library(tidyverse)
library(ggplot2)
library(grid)
library(cowplot)
library(ggbreak)

setwd("~/Documents/Universite/memoire2/data")
load("data.RData")
rm(list = grep(ls(), pattern = "SelsNutri", invert = T, value = T))
load("floraison_posido.RData")
source("../r/exploratory_analysis/functions.R")

SelsNutri <- data.frame(SelsNutri, 
                        Saison = saisons(SelsNutri$Date), 
                        Annee = as.integer(format(SelsNutri$Date, format = "%Y")))

pivot_longer(data = SelsNutri, 
             cols = c(Nitrite, Nitrate, Ammonium, Phosphate, Silicate)) %>%
  ggplot(aes(x = Date, y = value, color = name, linetype = name))+
  geom_line()+
  scale_y_break(c(4.7,10), scales = "fixed")+
  scale_y_break(c(10.5, 12.5), scales = "fixed")+
  ylab("Concentrations [µmol/L]")+
  # facet_wrap(~ Saison) +
  theme_light()+
  labs(color = "Sel nutritif", linetype = "Sel nutritif")+
  # ggtitle("Evolution de la concentration de différents sels nutritifs depuis 2012")+
  theme(plot.title = element_text(size = 17, face = "bold.italic")) -> SN

# on enlève les donnees superieures a 1.8 dans un premier temps
SelsNutriTronc <- SelsNutri %>% filter(Nitrite <= 1.8 & Nitrate <= 1.8 
                                       & Ammonium <= 1.8 & Phosphate <= 1.8 
                                       & Silicate <= 1.8)

pivot_longer(data = SelsNutriTronc, 
             cols = c(Nitrite, Nitrate, Ammonium, Phosphate, Silicate)) %>%
  ggplot(aes(x = Date, y = value, color = name, linetype = name))+
  geom_line()+
  # scale_y_break(c(4.7,10), scales = "fixed")+
  # scale_y_break(c(10.5, 12.5), scales = "fixed")+
  ylab("Concentrations [µmol/L]")+
  # facet_wrap(~ Saison) +
  theme_light()+
  labs(color = "Sel nutritif", linetype = "Sel nutritif")+
  # ggtitle("Evolution de la concentration de différents sels nutritifs depuis 2012 (tronqué)")+
  theme(plot.title = element_text(size = 17, face = "bold.italic")) -> SNt

plot_grid(SN, SNt)

# moyenner valeurs par saison
SelsNutriTronc %>%
  # group_by(Annee, Saison) %>%
  # summarise(NitriteMoy = mean(Nitrite), 
  #           NitrateMoy = mean(Nitrate),
  #           AmmoniumMoy = mean(Ammonium),
  #           PhosphateMoy = mean(Phosphate),
  #           SilicateMoy  = mean(Silicate)) %>%
  pivot_longer(cols = c(Nitrite, Nitrate, Ammonium, Phosphate, Silicate)) %>%
  ggplot(aes(x = Annee, y = value, color = name, group = 1)) +
  geom_point() +
    geom_smooth() +
  facet_wrap(~ name) +
  theme_light() +
  labs(color = "Sels") +
  ylab("Concentrations [µmol/L]") +
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# correlations de pearson sur valeurs medianes annuelles
corSelsAnnee <- function(seuil = 0){ # seuil = nombre d'observations annuelles minimal
  SelsNutriTronc %>%
    group_by(Annee) %>%
    mutate(n = n()) %>%
    # filter(n >= seuil) %>%
    summarise(NitriteMoy = median(Nitrite),
              NitrateMoy = median(Nitrate),
              AmmoniumMoy = median(Ammonium),
              PhosphateMoy = median(Phosphate),
              SilicateMoy  = median(Silicate)) %>%
    merge(Posido77_20 %>% reframe(Annee = Annee, Moyenne = Moyenne)) %>%
    drop_na(Moyenne) %>%
    subset(select = -Annee) %>%
    cor(method = "pearson") -> Rannee
  return(Rannee)
}

Rannee <- corSelsAnnee(seuil = 0)
RanneeN20 <- corSelsAnnee(seuil = 20)

##########"# si je ne fais pas sur les medianes, les resultats sont moches
# SelsNutriTronc %>%
#   group_by(Annee) %>%
#   summarise(NitriteMoy = Nitrite,
#             NitrateMoy = Nitrate,
#             AmmoniumMoy = Ammonium,
#             PhosphateMoy = Phosphate,
#             SilicateMoy  = Silicate) %>%
#   data.frame(IFMoy = Posido77_20[Posido77_20$Annee >= 2012, ]$Moyenne) %>%
#   drop_na(IFMoy) %>%
#   subset(select = -Annee) %>%
#   cor(method = "pearson")


# correlations de pearson sur valeurs moyennes saisonnieres
corSelsSaison <- function(saison, seuil = 0){
  SelsNutriTronc %>%
    group_by(Annee, Saison) %>%
    mutate(n = n()) %>%
    filter(n >= seuil) %>%
    summarise(NitriteMed = median(Nitrite),
              NitrateMed = median(Nitrate),
              AmmoniumMed = median(Ammonium),
              PhosphateMed = median(Phosphate),
              SilicateMed  = median(Silicate)) %>%
    merge(Posido77_20 %>% filter(Annee >= 2012) %>% subset(select = c(Annee, Moyenne))) %>%
    drop_na(Moyenne) %>%
    subset(select = -Annee) %>%
    filter(Saison == saison) %>%
    subset(select = -Saison) %>%
    cor(method = "pearson") -> Rsaison
  return(Rsaison)
}

# correlations avec IF (pas de seuil)
data.frame(Annee = round(Rannee[6, -6], 3), # 9 valeurs
           Ete = round(corSelsSaison("ete", seuil = 0)[6,-6], 3), #  9 valeurs utilisees
           Automne = round(corSelsSaison("automne", seuil = 0)[6,-6], 3), #  8 valeurs
           Hiver = round(corSelsSaison("hiver", seuil = 0)[6,-6], 3),     #  8 valeurs
           Printemps = round(corSelsSaison("printemps", seuil = 0)[6,-6], 3)) # 9 valeurs
# %>% write.csv(file = "../r/exploratory_analysis/Rselsnutri.csv")

# correlations sans IF (seuil)
data.frame(Annee = round(RanneeN20[6, -6], 3), # 7 valeurs
           Ete = round(corSelsSaison("ete", seuil = 10)[6,-6], 3), # 6 valeurs utilisees
           Automne = round(corSelsSaison("automne", seuil = 10)[6,-6], 3), # 4 valeurs
           Hiver = round(corSelsSaison("hiver", seuil = 10)[6,-6], 3),     # 4 valeurs
           Printemps = round(corSelsSaison("printemps", seuil = 10)[6,-6], 3)) # 6 valeurs
#%>% write.csv(file = "../r/exploratory_analysis/RselsnutriNmin.csv")

# cor etonnante hiver silicate : peu de valeurs utilisees !

SelsNutriTronc %>%
  group_by(Annee) %>%
  mutate(n = n()) %>%
  filter(n >= 0) %>% 
  count() %>% t() %>% write.csv(file = "../r/exploratory_analysis/selnutriOBSannee.csv")

SelsNutriTronc %>%
  group_by(Annee, Saison) %>%
  mutate(n = n()) %>%
  filter(n >= 0) %>%
  count()  %>% t() %>% noquote %>% write.csv(file = "../r/exploratory_analysis/selnutriOBSsaison.csv")
