library("tidyverse")
library("ggplot2")
library("viridis")
library("cowplot")
library("arules")

setwd("~/Documents/Universite/memoire2/r")

### 1. CHARGER DONNEES ET FONCTIONS

source("exploratory_analysis/functions.R")

TempEau <- subset(readRDS("../data/temp_eau.rds"), select = -tssta_codesta)
colnames(TempEau)[3] <- "Pression_abs_kPa"

### 2a. TEMPERATURES MOYENNES (9H00-10H59)

# reperer transition de mesure (2000-04-18 = indice 6769)
TempEau[6767:6775, ]
# voir les heures récurrentes (9-10h)
table(format(TempEau[1:6769, ]$daytime, format = "%H"))
# ne garder que les mesures sur ces heures la (et moyenner)
TempEauNorm <- data.frame(TempEau,
                              Date  = as.POSIXct(format(TempEau$daytime, format = "%Y-%m-%d")),
                              Heure = format(TempEau$daytime, format = "%H")) %>%
  filter(Heure == "09" | Heure == "10") %>%
  group_by(Date) %>%
  summarise(Temp_moy = mean(Temperature), Press_moy = mean(Pression_abs_kPa, na.rm = T))

### 2b. MODELES LINEAIRES ET TENDANCES ANNUELLES (9H00-10H59)

## i. Toutes les annees
# garder les annees du 1 janvier au 31 dec
TempEauNorm0101 <- subset(TempEauNorm, subset = (Date >= as.POSIXct("1982-01-01") & Date < as.POSIXct("2023-01-01")))
# modele lineaire (elevation temperature quotidienne)
jour <- lm(TempEauNorm0101$Temp_moy ~ as.integer((as.numeric(TempEauNorm0101$Date)-as.numeric(TempEauNorm0101$Date[1])) / (24*60*60)))
equation <- paste0(signif(jour$coefficients[1], 4), '°C + ', signif(jour$coefficients[2], 4), ' x #jours')
# elevation temp annuelle moyenne
tendanceAnnee <- function(Dat, mod, Date = 1){
  amplitude <- mod$fitted.values[length(mod$fitted.values)] - mod$fitted.values[1]
  nAnnees   <- (as.numeric(Dat[nrow(Dat), Date] - as.numeric(Dat[1, Date]))) / (365*24*60*60)
  return(unname(amplitude / nAnnees))
}

tend <- tendanceAnnee(Dat=TempEauNorm0101, mod=jour)
tendchr <- paste0('Tendance annuelle : +', signif(tend, 4), '°C/an')

## ii. Quelles annees elevation max (forward) ?
for(i in 1:length(seq(1982, 2020, 1))){
  # initialisation
  if(i==1){
    subTempEauNorm <- TempEauNorm0101
    subtend <- NULL
  }
  # modeles lineaires et tendances correspondantes ("enlever annees par la gauche")
  subTempEauNorm <- subset(TempEauNorm0101, subset = Date >= as.POSIXct(paste0(seq(1982, 2020, 1)[i], '-01-01')))
  subjour <- lm(subTempEauNorm$Temp_moy ~ as.integer((as.numeric(subTempEauNorm$Date)-as.numeric(subTempEauNorm$Date[1])) / (24*60*60)))
  subtend <- append(subtend, tendanceAnnee(subTempEauNorm, mod = subjour))
  # vecteur des tendances (nommage et envoi a l'ecran)
  if(i==length(seq(1982, 2020, 1))){
    names(subtend) <- paste0(seq(1982, 2020, 1))
    print(subtend)
  }
}

# Tendances annuelles forward
data.frame(Annee = names(subtend), Tendance = subtend) %>%
  ggplot(aes(x = Annee, y = Tendance, group = 1)) +
  geom_point() +
  geom_path() +
  ylab("Tendance [°C/an]") +
  theme_light()

## iii. Quelles annees elevation max (backward) ?

for(i in 1:length(seq(2023, 1985, -1))){
  # initialisation
  if(i==1){
    subTempEauNorm <- TempEauNorm
    subtendB <- NULL
  }
  # modeles lineaires et tendances correspondantes ("enlever annees par la droite")
  subTempEauNorm <- subset(TempEauNorm0101, subset = Date < as.POSIXct(paste0(seq(2023, 1985, -1)[i], '-01-01')))
  subjour <- lm(subTempEauNorm$Temp_moy ~ as.integer((as.numeric(subTempEauNorm$Date)-as.numeric(subTempEauNorm$Date[1])) / (24*60*60)))
  subtendB <- append(subtendB, tendanceAnnee(subTempEauNorm, mod = subjour))
  # vecteur des tendances (nommage et envoi a l'ecran)
  if(i==length(seq(2023, 1985, -1))){
    names(subtendB) <- paste0(seq(2023, 1985, -1))
    print(subtendB)
  }
}

rm(subTempEauNorm)

# Tendances annuelles backward
data.frame(Annee = names(subtendB), Tendance = subtendB) %>%
  ggplot(aes(x = Annee, y = Tendance, group = 1)) +
  geom_point() +
  geom_line() +
  theme_light() ## TENDANCE FORTE ANNEES 90' !! VOIR MOYENNE PAR ANNEE

## iv. Evolution temperatures moyennes annuelles

groupAnnee <- rep("o", 41)
groupAnnee[c(1:3, 27:29)] <- "n"
data.frame(TempEauNorm0101, Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
  group_by(Annee) %>%
  summarize(Temp_moy_annee = mean(Temp_moy), Press_moy_annee = mean(Press_moy, na.rm = T)) %>%
  ggplot(aes(x = Annee, y = Temp_moy_annee, group = 1)) +
  geom_point(aes(color = groupAnnee, shape = groupAnnee), size = 2) +
  geom_line() +
  # geom_smooth(method = 'lm', alpha = 0.2) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Année") +
  ylab("Température moyenne annuelle [°C]") +
  scale_color_manual(values=c("red", "black")) +
  scale_shape_manual(values=c(4, 19)) +
  theme(legend.position="none")

### 2c. GRAPH EVOLUTION TEMPERATURE GLOBALE 9h-11h (9H00-10H59)

data.frame(TempEauNorm0101, Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
  group_by(Annee) %>%
  mutate(Temp_moy_annee = mean(Temp_moy), Press_moy_annee = mean(Press_moy, na.rm = T)) %>%
  ggplot(aes(x = Date, group = 1)) +
  geom_line(aes(y = Temp_moy)) +
  geom_line(aes(y = Temp_moy_annee, color = "red")) +
  geom_smooth(aes(y = Temp_moy), method = "lm", level = 0.95, linewidth = 0.75, alpha = 0.2) +
  theme_classic() +
  geom_text(aes(x = as.POSIXct("1990-01-01"), y = 30, label = equation)) +
  geom_text(aes(x = as.POSIXct("1990-01-01"), y = 29, label = tendchr)) +
  ylab("Température (moyenne 9h-11h) [°C]") +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_color_discrete(labels = 'Moyenne annuelle') +
  ggtitle("Evolution de la température de l'eau depuis 1982")

# Graph de densites 1982-2022
D8222 <- TempEauNorm0101 %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "lightblue", fill = "lightblue", bins = 60) +
  geom_rug(color = "lightblue") +
  geom_density(color = "red") +
  xlim(10, 30) +
  ylim(0, 0.225) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("Graph de densité des températures de 1982-2022")

# Densite 1985-2007
D8507 <- TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("1985-01-01")) %>%
  filter(Date <  as.POSIXct("2008-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "#DE75BF", fill = "#DE75BF", bins = 60) +
  geom_rug(color = "#DE75BF") +
  geom_density(color = "red") +
  geom_vline(aes(xintercept = 13.6), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = 23.4), linetype = "dashed", color = "red") +
  xlim(10, 30) +
  ylim(0, 0.225) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("Graph de densité des températures de 1985-2007")

# Densite 2011-2022
D1122 <- TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("2011-01-01")) %>%
  filter(Date <  as.POSIXct("2023-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "#DE75BF", fill = "#DE75BF", bins = 60) +
  geom_rug(color = "#DE75BF") +
  geom_density(color = "red") +
  geom_vline(aes(xintercept = 14), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = 24.5), linetype = "dashed", color = "red") +
  xlim(10, 30) +
  ylim(0, 0.225) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("Graph de densité des températures de 2011-2022")

plot_grid(D8222, D8507, D1122, nrow = 1)
plot_grid(D8507, D1122, ncol = 1)

# Intervalles de températures 1985-2007
TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("1985-01-01")) %>%
  filter(Date <  as.POSIXct("2008-01-01")) %>%
  mutate(Intervalle_temp = discretize(Temp_moy, method = "fixed", breaks = seq(10, 30, 2))) %>%
  count(Intervalle_temp) %>%
  mutate(n = n/sum(n)*100)
  
# Intervalles de températures 2011-2022
TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("2011-01-01")) %>%
  filter(Date <  as.POSIXct("2023-01-01")) %>%
  mutate(Intervalle_temp = discretize(Temp_moy, method = "fixed", breaks = seq(10, 30, 2))) %>%
  count(Intervalle_temp) %>%
  mutate(n = n/sum(n)*100)

####### VOIR SI C'EST DU A ETES PLUS CHAUDS, HIVER MOINS FROIDS ? VOIR COMMENT SE REPARTISSENT LES TEMP PAR SAISON. VOIR NAO ET TEMP (relation)

### 3. DETAIL TEMP PAR SAISON
# creation data frame
TempEauNorm0101Saison <- data.frame(TempEauNorm0101, 
                                    Saison = saisons(TempEauNorm0101$Date),
                                    Annee = format(TempEauNorm0101$Date, format = "%Y"))

# modeles lineaires et elevations annuelles moyennes par saison
for(saison in c("ete", "automne", "printemps", "hiver")){
  Data <- subset(TempEauNorm0101Saison, subset = Saison==saison)
  mod <- lm(Data$Temp_moy ~ as.integer((as.numeric(Data$Date)-as.numeric(Data$Date[1])) / (24*60*60)))
  tend <- tendanceAnnee(Dat = TempEauNorm0101Saison, mod = mod)
  assign(paste("tend", saison, sep = '_'),
         value = paste0('Tendance annuelle : +', signif(tend, 4), '°C/an'))
}

# evolution temp moy par saison
TempEauNorm0101Saison %>% 
  group_by(Annee, Saison) %>%
  mutate(Temp_moy_saison = mean(Temp_moy)) %>%
  ggplot(aes(x = Annee, y = Temp_moy_saison, color = Saison, group = Saison)) +
  geom_line() +
  geom_smooth(aes(y = Temp_moy), method = "lm", alpha = 0.2) +
  xlab("Année") +
  ylab("Température moyenne [°C]") +
  scale_color_manual(breaks = c("ete", "automne", "printemps", "hiver"),
                     labels = c("été", "automne", "printemps", "hiver"),
                     values = c("red", "orange", "lightgreen", "lightblue")) +
  theme_light() +
  geom_text(aes(x = "1988", y = 25, label = tend_ete), color = "red", size = 5) +
  geom_text(aes(x = "1988", y = 20, label = tend_automne), color = "orange", size = 5) +
  geom_text(aes(x = "1988", y = 17.5, label = tend_printemps), color = "lightgreen", size = 5) +
  geom_text(aes(x = "1988", y = 12, label = tend_hiver), color = "lightblue", size = 5) +
  ggtitle("Evolution de la température en fonction de la saison")

# Densites par saison et nombre de jours par intervalle de temperatures
source("exploratory_analysis/densiteParSaison.R")
plot_grid(DE8507, DA8507, DP8507, DH8507, DE1122, DA1122, DP1122, DH1122, nrow = 2, ncol = 4)

### 4. Profil des températures pour des intervalles de temps

# 1985-2007 + 2011-2019 + 2020-2022
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

# 1985-2007 + 2011-2022
groupAnnee <- rep("2008-2010", nrow(TempEauNorm0101))
groupAnnee[format(TempEauNorm0101$Date, format = "%Y") < 2008] <- "1985-2007"
groupAnnee[format(TempEauNorm0101$Date, format = "%Y") > 2010] <- "2011-2022"
groupAnnee <- as.factor(groupAnnee)

P2 <- TempEauNorm0101 %>%
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

plot_grid(P2, P3, nrow = 1)

save(list = ls(pattern = "TempEau"), file = "../data/statEau_TempEau.RData")



# ### 5a. PROFILS HORAIRES
#   
# TempEauMoyHeure <- data.frame(TempEau,
#                               Date  = format(TempEau$daytime, format = "%Y-%m-%d"),
#                               Heure = format(TempEau$daytime, format = "%H")) %>%
#   group_by(Date, Heure) %>%
#   summarise(Temp_moy = mean(Temperature), Press_moy = mean(Pression_abs_kPa, na.rm = T))
#     