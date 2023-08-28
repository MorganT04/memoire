# Ce script cree des graphiques utilises dans "statEau.R".

# graphs densite des temperatures par saison
# etes 1985-2007
DE8507 <- TempEauNorm0101Saison %>%
  filter(Saison=="ete") %>%
  filter(Date >= as.POSIXct("1985-01-01")) %>%
  filter(Date <  as.POSIXct("2008-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "red", fill = "red", bins = 60) +
  geom_rug(color = "red") +
  geom_density(color = "yellow") +
  geom_vline(aes(xintercept = 23.715), linetype = "dashed", color = "yellow") +
  xlim(17.5, 30) +
  ylim(0, 0.4) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("A.1")
  # ggtitle("Températures estivales de 1985-2007")
# etes 2011-2022
DE1122 <- TempEauNorm0101Saison %>%
  filter(Saison=="ete") %>%
  filter(Date >= as.POSIXct("2011-01-01")) %>%
  filter(Date <  as.POSIXct("2023-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "red", fill = "red", bins = 60) +
  geom_rug(color = "red") +
  geom_density(color = "yellow") +
  geom_vline(aes(xintercept = 24.615), linetype = "dashed", color = "yellow") +
  xlim(17.5, 30) +
  ylim(0, 0.4) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("B.1")
  # ggtitle("Températures estivales de 2011-2022")
# automnes 1985-2007
DA8507 <- TempEauNorm0101Saison %>%
  filter(Saison=="automne") %>%
  filter(Date >= as.POSIXct("1985-01-01")) %>%
  filter(Date <  as.POSIXct("2008-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "orange", fill = "orange", bins = 60) +
  geom_rug(color = "orange") +
  geom_density(color = "#EF5350") +
  geom_vline(aes(xintercept = 17), linetype = "dashed", color = "#EF5350") +
  geom_vline(aes(xintercept = 20.5), linetype = "dashed", color = "#EF5350") +
  xlim(11, 26) +
  ylim(0, 0.25) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("A.2")
  # ggtitle("Températures automnales de 1985-2007")
# automnes 2011-2022
DA1122 <- TempEauNorm0101Saison %>%
  filter(Saison=="automne") %>%
  filter(Date >= as.POSIXct("2011-01-01")) %>%
  filter(Date <  as.POSIXct("2023-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "orange", fill = "orange", bins = 60) +
  geom_rug(color = "orange") +
  geom_density(color = "#EF5350") +
  geom_vline(aes(xintercept = 17), linetype = "dashed", color = "#EF5350") +
  geom_vline(aes(xintercept = 21.5), linetype = "dashed", color = "#EF5350") +
  xlim(11, 26) +
  ylim(0, 0.25) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("B.2")
  # ggtitle("Températures automnales de 2011-2022")
# printemps 1985-2007
DP8507 <- TempEauNorm0101Saison %>%
  filter(Saison=="printemps") %>%
  filter(Date >= as.POSIXct("1985-01-01")) %>%
  filter(Date <  as.POSIXct("2008-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "lightgreen", fill = "lightgreen", bins = 60) +
  geom_rug(color = "lightgreen") +
  geom_density(color = "#EF5350") +
  geom_vline(aes(xintercept = 14.3), linetype = "dashed", color = "#EF5350") +
  geom_vline(aes(xintercept = 18.7), linetype = "dashed", color = "#EF5350") +
  xlim(10, 27) +
  ylim(0, 0.25) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("A.3")
  # ggtitle("Températures printanières de 1985-2007")
# printemps 2011-2022
DP1122 <- TempEauNorm0101Saison %>%
  filter(Saison=="printemps") %>%
  filter(Date >= as.POSIXct("2011-01-01")) %>%
  filter(Date <  as.POSIXct("2023-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "lightgreen", fill = "lightgreen", bins = 60) +
  geom_rug(color = "lightgreen") +
  geom_density(color = "#EF5350") +
  geom_vline(aes(xintercept = 15), linetype = "dashed", color = "#EF5350") +
  geom_vline(aes(xintercept = 20), linetype = "dashed", color = "#EF5350") +
  xlim(10, 27) +
  ylim(0, 0.25) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("B.3")
  # ggtitle("Températures printanières de 2011-2022")
# hivers 1985-2007
DH8507 <- TempEauNorm0101Saison %>%
  filter(Saison=="hiver") %>%
  filter(Date >= as.POSIXct("1985-01-01")) %>%
  filter(Date <  as.POSIXct("2008-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "lightblue", fill = "lightblue", bins = 60) +
  geom_rug(color = "lightblue") +
  geom_density(color = "#EF5350") +
  geom_vline(aes(xintercept = 13.05), linetype = "dashed", color = "#EF5350") +
  xlim(11, 18) +
  ylim(0, 0.85) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("A.4")
  # ggtitle("Températures hivernales de 1985-2007")
# hivers 2011-2022
DH1122 <- TempEauNorm0101Saison %>%
  filter(Saison=="hiver") %>%
  filter(Date >= as.POSIXct("2011-01-01")) %>%
  filter(Date <  as.POSIXct("2023-01-01")) %>%
  ggplot(aes(x = Temp_moy)) +
  geom_histogram(aes(y = after_stat(density)), color = "lightblue", fill = "lightblue", bins = 60) +
  geom_rug(color = "lightblue") +
  geom_density(color = "#EF5350") +
  geom_vline(aes(xintercept = 13.6), linetype = "dashed", color = "#EF5350") +
  xlim(11, 18) +
  ylim(0, 0.85) +
  theme_light() +
  xlab("Température moyenne [°C]") +
  ylab("Densité") +
  ggtitle("B.4")
  # ggtitle("Températures hivernales de 2011-2022")

# nombre de jours par intervalle de temperatures
  # creation de la fonction
pourc_temp_saison <- function(saison, dateInf, dateSup){
  TempEauNorm0101Saison %>%
    filter(Saison == saison) %>%
    filter(Date >= as.POSIXct(dateInf)) %>%
    filter(Date <  as.POSIXct(dateSup)) %>%
    mutate(Intervalle_temp = discretize(Temp_moy, method = "fixed", breaks = seq(10, 30, 2))) %>%
    count(Intervalle_temp) %>%
    mutate(n = n/sum(n)*100) -> tab
  tab$n <- signif(tab$n, 3)
  return(tab)
}
  # application de la fonction
print(pourc_temp_saison("ete",       dateInf = "1985-01-01", dateSup = "2008-01-01"))
print(pourc_temp_saison("ete",       dateInf = "2011-01-01", dateSup = "2023-01-01"))
print(pourc_temp_saison("automne",   dateInf = "1985-01-01", dateSup = "2008-01-01"))
print(pourc_temp_saison("automne",   dateInf = "2011-01-01", dateSup = "2023-01-01"))
print(pourc_temp_saison("printemps", dateInf = "1985-01-01", dateSup = "2008-01-01"))
print(pourc_temp_saison("printemps", dateInf = "2011-01-01", dateSup = "2023-01-01"))
print(pourc_temp_saison("hiver",     dateInf = "1985-01-01", dateSup = "2008-01-01"))
print(pourc_temp_saison("hiver",     dateInf = "2011-01-01", dateSup = "2023-01-01"))

