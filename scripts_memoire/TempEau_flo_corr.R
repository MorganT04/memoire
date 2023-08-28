library("tidyverse")

setwd("~/Documents/Universite/memoire2/r")

# chargement des dataframes sur la temp eau et la posido + fonctions
load("../data/floraison_posido.RData")
load("../data/statEau_TempEau.RData")
source("exploratory_analysis/functions.R")


##################################################
# 1. CORRELATIONS TEMP ANNUELLES - IF
##################################################

# IF - ANNEE CORRESPONDANTE
anneeCor <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1982],
                       TempMoyAn = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                         subset(select = Temp_moy_annee) %>%
                         as.vector()%>%
                         unlist(), 
                       TempMedAn = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_med_annee = median(Temp_moy)) %>%
                         subset(select = Temp_med_annee) %>%
                         as.vector() %>%
                         unlist(),
                       TempMaxAn = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_max_annee = max(Temp_moy)) %>%
                         subset(select = Temp_max_annee) %>%
                         as.vector() %>%
                         unlist(),
                       TempMinAn = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_min_annee = min(Temp_moy)) %>%
                         subset(select = Temp_min_annee) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax5 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_5) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax10 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_10) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax15 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_15) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax20 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_20) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax30 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_30) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax50 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_50) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax5 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_5) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax10 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_10) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax15 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_15) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax20 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_20) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax30 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_30) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax50 = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_50) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours23deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours24deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours25deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours26deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours27deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours27.5deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours28deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours28.5deg = TempEauNorm0101 %>%
                         mutate(Annee = format(TempEauNorm0101$Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       row.names = seq(1982, 2022))

cor(anneeCor, use = "complete.obs", method = "spearman")[, 1]

# IF - ANNEE PRECEDANTE
# decaller la colonne des floraisons de 1 pour simuler les cor entre IF et mesures temp annee precedente
anneePlusCor <- anneeCor
anneePlusCor$IFmoyAn <- c(anneePlusCor$IFmoyAn[-1], 100)
anneePlusCor <- anneePlusCor[-dim(anneePlusCor)[1], ]
rownames(anneePlusCor) <- seq(1983, 2022)

data.frame(Annee0 = cor(anneeCor, use = "complete.obs", method = "spearman")[, 1],
           Annee1 = cor(anneePlusCor, use = "complete.obs", method = "spearman")[, 1])
  # NB : NA pour temp sup a 28.5 car temp sup seulement pour 2022

# IF - ANNEE CORRESPONDANTE+PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- TempEauNorm0101 %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1983-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
annee2Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                        TempMoyAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_moy_annee) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_med_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMaxAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_max_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMinAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_min_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours23deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours24deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours25deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours26deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        row.names = seq(1983, 2022))

data.frame(Annee_cour      = cor(anneeCor, use = "complete.obs", method = "spearman")[, 1],
           Annee_prec      = cor(anneePlusCor, use = "complete.obs", method = "spearman")[, 1],
           Annee_cour_prec = cor(annee2Cor, use = "complete.obs", method = "spearman")[, 1])


# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante et precedante
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1984-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
annee3Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                        TempMoyAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_moy_annee) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_med_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMaxAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_max_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMinAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_min_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours23deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours24deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours25deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours26deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        row.names = seq(1984, 2022))

data.frame(Annee_cour       = cor(anneeCor, use = "complete.obs", method = "spearman")[, 1],
           Annee_prec       = cor(anneePlusCor, use = "complete.obs", method = "spearman")[, 1],
           Annee_cour_prec  = cor(annee2Cor, use = "complete.obs", method = "spearman")[, 1],
           Annee_cour_prec2 = cor(annee3Cor, use = "complete.obs", method = "spearman")[, 1])


# ANNEE COURANTE 1985-2007
anneeCor8507 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1985 & Posido77_20$Annee <= 2007],
                       TempMoyAn = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                         subset(select = Temp_moy_annee) %>%
                         as.vector()%>%
                         unlist(), 
                       TempMedAn = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_med_annee = median(Temp_moy)) %>%
                         subset(select = Temp_med_annee) %>%
                         as.vector() %>%
                         unlist(),
                       TempMaxAn = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_max_annee = max(Temp_moy)) %>%
                         subset(select = Temp_max_annee) %>%
                         as.vector() %>%
                         unlist(),
                       TempMinAn = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_min_annee = min(Temp_moy)) %>%
                         subset(select = Temp_min_annee) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax5 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_5) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax10 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_10) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax15 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_15) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax20 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_20) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax30 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_30) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursChaudsMax50 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                         subset(select = Temp_chaud_annee_50) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax5 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_5) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax10 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_10) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax15 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_15) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax20 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_20) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax30 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_30) %>%
                         as.vector() %>%
                         unlist(),
                       TempJoursFroidsMax50 = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                         subset(select = Temp_froid_annee_50) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours23deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours24deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours25deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours26deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours27deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours27.5deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours28deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       TempNJours28.5deg = TempEauNorm0101 %>%
                         filter(Date >= as.POSIXct("1985-01-01")) %>%
                         filter(Date < as.POSIXct("2008-01-01")) %>%
                         mutate(Annee = format(Date, format = "%Y")) %>%
                         group_by(Annee) %>%
                         summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                         subset(select = Temp_chaud_annee_up) %>%
                         as.vector() %>%
                         unlist(),
                       row.names = seq(1985, 2007))

# ANNEE COURANTE 2011-2022
anneeCor1122 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 2011 & Posido77_20$Annee <= 2022],
                           TempMoyAn = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                             subset(select = Temp_moy_annee) %>%
                             as.vector()%>%
                             unlist(), 
                           TempMedAn = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_med_annee = median(Temp_moy)) %>%
                             subset(select = Temp_med_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempMaxAn = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_max_annee = max(Temp_moy)) %>%
                             subset(select = Temp_max_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempMinAn = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_min_annee = min(Temp_moy)) %>%
                             subset(select = Temp_min_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax5 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_5) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax10 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_10) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax15 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_15) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax20 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_20) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax30 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_30) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax50 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_50) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax5 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_5) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax10 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_10) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax15 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_15) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax20 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_20) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax30 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_30) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax50 = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_50) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours23deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours24deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours25deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours26deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours27deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours27.5deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours28deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours28.5deg = TempEauNorm0101 %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("1984-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
annee2Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                        TempMoyAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_moy_annee) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_med_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMaxAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_max_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMinAn = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_min_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours23deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours24deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours25deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours26deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("2010-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
annee2Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                            TempMoyAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMaxAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMinAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours23deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours24deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours25deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours26deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et 2*precedante
Data_precAn <- TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("1983-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
annee3Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                            TempMoyAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMaxAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMinAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours23deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours24deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours25deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours26deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- TempEauNorm0101 %>%
  filter(Date >= as.POSIXct("2009-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
annee3Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                            TempMoyAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMaxAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMinAn = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours23deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours24deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours25deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours26deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            row.names = seq(2011, 2022))

# correlations :
Rannees <- data.frame(Annee_cour               = cor(anneeCor, use = "complete.obs", method = "spearman")[, 1],
                      Annee_prec               = cor(anneePlusCor, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_prec          = cor(annee2Cor, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_prec2x        = cor(annee3Cor, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_8507          = cor(anneeCor8507, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_1122          = cor(anneeCor1122, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_prec_8507     = cor(annee2Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_prec_1122     = cor(annee2Cor1122, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_prec2x_8507   = cor(annee3Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Annee_cour_prec2x_1122   = cor(annee3Cor1122, use = "complete.obs", method = "spearman")[, 1])

# graphe des correlations calculees
par(mar = c(1,3,1,0) + 4)
plot(Rannees$Annee_cour[-1], rep(2, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("black", 0.5),
     ylim = c(1,2.1), xlim = c(-0.7, 0.7),
     xlab = "Coefficient de Pearson", ylab = "", 
     yaxt="n", main = "Toutes les saisons")
points(Rannees$Annee_prec[-1], rep(1.9, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("red", 0.5))
points(Rannees$Annee_cour_prec[-1], rep(1.8, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("green", 0.5))
points(Rannees$Annee_cour_prec2x[-1], rep(1.7, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("blue", 0.5))
points(Rannees$Annee_cour_8507[-1], rep(1.6, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("salmon", 0.5))
points(Rannees$Annee_cour_1122[-1], rep(1.5, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("purple", 0.5))
points(Rannees$Annee_cour_prec_8507[-1], rep(1.4, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("orange", 0.5))
points(Rannees$Annee_cour_prec_1122[-1], rep(1.3, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("yellow", 0.5))
points(Rannees$Annee_cour_prec2x_8507[-1], rep(1.2, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("cyan", 0.5))
points(Rannees$Annee_cour_prec2x_1122[-1], rep(1.1, length(Rannees$Annee_cour[-1])), pch = 15, col = alpha("lightgreen", 0.5))
yaxes.noms <- c("c","p","c+p","c+2*p","c(1985-2007)","c(2011-2022)","c+p(1985-2007)",
                "c+p(2011-2022)","c+2*p(1985-2007)","c+2*p(2011-2022)")
axis(2, at=seq(2, 1.1, -0.1), labels=yaxes.noms, cex.axis=0.75, las = 2)


####################################
# CORRELATIONS TEMP ETE - IF
####################################
# matrice saisons
Data_saison <- TempEauNorm0101Saison %>%
  filter(Saison=="ete")
saisonCor <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1982],
                        TempMoySai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_moy_saison = mean(Temp_moy)) %>%
                          subset(select = Temp_moy_saison) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_med_saison = median(Temp_moy)) %>%
                          subset(select = Temp_med_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMaxSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_max_saison = max(Temp_moy)) %>%
                          subset(select = Temp_max_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMinSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_min_saison = min(Temp_moy)) %>%
                          subset(select = Temp_min_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours23deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 23)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours24deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 24)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours25deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 25)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours26deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 26)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        row.names = seq(1982, 2022))

cor(saisonCor, use = "complete.obs", method = "spearman")[,1]
# FAIRE TOUTES LES CORR POUR ETE PUIS FAIRE AVEC LES AUTRES SAISONS

# IF - ANNEE PRECEDANTE
# decaller la colonne des floraisons de 1 pour simuler les cor entre IF et mesures temp annee precedente
saisonPlusCor <- saisonCor
saisonPlusCor$IFmoyAn <- c(saisonPlusCor$IFmoyAn[-1], 100)
saisonPlusCor <- saisonPlusCor[-dim(saisonPlusCor)[1], ]
rownames(saisonPlusCor) <- seq(1983, 2022)

data.frame(Annee0 = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
           Annee1 = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1])

# IF - ANNEE CORRESPONDANTE+PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1983-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                        TempMoySai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_moy_annee) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedSai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_med_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMaxSai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_max_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMinSai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_min_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours23deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours24deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours25deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours26deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        row.names = seq(1983, 2022))

# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante et precedante
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1984-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                        TempMoySai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_moy_annee) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedSai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_med_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMaxSai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_max_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempMinSai = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                          subset(select = Temp_min_annee) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_5) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_10) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_15) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_20) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_30) %>%
                          as.vector()%>%
                          unlist(),
                        TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_froids_annee_50) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours23deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours24deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours25deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours26deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                          group_by(annee) %>%
                          summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_annee_up) %>%
                          as.vector()%>%
                          unlist(),
                        row.names = seq(1984, 2022))

# ANNEE COURANTE 1985-2007
saisonCor8507 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1985 & Posido77_20$Annee <= 2007],
                           TempMoySai = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                             subset(select = Temp_moy_annee) %>%
                             as.vector()%>%
                             unlist(), 
                           TempMedSai = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_med_annee = median(Temp_moy)) %>%
                             subset(select = Temp_med_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempMaxSai = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_max_annee = max(Temp_moy)) %>%
                             subset(select = Temp_max_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempMinSai = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_min_annee = min(Temp_moy)) %>%
                             subset(select = Temp_min_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax5 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_5) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax10 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_10) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax15 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_15) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax20 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_20) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax30 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_30) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax50 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_50) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax5 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_5) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax10 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_10) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax15 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_15) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax20 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_20) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax30 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_30) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax50 = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_50) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours23deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours24deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours25deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours26deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours27deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours27.5deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours28deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours28.5deg = Data_saison %>%
                             filter(Date >= as.POSIXct("1985-01-01")) %>%
                             filter(Date < as.POSIXct("2008-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           row.names = seq(1985, 2007))

# ANNEE COURANTE 2011-2022
saisonCor1122 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 2011 & Posido77_20$Annee <= 2022],
                           TempMoySai = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                             subset(select = Temp_moy_annee) %>%
                             as.vector()%>%
                             unlist(), 
                           TempMedSai = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_med_annee = median(Temp_moy)) %>%
                             subset(select = Temp_med_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempMaxSai = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_max_annee = max(Temp_moy)) %>%
                             subset(select = Temp_max_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempMinSai = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_min_annee = min(Temp_moy)) %>%
                             subset(select = Temp_min_annee) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax5 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_5) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax10 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_10) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax15 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_15) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax20 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_20) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax30 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_30) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursChaudsMax50 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                             subset(select = Temp_chaud_annee_50) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax5 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_5) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax10 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_10) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax15 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_15) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax20 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_20) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax30 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_30) %>%
                             as.vector() %>%
                             unlist(),
                           TempJoursFroidsMax50 = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                             subset(select = Temp_froid_annee_50) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours23deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours24deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours25deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours26deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours27deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours27.5deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours28deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           TempNJours28.5deg = Data_saison %>%
                             filter(Date >= as.POSIXct("2011-01-01")) %>%
                             filter(Date < as.POSIXct("2023-01-01")) %>%
                             mutate(Annee = format(Date, format = "%Y")) %>%
                             group_by(Annee) %>%
                             summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                             subset(select = Temp_chaud_annee_up) %>%
                             as.vector() %>%
                             unlist(),
                           row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1984-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                            TempMoySai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMaxSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMinSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours23deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours24deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours25deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours26deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2010-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                            TempMoySai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMaxSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMinSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours23deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours24deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours25deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours26deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et 2*precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1983-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                            TempMoySai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMaxSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMinSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours23deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours24deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours25deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours26deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2009-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                            TempMoySai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMaxSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempMinSai = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_5) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_10) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_15) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_20) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_30) %>%
                              as.vector()%>%
                              unlist(),
                            TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_froids_annee_50) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours23deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours24deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours25deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours26deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                              group_by(annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector()%>%
                              unlist(),
                            row.names = seq(2011, 2022))

# correlations :
Rsaison <- data.frame(Saison_cour               = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_prec               = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec          = cor(saison2Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x        = cor(saison3Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_8507          = cor(saisonCor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_1122          = cor(saisonCor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_8507     = cor(saison2Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_1122     = cor(saison2Cor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_8507   = cor(saison3Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_1122   = cor(saison3Cor1122, use = "complete.obs", method = "spearman")[, 1])

# graphe des correlations calculees
par(mar = c(1,3,1,0) + 4)
plot(Rsaison$Saison_cour[-1], rep(2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("black", 0.5),
     ylim = c(1,2.1), xlim = c(-0.7, 0.7),
     xlab = "Coefficient de Pearson", ylab = "", 
     yaxt="n", main = "Été")
points(Rsaison$Saison_prec[-1], rep(1.9, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("red", 0.5))
points(Rsaison$Saison_cour_prec[-1], rep(1.8, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("green", 0.5))
points(Rsaison$Saison_cour_prec2x[-1], rep(1.7, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("blue", 0.5))
points(Rsaison$Saison_cour_8507[-1], rep(1.6, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("salmon", 0.5))
points(Rsaison$Saison_cour_1122[-1], rep(1.5, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("purple", 0.5))
points(Rsaison$Saison_cour_prec_8507[-1], rep(1.4, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("orange", 0.5))
points(Rsaison$Saison_cour_prec_1122[-1], rep(1.3, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("yellow", 0.5))
points(Rsaison$Saison_cour_prec2x_8507[-1], rep(1.2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("cyan", 0.5))
points(Rsaison$Saison_cour_prec2x_1122[-1], rep(1.1, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("lightgreen", 0.5))
yaxes.noms <- c("c","p","c+p","c+2*p","c(1985-2007)","c(2011-2022)","c+p(1985-2007)",
                "c+p(2011-2022)","c+2*p(1985-2007)","c+2*p(2011-2022)")
axis(2, at=seq(2, 1.1, -0.1), labels=yaxes.noms, cex.axis=0.75, las = 2)

####################################
# CORRELATIONS TEMP AUTOMNE - IF
####################################
# matrice saisons
Data_saison <- TempEauNorm0101Saison %>%
  filter(Saison=="automne")
saisonCor <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1982],
                        TempMoySai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_moy_saison = mean(Temp_moy)) %>%
                          subset(select = Temp_moy_saison) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_med_saison = median(Temp_moy)) %>%
                          subset(select = Temp_med_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMaxSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_max_saison = max(Temp_moy)) %>%
                          subset(select = Temp_max_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMinSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_min_saison = min(Temp_moy)) %>%
                          subset(select = Temp_min_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours23deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 23)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours24deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 24)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours25deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 25)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours26deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 26)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        row.names = seq(1982, 2022))

cor(saisonCor, use = "complete.obs", method = "spearman")[,1]
# FAIRE TOUTES LES CORR POUR ETE PUIS FAIRE AVEC LES AUTRES SAISONS

# IF - ANNEE PRECEDANTE
# decaller la colonne des floraisons de 1 pour simuler les cor entre IF et mesures temp annee precedente
saisonPlusCor <- saisonCor
saisonPlusCor$IFmoyAn <- c(saisonPlusCor$IFmoyAn[-1], 100)
saisonPlusCor <- saisonPlusCor[-dim(saisonPlusCor)[1], ]
rownames(saisonPlusCor) <- seq(1983, 2022)

data.frame(Annee0 = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
           Annee1 = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1])

# IF - ANNEE CORRESPONDANTE+PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1983-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                         TempMoySai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_moy_annee) %>%
                           as.vector()%>%
                           unlist(), 
                         TempMedSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_med_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMaxSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_max_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMinSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_min_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours23deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours24deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours25deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours26deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         row.names = seq(1983, 2022))

# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante et precedante
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1984-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                         TempMoySai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_moy_annee) %>%
                           as.vector()%>%
                           unlist(), 
                         TempMedSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_med_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMaxSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_max_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMinSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_min_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours23deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours24deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours25deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours26deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         row.names = seq(1984, 2022))

# ANNEE COURANTE 1985-2007
saisonCor8507 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1985 & Posido77_20$Annee <= 2007],
                            TempMoySai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_med_annee = median(Temp_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMaxSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_max_annee = max(Temp_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMinSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_min_annee = min(Temp_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours23deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours24deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours25deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours26deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            row.names = seq(1985, 2007))

# ANNEE COURANTE 2011-2022
saisonCor1122 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 2011 & Posido77_20$Annee <= 2022],
                            TempMoySai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_med_annee = median(Temp_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMaxSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_max_annee = max(Temp_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMinSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_min_annee = min(Temp_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours23deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours24deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours25deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours26deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1984-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2010-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et 2*precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1983-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2009-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(2011, 2022))

# correlations :
Rsaison <- data.frame(Saison_cour               = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_prec               = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec          = cor(saison2Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x        = cor(saison3Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_8507          = cor(saisonCor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_1122          = cor(saisonCor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_8507     = cor(saison2Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_1122     = cor(saison2Cor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_8507   = cor(saison3Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_1122   = cor(saison3Cor1122, use = "complete.obs", method = "spearman")[, 1])

# graphe des correlations calculees
par(mar = c(1,3,1,0) + 4)
plot(Rsaison$Saison_cour[-1], rep(2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("black", 0.5),
     ylim = c(1,2.1), xlim = c(-0.7, 0.7),
     xlab = "Coefficient de Pearson", ylab = "", 
     yaxt="n", main = "Automne")
points(Rsaison$Saison_prec[-1], rep(1.9, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("red", 0.5))
points(Rsaison$Saison_cour_prec[-1], rep(1.8, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("green", 0.5))
points(Rsaison$Saison_cour_prec2x[-1], rep(1.7, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("blue", 0.5))
points(Rsaison$Saison_cour_8507[-1], rep(1.6, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("salmon", 0.5))
points(Rsaison$Saison_cour_1122[-1], rep(1.5, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("purple", 0.5))
points(Rsaison$Saison_cour_prec_8507[-1], rep(1.4, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("orange", 0.5))
points(Rsaison$Saison_cour_prec_1122[-1], rep(1.3, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("yellow", 0.5))
points(Rsaison$Saison_cour_prec2x_8507[-1], rep(1.2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("cyan", 0.5))
points(Rsaison$Saison_cour_prec2x_1122[-1], rep(1.1, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("lightgreen", 0.5))
yaxes.noms <- c("c","p","c+p","c+2*p","c(1985-2007)","c(2011-2022)","c+p(1985-2007)",
                "c+p(2011-2022)","c+2*p(1985-2007)","c+2*p(2011-2022)")
axis(2, at=seq(2, 1.1, -0.1), labels=yaxes.noms, cex.axis=0.75, las = 2)


####################################
# CORRELATIONS TEMP HIVER - IF
####################################
# matrice saisons
Data_saison <- TempEauNorm0101Saison %>%
  filter(Saison=="hiver")
saisonCor <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1982],
                        TempMoySai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_moy_saison = mean(Temp_moy)) %>%
                          subset(select = Temp_moy_saison) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_med_saison = median(Temp_moy)) %>%
                          subset(select = Temp_med_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMaxSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_max_saison = max(Temp_moy)) %>%
                          subset(select = Temp_max_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMinSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_min_saison = min(Temp_moy)) %>%
                          subset(select = Temp_min_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours23deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 23)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours24deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 24)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours25deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 25)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours26deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 26)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        row.names = seq(1982, 2022))

cor(saisonCor, use = "complete.obs", method = "spearman")[,1]
# FAIRE TOUTES LES CORR POUR ETE PUIS FAIRE AVEC LES AUTRES SAISONS

# IF - ANNEE PRECEDANTE
# decaller la colonne des floraisons de 1 pour simuler les cor entre IF et mesures temp annee precedente
saisonPlusCor <- saisonCor
saisonPlusCor$IFmoyAn <- c(saisonPlusCor$IFmoyAn[-1], 100)
saisonPlusCor <- saisonPlusCor[-dim(saisonPlusCor)[1], ]
rownames(saisonPlusCor) <- seq(1983, 2022)

data.frame(Annee0 = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
           Annee1 = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1])

# IF - ANNEE CORRESPONDANTE+PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1983-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                         TempMoySai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_moy_annee) %>%
                           as.vector()%>%
                           unlist(), 
                         TempMedSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_med_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMaxSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_max_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMinSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_min_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours23deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours24deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours25deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours26deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         row.names = seq(1983, 2022))

# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante et precedante
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1984-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                         TempMoySai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_moy_annee) %>%
                           as.vector()%>%
                           unlist(), 
                         TempMedSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_med_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMaxSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_max_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMinSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_min_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours23deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours24deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours25deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours26deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         row.names = seq(1984, 2022))

# ANNEE COURANTE 1985-2007
saisonCor8507 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1985 & Posido77_20$Annee <= 2007],
                            TempMoySai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_med_annee = median(Temp_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMaxSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_max_annee = max(Temp_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMinSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_min_annee = min(Temp_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours23deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours24deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours25deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours26deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            row.names = seq(1985, 2007))

# ANNEE COURANTE 2011-2022
saisonCor1122 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 2011 & Posido77_20$Annee <= 2022],
                            TempMoySai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_med_annee = median(Temp_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMaxSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_max_annee = max(Temp_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMinSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_min_annee = min(Temp_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours23deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours24deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours25deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours26deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1984-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2010-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et 2*precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1983-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2009-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(2011, 2022))

# correlations :
Rsaison <- data.frame(Saison_cour               = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_prec               = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec          = cor(saison2Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x        = cor(saison3Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_8507          = cor(saisonCor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_1122          = cor(saisonCor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_8507     = cor(saison2Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_1122     = cor(saison2Cor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_8507   = cor(saison3Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_1122   = cor(saison3Cor1122, use = "complete.obs", method = "spearman")[, 1])

# graphe des correlations calculees
par(mar = c(1,3,1,0) + 4)
plot(Rsaison$Saison_cour[-1], rep(2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("black", 0.5),
     ylim = c(1,2.1), xlim = c(-0.7, 0.7),
     xlab = "Coefficient de Pearson", ylab = "", 
     yaxt="n", main = "Hiver")
points(Rsaison$Saison_prec[-1], rep(1.9, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("red", 0.5))
points(Rsaison$Saison_cour_prec[-1], rep(1.8, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("green", 0.5))
points(Rsaison$Saison_cour_prec2x[-1], rep(1.7, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("blue", 0.5))
points(Rsaison$Saison_cour_8507[-1], rep(1.6, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("salmon", 0.5))
points(Rsaison$Saison_cour_1122[-1], rep(1.5, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("purple", 0.5))
points(Rsaison$Saison_cour_prec_8507[-1], rep(1.4, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("orange", 0.5))
points(Rsaison$Saison_cour_prec_1122[-1], rep(1.3, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("yellow", 0.5))
points(Rsaison$Saison_cour_prec2x_8507[-1], rep(1.2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("cyan", 0.5))
points(Rsaison$Saison_cour_prec2x_1122[-1], rep(1.1, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("lightgreen", 0.5))
yaxes.noms <- c("c","p","c+p","c+2*p","c(1985-2007)","c(2011-2022)","c+p(1985-2007)",
                "c+p(2011-2022)","c+2*p(1985-2007)","c+2*p(2011-2022)")
axis(2, at=seq(2, 1.1, -0.1), labels=yaxes.noms, cex.axis=0.75, las = 2)

####################################
# CORRELATIONS TEMP PRINTEMPS - IF
####################################
# matrice saisons
Data_saison <- TempEauNorm0101Saison %>%
  filter(Saison=="printemps")
saisonCor <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1982],
                        TempMoySai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_moy_saison = mean(Temp_moy)) %>%
                          subset(select = Temp_moy_saison) %>%
                          as.vector()%>%
                          unlist(), 
                        TempMedSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_med_saison = median(Temp_moy)) %>%
                          subset(select = Temp_med_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMaxSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_max_saison = max(Temp_moy)) %>%
                          subset(select = Temp_max_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempMinSai = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_min_saison = min(Temp_moy)) %>%
                          subset(select = Temp_min_saison) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursChaudsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                          subset(select = Temp_chaud_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax5 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_5) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax10 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_10) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax15 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_15) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax20 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_20) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax30 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_30) %>%
                          as.vector() %>%
                          unlist(),
                        TempJoursFroidsMax50 = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_froid_saison_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                          subset(select = Temp_froid_saison_50) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours23deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 23)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours24deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 24)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours25deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 25)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours26deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 26)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours27.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        TempNJours28.5deg = Data_saison %>%
                          mutate(Annee = format(Data_saison$Date, format = "%Y")) %>%
                          group_by(Annee) %>%
                          summarise(Temp_chaud_saison_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                          subset(select = Temp_chaud_saison_up) %>%
                          as.vector() %>%
                          unlist(),
                        row.names = seq(1982, 2022))

cor(saisonCor, use = "complete.obs", method = "spearman")[,1]
# FAIRE TOUTES LES CORR POUR ETE PUIS FAIRE AVEC LES AUTRES SAISONS

# IF - ANNEE PRECEDANTE
# decaller la colonne des floraisons de 1 pour simuler les cor entre IF et mesures temp annee precedente
saisonPlusCor <- saisonCor
saisonPlusCor$IFmoyAn <- c(saisonPlusCor$IFmoyAn[-1], 100)
saisonPlusCor <- saisonPlusCor[-dim(saisonPlusCor)[1], ]
rownames(saisonPlusCor) <- seq(1983, 2022)

data.frame(Annee0 = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
           Annee1 = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1])

# IF - ANNEE CORRESPONDANTE+PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1983-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                         TempMoySai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_moy_annee) %>%
                           as.vector()%>%
                           unlist(), 
                         TempMedSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_med_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMaxSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_max_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMinSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_min_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours23deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours24deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours25deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours26deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         row.names = seq(1983, 2022))

# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante et precedante
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1984-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                         TempMoySai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_moy_annee) %>%
                           as.vector()%>%
                           unlist(), 
                         TempMedSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_med_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMaxSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_max_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempMinSai = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                           subset(select = Temp_min_annee) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_chaud_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_5) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_10) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_15) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_20) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_30) %>%
                           as.vector()%>%
                           unlist(),
                         TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                           subset(select = Temp_froids_annee_50) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours23deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours24deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours25deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours26deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                           group_by(annee) %>%
                           summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                           subset(select = Temp_chaud_annee_up) %>%
                           as.vector()%>%
                           unlist(),
                         row.names = seq(1984, 2022))

# ANNEE COURANTE 1985-2007
saisonCor8507 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 1985 & Posido77_20$Annee <= 2007],
                            TempMoySai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_med_annee = median(Temp_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMaxSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_max_annee = max(Temp_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMinSai = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_min_annee = min(Temp_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours23deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours24deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours25deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours26deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("1985-01-01")) %>%
                              filter(Date < as.POSIXct("2008-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            row.names = seq(1985, 2007))

# ANNEE COURANTE 2011-2022
saisonCor1122 <- data.frame(IFmoyAn = Posido77_20$Moyenne[Posido77_20$Annee >= 2011 & Posido77_20$Annee <= 2022],
                            TempMoySai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_moy_annee = mean(Temp_moy)) %>%
                              subset(select = Temp_moy_annee) %>%
                              as.vector()%>%
                              unlist(), 
                            TempMedSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_med_annee = median(Temp_moy)) %>%
                              subset(select = Temp_med_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMaxSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_max_annee = max(Temp_moy)) %>%
                              subset(select = Temp_max_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempMinSai = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_min_annee = min(Temp_moy)) %>%
                              subset(select = Temp_min_annee) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_5 = extract_elements(Temp_moy, end = 5, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_10 = extract_elements(Temp_moy, end = 10, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_15 = extract_elements(Temp_moy, end = 15, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_20 = extract_elements(Temp_moy, end = 20, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_30 = extract_elements(Temp_moy, end = 30, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursChaudsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_50 = extract_elements(Temp_moy, end = 50, SUM = TRUE)) %>%
                              subset(select = Temp_chaud_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax5 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_5 = extract_elements(Temp_moy, end = 5, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_5) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax10 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_10 = extract_elements(Temp_moy, end = 10, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_10) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax15 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_15 = extract_elements(Temp_moy, end = 15, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_15) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax20 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_20 = extract_elements(Temp_moy, end = 20, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_20) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax30 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_30 = extract_elements(Temp_moy, end = 30, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_30) %>%
                              as.vector() %>%
                              unlist(),
                            TempJoursFroidsMax50 = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_froid_annee_50 = extract_elements(Temp_moy, end = 50, DEC = FALSE, SUM = TRUE)) %>%
                              subset(select = Temp_froid_annee_50) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours23deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 23)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours24deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 24)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours25deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 25)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours26deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 26)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours27.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 27.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            TempNJours28.5deg = Data_saison %>%
                              filter(Date >= as.POSIXct("2011-01-01")) %>%
                              filter(Date < as.POSIXct("2023-01-01")) %>%
                              mutate(Annee = format(Date, format = "%Y")) %>%
                              group_by(Annee) %>%
                              summarise(Temp_chaud_annee_up = count_higher_elements(Temp_moy, min = 28.5)) %>%
                              subset(select = Temp_chaud_annee_up) %>%
                              as.vector() %>%
                              unlist(),
                            row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1984-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2010-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-1]){
  if(annee == unique(Data_precAn$Annee)[-1][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-1][length(unique(Data_precAn$Annee)[-1])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison2Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(2011, 2022))


# IF - ANNEE CORRESPONDANTE+2*PRECEDANTE (1985-2007)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et 2*precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("1983-01-01")) %>%
  filter(Date < as.POSIXct("2008-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("1985-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2008-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor8507 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(1985, 2007))


# IF - ANNEE CORRESPONDANTE+PRECEDANTE (2011-2022)
# Creation du data frame contenant IF moyen par annee et les temperatures de l'annee correspondante et precedante
Data_precAn <- Data_saison %>%
  filter(Date >= as.POSIXct("2009-01-01")) %>%
  filter(Date < as.POSIXct("2023-01-01")) %>%
  mutate(Annee = as.integer(format(Date, format = "%Y")))
for(annee in unique(Data_precAn$Annee)[-(1:2)]){
  if(annee == unique(Data_precAn$Annee)[-(1:2)][1]){TempEauNorm0101_anDBL <- data.frame()}
  Temps_moy <- list(Data_precAn[Data_precAn$Annee == annee | Data_precAn$Annee == annee-1 | Data_precAn$Annee == annee-2, ]$Temp_moy)
  TempEauNorm0101_anDBL <- rbind(TempEauNorm0101_anDBL, cbind(annee, Temps_moy))
  if(annee == unique(Data_precAn$Annee)[-(1:2)][length(unique(Data_precAn$Annee)[-(1:2)])]){
    TempEauNorm0101_anDBL <- data.frame(IF_moy = Posido77_20[Posido77_20$Date >= as.POSIXct("2011-01-01") &
                                                               Posido77_20$Date < as.POSIXct("2023-01-01"), ]$Moyenne, 
                                        TempEauNorm0101_anDBL)
  }
}

# Creation de la matrice par annee
saison3Cor1122 <- data.frame(IFmoyAn = TempEauNorm0101_anDBL$IF_moy,
                             TempMoySai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_moy_annee = mean(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_moy_annee) %>%
                               as.vector()%>%
                               unlist(), 
                             TempMedSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_med_annee = median(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_med_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMaxSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_max_annee = max(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_max_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempMinSai = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_min_annee = min(Temps_moy$Temps_moy)) %>%
                               subset(select = Temp_min_annee) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_5 = extract_elements(Temps_moy$Temps_moy, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_10 = extract_elements(Temps_moy$Temps_moy, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_15 = extract_elements(Temps_moy$Temps_moy, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_20 = extract_elements(Temps_moy$Temps_moy, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_30 = extract_elements(Temps_moy$Temps_moy, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursChaudsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_50 = extract_elements(Temps_moy$Temps_moy, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_chaud_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax5 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_5 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 5, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_5) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax10 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_10 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 10, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_10) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax15 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_15 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 15, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_15) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax20 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_20 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 20, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_20) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax30 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_30 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 30, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_30) %>%
                               as.vector()%>%
                               unlist(),
                             TempJoursFroidsMax50 = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_froids_annee_50 = extract_elements(Temps_moy$Temps_moy, DEC = FALSE, end = 50, SUM = TRUE)) %>%
                               subset(select = Temp_froids_annee_50) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours23deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 23)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours24deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 24)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours25deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 25)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours26deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 26)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours27.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 27.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             TempNJours28.5deg = TempEauNorm0101_anDBL %>%
                               group_by(annee) %>%
                               summarise(Temp_chaud_annee_up = count_higher_elements(Temps_moy$Temps_moy, min = 28.5)) %>%
                               subset(select = Temp_chaud_annee_up) %>%
                               as.vector()%>%
                               unlist(),
                             row.names = seq(2011, 2022))

# correlations :
Rsaison <- data.frame(Saison_cour               = cor(saisonCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_prec               = cor(saisonPlusCor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec          = cor(saison2Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x        = cor(saison3Cor, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_8507          = cor(saisonCor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_1122          = cor(saisonCor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_8507     = cor(saison2Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec_1122     = cor(saison2Cor1122, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_8507   = cor(saison3Cor8507, use = "complete.obs", method = "spearman")[, 1],
                      Saison_cour_prec2x_1122   = cor(saison3Cor1122, use = "complete.obs", method = "spearman")[, 1])

# graphe des correlations calculees
par(mar = c(1,3,1,0) + 4)
plot(Rsaison$Saison_cour[-1], rep(2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("black", 0.5),
     ylim = c(1,2.1), xlim = c(-0.7, 0.7),
     xlab = "Coefficient de Pearson", ylab = "", 
     yaxt="n", main = "Printemps")
points(Rsaison$Saison_prec[-1], rep(1.9, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("red", 0.5))
points(Rsaison$Saison_cour_prec[-1], rep(1.8, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("green", 0.5))
points(Rsaison$Saison_cour_prec2x[-1], rep(1.7, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("blue", 0.5))
points(Rsaison$Saison_cour_8507[-1], rep(1.6, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("salmon", 0.5))
points(Rsaison$Saison_cour_1122[-1], rep(1.5, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("purple", 0.5))
points(Rsaison$Saison_cour_prec_8507[-1], rep(1.4, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("orange", 0.5))
points(Rsaison$Saison_cour_prec_1122[-1], rep(1.3, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("yellow", 0.5))
points(Rsaison$Saison_cour_prec2x_8507[-1], rep(1.2, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("cyan", 0.5))
points(Rsaison$Saison_cour_prec2x_1122[-1], rep(1.1, length(Rsaison$Saison_cour[-1])), pch = 15, col = alpha("lightgreen", 0.5))
yaxes.noms <- c("c","p","c+p","c+2*p","c(1985-2007)","c(2011-2022)","c+p(1985-2007)",
                "c+p(2011-2022)","c+2*p(1985-2007)","c+2*p(2011-2022)")
axis(2, at=seq(2, 1.1, -0.1), labels=yaxes.noms, cex.axis=0.75, las = 2)

# save(list = ls(pattern = "R"), file = "exploratory_analysis/Rpearson.RData")



