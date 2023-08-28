##################################
####### add_n_year ###############
##################################

# Ajoute n annees aux dates entrees et renvoie le vecteur de dates modifie.
add_n_year <- function(dates, nyear = 0){
  dates <- dates[format(dates, format = "%m-%d") != "02-29"]
  year <- as.integer(format(dates, format = "%Y"))
  year <- as.character(year + nyear)
  md   <- format(dates, format = "-%m-%d")
  return(as.POSIXct(paste0(year, md)))
}


##################################
####### compute_CI ###############
##################################

# Cette fonction accepte une vecteur de nombres ainsi qu'une valeur d'erreur alpha (default : 0.95).
# Elle renvoie une liste contenant :
  # la moyenne
  # l'ecart-type
  # la valeur du quantile de la loi de student a n-1 dl
  # le degre de liberte utilise
  # les bornes inferieures et superieures
  # la deviation a la moyenne
# ARGS :
  # values = vecteur de nombres
  # a = valeur de confiance (default : 0.95)
compute_CI <- function(values, a = 0.95){
  df <- length(values)-1
  t <- qt(1-((1-a)/2), df)
  inf <- mean(values)-(t*(sd(values)/sqrt(length(values))))
  sup <- mean(values)+(t*(sd(values)/sqrt(length(values))))
  dev <- sup-mean(values)
  return(list(mean   = mean(values),
              sd     = sd(values),
              t      = t,
              df     = df,
              bounds = c(inf, sup),
              dev    = dev))
}


##################################
####### count_higher_elements ####
##################################

# Compte le nombre d'elements d'un vecteur superieurs ou égaux a une valeur seuil et renvoie la valeur.
# ARGS :
  # vec = vecteur de nombres
  # min = valeur seuil
count_higher_elements <- function(vec, min = 0){
  vec <- vec[vec >= min]
  return(length(vec))
}


##################################
####### count_na ################# 
##################################

# Donne le nombre de NAs dans chaque colonne d'un dataframe et le pourcentage correspondant.
# ARGS :
  # dat = dataframe duquel compter les NAs
count_na <- function(dat){
  func <- function(dat) {
    sum(is.na(dat))
  }
  VAL.ABS. <- apply(dat, 2, func)
  div <- dim(dat)[1]
  POURC. <- round(VAL.ABS./div*100)
  return(rbind(VAL.ABS., POURC.))
}


##################################
####### extract_elements #########
##################################

# Fonction qui extrait les elements d'un vecteur entre les positions start et end,
# sur le vecteur trie (SORT = TRUE) ou non (SORT = FALSE) dans l'ordre decroissant
# (DEC = TRUE) ou non (DEC = FALSE). Si SUM = TRUE, alors la somme est renvoyee.
# ARGS :
  # vec = vecteur de nombres
  # start = position du premier element a prendre (defaut : premiere position)
  # end = position du dernier element a prendre (defaut : derniere position)
  # SORT = booleen determinant si le vecteur doit etre trie avant d'extraire les elements (defaut : TRUE)
  # DEC = booleen determinant le sens du tri quand SORT=TRUE. Si DEC = TRUE, le tri est decroissant, 
    # sinon croissant (defaut : TRUE)
  # SUM = change l'output de la fonction. Boolen qui specifie si la sortie doit etre remplacee par la somme
    # des elements du vecteur de sortie au lieu du vecteur lui-meme (defaut : FALSE).
extract_elements <- function(vec, start = 1, end = length(vec), SORT = TRUE, DEC = TRUE, SUM = FALSE){
  if(SORT == TRUE){vec <- sort(vec, decreasing = DEC)}
  if(SUM == TRUE){
    return(sum(vec[start:end]))
  }
  return(vec[start:end])
}

##################################
####### justesse_temp_ampl ###
##################################

# Fonction qui pour une valeur seuil, renvoie la fraction
# d'annees qui ont ete correctement predites, cad si la valeur seuil est inferieure a l'amplitude
# de temperature des saisons de l'annee en question et qu'il y a eu floraison, ou le contraire.
# Une annee est classee TRUE si l'amplitude de temperatures entre le min et le max est superieure au seuil, FALSE sinon.
# ARGS :
# tresh_val = un nombre
# seasons = saisons a considerer (defaut : ete et hiver).
# ete, automne, hiver et printemps.
# period = veteur contenant deux entiers correspondant aux bornes des annees a 
# considerer dans le calcul d'exactitude (defaut : 1982 a 2022)
justesse_temp_ampl <- function(tresh_val, n1 = 1, n2 = 1, seasons = c("ete","hiver"), period = c(1982, 2022)){
  if(length(seasons) != 2 | length(tresh_val) != 1){
    print("Deux saisons et  une valeur seuil doivent etre entrees !")
    return()
  }
  
  # creation de l'expression qui filtre les donnees selon la saison
  cmd1 <- paste0("TempEauNorm0101Saison[")
  cmd2 <- paste0(", ]")
  sea <- paste0('TempEauNorm0101Saison$Saison == "', seasons[1],'" | 
                                              TempEauNorm0101Saison$Saison == "', seasons[2], '"')
  
  cmd <- paste0(cmd1, sea, cmd2)
  
  eval(parse(text = cmd)) %>% # insere le tableau de donnees ne contenant que les saisons demandees
    group_by(Annee) %>%
    summarise(Ampl = mean(extract_elements(Temp_moy, end = n1, SORT = TRUE, DEC = TRUE)) -
                mean(extract_elements(Temp_moy, end = n2, SORT = TRUE, DEC = FALSE))) %>% # ordre saisons : automne, ete, hiver, printemps
    data.frame(Seuil = rep(tresh_val,       # ajouter les valeurs seuil au tableau et les IF
                           times = max(as.integer(TempEauNorm0101Saison$Annee))-min(as.integer(TempEauNorm0101Saison$Annee))+1),
               IF = rep(Posido77_20$Moyenne[Posido77_20$Date >= as.POSIXct("1982-01-01")], each = length(tresh_val))) %>%
    mutate(Temp_sup = Ampl >= Seuil) -> data # Temp_inf = T si temperature min sous le seuil, F sinon
  if(length(period) != 2){print("period doit contenir 2 entiers")}
  else{ # filtrer sur les annees demandees
    data <- data %>%
      filter(Annee >= period[1]) %>%
      filter(Annee <= period[2])
  }
  data$IFbin <- FALSE
  data$IFbin[data$IF > 0] <- TRUE
  data$IFbin[is.na(data$IF)==TRUE] <- NA # IF binaire (T si IF > 0, sinon F)
  data <- data %>% group_by(Annee) %>% mutate(Temp_sup = mean(Temp_sup)) # faire "moyenne" sur true/false par annee
  data$Temp_sup[data$Temp_sup != 1] <- FALSE # false si toutes les saisons d'une annee n'etaient pas a true
  data$Temp_sup[data$Temp_sup == 1] <- TRUE # true si toutes les saisons d'une annee sont a true
  print(data, n = 'inf')
  tab <- table(data$IFbin, data$Temp_sup)
  if(dim(tab)[2] != 2){tab <- cbind(tab, c(0, 0))}
  return(c(sum(tab[1,1], tab[2,2])/sum(tab), print(round(tresh_val, 2)), n1, n2)) # renvoie fraction d'annees TT ou FF et le(s) seuil(s)
}



##################################
####### justesse_temp_inf ########
##################################

# Fonction qui pour une serie de valeurs seuils (max 4, une par saison), renvoie la fraction
# d'annees qui ont ete correctement predites, cad si la valeur seuil est superieure au min
# de temperature de la saison de l'annee en question et qu'il y a eu floraison, ou le contraire.
# Une annee est classee TRUE si toutes les saisons respectent leur seuil, FALSE sinon.
# ARGS :
# tresh_val = vecteur de nombres en contenant jusqu'a 4
# seasons = saisons a considerer (defaut : all). Peut etre un a quatre parmi 
# ete, automne, hiver et printemps.
# period = veteur contenant deux entiers correspondant aux bornes des annees a 
# considerer dans le calcul d'exactitude (defaut : 1982 a 2022)
justesse_temp_inf <- function(tresh_val, seasons = "all", period = c(1982, 2022), prev = FALSE){
  if(seasons[1] == "all" & length(tresh_val) != 4){
    print("Un vecteur contenant 4 nombres doit etre entre !")
    return()
  }
  else if(seasons[1] != "all" & length(tresh_val) != length(seasons)){
    print("Le nombre de valeurs seuil doit être égal au nombre de saisons !")
    return()
  }
  
  # creation de l'expression qui filtre les donnees selon la saison
  cmd1 <- paste0("TempEauNorm0101Saison[")
  cmd2 <- paste0(", ]")
  if(seasons[1] == "all" | length(seasons) == 4){sea <- paste0('TempEauNorm0101Saison$Saison == "ete" | 
                                                            TempEauNorm0101Saison$Saison == "automne" | 
                                                            TempEauNorm0101Saison$Saison == "hiver" |
                                                            TempEauNorm0101Saison$Saison == "printemps"')}
  
  else if(length(seasons) == 3){sea <- paste0('TempEauNorm0101Saison$Saison == "', seasons[1],'" | 
                                                            TempEauNorm0101Saison$Saison == "', seasons[2], '" |
                                                            TempEauNorm0101Saison$Saison == "', seasons[3], '"')}
  
  else if(length(seasons) == 2){sea <- paste0('TempEauNorm0101Saison$Saison == "', seasons[1],'" | 
                                              TempEauNorm0101Saison$Saison == "', seasons[2], '"')}
  
  else{sea <- paste0('TempEauNorm0101Saison$Saison == "', seasons[1],'"')}
  
  cmd <- paste0(cmd1, sea, cmd2)
  
  eval(parse(text = cmd)) %>% # insere le tableau de donnees ne contenant que les saisons demandees
    group_by(Annee, Saison) %>%
    summarise(Temp_min = min(Temp_moy)) %>% # ordre saisons : automne, ete, hiver, printemps
    data.frame(Seuil = rep(tresh_val,       # ajouter les valeurs seuil au tableau et les IF
                           times = max(as.integer(TempEauNorm0101Saison$Annee))-min(as.integer(TempEauNorm0101Saison$Annee))+1),
               IF = rep(Posido77_20$Moyenne[Posido77_20$Date >= as.POSIXct("1982-01-01")], each = length(tresh_val))) %>%
    mutate(Temp_inf = Temp_min <= Seuil) -> data # Temp_inf = T si temperature min sous le seuil, F sinon
  if(length(period) != 2){print("period doit contenir NA ou 2 entiers")}
  else{ # filtrer sur les annees demandees
    data <- data %>%
      filter(Annee >= period[1]) %>%
      filter(Annee <= period[2])
  }
  if(length(seasons) == 1 & prev==TRUE){ # decaller d'une annee si prev = T
    data <- data.frame(Annee = seq(data$Annee[2], data$Annee[dim(data)[1]], by = 1), 
                       data[-dim(data)[1], 2:4], IF = data$IF[-1], 
                       Temp_inf = data$Temp_inf[-dim(data)[1]])
  }
  else if(length(seasons) != 1 & prev==TRUE){
    print("seasons > 1 and prev = TRUE not implemented.")
    return()
  }
  data$IFbin <- FALSE
  data$IFbin[data$IF > 0] <- TRUE
  data$IFbin[is.na(data$IF)==TRUE] <- NA # IF binaire (T si IF > 0, sinon F)
  data <- data %>% group_by(Annee) %>% mutate(Temp_inf = mean(Temp_inf)) # faire "moyenne" sur true/false par annee
  data$Temp_inf[data$Temp_inf != 1] <- FALSE # false si toutes les saisons d'une annee n'etaient pas a true
  data$Temp_inf[data$Temp_inf == 1] <- TRUE # true si toutes les saisons d'une annee sont a true
  tab <- table(data$IFbin, data$Temp_inf)
  if(dim(tab)[2] != 2){tab <- cbind(tab, c(0, 0))}
  return(c(sum(tab[1,1], tab[2,2])/sum(tab), print(round(tresh_val, 2)))) # renvoie fraction d'annees TT ou FF et le(s) seuil(s)
}


##################################
####### justesse_temp_sup ########
##################################

# Fonction qui pour une serie de valeurs seuils (max 4, une par saison), renvoie la fraction
# d'annees qui ont ete correctement predites, cad si la valeur seuil est depassee par le max 
# de temperature de la saison de l'annee en question et qu'il y a eu floraison, ou le contraire.
# Une annee est classee TRUE si toutes les saisons respectent leur seuil, FALSE sinon.
# ARGS :
  # tresh_val = vecteur de nombres en contenant jusqu'a 4
  # seasons = saisons a considerer (defaut : all). Peut etre un a quatre parmi 
    # ete, automne, hiver et printemps.
  # period = veteur contenant deux entiers correspondant aux bornes des annees a 
    # considerer dans le calcul d'exactitude (defaut : 1982 a 2022)
justesse_temp_sup <- function(tresh_val = c(20, 25, 14, 23), seasons = "all", period = c(1982, 2022)){
  if(seasons[1] == "all" & length(tresh_val) != 4){
    print("Un vecteur contenant 4 nombres doit etre entre !")
    return()
  }
  else if(seasons[1] != "all" & length(tresh_val) != length(seasons)){
    print("Le nombre de valeurs seuil doit être égal au nombre de saisons !")
    return()
  }
  
  # creation de l'expression qui filtre les donnees selon la saison
  cmd1 <- paste0("TempEauNorm0101Saison[")
  cmd2 <- paste0(", ]")
  if(seasons[1] == "all" | length(seasons) == 4){sea <- paste0('TempEauNorm0101Saison$Saison == "ete" | 
                                                                TempEauNorm0101Saison$Saison == "automne" | 
                                                                TempEauNorm0101Saison$Saison == "hiver" |
                                                                TempEauNorm0101Saison$Saison == "printemps"')}
  
  else if(length(seasons) == 3){sea <- paste0('TempEauNorm0101Saison$Saison == "', seasons[1],'" | 
                                                            TempEauNorm0101Saison$Saison == "', seasons[2], '" |
                                                            TempEauNorm0101Saison$Saison == "', seasons[3], '"')}
  
  else if(length(seasons) == 2){sea <- paste0('TempEauNorm0101Saison$Saison == "', seasons[1],'" | 
                                              TempEauNorm0101Saison$Saison == "', seasons[2], '"')}
  
  else{sea <- paste0('TempEauNorm0101Saison$Saison == "', seasons[1],'"')}
  
  cmd <- paste0(cmd1, sea, cmd2)
  
  eval(parse(text = cmd)) %>% # insere le tableau de donnees ne contenant que les saisons demandees
    group_by(Annee, Saison) %>%
    summarise(Temp_max = max(Temp_moy)) %>% # ordre saisons : automne, ete, hiver, printemps
    data.frame(Seuil = rep(tresh_val,       # ajouter les valeurs seuil au tableau et les IF
                           times = max(as.integer(TempEauNorm0101Saison$Annee))-min(as.integer(TempEauNorm0101Saison$Annee))+1),
               IF = rep(Posido77_20$Moyenne[Posido77_20$Date >= as.POSIXct("1982-01-01")], each = length(tresh_val))) %>%
    mutate(Temp_sup = Temp_max >= Seuil) -> data # Temp_sup = T si temperature max depasse le seuil, F sinon
  if(length(period) != 2){print("period doit contenir NA ou 2 entiers")}
  else{ # filtrer sur les annees demandees
    data <- data %>%
      filter(Annee >= period[1]) %>%
      filter(Annee <= period[2])
  }
  data$IFbin <- FALSE
  data$IFbin[data$IF > 0] <- TRUE
  data$IFbin[is.na(data$IF)==TRUE] <- NA # IF binaire (T si IF > 0, sinon F)
  data <- data %>% group_by(Annee) %>% mutate(Temp_sup = mean(Temp_sup)) # faire "moyenne" sur true/false par annee
  data$Temp_sup[data$Temp_sup != 1] <- FALSE # false si toutes les saisons d'une annee n'etaient pas a true
  data$Temp_sup[data$Temp_sup == 1] <- TRUE # true si toutes les saisons d'une annee sont a true
  tab <- table(data$IFbin, data$Temp_sup)
  if(dim(tab)[2] != 2){tab <- cbind(tab, c(0, 0))}
  return(c(sum(tab[1,1], tab[2,2])/sum(tab), print(round(tresh_val, 2)))) # renvoie fraction d'annees TT ou FF et le(s) seuil(s)
}



##################################
####### getDatesBornes ###########
##################################

# Necessite un dataframe contenant au moins une colonne de dates et une variable pouvant
# contenir des NAs, et renvoie une liste contenant les bornes correspondant aux periodes 
# ne contenant aucun NA pour la variable etudiee.
# ARGS :
  # Dat   = dataframe a etudier
  # Dates = numero de la colonne de Dat contenant les dates
  # VarNA = numero de la colonne de Dat correspondant a la variable etudiee
  # Continuous.Dates = booleen determinant si les dates non reprises dans le dataframe
    # ont un impact dans la delimitation par les bornes (defaut, FALSE) ou si elles
    # doivent etre considerees comme contenant des valeurs manquantes (TRUE)
  # Increment.Time = expression numerique exprimant quel est l'increment de temps a
    # considerer quand Continuous.Date est TRUE (default : 'jour').
  # Data.Value = chaine de caracteres definissant la facon dont les valeurs multiples
    # pour une meme date doivent etre traitees ('partial' ou 'p' (defaut) signifie que
    # au moins une valeur doit etre connue pour etre consideree comme contenant de la
    # donnee, 'complete' ou 'c' exige que toutes les valeurs pour une date soient connues).
  # Show.Text = Booleen qui determine si le tableau recapitulatif de donnees doit etre
    # visualise ainsi que sa lecture litteraire.
getDatesBornes <- function(Dat, Dates=1, VarNA=2, Continuous.Dates=FALSE, 
                           Increment.Time="jour", Data.Value="partial",
                           Show.Text=FALSE){
  Increment.TimeInit <- Increment.Time
  # Copier dataframe
  Datc <- Dat[, c(Dates, VarNA)]
  colnames(Datc) <- c("Date", "Variable")
  # Creer le bon format et bons attributs, et ordonner selon le temps
  Datc$Date <- as.POSIXct(Datc$Date)
  attributes(Datc$Date) <- attributes(as.POSIXct("0001-01-01"))
  Datc <- Datc[order(Datc$Date), ]
  # Etablir l'increment de temps
  if(Increment.Time=="jour")
    Increment.Time <- 24*60*60
  else if(Increment.Time=="semaine")
    Increment.Time <- 7*24*60*60
  else if(Increment.Time=="mois")
    Increment.Time <- 31*24*60*60
  else if(Increment.Time=="annee")
    Increment.Time <- 365*24*60*60
  else
    message("ATTENTION : si l'incrément de temps n'est pas un parmi 'jour',
            'semaine', 'mois' ou 'annee', des choses étranges peuvent se passer.")
  
  if(Increment.TimeInit=="mois" | Increment.TimeInit=="annee"){
    message("L'implémentation pour 'mois' et 'annee' est détraquée.")
    return()
  }
  
  # Si on veut dates continues, remplacer variable par NA (je pense)
  if(Continuous.Dates==FALSE)
    newDat <- data.frame(Date = seq(from = min(Datc$Date), 
                                    to = max(Datc$Date), 
                                    by = Increment.Time), 
                         Variable = 1)
  else if(Continuous.Dates==TRUE)
    newDat <- data.frame(Date = seq(from = min(Datc$Date), 
                                    to = max(Datc$Date), 
                                    by = Increment.Time), 
                         Variable = NA)
  else{
    print("Continuous.Dates doit être TRUE ou FALSE.")
    return()
  }
  
  newDat <- rbind(newDat, data.frame(Date = max(Datc$Date)+Increment.Time, 
                                     Variable = NA))
  
  # Corriger decallage...
  # ... pour jour
  if(Increment.Time==24*60*60)
    for(i in 1:length(newDat$Date)){
      if(format(newDat[i,1], format = "%H")!="00"){newDat[i,1] <- newDat[i,1]+1*60*60}
    }
  # ... pour semaine
  else if(Increment.Time==7*24*60*60)
    for(i in 1:length(newDat$Date)){
      if(format(newDat[i,1], format = "%H")!="00"){newDat[i,1] <- newDat[i,1]-1*60*60}
    }
  # ... pour mois
  else if(Increment.Time==31*24*60*60){
    anneesBisex <- seq(1904, 2096, 4)
    for(i in 2:dim(newDat)[1]){
      # mois de 31 jours
      if(sum(format(newDat[i-1, 1], format = "%m")==c("01","03",'05',"07","08","10","12")) == 1){
        newDat$Date[i] <- newDat$Date[i-1]+31*24*60*60
        newDat$Increment[i-1] <- 31*24*60*60
      }
      # mois de 30 jours
      else if(sum(format(newDat[i-1, 1], format = "%m")==c("04","06",'09',"11")) == 1){
        newDat$Date[i] <- newDat$Date[i-1]+30*24*60*60
        newDat$Increment[i-1] <- 30*24*60*60
      }
      # fevrier
      else{
        newDat$Date[i] <- newDat$Date[i-1]+28*24*60*60
        newDat$Increment[i-1] <- 38*24*60*60
        # ajouter 1 jour si annee bisextile
        if(sum(format(newDat[i-1, 1], format = "%Y")==anneesBisex) == 1){
          newDat$Date[i] <- newDat$Date[i]+1*24*60*60
          newDat$Increment[i-1] <- 29*24*60*60
        }
      }
      # ajuster decallage horaire
      for(i in 1:length(newDat$Date)){
        if(format(newDat[i,1], format = "%H")!="00"){newDat[i,1] <- newDat[i,1]-1*60*60}
      }
    }
    # ajouter derniere date
    newDat <- rbind(newDat, data.frame(Date = max(Datc$Date)+Increment.Time, 
                                       Variable = NA,
                                       Increment = Increment.Time))
  }
  # ... pour annee
  else if(Increment.Time==365*24*60*60){
    mois <- format(newDat[1,1], format = "%m")
    jour <- format(newDat[1,1], format = "%d")
    annees <- as.character(seq(as.integer(format(newDat[1,1], format = "%Y")),
                               as.integer(format(newDat[dim(newDat)[1],1], format = "%Y")),
                               by = 1))
    Date <- as.POSIXct(paste0(annees, "-", mois, "-", jour))
    newDat$Date <- Date
  }
  
  # Atttribuer les valeurs connues a newDat...
  for(i in 1:length(unique(Datc$Date))){
    DateCour <- unique(Datc$Date)[i]
    Valeurs  <- Datc[Datc$Date==DateCour, ]$Variable
    # ... soit en attribuant '1' quand au moins une valeur par date est connue
    if(Data.Value=="partial" | Data.Value=="p"){
      if(sum(!is.na(Valeurs)) >= 1){Valeur <- 1}
      else{Valeur <- NA}
    }
    # ... soit quand toutes les valeurs par date sont connues
    else if(Data.Value=="complete" | Data.Value=="c"){
      if(sum(!is.na(Valeurs)) == length(Valeurs)){Valeur <- 1}
      else{Valeur <- NA}
    }
    else{
      message("ERREUR : Data.Value doit être 'complete'/'c' ou 'partial'/'p'.")
      return()
    }
    
    newDat$Variable[which(newDat$Date==DateCour)] <- Valeur
  }
  
  if(Show.Text==TRUE){View(newDat)}
  
  # Generer bornes
  bornes <- vector()
  STOP <- FALSE
  for(date in newDat$Date){
    attributes(date) <- attributes(newDat$Date)
    Valeur <- newDat[which(newDat$Date==date), 2]
    # Cas de la 1e date 
    if(date==newDat$Date[1]){
      if(Show.Text==TRUE){print("1e date")}
      if(!is.na(Valeur)){
        bornes <- append(bornes, date)
        if(Show.Text==TRUE){print("1e date n'est pas NA")}
      }
    }
    # Cas de la valeur connue demarrante...
    else if(!is.na(Valeur) & is.na(newDat[which(newDat$Date==dateAnt), 2])){
      # ... entouree de 2 NAs
      ## (definition increment dans le cas du mois)
      if(Increment.TimeInit=="mois")
        Increment.Time <- newDat[which(newDat$Date==date), 3]
      if(is.na(newDat[which(newDat$Date==date+Increment.Time), 2])){
        bornes <- append(bornes, c(date, date))
        STOP <- TRUE
        if(Show.Text==TRUE){print("Val connue entouree NAs")}
      }
      # ... ou non
      else{
        bornes <- append(bornes, date)
        if(Show.Text==TRUE){print("Val connue demarrante")}
      }
    }
    # Cas de la valeur connue fermante
    else if(is.na(Valeur) & !is.na(newDat[which(newDat$Date==dateAnt), 2]) & STOP == FALSE){
      bornes <- append(bornes, dateAnt)
      STOP <- FALSE
      if(Show.Text==TRUE){print("Val connue fermante")}
    }
    else{
      if(Show.Text==TRUE){print("Juste NA ou suite de 1")}
    }
    dateAnt <- date
  }
  
  # Vecteurs bornants
  start <- bornes[seq(1, length(bornes), 2)]
  end   <- bornes[seq(2, length(bornes), 2)]
  if(length(start)-length(end)==1)
    end <- append(end, drop_na(newDat)[dim(drop_na(newDat))[1], 1])
  
  # assign("newDat", newDat, envir = globalenv())
  
  return(list(start = as.character(start), end = as.character(end)))
}




##################################
####### lmEvolDate ###############
##################################

# Necessite un dataframe et un modele lineaire, et renvoie une liste contenant
# l'evolution annuelle de la variable etudiee, le minimum et le maximum du 
# modele (extremites du modele lineaire), et le nombre d'annees entre le debut
# et la fin des predictions.
# ARGS :
  # dat = dataframe a etudier. La colonne contenant les dates doit etre nommee Date
  # mod = modele lineaire
lmEvolDate <- function(dat, mod){
  # ...predire temperatures au debut et a la fin selon le modele...
  modMinMax <- predict(mod)[c(1, length(predict(mod)))]
  # ...calculer n annees...
  nAnnees <- as.integer(format(max(dat$Date), format = "%Y"))-
    as.integer(format(min(dat$Date), format = "%Y"))
  # ... et calculer evolution annuelle
  evolParAn <- as.numeric((modMinMax[2]-modMinMax[1])/nAnnees)
  return(list(evolParAn = evolParAn, modMinMax = modMinMax, nAnnees = nAnnees))
}

##################################
####### saisons ##################
##################################

# Necessite un vecteur de dates de type POSIXct, et renvoie un vecteur
# contenant les saisons correspondantes.
# ARGS :
  # vecDates = un vecteur contenant des dates de type POSIXct

saisons <- function(vecDates){
  Saisons <- rep(NA, length(vecDates))
  Data <- data.frame(Dates = vecDates, 
                     Mois  = format(vecDates, format = "%m"), 
                     Jour  = format(vecDates, format = "%d"))
  # ete (21 juin - 20 sep)
  indETE <- which( (Data$Mois == "06" & Data$Jour >= "21") | 
                     Data$Mois == "07" | Data$Mois == "08" | 
                     (Data$Mois == "09" & Data$Jour < "21") )
  Saisons[indETE] <- "ete"
  # automne (21 sep - 20 dec)
  indAUT <- which( (Data$Mois == "09" & Data$Jour >= "21") | 
                     Data$Mois == "10" | Data$Mois == "11" | 
                     (Data$Mois == "12" & Data$Jour < "21") )
  Saisons[indAUT] <- "automne"
  # hiver (21 dec - 20 mars)
  indHIV <- which( (Data$Mois == "12" & Data$Jour >= "21") | 
                     Data$Mois == "01" | Data$Mois == "02" | 
                     (Data$Mois == "03" & Data$Jour < "21") )
  Saisons[indHIV] <- "hiver"
  # printemps (21 mars - 20 juin)
  indPRI <- which( (Data$Mois == "03" & Data$Jour >= "21") | 
                     Data$Mois == "04" | Data$Mois == "05" | 
                     (Data$Mois == "06" & Data$Jour < "21") )
  Saisons[indPRI] <- "printemps"
  
  if(anyNA(Saisons) == TRUE){warning("NAs introduits dans saisons")}
  
  return(Saisons)
}


##################################
####### simili_window ############
##################################

# Cette fonction accepte deux entiers representant 2 jours de l'annee (bound1 et bound2)
# et qui serviront de bornes pour la fenetre. 
# Sur base d'un seuil donne (ou calcule sur base de la taille de la fenetre si thresh = NA, defaut),
# la proportion d'annees dont la somme des temperatures au sein de la fenetre et superieur au seuil
# et pour lesquelles il y a eu floraison + celles dont la somme ne depasse pas le seuil et pour
# lesquelles il n'y a pas eu de floraison est renvoyee.
# ARGS :
# bound1 et bound2 = les entiers determinant les jours bornant
# thresh = un entier determinant le seuil de temperatures. Par defaut, il est calcule
# comme etant le produit de la taille de la fenetre par la moyenne des temperatures considerees 
# et par un nombre aleatoire de laloi normale de moyenne 1 et d'ecart-type 0.25.

simili_window <- function(bound1, bound2, thresh = NA){
  inf <- which(data$MoisJour == pool[min(c(bound1,bound2))]) # reperer indices min
  sup <- which(data$MoisJour == pool[max(c(bound1,bound2))]) # reperer indices max
  # creation d'un data frame ne contenant que les periodes demandees pour chaque annee
  subdata <- data.frame()
  for(k in 1:length(sup)){
    sub <- data[inf[k]:sup[k], ]
    subdata <- rbind(subdata, sub)
  }
  # fixer la valeur de seuil
  if(is.na(thresh)==TRUE){thresh <- abs(bound1-bound2)*mean(subdata$Temp_moy)*rnorm(1, mean = 1, sd = 0.25)}
  # calculer somme temp par annee sur la periode demandee...
  subdata %>%
    group_by(Annee) %>%
    summarise(Temp_sum = sum(Temp_moy), Seuil = thresh) %>%
    mutate(Temp_bool = Temp_sum >= Seuil) %>% # ... et la comparer au seuil
    data.frame(IF_moy = IFs$Moyenne) -> subdata
  subdata$IFbin <- FALSE
  subdata$IFbin[subdata$IF_moy > 0] <- TRUE
  subdata$IFbin[is.na(subdata$IF_moy)==TRUE] <- NA # IF binaire (T si IF > 0, sinon F)
  tab <- table(subdata$IFbin, subdata$Temp_bool)
  if(dim(tab)[2] != 2){tab <- cbind(tab, c(0, 0))}
  if(bound1 < bound2){
    return(c(as.numeric(sum(tab[1,1], tab[2,2])/sum(tab)), as.integer(bound1), as.integer(bound2),
             pool[bound1], pool[bound2], as.integer(abs(bound1-bound2)), as.numeric(thresh)))
  }
  else{
    return(c(as.numeric(sum(tab[1,1], tab[2,2])/sum(tab)), as.integer(bound2), as.integer(bound1),
             pool[bound2], pool[bound1], as.integer(abs(bound1-bound2)), as.numeric(thresh)))
  }
}



##################################
####### temp_herbier_visu ########
##################################

# ATTENTION, cette fonction est tres specifique au jeu de donnees concernant la
# temperature moyenne au niveau de herbiers.
# Sur base d'un couple de profondeurs donne, elle renvoie un objet visualisable par
# la fonction ggvistime().
# ARGS :
  # prof1 et prof2 = les profondeurs a analyser (characteres)
  # group = le nom a entrer comme descriptif dans me graph (caractere)
  # color = couleur de la barre dans la lighe du temps

temp_herbier_visu <- function(prof1="10m", prof2="10+2m", 
                              group="Temp. herbier 10-12m", color = "#24BBC4"){
  # obtention des bornes
  bornes <- getDatesBornes(subset(MoyJourTempHerbier, 
                                  subset = (Profondeur==prof1 | Profondeur==prof2)), 
                           Dates = 1, 
                           VarNA = 3,
                           Continuous.Dates = FALSE,
                           Data.Value = "complete")
  # creation de l'objet
  MoyJourTempHerbier_x_x_temps <- data.frame(event = rep("", length(bornes$start)),
                                             start = bornes$start,
                                             end   = bornes$end,
                                             group = rep(group, length(bornes$start)),
                                             color = rep(color, length(bornes$start)))
  return(MoyJourTempHerbier_x_x_temps)
}