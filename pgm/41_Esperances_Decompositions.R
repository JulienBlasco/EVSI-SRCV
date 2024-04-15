# Décomposition de l'écart d'EVSI entre deux groupes
decomposition_EVSI <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_non_limite[elements1]) - sum(df$survie_non_limite[elements2]),
    survie = sum((df$survie[elements1] - df$survie[elements2])*df$non_limite[elements2]), # Calcul de la différence d'EVSI expliquée par la survie
    sante = sum(df$survie[elements2]*(df$non_limite[elements1] - df$non_limite[elements2])) # diff d'EVSI expliquée par la santé
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

decomposition_EVSIF <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_non_limite_forte[elements1]) - sum(df$survie_non_limite_forte[elements2]),
    survie = sum((df$survie[elements1] - df$survie[elements2])*df$non_limite_forte[elements2]), # Calcul de la différence d'EVSI expliquée par la survie
    sante = sum(df$survie[elements2]*(df$non_limite_forte[elements1] - df$non_limite_forte[elements2])) # diff d'EVSI expliquée par la santé
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}


# Fonction de décomposition de l'écart d'ER entre deux groupes
decomposition_ER <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la 1ere categorie, elements2 = liste des indices pour la 2e catégorie
  composants <- c(
    total = sum(df$survie_retraite[elements1]) - sum(df$survie_retraite[elements2]),
    survie = sum((df$survie[elements1] - df$survie[elements2])*df$retraite[elements2]), # Calcul de la différence d'ER expliquée par la survie
    retraite = sum(df$survie[elements2]*(df$retraite[elements1] - df$retraite[elements2])) # diff d'ER expliquée par la retraite
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}


# Fonction de décomposition de l'écart d'ERSI entre deux groupes
decomposition_ERSI <- function(df, elements1, elements2){ #data = dataframe incluant les colonnes nécessaires, elements1 = liste des indices pour la categorie examinée, elements2 = liste des indices pour la catégorie de référence
  composants <- c(total = sum(df$survie_retraite_non_limite[elements1]) - sum(df$survie_retraite_non_limite[elements2]),
                  survie = sum((df$survie[elements1]-df$survie[elements2])*df$retraite_non_limite[elements2]),
                  retraite = sum(df$survie[elements2]*df$non_limite_parmi_retraite[elements2]* (df$retraite[elements1] - df$retraite[elements2]), na.rm=T),
                  sante = sum(df$survie[elements2]*(df$non_limite_parmi_retraite[elements1] - df$non_limite_parmi_retraite[elements2])* df$retraite[elements2], na.rm=T)
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

# Décomposition de l'écart d'espérance de retraite sans incapacité forte
decomposition_ERSIF <- function(df, elements1, elements2){
  composants <-  c(total = sum(df$survie_retraite_non_limite_forte[elements1]) - sum(df$survie_retraite_non_limite_forte[elements2]),
                   survie = sum((df$survie[elements1]-df$survie[elements2])*df$retraite_non_limite_forte[elements2]),
                   retraite = sum(df$survie[elements2]*df$non_limite_forte_parmi_retraite[elements2]* (df$retraite[elements1] - df$retraite[elements2]), na.rm=T),
                   sante = sum(df$survie[elements2]*(df$non_limite_forte_parmi_retraite[elements1] - df$non_limite_forte_parmi_retraite[elements2])* df$retraite[elements2], na.rm=T)
  )
  composants["residu"] <- 2*composants["total"]- sum(composants)
  return(composants)
}

calcul_esperances <- function(df, elements1, elements2){
  decomp_evsi <- decomposition_EVSI(df, elements1, elements2)
  decomp_evsif <- decomposition_EVSIF(df, elements1, elements2)
  decomp_er <- decomposition_ER(df,elements1,elements2)
  decomp_ersi <- decomposition_ERSI(df, elements1, elements2)
  decomp_ersif <- decomposition_ERSIF(df, elements1, elements2)
  
  # On construit une ligne prête à être ajoutée à un dataframe
  data.frame(
    ev = sum(df$survie[elements1]), # Calcul de l'EV
    survie60 = df$survie[elements1[60-30+1]],
    evsi = sum(df$survie_non_limite[elements1]), # Calcul de l'EVSI
    evsif = sum(df$survie_non_limite_forte[elements1]),
    survie60_non_limite = df$survie_non_limite[elements1[60-30+1]],
    survie60_non_limite_forte = df$survie_non_limite_forte[elements1[60-30+1]],
    er = sum(df$survie_retraite[elements1]), # ER
    ersi = sum(df$survie_retraite_non_limite[elements1]), #ERSI
    age_depart = sum(1 - df$retraite[elements1]), # Âge conjoncturel de départ 
    diff_evsi = decomp_evsi["total"], # diff d'EVSI avec les cadres
    diff_evsi_survie = decomp_evsi["survie"],
    diff_evsi_sante = decomp_evsi["sante"],
    diff_evsi_residu = decomp_evsi["residu"],
    diff_evsif = decomp_evsif["total"], # diff d'EVSIF avec les cadres
    diff_evsif_survie = decomp_evsif["survie"],
    diff_evsif_sante = decomp_evsif["sante"],
    diff_evsif_residu = decomp_evsif["residu"],
    diff_er = decomp_er["total"], # diff d'ER avec les cadres
    diff_er_survie = decomp_er["survie"], # Calcul de la différence d'ER avec les cadres expliquée par la survie
    diff_er_retraite = decomp_er["retraite"], # diff d'ER expliquée par la retraite
    diff_er_residu = decomp_er["residu"],
    diff_ersi = decomp_ersi["total"], # diff d'ERSI avec les cadres
    diff_ersi_survie = decomp_ersi["survie"],
    diff_ersi_retraite = decomp_ersi["retraite"],
    diff_ersi_sante = decomp_ersi["sante"],
    diff_ersi_residu = decomp_ersi["residu"],
    ersif = sum(df$survie_retraite_non_limite_forte[elements1]), #ERSI forte
    diff_ersif = decomp_ersif["total"],
    diff_ersif_survie = decomp_ersif["survie"],
    diff_ersif_retraite = decomp_ersif["retraite"],
    diff_ersif_sante = decomp_ersif["sante"],
    diff_ersif_residu = decomp_ersif["residu"]
  )%>% return
}

# On crée le data.frame esperances
esperances <- data.frame()
esperances_sexe <- data.frame()
esperances_sexe_var85 <- data.frame()

# Pour chaque catégorie, on calcule les espérances et la décomposition des écarts
elements_hommes <- which((limitations_retraite_survie$PCS == "Ensemble") & (limitations_retraite_survie$Sexe == "Hommes"))
elements_hommes_var85 <- which((limitations_retraite_survie_var85$PCS == "Ensemble") & (limitations_retraite_survie_var85$Sexe == "Hommes"))
for(sexe in unique(limitations_retraite_survie$Sexe)){
  elements_sexe <- which((limitations_retraite_survie$PCS == "Ensemble") & (limitations_retraite_survie$Sexe == sexe))
  esperances_sexe <-  calcul_esperances(limitations_retraite_survie, elements_sexe, elements_hommes)%>%
    mutate(Sexe = sexe)%>%
    rbind(esperances_sexe,.)
  
  # Variante 85
  elements_sexe_var85 <- which((limitations_retraite_survie_var85$PCS == "Ensemble") & (limitations_retraite_survie_var85$Sexe == sexe))
  esperances_sexe_var85 <-  calcul_esperances(limitations_retraite_survie_var85, elements_sexe_var85, elements_hommes_var85)%>%
    mutate(Sexe = sexe)%>%
    rbind(esperances_sexe_var85,.)
  
  # On repère d'abord pour les cadres, qu'on prend comme PCS de référence
  elements_cadres <- which((limitations_retraite_survie$PCS == "3 - Cadres") & (limitations_retraite_survie$Sexe == sexe))

  for(pcs in unique(limitations_retraite_survie$PCS)){
    elements <- which((limitations_retraite_survie$PCS == pcs) & (limitations_retraite_survie$Sexe == sexe)) # sélection des lignes correspondantes
    esperances <-  calcul_esperances(limitations_retraite_survie, elements, elements_cadres)%>%
      mutate(PCS = pcs,
             Sexe = sexe)%>%
      rbind(esperances,.)
  }
}
row.names(esperances_sexe) <- NULL
esperances_sexe <- select(esperances_sexe, Sexe, everything())
View(esperances_sexe)

row.names(esperances_sexe_var85) <- NULL
esperances_sexe_var85 <- select(esperances_sexe_var85, Sexe, everything())
View(esperances_sexe_var85)


row.names(esperances) <- NULL
esperances <- select(esperances, PCS, Sexe, everything())


View(esperances)
write_csv2(esperances_sexe, "sorties/esperances_sexe.csv")
write_csv2(esperances_sexe_var85, "sorties/esperances_sexe_var85.csv")
write_csv2(esperances, "sorties/esperances.csv")


## SÉRIE LONGUE

# On crée le data.frame resultats
esperances_SL <- data.frame()

# Pour chaque catégorie, on calcule l'EV et l'EVSI
for(sexe in unique(limitations_retraite_survie_SL$Sexe)){
  # On calcule d'abord pour les cadres, qui sont la PCS de référence
  elements_t0 <- which((limitations_retraite_survie_SL$AENQ == 2008) & (limitations_retraite_survie_SL$Sexe == sexe))

  for(annee in unique(limitations_retraite_survie_SL$AENQ)){
    elements <- which((limitations_retraite_survie_SL$AENQ == annee) & (limitations_retraite_survie_SL$Sexe == sexe)) # sélection des lignes correspondantes
    esperances_SL <-  calcul_esperances(limitations_retraite_survie_SL, elements, elements_t0)%>%
      mutate(AENQ = annee,
             Sexe = sexe)%>%
      rbind(esperances_SL,.)
  }
}

row.names(esperances_SL) <- NULL
esperances_SL <- select(esperances_SL, AENQ, Sexe, everything())


View(esperances_SL)
write_csv2(esperances_SL, "sorties/esperances_SL.csv")
