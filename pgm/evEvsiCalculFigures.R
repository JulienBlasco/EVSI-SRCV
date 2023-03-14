library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(forcats)
library(gridExtra)


# Importation des données
limitations <- read_delim("limitationsRetraiteSurvie1303.csv", 
                               delim = ",", escape_double = FALSE, 
                               comment = "#", trim_ws = TRUE)
View(limitations)

# On crée le data.frame resultats
resultats <- data.frame(
  PCS=character(0),
  Sexe=character(0),
  ev=numeric(0),
  survie60=numeric(0),
  survie70=numeric(0),
  evsi=numeric(0),
  survie_valide60=numeric(0),
  survie_valide70=numeric(0),
  e_retraite=numeric(0),
  e_retraite_limite=numeric(0),
  e_retraite_non_limite=numeric(0)
)

# Pour chaque catégorie, on calcule l'EV et l'EVSI
for(genre in unique(limitations$Sexe)){
  for(pcs in unique(limitations$PCS)){
    print(paste(genre,pcs))
    elements <- which((limitations$PCS == pcs) & (limitations$Sexe == genre)) # sélection des lignes correspondantes
    ev <- sum(limitations$survie[elements])/100000 # Calcul de l'EV
    evsi <- sum(limitations$survie_non_limite[elements]) # Calcul de l'EVSI
    e_retraite <- sum(limitations$survie_retraite[elements])
    e_retraite_limite <- sum(limitations$survie_retraite_limite[elements])
    e_retraite_non_limite <- sum(limitations$survie_retraite_non_limite[elements])
    
    # Affichage :
    print(paste("EV : ",ev))
    print(paste("EVSI, y compris déjà invalides : ",evsi))
    # On complète le dataframe
    n <- nrow(resultats)+1
    resultats[n,] <- NA
    resultats[n,"PCS"] <- pcs
    resultats[n,"Sexe"] <- genre
    resultats[n,"ev"] <- ev
    resultats[n,"survie60"] <- limitations$survie[elements[60-30+1]]/100000
    resultats[n,"survie70"] <- limitations$survie[elements[70-30+1]]/100000
    resultats[n,"evsi"] <- evsi
    resultats[n,"survie_valide60"] <-  limitations$survie_non_limite[elements[60-30+1]]
    resultats[n,"survie_valide70"] <-  limitations$survie_non_limite[elements[70-30+1]]
    resultats[n,"e_retraite"] <- e_retraite
    resultats[n,"e_retraite_limite"] <- e_retraite_limite
    resultats[n,"e_retraite_non_limite"] <- e_retraite_non_limite
  }
}

write_csv(resultats, "evEvsi1303.csv")

# EVI = Espérance de vie avec incapacité = EV - EVSI
resultats$evi <- resultats$ev - resultats$evsi
# Proba de survie avec incapacité = Proba de survie - Proba de survie valide
resultats$survie_i60 <- resultats$survie60 - resultats$survie_valide60

# On change le nom des PCS de manière plus agréable
pcs_legende <- c()
pcs_legende["Ensemble"] <- "Ensemble"
pcs_legende["3 - Cadres"] <- "Cadres"
pcs_legende["4 - Professions intermédiaires"] <- "Prof. inter."
pcs_legende["5 - Employés"] <- "Employés"
pcs_legende["6 - Ouvriers"] <- "Ouvriers"
pcs_legende["1 - Agriculteurs"] <- "Agriculteurs"
pcs_legende["2 - Artisans, commerçants et chefs d’entreprise"] <- "Artisans"
resultats$PCS <- pcs_legende[resultats$PCS]

resultats$PCS <- factor(resultats$PCS, levels=c("Agriculteurs","Artisans","Ouvriers","Employés","Prof. inter.", "Cadres","Ensemble"))

# On extrait les moyennes
evsiH_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Hommes")),"evsi"])
eviH_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Hommes")),"evi"])
evsiF_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Femmes")),"evsi"])
eviF_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Femmes")),"evi"])
survie_valide60H_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Hommes")),"survie_valide60"])
survie_i60H_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Hommes")),"survie_i60"])
survie_valide60F_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Femmes")),"survie_valide60"])
survie_i60F_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Femmes")),"survie_i60"])
e_retraiteH_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Hommes")),"e_retraite"])
e_retraiteF_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Femmes")),"e_retraite"])
e_retraite_non_limiteH_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Hommes")),"e_retraite_non_limite"])
e_retraite_non_limiteF_moy <- as.numeric(resultats[which((resultats$PCS == "Ensemble") & (resultats$Sexe == "Femmes")),"e_retraite_non_limite"])

# On ne garde que les 4 PCS de salariés
#resultats <- filter(resultats, PCS %in% c("Cadres","Prof. inter.","Employés","Ouvriers"))

# 1 variable par ligne
resultats_melt <- resultats %>%
  filter(PCS %in% c("Cadres","Prof. inter.","Employés","Ouvriers")) %>%
  melt(id.vars=c("PCS","Sexe"),measure.vars=c("evi","evsi","survie_i60","survie_valide60","e_retraite_limite","e_retraite_non_limite"))



# Plot EV, EVSI
# Hommes
evH <- filter(resultats_melt, Sexe == "Hommes", variable %in% c("evi","evsi"))
graphe_evH <- ggplot(evH, aes(fill=variable,y=value, x=reorder(PCS,value))) + 
  geom_bar(position="stack", stat="identity",show.legend="false")+
  scale_fill_brewer(palette="YlOrBr")+
  scale_y_continuous(breaks = seq(30, 60, by = 5), limits=c(0,59))+
  geom_hline(yintercept=evsiH_moy, linetype="dashed", color = "orange")+
  annotate("text", x=0.9,y=evsiH_moy+1, label="Moyenne",color="orange",size=3.5)+
  geom_hline(yintercept=evsiH_moy+eviH_moy, linetype="dashed", color = "grey")+
  annotate("text", x=0.9,y=evsiH_moy+eviH_moy+1, label="Moyenne",color="grey",size=3.5)+
  ggtitle("Hommes")+
  xlab("")+ylab("")

# Femmes
evF <- filter(resultats_melt, Sexe == "Femmes", variable %in% c("evi","evsi"))
graphe_evF <- ggplot(evF, aes(fill=variable,y=value, x=reorder(PCS,value))) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(labels = c("avec incapacité","sans incapacité"), palette="YlOrBr")+
  scale_y_continuous(breaks = seq(30, 60, by = 5), limits=c(0,59))+
  guides(fill = guide_legend(title = "Espérance de vie"))+
  geom_hline(yintercept=evsiF_moy, linetype="dashed", color = "orange")+
  geom_hline(yintercept=evsiF_moy+eviF_moy, linetype="dashed", color = "grey")+
  ggtitle("Femmes")+
  xlab("")+ylab("")

# Combinaison des 2
grid.arrange(graphe_evH, graphe_evF, ncol = 2, widths=c(1,1.5))

# Plot probas de survie
# Hommes
survieH <- filter(resultats_melt, Sexe == "Hommes", variable %in% c("survie_valide60","survie_i60"))
graphe_survieH <- ggplot(survieH, aes(fill=variable,y=value, x=reorder(PCS,value))) + 
  geom_bar(position="stack", stat="identity",show.legend="false")+
  scale_fill_brewer(palette="YlOrBr")+
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.1), limits=c(0,1))+
  geom_hline(yintercept=survie_valide60H_moy, linetype="dashed", color = "orange")+
  annotate("text", x=0.9,y=survie_valide60H_moy+0.017, label="Moyenne",color="orange",size=3.5)+
  geom_hline(yintercept=survie_valide60H_moy+survie_i60H_moy, linetype="dashed", color = "grey")+
  annotate("text", x=0.9,y=survie_valide60H_moy+survie_i60H_moy+0.017, label="Moyenne",color="grey",size=3.5)+
  ggtitle("Hommes")+
  xlab("")+ylab("")

# Femmes
survieF <- filter(resultats_melt, Sexe == "Femmes", variable %in% c("survie_valide60","survie_i60"))
graphe_survieF <- ggplot(survieF, aes(fill=variable,y=value, x=reorder(PCS,value))) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="YlOrBr")+
  scale_fill_brewer(labels = c("avec incapacité","sans incapacité"), palette="YlOrBr")+
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.1), limits=c(0,1))+
  guides(fill = guide_legend(title = "Probabilité de survie \n à 60 ans"))+
  geom_hline(yintercept=survie_valide60F_moy, linetype="dashed", color = "orange")+
  geom_hline(yintercept=survie_valide60F_moy+survie_i60F_moy, linetype="dashed", color = "grey")+
  ggtitle("Femmes")+
  xlab("")+ylab("")

# Combinaison des 2
grid.arrange(graphe_survieH, graphe_survieF, ncol = 2, widths=c(1,1.5))

# Plot durées de retraite
#Hommes
retraiteH <- filter(resultats_melt, Sexe == "Hommes", variable %in% c("e_retraite_limite","e_retraite_non_limite"))
graphe_retraiteH <- ggplot(retraiteH, aes(fill=variable,y=value, x=reorder(PCS,value))) + 
  geom_bar(position="stack", stat="identity",show.legend="false")+
  scale_fill_brewer(palette="YlOrBr")+
  scale_y_continuous(breaks = seq(10, 30, by = 5), limits=c(0,27))+
  geom_hline(yintercept=e_retraite_non_limiteH_moy, linetype="dashed", color = "orange")+
  annotate("text", x=1,y=e_retraite_non_limiteH_moy+1, label="Moyenne",color="orange",size=3.5)+
  geom_hline(yintercept=e_retraiteH_moy, linetype="dashed", color = "grey")+
  annotate("text", x=1,y=e_retraiteH_moy+1, label="Moyenne",color="grey",size=3.5)+
  ggtitle("Hommes")+
  xlab("")+ylab("")


#Femmes
retraiteF <- filter(resultats_melt, Sexe == "Femmes", variable %in% c("e_retraite_limite","e_retraite_non_limite"))
graphe_retraiteF <- ggplot(retraiteF, aes(fill=variable,y=value,x=PCS)) + 
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(breaks = seq(10, 30, by = 5), limits=c(0,27))+
  geom_hline(yintercept=e_retraite_non_limiteF_moy, linetype="dashed", color = "orange")+
  annotate("text", x=1,y=e_retraite_non_limiteF_moy+1, label="Moyenne",color="orange",size=3.5)+
  geom_hline(yintercept=e_retraiteF_moy, linetype="dashed", color = "grey")+
  annotate("text", x=1,y=e_retraiteF_moy+1, label="Moyenne",color="grey",size=3.5)+
  scale_fill_brewer(labels = c("avec incapacité","sans incapacité"), palette="YlOrBr")+
  guides(fill = guide_legend(title = "Espérance \n de durée \n de retraite"))+
  ggtitle("Femmes")+
  xlab("")+ylab("")

# Combinaison des 2
grid.arrange(graphe_retraiteH, graphe_retraiteF, ncol = 2, widths=c(1,1.5))