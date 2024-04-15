# PARAMÈTRES
pcs_principales <- c("3 - Cadres", "4 - Professions intermédiaires", "5 - Employés", "6 - Ouvriers")
pcs_principales_labels <- c("Cadres","Prof. inter.","Employés","Ouvriers")
largeur <- 18
hauteur <- largeur*0.57

# ESPERANCE DE VIE PAR PCS
ev_long <- esperances %>%
  mutate(
    ev_limite_forte = ev - evsif,
    ev_limite_moderee = evsif - evsi
  ) %>%
  select(PCS, Sexe, evsi,ev_limite_forte,ev_limite_moderee)%>%
  filter(PCS %in% pcs_principales)%>%
  pivot_longer(., cols=-c(PCS, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")

ev_moy_long <- esperances %>%
  filter(PCS == "Ensemble")%>%
  select(Sexe, ev, evsi, evsif) %>%
  pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")

(ggplot() + 
  geom_bar(data = ev_long, aes(x=PCS,y=valeur_esperance, fill=factor(type_esperance, levels=c("ev_limite_forte","ev_limite_moderee","evsi"))),
           position="stack", stat="identity")+
  labs(x="PCS",y="Années",fill="Espérance de vie \n à 30 ans")+
  scale_x_discrete(labels=pcs_principales_labels)+
  scale_fill_brewer(breaks=c("evsi","ev_limite_moderee","ev_limite_forte"),
                    labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                    palette="YlOrBr", direction=-1)+
  geom_hline(data = ev_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed", show.legend=FALSE)+
  facet_wrap(~Sexe)+
  scale_y_continuous(breaks = seq(30, 60, by = 5), limits=c(0,59)))%>%
  ggsave("graphes/EV_par_PCS.png", plot=., width=largeur, height=hauteur, unit="cm")
  
## ESPERANCE DE RETRAITE PAR PCS
er_long <- esperances %>%
  mutate(
    er_limite_forte = er - ersif,
    er_limite_moderee = ersif - ersi
  ) %>%
  select(PCS, Sexe, ersi,er_limite_forte,er_limite_moderee)%>%
  filter(PCS %in% pcs_principales)%>%
  pivot_longer(., cols=-c(PCS, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")

er_moy_long <- esperances %>%
  filter(PCS == "Ensemble")%>%
  select(Sexe, er, ersi, ersif) %>%
  pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")

(ggplot() + 
    geom_bar(data = er_long, aes(x=PCS,y=valeur_esperance, fill=factor(type_esperance, levels=c("er_limite_forte","er_limite_moderee","ersi"))),
             position="stack", stat="identity")+
    labs(x="PCS",y="Années",fill="Espérance de retraite")+
    scale_x_discrete(labels=pcs_principales_labels)+
    scale_fill_brewer(breaks=c("ersi","er_limite_moderee","er_limite_forte"),
                      labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                      palette="YlOrBr", direction=-1)+
    geom_hline(data = er_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
    facet_wrap(~Sexe)+
    scale_y_continuous(breaks = seq(5, 30, by = 5)))%>%
  ggsave("graphes/ER_par_PCS.png",plot=., width=largeur, height=hauteur, unit="cm")

## PROBABILITES DE SURVIE A 60 ANS, PAR PCS
survie60_long <- esperances %>%
  mutate(
    survie60_limite_forte = survie60 - survie60_non_limite_forte,
    survie60_limite_moderee = survie60_non_limite_forte - survie60_non_limite
  ) %>%
  select(PCS, Sexe, survie60_non_limite,survie60_limite_forte,survie60_limite_moderee)%>%
  filter(PCS %in% pcs_principales)%>%
  pivot_longer(., cols=-c(PCS, Sexe), names_to = "type_esperance", values_to = "valeur_esperance")

survie60_moy_long <- esperances %>%
  filter(PCS == "Ensemble")%>%
  select(Sexe, survie60, survie60_non_limite, survie60_non_limite_forte) %>%
  pivot_longer(., cols=-c(Sexe), names_to ="type_esperance", values_to = "valeur_esperance")

(ggplot() + 
    geom_bar(data = survie60_long, aes(x=PCS,y=valeur_esperance, fill=factor(type_esperance, levels=c("survie60_limite_forte","survie60_limite_moderee","survie60_non_limite"))),
             position="stack", stat="identity")+
    labs(x="PCS",y="Proportion",fill="Survie à 60 ans")+
    scale_x_discrete(labels=pcs_principales_labels)+
    scale_fill_brewer(breaks=c("survie60_non_limite","survie60_limite_moderee","survie60_limite_forte"),
                      labels=c("sans incapacité","avec une incapacité \n modérée","avec une incapacité \n forte"),
                      palette="YlOrBr", direction=-1)+
    geom_hline(data = survie60_moy_long, aes(yintercept=valeur_esperance), color="grey",linetype="dashed")+
    facet_wrap(~Sexe)+
    scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)))%>%
  ggsave("graphes/survie60_par_PCS.png",plot=., width=largeur, height=hauteur, unit="cm")

## EVSI, DECOMPOSITION DES ECARTS ENTRE PCS
diff_evsi_long <- esperances %>%
  select(PCS, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu) %>%
  filter(PCS %in% pcs_principales) %>%
  pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")

(ggplot()+
    geom_bar(data = diff_evsi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_evsi_residu", "diff_evsi_survie","diff_evsi_sante"))),
             position="stack", stat="identity")+
    labs(x="PCS", y="Années",fill="Espérance de vie \n sans incapacité, \n écart avec les cadres")+
    scale_x_discrete(labels=pcs_principales_labels)+
    scale_fill_brewer(breaks=c("diff_evsi_sante","diff_evsi_survie","diff_evsi_residu"),
                      labels=c("expliqué par la santé","expliqué par la mortalité","Résidu"),
                      palette="YlOrBr", direction=-1)+
    geom_point(data = filter(esperances, PCS %in% pcs_principales), aes(x = PCS, y = diff_evsi), color="black", size=2)+
    facet_wrap(~Sexe)
) %>%
  ggsave("graphes/diff_EVSI_par_PCS.png",plot=., width=largeur, height=hauteur, unit="cm")

## ESPERANCE DE RETRAITE, DECOMPOSITION DES ECARTS ENTRE PCS
diff_er_long <- esperances %>%
  select(PCS, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu) %>%
  filter(PCS %in% pcs_principales) %>%
  pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")

(ggplot()+
    geom_bar(data = diff_er_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_er_residu", "diff_er_survie","diff_er_retraite"))),
             position="stack", stat="identity")+
    labs(x="PCS", y="Années",fill="Espérance de retraite, \n écart avec les cadres...")+
    scale_x_discrete(labels=pcs_principales_labels)+
    scale_fill_brewer(breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                      labels=c("expliqué par la mortalité","expliqué par la retraite","Résidu"),
                      palette="YlOrBr", direction=-1)+
    geom_point(data = filter(esperances, PCS %in% pcs_principales), aes(x = PCS, y = diff_er), color="black", size=2)+
    facet_wrap(~Sexe)
) %>%
  ggsave("graphes/diff_ER_par_PCS.png",plot=., width=largeur, height=hauteur, unit="cm")

## ERSI, DECOMPOSITION DES ECARTS ENTRE PCS
diff_ersi_long <- esperances %>%
  select(PCS, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu) %>%
  filter(PCS %in% pcs_principales) %>%
  pivot_longer(., cols=-c(PCS, Sexe), names_to = "composant", values_to = "valeur")

(ggplot()+
    geom_bar(data = diff_ersi_long, aes(x = PCS, y=valeur, fill=factor(composant, levels=c("diff_ersi_residu", "diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))),
             position="stack", stat="identity")+
    labs(x="PCS", y="Années",fill="Espérance de retraite \n sans invalidité, \n écart avec les cadres")+
    scale_x_discrete(labels=pcs_principales_labels)+
    scale_fill_brewer(breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                      labels=c("expliqué par la mortalité","expliqué par la santé","expliqué par la retraite","Résidu"),
                      palette="YlOrBr", direction=-1)+
    geom_point(data = filter(esperances, PCS %in% pcs_principales), aes(x = PCS, y = diff_ersi), color="black", size=2)+
    facet_wrap(~Sexe)
) %>%
  ggsave("graphes/diff_ERSI_par_PCS.png",plot=., width=largeur, height=hauteur, unit="cm")

## ECARTS ENTRE SEXES, DECOMPOSITION
ev_hommes <- esperances_sexe %>% filter(Sexe == "Hommes") %>% select(ev) %>% pull()

diff_sexe_long <- esperances_sexe %>%
  filter(Sexe == "Femmes") %>%
  mutate(diff_ev = ev - ev_hommes) %>% mutate(diff_ev_survie = diff_ev) %>%
  select(matches("^diff"))%>%
  pivot_longer(.,everything(), names_to = "variable", values_to = "valeur") %>%
  mutate(
    type_esperance = sapply(strsplit(variable, "_"), "[", 2),
    composant = sapply(strsplit(variable, "_"), "[", 3)
  )

(ggplot()+
    geom_bar(data = filter(diff_sexe_long, !is.na(composant)), aes(x = factor(type_esperance, levels=c("ev","er","evsi","ersi","evsif","ersif")), y=valeur, fill=factor(composant, levels=c("residu", "survie","sante","retraite"))),
             position="stack", stat="identity")+
    labs(x="", y="Années",fill="Écart des femmes \n avec les hommes...")+
   scale_x_discrete(labels=c("Espérance de vie","Espérance de retraite","Espérance de vie \n sans invalidité","Espérance de retraite \n sans invalidité", "Espérance de vie \n sans invalidité forte", "Espérance de retraite \n sans invalidité forte"))+
    scale_y_continuous(breaks = seq(-2,6, by=1))+
  scale_fill_brewer(breaks=c("survie","sante","retraite","residu"),
                      labels=c("expliqué par la mortalité","expliqué par la santé","expliqué par la retraite","Résidu"),
                      palette="YlOrBr", direction=-1)+
    theme(axis.text.x = element_text(angle = 30, hjust = 1))+
    geom_point(data = filter(diff_sexe_long, is.na(composant)), aes(x = type_esperance, y = valeur), color="black", size=2)
) %>%
  ggsave("graphes/diff_Sexe.png",plot=., width=largeur, height=hauteur, unit="cm")


## ESPÉRANCE DE VIE, SÉRIE TEMPORELLE
ev_SL_long <- esperances_SL %>% select(AENQ, Sexe, ev, evsif, evsi)%>%
  rename(evsigenerale = evsi)%>%
  pivot_longer(., cols = -c(Sexe, AENQ), names_to = "type_esperance", values_to = "valeur_esperance")

# Plot the data
(ggplot(ev_SL_long, aes(x = AENQ, y = valeur_esperance, color = type_esperance)) +
  geom_line() +
  facet_wrap(~Sexe) +
  labs(x = "Année",
       y = "",
       color = "Espérance de vie à 30 ans") +
  scale_color_brewer(palette="Reds",
                     labels = c("totale",
                                "sans incapacité forte",
                                "sans incapacité")) +
  scale_x_continuous(breaks = seq(2008,2019, by=4))+
  scale_y_continuous(breaks = seq(35,55,by=2.5))+
  theme_minimal())%>%
  ggsave("graphes/EV_SL.png",plot=., width=largeur, height=hauteur, unit="cm")

## ESPERANCE DE RETRAITE, SERIE TEMPORELLE
er_SL_long <- esperances_SL %>% select(AENQ, Sexe, er, ersif, ersi)%>%
  rename(ersigenerale = ersi)%>%
  pivot_longer(., cols = -c(Sexe, AENQ), names_to = "type_esperance", values_to = "valeur_esperance")

# Plot the data
(ggplot(er_SL_long, aes(x = AENQ, y = valeur_esperance, color = type_esperance)) +
    geom_line() +
    facet_wrap(~Sexe) +
    labs(x = "Année",
         y = "",
         color = "Espérance de retraite") +
    scale_color_brewer(palette="Reds",
                       labels = c("totale",
                                  "sans incapacité forte",
                                  "sans incapacité")) +
    scale_x_continuous(breaks = seq(2008,2019, by=4))+
    scale_y_continuous(breaks=seq(10,25,by=2))+
    theme_minimal())%>%
  ggsave("graphes/ER_SL.png",plot=., width=largeur, height=hauteur, unit="cm")

## ESPÉRANCE DE VIE SANS INCAPACITÉ, SÉRIE TEMPORELLE AVEC DÉCOMPOSITION
diff_evsi_SL_long <- esperances_SL %>% select(AENQ, Sexe, diff_evsi_survie, diff_evsi_sante, diff_evsi_residu)%>%
  pivot_longer(., cols=-c(Sexe, AENQ), names_to = "composant", values_to="valeur")

(ggplot() +
  geom_col(data = diff_evsi_SL_long, aes(x = AENQ, y = valeur, fill = factor(composant, levels=c("diff_evsi_residu","diff_evsi_survie","diff_evsi_sante"))), position = "stack") +  # Barres empilées
  facet_wrap(~Sexe) +
  geom_line(data = esperances_SL, aes(x = AENQ, y = diff_evsi), color = "black", alpha=0.8) +  # Point noir représentant la somme
  labs(x = "Année",
       y = "",
       fill = "Écart d'EVSI \n par rapport à 2008") +
  scale_fill_brewer(palette="Reds",
                    breaks=c("diff_evsi_survie","diff_evsi_sante","diff_evsi_residu"),
                     labels = c("expliqué par la mortalite", "expliqué par la santé", "Résidu"))+
  scale_x_continuous(breaks=seq(2008,2019,by=4))+
  theme_minimal())%>%
  ggsave("graphes/diff_EVSI_SL.png",plot=., width=largeur, height=hauteur, unit="cm")

## ESPÉRANCE DE RETRAITE, SÉRIE TEMPORELLE AVEC DÉCOMPOSITION
diff_er_SL_long <- esperances_SL %>% select(AENQ, Sexe, diff_er_survie, diff_er_retraite, diff_er_residu)%>%
  pivot_longer(., cols=-c(Sexe, AENQ), names_to = "composant", values_to="valeur")

(ggplot() +
    geom_col(data = diff_er_SL_long, aes(x = AENQ, y = valeur, fill = factor(composant, levels=c("diff_er_residu","diff_er_survie","diff_er_retraite"))), position = "stack") +  # Barres empilées
    facet_wrap(~Sexe) +
    geom_line(data = esperances_SL, aes(x = AENQ, y = diff_er), color = "black", alpha=0.8) +  # Point noir représentant la somme
    labs(x = "Année",
         y = "",
         fill = "Espérance de retraite, \n écart par rapport à 2008") +
    scale_fill_brewer(palette="Reds",
                      breaks=c("diff_er_survie","diff_er_retraite","diff_er_residu"),
                      labels = c("expliqué par la mortalite", "expliqué par la retraite", "Résidu"))+
    scale_x_continuous(breaks=seq(2008,2019,by=4))+
    theme_minimal())%>%
  ggsave("graphes/diff_ER_SL.png",plot=., width=largeur, height=hauteur, unit="cm")

## ESPÉRANCE DE RETRAITE SANS INCAPACITÉ, SÉRIE TEMPORELLE AVEC DÉCOMPOSITION
diff_ersi_SL_long <- esperances_SL %>% select(AENQ, Sexe, diff_ersi_survie, diff_ersi_sante, diff_ersi_retraite, diff_ersi_residu)%>%
  pivot_longer(., cols=-c(Sexe, AENQ), names_to = "composant", values_to="valeur")

(ggplot() +
    geom_col(data = diff_ersi_SL_long, aes(x = AENQ, y = valeur, fill = factor(composant, levels=c("diff_ersi_residu","diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite"))), position = "stack") +  # Barres empilées
    facet_wrap(~Sexe) +
    geom_line(data = esperances_SL, aes(x = AENQ, y = diff_ersi), color = "black", alpha=0.8) +  # ligne noire représentant la somme
    labs(x = "Année",
         y = "",
         fill = "Espérance de retraite sans incapacité, \n écart par rapport à 2008") +
    scale_fill_brewer(palette="Reds",
                      breaks=c("diff_ersi_survie","diff_ersi_sante","diff_ersi_retraite","diff_ersi_residu"),
                      labels = c("expliqué par la mortalité","expliqué par la santé", "expliqué par la retraite", "Résidu"))+
    scale_x_continuous(breaks=seq(2008,2019,by=4))+
    theme_minimal())%>%
  ggsave("graphes/diff_ERSI_SL.png",plot=., width=largeur, height=hauteur, unit="cm")

## ROBUSTESSE : VARIANTE 75 VS 85
limitations_retraite_survie_var75_var85 <- limitations_retraite_survie %>%
  filter(PCS == "Ensemble") %>%
  mutate(var = "var75") %>%
  bind_rows(
     limitations_retraite_survie_var85 %>% mutate (var = "var85")
  )

(ggplot()+
  geom_line(data = filter(limitations_retraite_survie_var75_var85, AGE >= 65), aes(x = AGE, y = limite, color = var, group = var))+
  facet_wrap(~Sexe)+
  labs(
    x = "Âge",
    y = "Proportion de personnes avec incapacité",
    color = ""
  )+
  scale_color_brewer(palette="Reds",
                    labels = c("Seuil à 75 ans", "Seuil à 85 ans"))+
  theme_minimal()) %>%
  ggsave("graphes/limitations_var75_var85.png",plot=., width=largeur, height=hauteur, unit="cm")
