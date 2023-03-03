repartition_retraites <- function(donnees, age, ...) {
  donnees %>% 
    filter(AGE >= age-2 & AGE <= age+2) %>% 
    group_by(...) %>% 
    summarise(
      AGE = age,
      non_retraite_non_limite = weighted.mean(!retraite & !limite, PB040),
      retraite_non_limite = weighted.mean(retraite & !limite, PB040),
      non_retraite_limite = weighted.mean(!retraite & limite, PB040),
      retraite_limite = weighted.mean(retraite & limite, PB040),
    )
}

limitretraites <-   bind_rows(
  30:75 %>% 
    map(~repartition_retraites(
      adultes %>% 
        filter(PCS != "" & !is.na(retraite) & !is.na(limite)) %>% 
        filter(CS %in% 1:2), 
      age=., PCS)
    ) %>% 
    bind_rows() %>% 
    mutate(SEXE = "Ensemble"),
  30:75 %>% 
    map(~repartition_retraites(
      adultes %>% 
        filter(PCS != "" & !is.na(retraite) & !is.na(limite)) %>% 
        filter(CS %in% 3:7), 
      age=., SEXE, PCS)
    ) %>% 
    bind_rows(), 
  30:75 %>% 
    map(~repartition_retraites(
      adultes %>% 
        filter(PCS != "" & !is.na(retraite) & !is.na(limite)), 
      age=., SEXE)
    ) %>% 
    bind_rows() %>% 
    mutate(PCS = "Ensemble")
)

limitretraites %>% 
  ggplot(aes(AGE, retraite_limite, color=SEXE)) +
  geom_line() +
  facet_wrap(~PCS) 


texte_limitretraites <- 
  "# Répartition des personnes en retraite et déclarant des limitation dans les activités courantes
# à cause d'un problème de santé depuis au moins 6 mois, par sexe, PCS et âge.
# Champ : adultes de 30 ans ou plus, France métropolitaine 2018.
# Source : enquêtes SRCV 2016 à 2018.
# 
# Notes méthodologiques :
# - PCS en 8 postes, actuelle ou passée (pour les retraités),
# - Pour des raisons de nombres d'observation, hommes et femmes ne sont pas séparés
# pour les PCS Agriculteurs et Artisans commerçants & chefs d'entreprise
# - Les taux sont calculés en moyenne sur la tranche d'âge quinquennale centrée en l'âge en question"

cat(texte_limitretraites, file=c("sorties/limitations_retraite.csv"), sep="\n")

limitretraites %>% 
  mutate(SEXE = ifelse(SEXE == 1, "Hommes", SEXE),
         SEXE = ifelse(SEXE == 2, "Femmes", SEXE)) %>% 
  write_csv2("sorties/limitations_retraite.csv", append=TRUE, col_names=TRUE)
