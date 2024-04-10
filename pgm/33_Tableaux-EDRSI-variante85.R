adultes_filtres <- adultes %>% 
  filter(!is.na(retraite) & !is.na(limite))

limitretraites <- bind_rows(
  30:85 %>% 
    map_dfr(~repartition_retraites(adultes_filtres, age=., Sexe)) %>%  
    mutate(PCS = "Ensemble"),
)

limitretraites_lastpoint <-bind_rows(
  repartition_retraites_last_point(adultes_filtres, age_cutoff=85, Sexe) %>%  
    mutate(PCS = "Ensemble")
)

ages_supplementaires <- 
  limitretraites %>% 
  filter(AGE==30) %>%
  slice(rep(1:n(), 15)) %>% 
  arrange(PCS, Sexe) %>% 
  select(PCS, Sexe) %>% 
  mutate(AGE = rep(86:100, n()/15)) %>% 
  left_join(limitretraites_lastpoint)

limitretraites_impute <- limitretraites %>% 
  bind_rows(ages_supplementaires) %>% 
  arrange(Sexe, PCS, AGE) %>% 
  group_by(PCS, Sexe) %>% 
  mutate(across(
    c(non_retraite_non_limite, non_retraite_limite, retraite_non_limite, retraite_limite),
    zoo::na.approx, rule=2
  ))

limitretraites_impute %>% 
  ggplot(aes(AGE, retraite_limite, color=Sexe)) +
  geom_line() +
  facet_wrap(~PCS) 

texte_limitretraites <- 
  "# Répartition des personnes en retraite et déclarant des limitation dans les activités courantes
# à cause d'un problème de santé depuis au moins 6 mois, par sexe, PCS et âge.
# Champ : adultes de 30 ans ou plus, France métropolitaine de 2016 à 2018.
# Source : enquêtes SRCV 2016 à 2018.
# 
# Notes méthodologiques :
# - PCS en 8 postes, actuelle ou passée (pour les retraités)
# - Pour des raisons de nombres d'observation, hommes et femmes ne sont pas séparés
# pour les PCS Agriculteurs et Artisans commerçants & chefs d'entreprise
# - Les taux sont calculés en moyenne sur la tranche d'âge quinquennale centrée en l'âge en question
# de 30 à 85 ans. Les taux de la tranche d'âge 'Plus de 85 ans' sont attribués à l'âge médian.
# Au-delà, les taux sont constants. Entre 85 et l'âge médian de la tranche supérieure, interpolation linéaire."

cat(texte_limitretraites, file=c("sorties/limitations_retraite_var85.csv"), sep="\n")

write_csv2(limitretraites_impute, "sorties/limitations_retraite_var85.csv", append=TRUE, col_names=TRUE)
