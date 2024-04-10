adultes_SL_filtres <- adultes_SL %>% 
  filter(!is.na(retraite) & !is.na(limite))

limitretraites <- 30:75 %>% 
    map_dfr(~repartition_retraites(adultes_SL_filtres, age=., AENQ, Sexe))

limitretraites_lastpoint <- repartition_retraites_last_point(adultes_SL_filtres, age_cutoff=75, AENQ, Sexe)

ages_supplementaires <- 
  limitretraites %>% 
  select(AENQ, Sexe) %>% 
  unique() %>% 
  slice(rep(1:n(), 25)) %>% 
  mutate(AGE = rep(76:100, n()/25)) %>% 
  left_join(limitretraites_lastpoint)

limitretraites_impute <- limitretraites %>% 
  bind_rows(ages_supplementaires) %>% 
  arrange(AENQ, Sexe, AGE) %>% 
  group_by(AENQ, Sexe) %>% 
  mutate(across(
    c(non_retraite_non_limite, non_retraite_limite, retraite_non_limite, retraite_limite),
    zoo::na.approx, rule=2
  ))

texte_limitretraites <- 
  "# Répartition des personnes en retraite et déclarant des limitation dans les activités courantes
# à cause d'un problème de santé depuis au moins 6 mois, par année, sexe et âge.
# Champ : adultes de 30 ans ou plus, France métropolitaine de 2008 à 2019.
# Source : enquêtes SRCV 2008 à 2019.
# 
# Notes méthodologiques :
# - Les taux sont calculés en moyenne sur la tranche d'âge quinquennale centrée en l'âge en question
# de 30 à 75 ans. Les taux de la tranche d'âge 'Plus de 75 ans' sont attribués à l'âge médian.
# Au-delà, les taux sont constants. Entre 75 et l'âge médian de la tranche supérieure, interpolation linéaire."

cat(texte_limitretraites, file=c("sorties/limitations_retraite_SL.csv"), sep="\n")

write_csv2(limitretraites_impute, "sorties/limitations_retraite_SL.csv", append=TRUE, col_names=TRUE)


