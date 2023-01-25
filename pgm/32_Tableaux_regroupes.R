limitations <- 
  bind_rows(
    adultes %>% 
      filter(CS %in% 1:2) %>% 
      limitations_par(PCS, age_qqual),
    adultes %>% 
      filter(CS %in% 3:7) %>% 
      limitations_par(Sexe, PCS, age_qqual),
    adultes %>% 
      limitations_par(Sexe, age_qqual) %>% 
      mutate(PCS = "Ensemble")
  )

limitations_imputees <- 
  data.frame(age = 30:100) %>%
  mutate(age_qqual = cut(age, breaks = breaks_qqual_regroupe, labels=labellize(breaks_qqual_regroupe), right=FALSE)) %>% 
  left_join(limitations, by="age_qqual") %>% 
  arrange(Sexe, PCS, age) %>% 
  group_by(Sexe, PCS) %>% 
  mutate(proportion_imputee = zoo::na.approx(
    ifelse(age == age_median, proportion, NA_real_), rule = 2)
  )

limitations_imputees %>% 
  ggplot(aes(x=age, col=Sexe, group=Sexe)) +
  geom_line(aes(y=proportion_imputee)) +
  geom_point(aes(y=proportion), data=~filter(., age == age_median)) +
  facet_wrap(~PCS)

# Proportion de personnes déclarant des limitation dans les activités courantes
# à cause d'un problème de santé depuis au moins 6 mois, par sexe, PCS et âge.
# Champ : adultes de 30 ans ou plus, France métropolitaine 2018.
# Source : enquête SRCV 2018.
# 
# Notes méthodologiques :
# - PCS en 8 postes, actuelle ou passée (pour les retraités),
# - Pour des raisons de nombres d'observation, hommes et femmes ne sont pas séparés
# pour les PCS Agriculteurs et Artisans commerçants & chefs d'entreprise
# - La prévalence des limitations est calculée par tranche d'âge quinquennale,
# puis imputée par interpolation linéaire entre les âges médians des tranches.

texte_regroupe <- 
"# Proportion de personnes déclarant des limitation dans les activités courantes
# à cause d'un problème de santé depuis au moins 6 mois, par sexe, PCS et âge.
# Champ : adultes de 30 ans ou plus, France métropolitaine 2018.
# Source : enquête SRCV 2018.
# 
# Notes méthodologiques :
# - PCS en 8 postes, actuelle ou passée (pour les retraités),
# - Pour des raisons de nombres d'observation, hommes et femmes ne sont pas séparés
# pour les PCS Agriculteurs et Artisans commerçants & chefs d'entreprise
# - La prévalence des limitations est calculée par tranche d'âge quinquennale,
# puis imputée par interpolation linéaire entre les âges médians des tranches."

cat(texte_regroupe, file=c("sorties/limitations_par_age_pcs_sexe_regroupe.csv"), sep="\n")

limitations_imputees %>% 
  mutate(Sexe = fct_explicit_na(Sexe, "Ensemble")) %>% 
  select(PCS, Sexe, age, proportion_imputee) %>% 
  write_csv2("sorties/limitations_par_age_pcs_sexe_regroupe.csv", append=TRUE, col_names=TRUE)
