# Par âge, sexe, année

texte <- c(
  "# Proportion de personnes déclarant des limitation dans les activités courantes ",
  "# à cause d'un problème de santé depuis au moins 6 mois, par année, sexe et âge quinquennal.",
  "# Champ : adultes de 30 ans ou plus, France de 2008 à 2019.",
  "# Source : enquêtes SRCV 2008 à 2019.",
  "# Note : les croisements comportant 10 observations ou moins sont censurés.",
  "#",
  "# Précisions sur les variables :",
  "# - marge_erreur_95pct : estimation de la marge d'erreur à 95%.",
  "# La valeur de la proportion est donc connue 'plus ou moins' la valeur de marge_erreur_95pct.",
  "#"
)

cat(texte, sep="\n", file=c("sorties/limitations_par_age_qqual_sexe_annee.csv"))

adultes_SL %>% 
  filter(AGE >= 30) %>% 
  group_by(Année = AENQ, Sexe, age_qqual) %>% 
  summarise(
    proportion = weighted.mean(limite, PB040, na.rm=TRUE),
    marge_erreur_95pct = errorprop(limite, PB040),
    n = n()
  ) %>% 
  mutate(proportion = ifelse(n<=10, NA_real_, proportion)) %>% 
  select(Année, Sexe, age_qqual,proportion,marge_erreur_95pct) %>% 
  write_csv2("sorties/limitations_par_age_qqual_sexe_annee.csv", append=TRUE, col_names=TRUE)

