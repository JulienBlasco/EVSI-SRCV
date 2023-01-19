# Par âge qqual -----------------------------------------------------------

adultes %>% 
  group_by(age_qqual) %>% 
  count(limitations, wt=PB040) %>% 
  drop_na() %>% 
  mutate(n=100*n/sum(n)) %>% 
  write.csv2("sorties/limitations_par_age_qqual.csv")

# Par âge, pcs et sexe ----------------------------------------------------

texte <- c(
  "# Proportion de personnes déclarant des limitation dans les activités courantes ",
  "# à cause d'un problème de santé depuis au moins 6 mois, par sexe, PCS et âge quinquennal.",
  "# Champ : adultes de 30 ans ou plus, France 2018.",
  "# Source : enquête SRCV 2018.",
  "# Note : les croisements comportant 10 observations ou moins sont censurés.",
  "#",
  "# Précisions sur les variables :",
  "# - PCS : en 8 postes, actuelle ou passée (pour les retraités)",
  "# - marge_erreur_95pct : estimation de la marge d'erreur à 95%.",
  "# La valeur de la proportion est donc connue 'plus ou moins' la valeur de marge_erreur_95pct.",
  "#"
)

cat(texte, sep="\n", file=c("sorties/limitations_par_age_qqual_pcs_sexe.csv"))

# Sans les ensembles

adultes %>% 
  filter(age >= 30 & !is.na(PCS)) %>% 
  group_by(Sexe, PCS, age_qqual) %>% 
  summarise(
    proportion = weighted.mean(limite, PB040, na.rm=TRUE),
    marge_erreur_95pct = errorprop(limite, PB040),
    n = n()
  ) %>% 
  mutate(proportion = ifelse(n<=10, NA_real_, proportion)) %>% 
  select(Sexe,PCS,age_qqual,proportion,marge_erreur_95pct) %>% 
  write_csv2("sorties/limitations_par_age_qqual_pcs_sexe.csv", append=TRUE, col_names=TRUE)


# Avec les ensembles


cat(texte, sep="\n", file=c("sorties/limitations_par_age_qqual_pcs_sexe_ensemble.csv"))

limitations_par <- function(matable, ...) {
  matable %>% 
    filter(age >= 30 & !is.na(PCS)) %>% 
    group_by(...) %>% 
    summarise(
      proportion = weighted.mean(limite, PB040, na.rm=TRUE),
      marge_erreur_95pct = errorprop(limite, PB040),
      n = n()
    ) %>% 
    mutate(proportion = ifelse(n<=10, NA_real_, proportion)) 
}

list(
  adultes %>% limitations_par(),
  adultes %>% limitations_par(Sexe),
  adultes %>% limitations_par(PCS),
  adultes %>% limitations_par(age_qqual),
  adultes %>% limitations_par(Sexe, PCS),
  adultes %>% limitations_par(Sexe, age_qqual),
  adultes %>% limitations_par(PCS, age_qqual),
  adultes %>% limitations_par(Sexe, PCS, age_qqual)
) %>% 
  bind_rows() %>% 
  mutate(
    across(c(Sexe, PCS), fct_explicit_na, "Ensemble"),
    age_qqual = fct_explicit_na(age_qqual, "Ensemble moins de 30 ans")
  ) %>% 
  select(Sexe, PCS, age_qqual, proportion, marge_erreur_95pct) %>% 
  write_csv2("sorties/limitations_par_age_qqual_pcs_sexe_ensemble.csv", append=TRUE, col_names=TRUE)
