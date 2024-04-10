library(data.table)
library(tidyverse)
library(fst)

# calcul d'intervalle de confiance
errorprop <- function(x, w = rep(1, length(x))) c(prop.test(sum(x*(w!=0), na.rm=T), sum(!is.na(x)&(w!=0)))$conf.int %*% (0:1-1/2))
ymin <- function(x, w = rep(1, length(x))) wtd.mean(x, w) - errorprop(x, w)
ymax <- function(x, w = rep(1, length(x))) wtd.mean(x, w) + errorprop(x, w) 
confinterval <- function(x, w = rep(1, length(x))) {
  estim = wtd.mean(x, w)
  error = errorprop(x, w)
  list(ymin = estim-error, ymax = estim+error)
}


# Calcul des limitations par caractéristiques croisées
# 
limitations_par <- function(matable, ..., censure=TRUE) {
  tableau <- matable %>% 
    filter(AGE >= 30 & !is.na(PCS)) %>% 
    group_by(...) %>% 
    summarise(
      proportion = weighted.mean(limite, PB040, na.rm=TRUE),
      marge_erreur_95pct = errorprop(limite, PB040),
      n = n(),
      age_median = Hmisc::wtd.quantile(AGE, PB040, 0.5)
    )
  if (censure) {
    tableau %>% 
      mutate(proportion = ifelse(n<=10, NA_real_, proportion)) 
  } else tableau
}

# Calcul des limitations par caractéristiques croisées
# 
limitations_et_retraite_par <- function(matable, ..., censure=TRUE) {
  tableau <- matable %>% 
    filter(age >= 30 & !is.na(PCS)) %>% 
    group_by(...) %>% 
    summarise(
      proportion = weighted.mean(limite, PB040, na.rm=TRUE),
      marge_erreur_95pct = errorprop(limite, PB040),
      n = n(),
      age_median = Hmisc::wtd.quantile(age, PB040, 0.5)
    )
  if (censure) {
    tableau %>% 
      mutate(proportion = ifelse(n<=10, NA_real_, proportion)) 
  } else tableau
}

# Calcul des prévalences de limitations et retraite par âge
# 
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

repartition_retraites_last_point <- function(donnees, age_cutoff, ...) {
  donnees %>% 
    filter(AGE > age_cutoff) %>% 
    group_by(...) %>% 
    summarise(
      AGE = round(Hmisc::wtd.quantile(AGE, PB040, 0.5)),
      non_retraite_non_limite = weighted.mean(!retraite & !limite, PB040),
      retraite_non_limite = weighted.mean(retraite & !limite, PB040),
      non_retraite_limite = weighted.mean(!retraite & limite, PB040),
      retraite_limite = weighted.mean(retraite & limite, PB040),
    )
}