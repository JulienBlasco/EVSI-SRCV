library(data.table)
library(tidyverse)

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