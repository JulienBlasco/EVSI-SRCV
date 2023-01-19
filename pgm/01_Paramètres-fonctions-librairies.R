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

# chemin des tables
chemin_tables <- Sys.getenv("chemin_tables")
