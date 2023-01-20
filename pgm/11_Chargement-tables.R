individus <- fread(paste0(chemin_tables, "INDIVIDUS18_DIFF.csv"))
#menages <- fread(paste0(chemin_tables, "MENAGES18_DIFF.csv"))

individus %>% 
  filter(age >= 16) %>% 
  count(DIM, wt=PB040) %>% 
  mutate(n = 100*n/sum(n))

labellize <- function(x) {
  l <- length(x)
  paste(x[-l], x[-1]-1, sep="-")
}

breaks_cat <- c(16, 25, 50, 55+5*(0:6), Inf)
breaks_qqual <- c(16, 20+5*(0:13), Inf)
breaks_qqual_regroupe <- c(16, 30, 40+5*(0:7), Inf)

adultes <- filter(individus, age >= 16) %>% 
  mutate(
    limitations = factor(recode(
    DIM, `1` = "1 - Oui, fortement limité(e)",
    `2` = "2 - Oui, limité(e), mais pas fortement",
    `3` = "3 - Non, pas limité(e) du tout"
  )),
  limite = DIM == 1 | DIM == 2,
  age_cat = cut(age, breaks = breaks_cat, labels=labellize(breaks_cat), right=FALSE),
  age_qqual = cut(age, breaks = breaks_qqual_regroupe, labels=labellize(breaks_qqual_regroupe), right=FALSE),
  CS = substr(CS24, 1, 1),
  CS = ifelse(CS == 7, substr(CS_ANTE, 1, 1), CS),
  PCS = factor(recode(
    CS,
    `1` = "1 - Agriculteurs",
    `2` = "2 - Artisans, commerçants et chefs d’entreprise",
    `3` = "3 - Cadres",
    `4` = "4 - Professions intermédiaires",
    `5` = "5 - Employés",
    `6` = "6 - Ouvriers",
    `7` = "7 - Retraités",
    `8` = "8 - Inactifs"
  )),
  Sexe = fct_recode(factor(SEXE), Hommes = "1", Femmes = "2")
  )

