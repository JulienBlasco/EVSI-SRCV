variables_a_retirer = c("proploca")

individus <- list(
  `2017` = "INDIVIDUS17_DIFF",
  `2018` = "INDIVIDUS18_DIFF",
  `2016` = "individus16_diff"
) %>% 
  map(~haven::read_dta(paste0("data/", ., ".dta"))) %>% 
  map(select, -all_of(variables_a_retirer)) %>% 
  bind_rows(.id="AENQ") %>% 
  rename_with(toupper)

individus %>% 
  filter(AGE >= 16) %>% 
  count(DIM, wt=PB040) %>% 
  mutate(n = 100*n/sum(n))

labellize <- function(x) {
  l <- length(x)
  paste(x[-l], x[-1]-1, sep="-")
}

breaks_cat <- c(16, 25, 50, 55+5*(0:6), Inf)
breaks_qqual <- c(16, 20+5*(0:13), Inf)
breaks_qqual_regroupe <- c(16, 30, 40+5*(0:7), Inf)

adultes <- filter(individus, AGE >= 16) %>% 
  mutate(
    limitations = factor(recode(
    DIM, `1` = "1 - Oui, fortement limité(e)",
    `2` = "2 - Oui, limité(e), mais pas fortement",
    `3` = "3 - Non, pas limité(e) du tout"
  )),
  limite = DIM == 1 | DIM == 2,
  age_cat = cut(AGE, breaks = breaks_cat, labels=labellize(breaks_cat), right=FALSE),
  age_qqual = cut(AGE, breaks = breaks_qqual_regroupe, labels=labellize(breaks_qqual_regroupe), right=FALSE),
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
  Sexe = fct_recode(factor(SEXE), Hommes = "1", Femmes = "2"),
  retraite = SITUA == 5
  )

