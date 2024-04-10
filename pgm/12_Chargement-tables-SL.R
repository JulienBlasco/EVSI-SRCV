variables_a_retirer = c("PROPLOCA", "PY200G", "PY021N", "PY021G", "Z01Q0P",
                        "HANDICH", "HANDICG")

variables_a_selectionner <- c("DIM", "AGE", "CS24", "CS_ANTE", "SEXE", "SITUA", "PB040")

individus_SL <- list(
  `2008` = "individus08_diff",
  `2009` = "individus09_diff",
  `2010` = "individus10_diff",
  `2011` = "individus11_diff",
  `2012` = "INDIVIDUS12_DIFFV2",
  `2013` = "INDIVIDUS13_DIFFV2",
  `2014` = "INDIVIDUS14_DIFF",
  `2015` = "INDIVIDUS15_DIFF",
  `2016` = "individus16_diff",
  `2017` = "INDIVIDUS17_DIFF",
  `2018` = "INDIVIDUS18_DIFF",
  `2019` = "INDIVIDUS19_DIFF"
) %>% 
  map(~haven::read_dta(
    paste0("data/", ., ".dta"), 
    col_select = any_of(c(variables_a_selectionner, tolower(variables_a_selectionner)))
    )) %>% 
  map(rename_with, toupper) %>% 
  map(mutate, across(ends_with(c("_F", "_FLAG")), as.character)) %>% 
  map(select, -any_of(variables_a_retirer)) %>% 
  bind_rows(.id="AENQ")

labellize <- function(x) {
  l <- length(x)
  paste(x[-l], x[-1]-1, sep="-")
}

breaks_cat <- c(16, 25, 50, 55+5*(0:6), Inf)
breaks_qqual <- c(16, 20+5*(0:13), Inf)
breaks_qqual_regroupe <- c(16, 30, 40+5*(0:7), Inf)

adultes_SL <- filter(individus_SL, AGE >= 16) %>% 
  mutate(
    limitations = factor(recode(
      DIM, `1` = "1 - Oui, fortement limité(e)",
      `2` = "2 - Oui, limité(e), mais pas fortement",
      `3` = "3 - Non, pas limité(e) du tout"
    )),
    limite = DIM == 1 | DIM == 2,
    age_cat = cut(AGE, breaks = breaks_cat, labels=labellize(breaks_cat), right=FALSE),
    age_qqual = cut(AGE, breaks = breaks_qqual_regroupe, labels=labellize(breaks_qqual_regroupe), right=FALSE),
    age_qqual_nonregroupe = cut(AGE, breaks = breaks_qqual, labels=labellize(breaks_qqual), right=FALSE),
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
      `8` = "8 - Inactifs",
      .default = NA_character_
    )),
    Sexe = fct_recode(factor(SEXE), Hommes = "1", Femmes = "2"),
    retraite = SITUA == 5
  )

write_fst(adultes_SL, "interm/adultes_SL.fst")
