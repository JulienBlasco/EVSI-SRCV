limitretraites <- fread("sorties/limitations_retraite.csv", dec=",")
limitations <- fread("sorties/limitations_par_age_pcs_sexe_regroupe.csv",
                    dec=",")

limitretraites %>% 
  {bind_rows(., mutate(filter(., AGE>80),AGE=100))} %>% 
  mutate(lim = non_retraite_limite + retraite_limite) %>% 
  ggplot(aes(AGE, lim, col=SEXE)) +
  geom_line() + facet_wrap(~PCS)

limitations %>% 
  ggplot(aes(age, proportion_imputee, col=Sexe)) +
  geom_line() + facet_wrap(~PCS)


limitretraites %>% 
  {bind_rows(., mutate(filter(., AGE>75),AGE=100))} %>% 
  mutate(lim = non_retraite_limite + retraite_limite, méthode="Moyenne mobile") %>% 
  bind_rows(mutate(limitations, AGE=age, lim=proportion_imputee, SEXE=Sexe, méthode="Imputation quinquennale")) %>% 
  ggplot(aes(x=AGE, y=lim, col=méthode)) +
  facet_grid(cols=vars(SEXE), rows=vars(PCS)) +
  geom_line()

# on constate une différence sur les cadres
adultes %>% 
  count(PCS, Sexe, AENQ, AGE>75) %>% 
  View

adultes %>% 
  filter(PCS == "3 - Cadres" & AGE > 75) %>% 
  group_by(AENQ, Sexe) %>% 
  summarise(
    lim = weighted.mean(limite, PB040),
    n = n()
    )

adultes %>% 
  filter(PCS == "3 - Cadres" & AGE > 75) %>% 
  group_by(Sexe) %>% 
  summarise(
    lim = weighted.mean(limite, PB040),
    n = n()
  )

