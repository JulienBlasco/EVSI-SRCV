# Limitations par âge

adultes %>% 
  group_by(age_cat) %>% 
  count(limitations, wt=PB040) %>% 
  drop_na() %>% 
  mutate(n=100*n/sum(n)) %>% 
  ggplot(aes(x=age_cat, y=n, fill=limitations)) +
  geom_col() +
  scale_fill_brewer(palette="Reds", direction=-1)

adultes %>% 
  group_by(age_qqual) %>% 
  count(limitations, wt=PB040) %>% 
  drop_na() %>% 
  mutate(n=100*n/sum(n)) %>% 
  ggplot(aes(x=age_qqual, y=n, fill=limitations)) +
  geom_col() +
  scale_fill_brewer(palette="Reds", direction=-1)

# Limitations par PCS et âge

adultes %>% 
  filter(CS != 8 & AGE >= 30) %>% 
  group_by(PCS, age_qqual) %>% 
  count(limitations, wt=PB040) %>% 
  drop_na() %>% 
  mutate(n=100*n/sum(n)) %>% 
  ggplot(aes(x=age_qqual, y=n, fill=limitations)) +
  geom_col() +
  scale_fill_brewer(
    palette="Reds", direction=-1, 
    name = "Subissez-vous une limitation dans les activités courantes 
    à cause d'un problème de santé depuis au moins 6 mois ?") +
  facet_wrap(~PCS) +
  labs(caption = "SRCV 2018, adultes de 30 ans ou plus.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

# Avec barres d'erreur et prop

adultes %>% 
  drop_na(PCS, limite) %>% 
  group_by(PCS, age_qqual) %>% 
  summarise(
    limite = weighted.mean(limite, PB040),
    err = errorprop(limite, PB040)
  ) %>% 
  ggplot(aes(x=age_qqual, y=limite)) +
  geom_col() +
  geom_errorbar(aes(ymin = limite-err, ymax = limite+err)) +
  facet_wrap(~PCS)
