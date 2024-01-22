limitations_retraite <- data.table::fread("sorties/limitations_retraite.csv", dec=",")

limitations_retraite %>% 
  pivot_longer(c(non_retraite_non_limite, non_retraite_limite, retraite_limite, retraite_non_limite)) %>% 
  filter(SEXE == "Hommes") %>% 
  ggplot(aes(AGE, value, fill=name)) +
  geom_area() + facet_wrap(~ PCS) +
  scale_fill_manual(values=c("lightgrey", "grey", "navyblue", "blue"))

limitations_retraite %>% 
  ggplot(aes(AGE, retraite_non_limite, color=SEXE)) +
  geom_line() + facet_wrap(~ PCS)
  

adultes %>% 
  mutate(
    age_qqual_nonregroupe = cut(AGE, breaks = breaks_qqual, labels=labellize(breaks_qqual), right=FALSE)
    ) %>% 
  group_by(PCS, Sexe, age_qqual_nonregroupe) %>% 
  summarise(n=n()) %>% 
  spread(age_qqual_nonregroupe, n) %>%
  View


50:60 %>% 
  map_dfr(~repartition_retraites(filter(adultes_filtres, CS %in% 1:2), age=., PCS)) %>% 
  mutate(SEXE = "Ensemble")
