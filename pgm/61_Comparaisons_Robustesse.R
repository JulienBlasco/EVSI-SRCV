# Comparaison de notre EVSI avec celle publiée par la DREES
evsi_SL_DREES <- read_delim("data/EVSI_par_age_annee_DREES.csv", 
                                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                       trim_ws = TRUE) %>%
                      rename(
                        AENQ = `Année`,
                        AGE = `Âge`,
                        evsi = `EVSI`,
                        ev = `EV`
                      ) %>%
                      mutate(
                        Sexe = recode(Sexe,
                        "homme" = "Hommes",
                        "femme" = "Femmes"),
                        source = "DREES"
                      ) %>%
                      filter(AGE == 30, AENQ %in% c(2008:2019)) %>%
                      select(AENQ, Sexe, ev, evsi, source)

evsi_SL_auteurs <- esperances_SL %>%
  select(AENQ, Sexe, ev, evsi) %>%
  mutate(source = "Auteurs")

evsi_comparaison <- rbind(
  evsi_SL_auteurs,
  evsi_SL_DREES
) %>%
  mutate(
    AENQ = as.numeric(AENQ)
  ) %>%
  pivot_longer(cols = c("ev","evsi"), names_to = "variable", values_to = "valeur")


(ggplot()+
  geom_line(data = evsi_comparaison, aes(x = AENQ, y=valeur, color=source, group=source))+
  facet_grid(variable ~ Sexe, scales = "free_y")+
  labs(
    title="Espérance de vie avec et sans invalidité à 30 ans selon la source",
    x = "Année",
    y = "",
    color = "Source"
  )+
  scale_x_continuous(breaks=seq(2008,2019,by=2))+
  scale_color_brewer(palette="Paired",direction=-1)
  )%>%
  ggsave("graphes/evsi_Auteurs_DREES.png",plot=., width=largeur, height=hauteur, unit="cm")



## ROBUSTESSE : VARIANTE 75 VS 85
limitations_retraite_survie_var75_var85 <- limitations_retraite_survie %>%
  filter(PCS == "Ensemble") %>%
  mutate(var = "var75") %>%
  bind_rows(
    limitations_retraite_survie_var85 %>% mutate (var = "var85")
  )

(ggplot()+
    geom_line(data = filter(limitations_retraite_survie_var75_var85, AGE >= 65), aes(x = AGE, y = limite, color = var, group = var))+
    facet_wrap(~Sexe)+
    labs(
      x = "Âge",
      y = "Proportion de personnes avec incapacité",
      color = ""
    )+
    scale_color_brewer(palette="Reds",
                       labels = c("Seuil à 75 ans", "Seuil à 85 ans"))+
    theme_minimal()) %>%
  ggsave("graphes/limitations_var75_var85.png",plot=., width=largeur, height=hauteur, unit="cm")