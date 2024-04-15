# On charge les données de limitations*retraite
limitations_retraite2 <- read_delim("sorties/limitations_retraite2.csv", 
                                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),  comment = "#", 
                                    trim_ws = TRUE)
limitations_retraite_forte <- read_delim("sorties/limitations_retraite_forte.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),  comment = "#", trim_ws = TRUE) %>%
                              rename(
                                non_retraite_non_limite_forte = non_retraite_non_limite,
                                non_retraite_limite_forte = non_retraite_limite,
                                retraite_non_limite_forte = retraite_non_limite,
                                retraite_limite_forte = retraite_limite
                              )

limitations_retraite <- merge(limitations_retraite2, limitations_retraite_forte, by=c("PCS","Sexe","AGE"))%>%
  arrange(PCS, Sexe, AGE)

limitations_retraite_SL <- read_delim("sorties/limitations_retraite_SL.csv", 
                                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),  comment = "#", 
                                    trim_ws = TRUE)
limitations_retraite_SL_forte <- read_delim("sorties/limitations_retraite_SL_forte.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),  comment = "#", trim_ws = TRUE) %>%
  rename(
    non_retraite_non_limite_forte = non_retraite_non_limite,
    non_retraite_limite_forte = non_retraite_limite,
    retraite_non_limite_forte = retraite_non_limite,
    retraite_limite_forte = retraite_limite
  )
limitations_retraite_SL <- merge(limitations_retraite_SL, limitations_retraite_SL_forte, by=c("AENQ","Sexe","AGE"))%>%
  arrange(AENQ, Sexe, AGE)


# On charge les données de mortalité par genre et PCS en cross-section
survie_hommes <- read_delim("data/Mortalite-FM-H-2009-2013.csv", 
                            delim = ";", escape_double = FALSE,  trim_ws = TRUE) %>%
                pivot_longer(cols = -age, names_to = "PCS", values_to = "survie") %>%
                mutate(Sexe = "Hommes")
survie_femmes <- read_delim("data/Mortalite-FM-F-2009-2013.csv", 
                            delim = ";", escape_double = FALSE,  trim_ws = TRUE) %>%
                pivot_longer(cols = -age, names_to = "PCS", values_to = "survie") %>%
                mutate(Sexe = "Femmes")

survie <- rbind(survie_hommes, survie_femmes) %>%
  mutate(PCS = recode(PCS,
                      `Agriculteurs` = "1 - Agriculteurs",
                      `Artisans` = "2 - Artisans, commerçants et chefs d’entreprise",
                      `Cadres` = "3 - Cadres",
                      `Prof inter` = "4 - Professions intermédiaires",
                      `Employés` = "5 - Employés",
                      `Ouvriers` = "6 - Ouvriers",
                      `Inactifs` = "8 - Inactifs"
  )) %>%
  rename(AGE = age) %>%
  mutate(survie = survie/100000)

# On charge les données de mortalité par genre en série longue
# Les données INSEE de mortalité par génération viennent d'ici : https://www.insee.fr/fr/statistiques/6543678?sommaire=6543680
# Elles ont été préalablement converties manuellement en .csv
mortalite_SL_femmes <- read_delim("data/MortaliteGenerationF.csv", 
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE) %>%
                  pivot_longer(col=-1) %>%
                  mutate(Sexe = "Femmes")
mortalite_SL_hommes <- read_delim("data/MortaliteGenerationH.csv", 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE) %>%
                  pivot_longer(col=-1) %>%
                  mutate(Sexe = "Hommes")

survie_SL <- rbind(mortalite_SL_femmes, mortalite_SL_hommes) %>%
              rename(AGE = name, mortalite = value) %>%
              mutate(AGE = as.numeric(AGE),
                     AENQ = birth + AGE) %>% # annee = naissance + age
              arrange(Sexe, AENQ, AGE) %>% 
              group_by(AENQ, Sexe) %>%
              mutate(survie = cumprod(1 - lag(mortalite,default=0)/100000)) %>% # On calcule la survie instantanée
              select(-birth, -mortalite) %>%
              filter(AGE >= 30, AGE <= 100, AENQ >= 2008, AENQ <= 2019)

completer_colonnes <- function(df, forte = TRUE){
  df <- df %>%
    mutate(
      non_limite = non_retraite_non_limite + retraite_non_limite,
      limite = 1 - non_limite,
      non_retraite = non_retraite_limite + non_retraite_non_limite,
      retraite = 1 - non_retraite,
      non_limite_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite/retraite),
      
      survie_non_limite = survie*non_limite,
      survie_retraite = survie*retraite,
      survie_retraite_non_limite = survie*retraite_non_limite
    )
  
  if(forte){
    df <- df %>%
      mutate(
        non_limite_forte = non_retraite_non_limite_forte + retraite_non_limite_forte,
        limite_forte = 1 - non_limite_forte,
        non_limite_forte_parmi_retraite = ifelse(retraite == 0, 1, retraite_non_limite_forte/retraite),
        survie_non_limite_forte = survie*non_limite_forte,
        survie_retraite_non_limite_forte = survie*retraite_non_limite_forte
      )
  } else{
    df <- df %>%
      mutate(
        non_limite_forte = NA,
        limite_forte = NA,
        non_limite_forte_parmi_retraite = NA,
        survie_non_limite_forte = NA,
        survie_retraite_non_limite_forte = NA
      )
  }
  return(df)
}

limitations_retraite_survie <- merge(limitations_retraite, survie, by=c("PCS","Sexe","AGE"))%>%
  arrange(PCS, Sexe, AGE)%>%
  completer_colonnes

View(limitations_retraite_survie)

limitations_retraite_survie_SL <- merge(limitations_retraite_SL, survie_SL, by=c("AENQ","Sexe","AGE"))%>%
  arrange(AENQ, Sexe, AGE) %>%
  completer_colonnes
  
View(limitations_retraite_survie_SL)

write_csv2(limitations_retraite_survie, "sorties/limitations_retraite_survie.csv")
write_csv2(limitations_retraite_survie_SL, "sorties/limitations_retraite_survie_SL.csv")

## On charge la "variante 85" limitations*retraite par genre avec lissage différent
limitations_retraite_var85 <- read_delim("sorties/limitations_retraite_var85.csv", 
                                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),  comment = "#", 
                                    trim_ws = TRUE)
limitations_retraite_survie_var85 <- merge(limitations_retraite_var85, filter(survie, PCS=="Ensemble"), by=c("Sexe","AGE","PCS"))%>%
  arrange(Sexe, AGE)%>%
  completer_colonnes(., forte = FALSE) %>%
  write_csv2(., "sorties/limitations_retraite_survie_var85.csv")

