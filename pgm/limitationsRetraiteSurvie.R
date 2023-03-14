library(dplyr)
library(readr)
`%notin%` <- Negate(`%in%`)

# Ici définir le dossier de travail
#setwd("Documents/ENS/Économie/Retraites/EVSI")

limitationsRetraite <- read_delim("limitations_retraite1303.csv", 
                                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                  comment = "#", trim_ws = TRUE)

limitationsRetraite <- rename(limitationsRetraite, age = AGE)

pcs12 <- c("1 - Agriculteurs", "2 - Artisans, commerçants et chefs d’entreprise")
limitationsRetraiteAutres <- filter(limitationsRetraite, PCS %notin% pcs12)
limitationsRetraite12H <- filter(limitationsRetraite, PCS %in% pcs12)
limitationsRetraite12H$Sexe <- "Hommes"
limitationsRetraite12F <- limitationsRetraite12H
limitationsRetraite12F$Sexe <- "Femmes"
limitationsRetraite <- rbind(limitationsRetraite12H, limitationsRetraite12F, limitationsRetraiteAutres)

# Ce bloc complétait le tableau de 75 à 100 ans
# mais ce n'est plus nécessaire maintenant
#for(genre in unique(limitationsRetraite$Sexe)){
#  for(pcs in unique(limitationsRetraite$PCS)){
#    print(paste(genre,pcs))
#    id75 <- which((limitationsRetraite$Sexe == genre)
#                  & (limitationsRetraite$PCS == pcs)
#                  & (limitationsRetraite$age == 75 ))
#    vieillesse <- limitationsRetraite[rep(id75, times=(100 - 75)),]
#    vieillesse$age <- c(76:100)
#    vieillesse$non_retraite_limite <- 0
#    vieillesse$non_retraite_non_limite <- 0
#    limitationsRetraite <- rbind(limitationsRetraite, vieillesse)
#  }
#}

# Construction de la colonne des taux de survie
limitationsRetraite$survie <- NA
# On récupère l'information depuis le tableau limitationsSurvie
limitationsSurvie <- read_delim("limitationsSurvie2501.csv", 
                                     delim = "\t", escape_double = FALSE, 
                                     comment = "#", trim_ws = TRUE)
# Pour chaque ligne, on ajoute l'information correspondante dans le tableau limitationsRetraite
for(i in c(1:nrow(limitationsRetraite))){
  limitationsRetraite[i,"survie"] <- as.double(
    limitationsSurvie[(limitationsSurvie$PCS == paste(limitationsRetraite[i,"PCS"]))
                       & (limitationsSurvie$Sexe == paste(limitationsRetraite[i,"Sexe"]))
                       & (limitationsSurvie$age == as.numeric(limitationsRetraite[i,"age"])),
                       "survie"]
  )
}

df <- limitationsRetraite
# Retraité = Retraité limité ou non limité
df$retraite <- df$retraite_limite+df$retraite_non_limite
# Limité = Limité retraité ou non
df$limite <- df$retraite_limite + df$non_retraite_limite
# Survie non limité
df$survie_non_limite <- df$survie*(1 - df$limite)/100000
# Survie en retraite
df$survie_retraite <- df$survie*df$retraite/100000
# Survie en retraite et limité
df$survie_retraite_limite <- df$survie*df$retraite_limite/100000
# Survie en retraite non limité
df$survie_retraite_non_limite <- df$survie*df$retraite_non_limite/100000

write_csv(df, "limitationsRetraiteSurvie1303.csv")