library(dplyr)
library(tidyverse)
setwd("/Users/ulojkine/Documents/ENS/Économie/Retraites/EVSI/Decomposition")

# Let us start by importing the INSEE datasets of mortality by generation,
# available from here: https://www.insee.fr/fr/statistiques/6543678?sommaire=6543680
# that were manually converted to .csv
MortalityFemale <- read_delim("MortaliteGenerationF.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                          trim_ws = TRUE)
MortalityMale <- read_delim("MortaliteGenerationH.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                          trim_ws = TRUE)
MortalityFemale <- MortalityFemale %>% select(c(1:102)) # we focus on ages 0:100

# Pivot to long format
MortalityLongFemale <- MortalityFemale %>% pivot_longer(col=-1)
MortalityLongMale <- MortalityMale %>% pivot_longer(col=-1)

# Merging the two gendered datasets
MortalityLongFemale$gender <- "F"
MortalityLongMale$gender <- "H"
MortalityLong <- rbind(MortalityLongFemale,MortalityLongMale)
MortalityLong <- MortalityLong %>% rename(age = name, mortality = value)
MortalityLong$age <- as.numeric(MortalityLong$age)
MortalityLong$year <- MortalityLong$birth + MortalityLong$age # year = birth + age

# Calculate the "instantaneous" survival rates as in the definition of life expectancy
# (survival rate for a virtual individual whose mortality rates at all ages would be that of the current year)
MortalityLong <- MortalityLong %>%
  arrange(gender, year, age) %>% 
  group_by(year, gender) %>%
  mutate(survival = cumprod(1 - lag(mortality,default=0)/100000))

View(MortalityLong)

write_csv(MortalityLong, "MortalityLong.csv")