donnees_zip <- list.files("data", full.names = TRUE)

for (fic in donnees_zip) {
  dir <- strsplit(fic, ".", fixed = TRUE)[[1]][1]
  zip::unzip(fic, exdir = dir)
}

for (fic_dta in list.files("data", pattern = ".dta.zip", full.names = TRUE)) {
  zip::zip_list(list.files("data", pattern = ".dta.zip", full.names = TRUE)[1])$filename
  zip::unzip(fic_dta, "Stata", exdir="data")
}

