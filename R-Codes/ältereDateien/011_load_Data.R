# 011_load_Data.R

# Deutschland:
load(file = "Daten/BTW21Deutschland.RData")  # BTW21Parteien,BTW21Rang,BTW21wk_shapesfile
# Jena:
load(file = "Daten/BTW21Jena.RData")  # BTW21JParteien,BTW21JRang,BTW21Jwk_shapesfile
# Leipzig:
load(file = "Daten/BTW21Leipzig.RData")  # BTW21LParteien,BTW21LRang,BTW21Lwk_shapesfile

#allgemein:

  
load(file = "Daten/BTW21",gebiet(),".RData") # BTW21Parteien,BTW21Rang,BTW21wk_shapesfile