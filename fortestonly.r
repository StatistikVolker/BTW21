# For test only
maptype = "normalmap"
Gebiet = "Jena"
GebietWK = "Wahlbezirk"
Darstellung = "Parteihochburg"
vote = "Z"
party = "FDP" # = c("CDU","CSU") #als Vector if Union
partycol = "red"# ="yellow"
#vote= "Zweitstimme" # or "Erststimme" # first letter ist enough
cartoweight = "Anzahl"
itermax = 10 #Default 10
colvar = "Prozent"
legendlab = "Zweitstimmenergebnis [%]"
colgrps = c(0,5,10,15,20,25,30,35,40,45)  # Grenzen der Farbabstufungen, start with 0
collabs = c("bis 5%","5-10%","10-15%","15-20%","20-25%","25-30%","30-35%","35-40%","40-45%","über 45%")
alphastart = 0.25
alphaend = 1
Shapedf = BTW21wk_shapes # (bzw. BTW21Jwk_shapesfile, BTW21Lwk_shapesfile - J,L Jena, Leipzig)
Rangdf  = BTW21Rang
Parteidf = BTW21Parteien# (bzw. BTW21JRang,BTW21LRang - J,L Jena, Leipzig)
