# 000_meta_btw.R


# Pakete laden ------------------------------------------------------------

source("010_load packages.r")

# Wahldaten einlesen ------------------------------------------------------

source("011_create_btwdata.R")
source("012_load_wkshapes.R")

# Karte -----------------------------------------------------------

# ---------------------------------------------------------------------------
# Farben der Parteien
# ---------------------------------------------------------------------------

print(levels(BTW21$Gruppenname_1))
pal <- colorFactor(
  palette = c('darkblue', 'black', 'grey25', 'purple', 'forestgreen',"red"),
  domain = BTW21$Gruppenname_1
)
#pal(levels(BTW21$Gruppenname_1))

# ---------------------------------------------------------------------------
# Unverzerrte Wahlkreiskarten
# ---------------------------------------------------------------------------

# Erstimmenergebnisse
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw1st_spdf, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)
m
saveWidget(m, file="maps/map_btw1st.html")
# Zweitstimmenergebnisse
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw2st_spdf, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)
m
saveWidget(m, file="maps/map_btw2st.html")

# ---------------------------------------------------------------------------
# Verzerrte Wahlkreiskarten: 
# ---------------------------------------------------------------------------

# Zweitstimmen: 

source("mkr_btw21cartogram.r")

mkr.btw21_cartogram(
party = "FDP", #als Vector if Union
prtycol = "yellow",
vote = "Zweitstimme" #,# or "Erststimme" # first letter is enough
#cartoweight = "Anzahl" ,
#itermax = 10, #Default 10
#colvar = "Prozent",
#legendlab = "Zweitstimmenergebnis [%]",
#colgrps = c(o,5,10,15,20,25,30,35,40,45) , # Grenzen der Farbabstufungen, start with "0"
#collabs = c("bis 5%","5-10%","10-15%","15-20%","20-25%","25-30%","30-35%","35-40%","40-45%","über 45%"),
#alphastart = 0.25,
#alphaend = 1
)

mkr.btw21_cartogram(
  party = "SPD", #als Vector if Union
  prtycol = "red",
  vote = "Z" #,# or "Erststimme" # first letter is enough
) 

mkr.btw21_cartogram(
  party = "GRÜNE", #als Vector if Union
  prtycol = "forestgreen",
  vote = "Z" #,# or "Erststimme" # first letter is enough
) 

mkr.btw21_cartogram(
  party = "AfD", #als Vector if Union
  prtycol = "darkblue",
  vote = "Z" #,# or "Erststimme" # first letter is enough
) 

mkr.btw21_cartogram(
  party = c("CDU","CSU"), #als Vector if Union
  prtycol = "black",
  vote = "Z" #,# or "Erststimme" # first letter is enough
) 

mkr.btw21_cartogram(
  party = "DIE LINKE", #als Vector if Union
  prtycol = "violet",
  vote = "Z" #,# or "Erststimme" # first letter is enough
) 





