# create_wk_map.r


# ---------------------------------------------------------------------------
# Shapes der Wahlkreisezu Wahlergebnis zuspielen
# ---------------------------------------------------------------------------

# Lade Shapes der Wahlkreise 
library(rgdal)
wk_spdf <- readOGR( 
  dsn= "Daten" , 
  layer="Geometrie_Wahlkreise_20DBT_VG250_geo",
  verbose=FALSE
)

names(wk_spdf)
names(BTW21)

# Kürze auf Existierende Parteien auf Platz 1 ein (nur diese werden eingfefärbt)
BTW21$Gruppenname_1 <- droplevels(BTW21$Gruppenname_1)

#merge WK Data to geometric data
btw1st_spdf <- merge(wk_spdf, BTW21 %>% filter(Stimme == 1), by.x="WKR_NR", by.y="Gebietsnummer")
btw2st_spdf <- merge(wk_spdf, BTW21 %>% filter(Stimme == 2), by.x="WKR_NR", by.y="Gebietsnummer")

# If you want to set your own colors manually:
pal <- colorFactor(
  palette = c('darkblue', 'black', 'grey25', 'purple', 'forestgreen',"red"),
  domain = BTW21$Gruppenname_1
)

# ---------------------------------------------------------------------------
# Unverzerrte Wahlkreiskarten
# ---------------------------------------------------------------------------
library(htmlwidgets)
library(sp)
library(leaflet)
names(btw1st_spdf)
# Erstimmenergebnisse
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw1st_spdf, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)
m
saveWidget(m, file="map_btw1st.html")
# Zweitstimmenergebnisse
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw2st_spdf, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)
m
saveWidget(m, file="map_btw2st.html")


# ---------------------------------------------------------------------------
# Verzerrte Wahlkreiskarten: 
# ---------------------------------------------------------------------------
library(cartogram)
library(sf)
library(plotly)

# Erststimmen: Differenz Erster zu Zweiter

btw1st_spdf2<-st_as_sf(btw1st_spdf) #as simple feature
# neue Coordinaten
btw1st_spdf2 <- st_transform(btw1st_spdf2, coords = c("lon", "lat"), 
                         crs = "+init=epsg:3395", 
                         agr = "constant")
# Cartogramm berechnen
btw21_cont <- cartogram_cont(btw1st_spdf2, "Anzahl_Diff",itermax = 10)
#zurücktransformieren
names(btw21_cont)
table(btw21_cont$LAND_NAME)
btw21_contBL<- st_transform(btw21_cont, 4326) %>% # ("+proj=longlat +datum=WGS84")
  mutate(alpha = Prozent_1/max(Prozent_1)) #%>%

summary(btw21_contBL$alpha)
# wieder SpatialPolygonsdataframe
btw21_cont2 <- as_Spatial(btw21_contBL)
# Karte
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw21_cont2, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~alpha)
m

library(htmlwidgets)
saveWidget(m, file="map_btw1st_cartogram.html")

# Nach Prozentualem Vorsprung
leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw21_cont2, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)



# Versuch: mit Plotly -- BAustelle
plot_ly(btw21_cont2) %>% 
  add_sf(
    color = ~Gruppenname_1, 
    #split = ~Gruppenname_1, 
    #span = I(1),
    text = ~paste0(Gruppenname_1," (",Prozent_1,"%) Vorsprung ",Anzahl_Diff," Stimmen"),
    hoverinfo = "text",
    hoveron = "fills"
  ) #%>%
  layout(showlegend = FALSE) #%>%
  colorbar(title = "Stimmenvorsprung Ersttimme")


