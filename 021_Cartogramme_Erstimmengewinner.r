# Cartogramme_Erstimmengewinner.r

# --------------------------------------------------------------------------------
# Erststimmen: 

btw1st_spdf2<-st_as_sf(btw1st_spdf) #as simple feature
# neue Coordinaten
btw1st_spdf2 <- st_transform(btw1st_spdf2, coords = c("lon", "lat"), 
                             crs = "+init=epsg:3395", 
                             agr = "constant")

# Cartogramm berechnen: Stimmen-Differenz Erster zu Zweiter ist Verzerrungsfaktor
btw1st_AD_conto <- cartogram_cont(btw1st_spdf2, "Anzahl_Diff",itermax = 10)
#zurücktransformieren
names(btw1st_AD_conto)
table(btw1st_AD_conto$LAND_NAME)
btw1st_AD_cont<- st_transform(btw1st_AD_conto, 4326) %>% # ("+proj=longlat +datum=WGS84")
  mutate(alpha = Prozent_1/max(Prozent_1)) #%>%
btw1st_AD_cont <- as_Spatial(btw1st_AD_cont) ## wieder SpatialPolygonsdataframe
# Karte
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw1st_AD_cont, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~alpha)
#addPolygons(data=btw1st_AD_cont, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)
m
saveWidget(m, file="maps/map_btw1st_cartogram.html")


# --------------------------------------------------------------------------------
# Erststimmen: Anzahl Stimmen Erster ist Verzerrungsfaktor
btw1st_AS_conto <- cartogram_cont(btw1st_spdf2, "Anzahl_1",itermax = 10)
btw1st_AS_cont<- st_transform(btw1st_AS_conto, 4326) %>%
  mutate(alpha =ifelse((Prozent_Diff/max(Prozent_Diff)+0.25)<1, Prozent_Diff/max(Prozent_Diff)+0.25,1),
         palcol = pal(Gruppenname_1)) #%>%
#st_cast("MULTIPOLYGON") %>%
#st_cast("POLYGON",do_group = FALSE) %>%
#filter(Gruppenname_1 == "DIE LINKE")
#filter(substr(Gebietsname,1,7) == "Leipzig")# ("+proj=longlat +datum=WGS84")
#filter(WKR_NR == 153)
summary(btw1st_AS_cont$alpha)

btw1st_AS_cont <- as_Spatial(btw1st_AS_cont)#,cast = FALSE)
class(btw1st_AS_cont)
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw1st_AS_cont, fillColor = ~palcol, weight=1,fillOpacity = ~alpha)
m
saveWidget(m, file="maps/BTW21_Erst_cartogram.html")
m#apshot(m, file = "BTW21_Erst_cartogram.png")



#leaflet() %>% #addTiles() %>% 
#  addPolygons(data=btw21_cont2, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = 1)



# --------------------------------------------------------------------------------
# JENA
# --------------------------------------------------------------------------------
btw2st_spdf2<-st_as_sf(btw2st_spdf) #as simple feature
# neue Coordinaten
btw2st_spdf2 <- st_transform(btw2st_spdf2, coords = c("lon", "lat"), 
                             crs = "+init=epsg:3395", 
                             agr = "constant")
# Erststimmen: Anzahl Stimmen Erster ist Verzerrungsfaktor
btw1st_AS_conto <- cartogram_cont(btw2st_spdf2, "Anzahl_1",itermax = 10)
btw1st_AS_cont<- st_transform(btw1st_AS_conto, 4326) %>%
  mutate(alpha =ifelse((Prozent_Diff/max(Prozent_Diff)+0.25)<1, Prozent_Diff/max(Prozent_Diff)+0.25,1),
         palcol = pal(Gruppenname_1)) #%>%
#st_cast("MULTIPOLYGON") %>%
#st_cast("POLYGON",do_group = FALSE) %>%
#filter(Gruppenname_1 == "DIE LINKE")
#filter(substr(Gebietsname,1,7) == "Leipzig")# ("+proj=longlat +datum=WGS84")
#filter(WKR_NR == 153)
summary(btw1st_AS_cont$alpha)

btw1st_AS_cont <- as_Spatial(btw1st_AS_cont)#,cast = FALSE)
class(btw1st_AS_cont)
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw1st_AS_cont, fillColor = ~palcol, weight=1,fillOpacity = ~alpha)
m
saveWidget(m, file="Jenamaps/BTW21_Zweit_nachWBZ_cartogram.html")
mapshot(m, file = "Jenamaps/BTW21_Zweit_nachWBZ_cartogram.png")



