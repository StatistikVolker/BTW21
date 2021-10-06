# create_wk_map.r


# If you want to set your own colors manually:
pal <- colorFactor(
  palette = c('darkblue', 'black', 'grey25', 'purple', 'forestgreen',"red"),
  domain = BTW21$Gruppenname_1
)

names(btw1st_spdf)
# Erstimmenergebnisse
leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw1st_spdf, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)
# Zweitstimmenergebnisse
leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw2st_spdf, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25)


btw1st_spdf2<-st_as_sf(btw1st_spdf)
names(btw1st_spdf2)
btw1st_spdf2 <- st_transform(btw1st_spdf2, coords = c("lon", "lat"), 
                         crs = "+init=epsg:3395", 
                         agr = "constant") %>%
  #filter(Gruppenname_1 == "DIE LINKE")
  ##filter(LAND_NAME == "Sachsen")
  filter(as.numeric(LAND_NR) > 7)

btw21_cont <- cartogram_cont(btw1st_spdf2, "Anzahl_1")
class(btw21_cont)


btw21_contf<- st_transform(btw21_cont, 4326) %>%
  mutate(alpha =ifelse((Prozent_Diff/max(Prozent_Diff)+0.25)<1, Prozent_Diff/max(Prozent_Diff)+0.25,1),
         palcol = pal(Gruppenname_1)) #%>%
  #st_cast("MULTIPOLYGON") %>%
  #st_cast("POLYGON",do_group = FALSE) %>%
  #filter(Gruppenname_1 == "DIE LINKE")
  #filter(substr(Gebietsname,1,7) == "Leipzig")# ("+proj=longlat +datum=WGS84")
  #filter(WKR_NR == 153)
summary(btw21_contf$alpha)

btw21_cont2 <- as_Spatial(btw21_contf)#,cast = FALSE)
class(btw21_cont2)
m<-leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw21_cont2, fillColor = ~palcol, weight=1,fillOpacity = ~alpha)
m
mapshot(m, file = "maps/BTW21_Erst_cartogram.png")

leaflet() %>% #addTiles() %>% 
  addPolygons(data=btw21_cont2, fillColor = ~pal(Gruppenname_1), weight=2,fillOpacity = 1)



plot_ly(btw21_cont) %>% 
  add_sf(
    color = ~Gruppenname_1, 
    split = ~Gruppenname_1, 
    span = I(1),
    text = ~paste0(Gruppenname_1," (",Prozent_1,"%) Vorsprung ",Anzahl_Diff," Stimmen"),
    hoverinfo = "text",
    hoveron = "fills"
  ) #%>%
  layout(showlegend = FALSE) #%>%
  colorbar(title = "Stimmenvorsprung Ersttimme")


