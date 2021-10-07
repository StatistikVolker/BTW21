# mkr_btw21karten.r





# ---------------------------------------------------------------------------
# Verzerrte Wahlkreiskarten: 
# ---------------------------------------------------------------------------
# --------------------------------------------------------------------------------

mkr.btw21karten <- function(
  maptype,# = "normalmap",
  Gebiet,# = "Jena",
  GebietWK,# = "Wahlbezirk",
  Darstellung,# = "Parteihochburg",
  vote,# = "Z",
  party,# = "SPD", # = c("CDU","CSU") #als Vector if Union
  partycol,# = "red",# ="yellow"
  #vote= "Zweitstimme", # or "Erststimme" # first letter ist enough
  cartoweight = "Anzahl",
  itermax = 10, #Default 10
  colvar = "Prozent",
  legendlab = "Zweitstimmenergebnis [%]",
  colgrps = c(0,5,10,15,20,25,30,35,40,45),  # Grenzen der Farbabstufungen, start with 0
  collabs = c("bis 5%","5-10%","10-15%","15-20%","20-25%","25-30%","30-35%","35-40%","40-45%","über 45%"),
  alphastart = 0.25,
  alphaend = 1,
  Shapedf = BTW21wk_shapes, # (bzw. BTW21Jwk_shapesfile, BTW21Lwk_shapesfile - J,L Jena, Leipzig)
  Rangdf  = BTW21Rang,
  Parteidf = BTW21Parteien# (bzw. BTW21JRang,BTW21LRang - J,L Jena, Leipzig)
)
{
  
  # Für Cartogram Dateiname - Vermeidung von Umlauten
  party4 <- party
  if (party == "GR\u00DCNE") party4 <- "B90G"
  # print(party)
  # print(party4)

  Gebiet2 <- Gebiet
  if (Gebiet == "Neue L\u00E4nder (+Berlin)") { Gebiet2 <- "NLmitB"}
  # print(Gebiet)
  # print(Gebiet2)
  
  # Some checks at the beginning
  # Correst Vote
  if (vote == "Erststimme" | vote == "E") {
    vote <- 1
  } else {
    if (vote == "Zweitstimme" | vote == "Z") {
      vote <- 2
    } else {
        stop("'vote' has to be 'Erststimme' or 'Zweitstimme' only (first letter is enogh.) Please check.")
    }
  }
  
  if (vote == 1) {
    legendlab = "Erststimmenergebnis [%]"
  }
 
  # correst collabs resp. colgrps
  lcollabs <- length(collabs)
  lcolgrps <- length(colgrps)
    if (lcollabs != lcolgrps) {
    stop("Number of 'colgrps' and 'collabs' do not match! Please ceck.")
    }
  
  # print(party)
  # print(Gebiet)
  if (party == "CDU/CSU") party <- "CDU"
  if (party == "CDU" & Gebiet == "Deutschland") party <- c("CDU","CSU")
  # print(party)
  
  if (!(party %in% c("AfD","CDU","CSU","FDP","GR\u00DCNE","DIE LINKE","SPD"))) {
    stop("'party' should be one of 'AfD,CDU,CSU,FDP,GR\u00DCNE,DIE LINKE,SPD'")
  }
  
  party2 <- party
  if (party == "GRÜNE") {
    party2 <- "BÜNDNIS 90/ DIE GRÜNEN"
    
  }
  if (party[1] %in% c("CDU","CSU)") & Gebiet == "Deutschland") {
    party2 <- "CDU (in Bayern: CSU)"
    
  }
  
  party3 <- party
  if (party == "DIE LINKE") {
    party3 <- "LINKE"
    
  }
  if (party[1] %in% c("CDU","CSU)")) {
    party3 <- "UNION"
    
  }
  
  unionlab = "CDU"
  if (Gebiet == "Deutschland") unionlab = "CDU/CSU"
  ##mit FDP
  #partycols <- c("red","black","forestgreen","yellow","darkblue","purple")
  #partylabs <- c( "SPD", unionlab,"B\u00DCNDNIS 90/ DIE GR\u00DCNEN","FDP","AfD","DIE LINKE")
  #ohne FDP
  partycols <- c("red","black","forestgreen","darkblue","purple")
  partylabs <- c( "SPD", unionlab,"B\u00DCNDNIS 90/ DIE GR\u00DCNEN","AfD","DIE LINKE")
  
  # join election data with shape data and filter by party
  #btw21 <- merge(wk_spdf, BTW21Parteien %>% filter(Stimme == vote & Gruppenname %in% c(party)), by.x="WKR_NR", by.y="Gebietsnummer")
  if (Darstellung == "Parteihochburg") {
    # print(party)
    btw21map <- merge(Shapedf, Parteidf %>% filter(Stimme == vote & Gruppenname %in% c(party) ), by.x="WKR_NR", by.y="Gebietsnummer"  )
    cartodata <- paste0("DataCartogram/",Gebiet2,"_",Darstellung,"_",party4,"_",vote,"_","by",cartoweight,"-",itermax,".Rdata")
    
  } else {
    btw21map <- merge(Shapedf, Rangdf %>% filter(Stimme == vote), by.x="WKR_NR", by.y="Gebietsnummer")  
    cartodata <- paste0("DataCartogram/",Gebiet2,"_",Darstellung,"_",vote,"_","by",cartoweight,"-",itermax,".Rdata")
    pal <- colorFactor(
      palette = c('darkblue', 'black', 'purple',"yellow", 'forestgreen',"red"),
      domain = btw21map$Gruppenname,
      alpha = TRUE
      
    )
    
  }
  #names(btw21map)
  # Transformiere Koordinaten in WGS84
  btw21map <- btw21map %>% 
    st_as_sf() %>%
    st_transform(4326) %>% 
    as_Spatial()
  # als simple feature
  btw21map <-st_as_sf(btw21map) #as simple feature
  # new Coordinates
  btw21map <- st_transform(btw21map, coords = c("lon", "lat"), 
                             crs = "+init=epsg:3395", 
                             agr = "constant") %>%
    filter(!is.na(Anzahl)) # Filter out WKs with no votes for Party
  


  if (maptype == "cartogram") {
  
    # calculate cartogram - only if not allready done
    # print(cartodata)
    if (file.exists(cartodata) == FALSE) {
      btw21_conto <- cartogram_cont(btw21map, cartoweight,itermax = itermax)
      save(btw21_conto,file=cartodata)
    } else {
      load(file=cartodata)
    }
    # back to WGS84 coordinates
    btw21_cont<- st_transform(btw21_conto, 4326) %>% # ("+proj=longlat +datum=WGS84")
      mutate(alpha = cut(as.numeric(Prozent),
                         c(colgrps,Inf),
                         collabs,
                         right=FALSE)
      )
    
    # print(names(btw21_cont))
        
    # extract borders of German Länder out of cartogram
    if (Gebiet == "Jena") {
      btw21_contuegebiet <- btw21_cont %>%
        group_by(plraum_nr) %>% 
        summarise(Anzahl = sum(Anzahl), do_union = TRUE) %>%
        ungroup() %>%
        as_Spatial()
      # print("Gebiet Jena")
    }
    if (Gebiet == "Leipzig") {
      btw21_contuegebiet <- btw21_cont %>%
        group_by(stadtbezirknr) %>% 
        summarise(Anzahl = sum(Anzahl), do_union = TRUE) %>%
        ungroup() %>%
        as_Spatial()
      # print("Leipzig stadtbezirknr")
    }
    if (Gebiet %in% c("Deutschland","Neue L\u00E4nder (+Berlin)")) {
      btw21_contuegebiet <- btw21_cont %>%
        group_by(LAND_NR) %>% 
        summarise(Anzahl = sum(Anzahl), do_union = TRUE) %>%
        ungroup() %>%
        as_Spatial()
      # print("Gebiet D, NL")
    }
    #print("Schritt 1")
    if (Darstellung == "Parteihochburg") {
      # Cols in SF-data.frame
      pal <- colorFactor(
        palette = c('grey25',alpha(partycol, seq(alphastart, alphaend, length.out = lcollabs))),
        domain = btw21_cont$alpha,
        alpha = TRUE
      )
      btw21_cont<- btw21_cont %>% mutate(palcol = pal(alpha))
      btw21_cont<- btw21_cont %>% as_Spatial() ## wieder SpatialPolygonsdataframe
      
      popuptxt <- paste0(GebietWK, btw21_cont$WKR_NR, ": ", btw21_cont$Gebietsname, "<br>",
                         "Stimmen (%):", btw21_cont$Gruppenname, ": ", btw21_cont$Anzahl," (", mkr.formatwithdigits(btw21_cont$Prozent,1), "%)")
      
      m<-leaflet(btw21_cont) %>% 
        addTiles(attribution = 'Volker Holzendorf, Stadt Jena, FD Finanzen, Team Controlling und Statiistik',
                 urlTemplate = "") %>%  #no normal map because of Scartogram 
        addPolygons(#data=btw21_cont, 
          fillColor = ~palcol, 
          weight=1,
          fillOpacity = 1,
          #Define PopUp Text
          popup = popuptxt
        ) %>%
        addPolylines(data = btw21_contuegebiet,weight = 2,color = "black")  %>% # plot border of German Lander
       # Legend
        addLegend("topright",pal = pal,
                  values = ~alpha,
                  title = paste0("#BTW21: ",party2,"<br>",legendlab),
                  opacity = 1)
      #m
      # print("Cartogramm_Karte erstellt")  
      

    } else {
      
      btw21_cont<- btw21_cont %>% 
        mutate(alpha =ifelse((Prozent_Diff/max(Prozent_Diff)+0.25)<1, Prozent_Diff/max(Prozent_Diff)+0.25,1),
               palcol = pal(Gruppenname)) %>%
      as_Spatial() ## wieder SpatialPolygonsdataframe
      
      popuptxt <- paste0(GebietWK, btw21_cont$WKR_NR, ": ", btw21_cont$Gebietsname, "<br>",
                         "Erster:", btw21_cont$Gruppenname, " ", btw21_cont$Anzahl," (", mkr.formatwithdigits(btw21_cont$Prozent,1), "%)", "<br>",
                         "Zweiter:", btw21_cont$Gruppenname_2, " ", btw21_cont$Anzahl_2," (", mkr.formatwithdigits(btw21_cont$Prozent_2,1), "%)")
      m <-leaflet() %>% #addTiles() %>% 
        addTiles(attribution = 'Volker Holzendorf, Stadt Jena, FD Finanzen, Team Controlling und Statiistik',
                 urlTemplate = "") %>%  #no normal map because of Scartogram 
        addPolygons(data=btw21_cont, 
                    fillColor = ~palcol, 
                    weight=2,
                    fillOpacity = ~alpha,
                    popup = popuptxt) %>%
        addPolylines(data = btw21_contuegebiet,weight = 2,color = "black")  %>% # plot border of German Lander
        # Legend
        addLegend("topright",#pal = pal,
                  colors = partycols,
                  labels = partylabs,
                  #values = ~Gruppenname,
                  title = paste0("#BTW21: Wahlkreisgewinner","<br>",
                                 legendlab,"<br>",
                                 "Farbs\u00E4ttigung gibt Abstand","<br>",
                                 "zum Zweitplatzierten an. Je heller,","<br>",
                                 "desto geringer der Abstand zwischen","<br>",
                                 "Erst- und Zweitplatzierten."),
                  opacity = 0.7)
      }
    # mDraw Map
  }
  
  if (maptype == "normalmap") {
    if (Darstellung == "Parteihochburg") {
      # Cols in SF-data.frame
      pal <- colorFactor(
        palette = c('grey25',alpha(partycol, seq(alphastart, alphaend, length.out = lcollabs))),
        domain = btw21map$alpha,
        alpha = TRUE
      )
    }
    # back to WGS84 coordinates
    btw21map<- st_transform(btw21map, 4326) %>% # ("+proj=longlat +datum=WGS84")
      mutate(alpha = cut(as.numeric(Prozent),
                         c(colgrps,Inf),
                         collabs,
                         right=FALSE),
             palcol = pal(alpha)
             )
    
    # extract borders of German Länder out of cartogram
    if (Gebiet == "Jena") {
      btw21_uegebiet <- btw21map %>%
        group_by(plraum_nr) %>% 
        summarise(Anzahl = sum(Anzahl), do_union = TRUE) %>%
        ungroup() %>%
        as_Spatial()
    }
    if (Gebiet == "Leipzig") {
      btw21_uegebiet <- btw21map %>%
        group_by(stadtbezirknr) %>% 
        summarise(Anzahl = sum(Anzahl), do_union = TRUE) %>%
        ungroup() %>%
        as_Spatial()
    }
    if (Gebiet %in% c("Deutschland","Neue L\u00E4nder (+Berlin)")) {
      btw21_uegebiet <- btw21map %>%
        group_by(LAND_NR) %>% 
        summarise(Anzahl = sum(Anzahl), do_union = TRUE) %>%
        ungroup() %>%
        as_Spatial()
    }
    btw21map <- as_Spatial(btw21map) ## wieder SpatialPolygonsdataframe
    
    if (Darstellung == "Parteihochburg") {
      popuptxt <- paste0(GebietWK, btw21map$WKR_NR, ": ", btw21map$Gebietsname, "<br>",
                         "Stimmen (%):", btw21map$Gruppenname, ": ", btw21map$Anzahl," (", mkr.formatwithdigits(btw21map$Prozent,1), "%)")
      # Draw Map
      m<-leaflet(btw21map) %>% 
        #addTiles() %>%  no normal map because of Scartogram 
        addTiles(attribution = 'Volker Holzendorf, Stadt Jena, FD Finanzen, Team Controlling und Statiistik',
                 urlTemplate = "") %>%  #no normal map because of Scartogram 
        addPolygons(#data=btw21_cont, 
          fillColor = ~palcol, weight=1,fillOpacity = 1,
          #Define PopUp Text
          popup = popuptxt) %>%
        addPolylines(data = btw21_uegebiet,weight = 2,color = "black") %>% 
        # Legend
        addLegend("topright",pal = pal,
                  values = ~alpha,
                  title = paste0("#BTW21: ",party2,"<br>",legendlab),
                  opacity = 1)
      
    } else {
      names(btw21map)
      popuptxt <- paste0(GebietWK, btw21map$WKR_NR, ": ", btw21map$Gebietsname, "<br>",
                         "Erster:", btw21map$Gruppenname, " ", btw21map$Anzahl," (", mkr.formatwithdigits(btw21map$Prozent,1), "%)", "<br>",
                         "Zweiter:", btw21map$Gruppenname_2, " ", btw21map$Anzahl_2," (", mkr.formatwithdigits(btw21map$Prozent_2,1), "%)")
      m <-leaflet() %>% #addTiles() %>% 
        addTiles(attribution = 'Volker Holzendorf, Stadt Jena, FD Finanzen, Team Controlling und Statiistik',
                 urlTemplate = "") %>%  #no normal map because of Scartogram 
        addPolygons(data=btw21map, 
                    fillColor = ~pal(Gruppenname), 
                    weight=2,
                    fillOpacity = ~(Prozent_Diff/max(Prozent_Diff))+0.25,
                    popup = popuptxt) %>%
        addPolylines(data = btw21_uegebiet,weight = 2,color = "black") %>%# plot border of German Lander
        # Legend
        addLegend("topright",#pal = pal,
                  colors = partycols,
                  labels = partylabs,
                  #values = ~Gruppenname,
                  title = paste0("#BTW21: Wahlkreisgewinner","<br>",
                                 legendlab,"<br>",
                                 "Farbs\u00E4ttigung gibt Abstand","<br>",
                                 "zum Zweitplatzierten an. Je heller,","<br>",
                                 "desto geringer der Abstand zwischen","<br>",
                                 "Erst- und Zweitplatzierten."),
                  opacity = 0.7)
      
    }
    
  
}
  return(m)
  
}
