# mkr_btw21cartogram.r

# ---------------------------------------------------------------------------
# Verzerrte Wahlkreiskarten: 
# ---------------------------------------------------------------------------
# --------------------------------------------------------------------------------

mkr.btw21_cartogram <- function(
  btw21, # Dataset
  party, # = c("CDU","CSU") #als Vector if Union
  prtycol,# ="yellow"
  vote= "Zweitstimme", # or "Erststimme" # first letter ist enough
  cartoweight = "Anzahl",
  itermax = 10, #Default 10
  colvar = "Prozent",
  legendlab = "Zweitstimmenergebnis [%]",
  colgrps = c(0,5,10,15,20,25,30,35,40,45),  # Grenzen der Farbabstufungen, start with 0
  collabs = c("bis 5%","5-10%","10-15%","15-20%","20-25%","25-30%","30-35%","35-40%","40-45%","über 45%"),
  alphastart = 0.25,
  alphaend = 1)
{
  
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
 
  # correst collabs resp. colgrps
  lcollabs <- length(collabs)
  lcolgrps <- length(colgrps)
    if (lcollabs != lcolgrps) {
    stop("Number of 'colgrps' and 'collabs' do not match! Please ceck.")
    }
  
  if (!(party %in% c("AfD","CDU","CSU","FDP","GRÜNE","DIE LINKE","SPD"))) {
    stop("'party' should be one of 'AfD,CDU,CSU,FDP,GRÜNE,DIE LINKE,SPD'")
  }
  
  party2 <- party
  if (party == "GRÜNE") {
    party2 <- "BÜNDNIS 90/ DIE GRÜNEN"
    
  }
  if (party[1] %in% c("CDU","CSU)")) {
    party2 <- "CDU (in Bayern: CSU)"
    
  }
  
  party3 <- party
  if (party == "DIE LINKE") {
    party3 <- "LINKE"
    
  }
  if (party[1] %in% c("CDU","CSU)")) {
    party3 <- "UNION"
    
  }
  # Name of Cartogram data.frame
  cartodata <- paste0("DataCartogram/",party3,"by",cartoweight,"-",itermax,".Rdata")
  
  
  # join election data with shape data and filter by party
  #btw21 <- merge(wk_spdf, BTW21Parteien %>% filter(Stimme == vote & Gruppenname %in% c(party)), by.x="WKR_NR", by.y="Gebietsnummer")
  #btw21 <- merge(wbzJ_spdf, BTW21JParteien %>% filter(Stimme == vote & Gruppenname %in% c(party)), by.x="WKR_NR", by.y="Gebietsnummer")
  btw21 <-st_as_sf(btw21) #as simple feature
  # new Coordinates
  btw21 <- st_transform(btw21, coords = c("lon", "lat"), 
                             crs = "+init=epsg:3395", 
                             agr = "constant") %>%
    filter(!is.na(Anzahl)) # Filter out WKs with no votes for Party
  
  # calculate cartogram - only if not allready done
  if (file.exists(cartodata) == FALSE) {
    btw21_conto <- cartogram_cont(btw21, cartoweight,itermax = itermax)
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
  
  # define colors 
  #print(levels(btw21_cont$alpha))
  pal <- colorFactor(
    palette = c('grey25',alpha(prtycol, seq(alphastart, alphaend, length.out = 9))),
    domain = btw21_cont$alpha,
    alpha = TRUE
  )
  
  # Cols in SF-data.frame
  btw21_cont<- btw21_cont %>% mutate(palcol = pal(alpha))
  
  
  # extract borders of German Länder out of cartogram
  btw21_laender <- btw21_cont %>%
    group_by(LAND_NR) %>% 
    summarise(Anzahl = sum(Anzahl), do_union = TRUE) %>%
    as_Spatial()
  
  btw21_cont <- as_Spatial(btw21_cont) ## wieder SpatialPolygonsdataframe
  
  # Draw Map
  m<-leaflet(btw21_cont) %>% 
    #addTiles() %>%  no normal map because of Scartogram 
    addPolygons(#data=btw21_cont, 
                fillColor = ~palcol, weight=1,fillOpacity = 1,
                #Define PopUp Text
                popup = paste0( "Wahlkreis"
                                , btw21_cont$WKR_NR
                                , ": "
                                , btw21_cont$Gebietsname
                                , "<br>"
                                , "Stimmen (%):"
                                , btw21_cont$Gruppenname
                                , ": "
                                , btw21_cont$Anzahl
                                , " ("
                                , mkr.formatwithdigits(btw21_cont$Prozent,1)
                                , "%)"
                )
                ) %>%
    # Legend
    addLegend("topright",pal = pal,
              values = ~alpha,
              title = paste0("#BTW21: ",party2,"<br>",legendlab),
              opacity = 1) %>% 
    addPolylines(data = btw21_laender,weight = 2,color = "black") # plot border of German Länder
  saveWidget(m, file=paste0("maps/map_btw21",party3,".html"))
  return(m)
  
}

