# 012_load_wkshapes.R

wk_spdf <- readOGR( 
  dsn= "Daten" , 
  layer="Geometrie_Wahlkreise_20DBT_VG250_geo",
  verbose=FALSE
)

names(wk_spdf)
names(BTW21)
BTW21$Gruppenname_1 <- droplevels(BTW21$Gruppenname_1)

#merge WK Data to geometric data
btw1st_spdf <- merge(wk_spdf, BTW21 %>% filter(Stimme == 1), by.x="WKR_NR", by.y="Gebietsnummer")
btw2st_spdf <- merge(wk_spdf, BTW21 %>% filter(Stimme == 2), by.x="WKR_NR", by.y="Gebietsnummer")

#names(BTW21Parteien)
btw2st_CDU <- merge(wk_spdf, BTW21Parteien %>% filter(Stimme == 2 & Gruppenname %in% c("CDU","CSU")), by.x="WKR_NR", by.y="Gebietsnummer")
btw2st_B90G <- merge(wk_spdf, BTW21Parteien %>% filter(Stimme == 2 & Gruppenname %in% c("GRÜNE")), by.x="WKR_NR", by.y="Gebietsnummer")
btw2st_SPD <- merge(wk_spdf, BTW21Parteien %>% filter(Stimme == 2 & Gruppenname %in% c("SPD")), by.x="WKR_NR", by.y="Gebietsnummer")
btw2st_FDP <- merge(wk_spdf, BTW21Parteien %>% filter(Stimme == 2 & Gruppenname %in% c("FDP")), by.x="WKR_NR", by.y="Gebietsnummer")
btw2st_LINKE <- merge(wk_spdf, BTW21Parteien %>% filter(Stimme == 2 & Gruppenname %in% c("DIE LINKE")), by.x="WKR_NR", by.y="Gebietsnummer")
