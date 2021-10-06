# 112_load_wbzJhapes.R

wbzJ_spdf <- readOGR( 
  dsn= "Daten/Jena" , 
  layer="PLR_WBZ_2021",
  verbose=FALSE
) 

class(wbzJ_spdf)
wbzJ_spdf<-st_as_sf(wbzJ_spdf)
class(wbzJ_spdf)
wbzJ_spdf <- wbzJ_spdf %>%  mutate("WBZ_NR" =str_pad(gid_2, 4, pad = "0"))
names(wbzJ_spdf)
wbzJ_spdf<-as_Spatial(wbzJ_spdf)
class(wbzJ_spdf)
names(wbzJ_spdf)

names(BTW21J)
table(BTW21J$Gruppenname_1) <- droplevels(BTW21J$Gruppenname_1)

#merge WK Data to geometric data
btw1st_spdf <- merge(wbzJ_spdf, BTW21J %>% filter(Stimme == 1), by.x="WBZ_NR", by.y="Gebietsnummer")
btw2st_spdf <- merge(wbzJ_spdf, BTW21J %>% filter(Stimme == 2), by.x="WBZ_NR", by.y="Gebietsnummer") %>% 
  st_as_sf() %>%
  st_transform(4326) %>% 
  as_Spatial()
names(btw2st_spdf)
##names(BTW21JParteien)
#btw2st_CDU <- merge(wbzJ_spdf, BTW21JParteien %>% filter(Stimme == 2 & Gruppenname %in% c("CDU","CSU")), by.x="WBZ_NR", by.y="Gebietsnummer")
#btw2st_B90G <- merge(wbzJ_spdf, BTW21JParteien %>% filter(Stimme == 2 & Gruppenname %in% c("GRÜNE")), by.x="WBZ_NR", by.y="Gebietsnummer")
#btw2st_SPD <- merge(wbzJ_spdf, BTW21JParteien %>% filter(Stimme == 2 & Gruppenname %in% c("SPD")), by.x="WBZ_NR", by.y="Gebietsnummer")
#btw2st_FDP <- merge(wbzJ_spdf, BTW21JParteien %>% filter(Stimme == 2 & Gruppenname %in% c("FDP")), by.x="WBZ_NR", by.y="Gebietsnummer")
#btw2st_LINKE <- merge(wbzJ_spdf, BTW21JParteien %>% filter(Stimme == 2 & Gruppenname %in% c("DIE LINKE")), by.x="WBZ_NR", by.y="Gebietsnummer")
