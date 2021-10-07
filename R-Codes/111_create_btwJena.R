# 111_create_btwJena.R

library(tidyverse)
library(rgdal) # lädt Shapefiles
require(readxl)
library(stringr)
library(sp)
library(sf)



# Originaldaten einlesen --------------------------------------------------

BTW21Jall <- read_xlsx("Daten/Jena/BW21191E_VH.xlsx",skip = 13)



# Datensatz Parteihochburg ------------------------------------------------

BTW21JParteien <- BTW21Jall %>% 
  select(Gebietsnummer = WBZnr,Gebietsname = WBZName,starts_with("1st"),starts_with("2nd")) %>%
  pivot_longer(cols = -c(Gebietsnummer,Gebietsname,),
               names_to = "Gruppenname",
               values_to = "Werte") %>%
  separate(Gruppenname, c("Stimme","Gruppenname", "Gruppenart") ) %>%
  mutate(Stimme = case_when(Stimme == "1st" ~ "1",
                            Stimme == "2nd" ~ "2",
                            TRUE ~ "0"),
         Stimme = as.numeric(Stimme),
         Gruppenart = case_when(Gruppenart == "N" ~ "Anzahl",
                         Gruppenart == "P" ~ "Prozent",
                         TRUE ~ ""),
         Gruppenname = case_when(Gruppenname == "LINKE" ~ "DIE LINKE",
                                 Gruppenname == "B90G" ~ "GRÜNE",
                                 TRUE ~ Gruppenname),
         Gebietsart = "WBZ",
         Gebietsnummer = str_trim(Gebietsnummer) 
         ) %>%
  filter(Gruppenart != "") %>% # nicht gültige ungütige Stimmen
  filter(Gebietsnummer != "0000") %>% # nicht Jena, Stadt
  pivot_wider(id_cols = -c(Gruppenart,Werte),names_from = "Gruppenart", values_from = "Werte") %>%
  group_by(Gebietsnummer,Stimme) %>%  # identifiziere pro Wahlkreis und Stimme (Erst, Zweit)
  mutate(Rang = rank(desc(Anzahl), ties.method = "first")) %>%
  select(Gebietsart, everything())



# Datensatz Wahlkreisgewinner ---------------------------------------------

BTW21JRang <- BTW21JParteien %>%   # den Rang nach Stimmenanzahl
  filter(Rang < 3)  %>% # nur Platz 1 und 2
  mutate(Gruppenname = factor(Gruppenname,levels = c("AfD","CDU","DIE LINKE","FDP","GRÜNE","SPD"))) %>%
  arrange(Gebietsnummer,Stimme, Rang) %>% # nach Rang sortieren
  select(Gebietsnummer,Gebietsname,Gruppenname,Rang,Stimme,Anzahl,Prozent) %>% # nur interessierende Spalten
  pivot_wider(id_cols = c(Gebietsnummer,Gebietsname,Stimme),  # Pivotoere Tabelle um einfacher Differenzen zu bilden
              names_from = Rang, 
              values_from = c(Gruppenname,Anzahl,Prozent)) %>%
  mutate(Anzahl_Diff = Anzahl_1 - Anzahl_2,    # Differenz zwischen Platz 1 und 2 nach Stimmen
         Prozent_Diff = Prozent_1 - Prozent_2) %>% # Differenz nach Prozent 
  rename(Gruppenname = Gruppenname_1,Anzahl = Anzahl_1, Prozent = Prozent_1)


# Shapedatei Wahlbezirke --------------------------------------------------

BTW21Jwk_shapes <- readOGR( 
  dsn= "Daten/Jena" , 
  layer="PLR_WBZ_2021",
  verbose=FALSE
) 
# KBZ_NR ins richtige Forma fürs Matchen bringen
BTW21Jwk_shapes <- BTW21Jwk_shapes %>% 
  st_as_sf() %>% 
  mutate("WKR_NR" =str_pad(gid_2, 4, pad = "0")) %>%
  st_transform(4326) %>% 
  as_Spatial()
class(BTW21Jwk_shapes)

# abspeichern -------------------------------------------------------------

save(BTW21JParteien,BTW21JRang,BTW21Jwk_shapes, file= "Daten/BTW21Jena.RData")  

names(BTW21JParteien)
names(BTW21JRang)
names(BTW21Jwk_shapes)

  
  
# Test only  
#  filter(Gebietsnummer == 1 & Stimme == 1)

