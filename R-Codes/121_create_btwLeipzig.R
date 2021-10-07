# 111_create_btwJena.R

library(tidyverse)
library(rgdal) # lädt Shapefiles
#require(readxl)
#library(stringr)
library(sp)
library(sf)



# Originaldaten einlesen --------------------------------------------------

BTW21LOrtsteile <- read.csv2("Daten/Leipzig/Open-Data-Bundestagswahl-_Land-Sachsen_3-3.csv")

names(BTW21LOrtsteile)
names(BTW21LParteien)
class(BTW21LParteien$Gruppenname)
# Datensatz Parteihochburg ------------------------------------------------

BTW21LParteien <- BTW21LOrtsteile %>% 
  select(Gebietsnummer = 'gebiet.nr',Gebietsname = 'gebiet.name',starts_with("D"),starts_with("F")) %>%
  select(-c(datum,D,F)) %>%
  pivot_longer(cols = -c(Gebietsnummer,Gebietsname),
               names_to = "Gruppenname",
               values_to = "Anzahl") %>%
  mutate(Stimme = case_when(substr(Gruppenname,1,1) == "D" ~ "1",
                            substr(Gruppenname,1,1) == "F" ~ "2",
                            TRUE ~ "0"),
         Stimme = as.numeric(Stimme),
         Gruppenname = case_when(Gruppenname == 'D1' ~ 'AfD',
                                Gruppenname == 'D2' ~ 'CDU',
                                Gruppenname == 'D3' ~ 'DIE LINKE',
                                Gruppenname == 'D4' ~ 'SPD',
                                Gruppenname == 'D5' ~ 'FDP',
                                Gruppenname == 'D6' ~ 'GRÜNE',
                                Gruppenname == 'D7' ~ 'Tierschutzpartei',
                                Gruppenname == 'D8' ~ 'Die PARTEI',
                                Gruppenname == 'D9' ~ 'NPD',
                                Gruppenname == 'D10' ~ 'FREIE WAHÄLER',
                                Gruppenname == 'D11' ~ 'PIRATEN',
                                Gruppenname == 'D12' ~ 'ÖDP',
                                Gruppenname == 'D13' ~ 'V-Partei',
                                Gruppenname == 'D14' ~ 'MLPD',
                                Gruppenname == 'D15' ~ 'dieBasis',
                                Gruppenname == 'D16' ~ 'CündnisC',
                                Gruppenname == 'D17' ~ 'III-Weg',
                                Gruppenname == 'D18' ~ 'DKP',
                                Gruppenname == 'D19' ~ 'Die Humanisten',
                                Gruppenname == 'D20' ~ 'Gesundheitsforschung',
                                Gruppenname == 'D21' ~ 'TeamTodenhöfer',
                                Gruppenname == 'D22' ~ 'Volt',
                                Gruppenname == 'D23' ~ 'EB1N',
                                Gruppenname == 'D24' ~ 'EB2N',
                                Gruppenname == 'D25' ~ 'EB3N',
                                Gruppenname == 'D26' ~ 'EB4N',
                                Gruppenname == 'D27' ~ 'EB1S',
                                Gruppenname == 'F1' ~ 'AfD',
                                Gruppenname == 'F2' ~ 'CDU',
                                Gruppenname == 'F3' ~ 'DIE LINKE',
                                Gruppenname == 'F4' ~ 'SPD',
                                Gruppenname == 'F5' ~ 'FDP',
                                Gruppenname == 'F6' ~ 'GRÜNE',
                                Gruppenname == 'F7' ~ 'Tierschutzpartei',
                                Gruppenname == 'F8' ~ 'Die PARTEI',
                                Gruppenname == 'F9' ~ 'NPD',
                                Gruppenname == 'F10' ~ 'FREIE WAHÄLER',
                                Gruppenname == 'F11' ~ 'PIRATEN',
                                Gruppenname == 'F12' ~ 'ÖDP',
                                Gruppenname == 'F13' ~ 'V-Partei',
                                Gruppenname == 'F14' ~ 'MLPD',
                                Gruppenname == 'F15' ~ 'dieBasis',
                                Gruppenname == 'F16' ~ 'CündnisC',
                                Gruppenname == 'F17' ~ 'III-Weg',
                                Gruppenname == 'F18' ~ 'DKP',
                                Gruppenname == 'F19' ~ 'Die Humanisten',
                                Gruppenname == 'F20' ~ 'Gesundheitsforschung',
                                Gruppenname == 'F21' ~ 'TeamTodenhöfer',
                                Gruppenname == 'F22' ~ 'Volt',
                                Gruppenname == 'F23' ~ 'EB1N',
                                Gruppenname == 'F24' ~ 'EB2N',
                                Gruppenname == 'F25' ~ 'EB3N',
                                Gruppenname == 'F26' ~ 'EB4N',
                                Gruppenname == 'F27' ~ 'EB1S',
                                TRUE ~ ""),
         Gebietsart = "Ortsteil" ,
         Gebietsname = case_when(Gebietsname == 'Zentrum' ~ 'Zentrum',
                                 Gebietsname == 'Zentrum-Ost' ~ 'Zentrum-Ost',
                                 Gebietsname == 'Zentrum-SÃ¼dost' ~ 'Zentrum-Südost',
                                 Gebietsname == 'Zentrum-SÃ¼d' ~ 'Zentrum Süd',
                                 Gebietsname == 'Zentrum-West' ~ 'Zentrum-West',
                                 Gebietsname == 'Zentrum-Nordwest' ~ 'Zentrum-Nordwest',
                                 Gebietsname == 'Zentrum-Nord' ~ 'Zentrum-Nord',
                                 Gebietsname == 'SchÃ¶nefeld-Abtnaundorf' ~ 'Schönefeld-Abtnaundorf',
                                 Gebietsname == 'SchÃ¶nefeld-Ost' ~ 'Schönefeld-Ost',
                                 Gebietsname == 'Mockau-SÃ¼d' ~ 'Mockau-Süd',
                                 Gebietsname == 'Mockau-Nord' ~ 'Mockau-Nord',
                                 Gebietsname == 'Thekla' ~ 'Thekla',
                                 Gebietsname == 'PlauÃŸig-Portitz' ~ 'Plaußig-Portitz',
                                 Gebietsname == 'Neustadt-NeuschÃ¶nefeld' ~ 'Neustadt-Neuschönefeld',
                                 Gebietsname == 'Volkmarsdorf' ~ 'Volksmarsdorf',
                                 Gebietsname == 'Anger-Crottendorf' ~ 'Anger-Crottendorf',
                                 Gebietsname == 'Sellerhausen-StÃ¼nz' ~ 'Sellerhausen-Stünz',
                                 Gebietsname == 'Paunsdorf' ~ 'Paunsdorf',
                                 Gebietsname == 'Heiterblick' ~ 'Heiterblick',
                                 Gebietsname == 'MÃ¶lkau' ~ 'Mölkau',
                                 Gebietsname == 'Engelsdorf' ~ 'Engelsdorf',
                                 Gebietsname == 'Baalsdorf' ~ 'Baalsdorf',
                                 Gebietsname == 'Althen-KleinpÃ¶sna' ~ 'Althen-Kleinpösna',
                                 Gebietsname == 'Reudnitz-Thonberg' ~ 'Reudnitz-Thonberg',
                                 Gebietsname == 'StÃ¶tteritz' ~ 'Stötteritz',
                                 Gebietsname == 'Probstheida' ~ 'Probstheida',
                                 Gebietsname == 'Meusdorf' ~ 'Meusdorf',
                                 Gebietsname == 'Liebertwolkwitz' ~ 'Liebertwolkwitz',
                                 Gebietsname == 'Holzhausen' ~ 'Holzhausen',
                                 Gebietsname == 'SÃ¼dvorstadt' ~ 'Südvorstadt',
                                 Gebietsname == 'Connewitz' ~ 'Connewitz',
                                 Gebietsname == 'Marienbrunn' ~ 'Marienbrunn',
                                 Gebietsname == 'LÃ¶ÃŸnig' ~ 'Lößnig',
                                 Gebietsname == 'DÃ¶litz-DÃ¶sen' ~ 'Dölitz-Dösen',
                                 Gebietsname == 'SchleuÃŸig' ~ 'Schleußig',
                                 Gebietsname == 'Plagwitz' ~ 'Plagwitz',
                                 Gebietsname == 'Kleinzschocher' ~ 'Kleinzschocher',
                                 Gebietsname == 'GroÃŸzschocher' ~ 'Großzschocher',
                                 Gebietsname == 'Knautkleeberg-Knauthain' ~ 'Knautkleeberg-Knauthain',
                                 Gebietsname == 'Hartmannsdorf-Knautnaundorf' ~ 'Hartmannsdorf-Knautnaundorf',
                                 Gebietsname == 'SchÃ¶nau' ~ 'Schönau',
                                 Gebietsname == 'GrÃ¼nau-Ost' ~ 'Grünau-Ost',
                                 Gebietsname == 'GrÃ¼nau-Mitte' ~ 'Grünau-Mitte',
                                 Gebietsname == 'GrÃ¼nau-Siedlung' ~ 'Grünau-Siedlung',
                                 Gebietsname == 'Lausen-GrÃ¼nau' ~ 'Lausen-Grünau',
                                 Gebietsname == 'GrÃ¼nau-Nord' ~ 'Grünau-Nord',
                                 Gebietsname == 'Miltitz' ~ 'Miltitz',
                                 Gebietsname == 'Lindenau' ~ 'Lindenau',
                                 Gebietsname == 'Altlindenau' ~ 'Altlindenau',
                                 Gebietsname == 'Neulindenau' ~ 'Neulindenau',
                                 Gebietsname == 'Leutzsch' ~ 'Leutzsch',
                                 Gebietsname == 'BÃ¶hlitz-Ehrenberg' ~ 'Böhlitz-Ehrenberg',
                                 Gebietsname == 'Burghausen-RÃ¼ckmarsdorf' ~ 'Burghausen-Rückmarsdorf',
                                 Gebietsname == 'MÃ¶ckern' ~ 'Möckern',
                                 Gebietsname == 'Wahren' ~ 'Wahren',
                                 Gebietsname == 'LÃ¼tzschena-Stahmeln' ~ 'Lützschena-Stahmeln',
                                 Gebietsname == 'Lindenthal' ~ 'Lindenthal',
                                 Gebietsname == 'Gohlis-SÃ¼d' ~ 'Gohlis-Süd',
                                 Gebietsname == 'Gohlis-Mitte' ~ 'Gohlis-Mitte',
                                 Gebietsname == 'Gohlis-Nord' ~ 'Gohlis-Nord',
                                 Gebietsname == 'Eutritzsch' ~ 'Eutritzsch',
                                 Gebietsname == 'Seehausen' ~ 'Seehausen',
                                 Gebietsname == 'Wiederitzsch' ~ 'Wiederitzsch',
                                 TRUE ~ ""),
         Gebietsnummer = case_when(Gebietsname == 'Zentrum' ~ '00',
                                   Gebietsname == 'Zentrum-Ost' ~ '01',
                                   Gebietsname == 'Zentrum-Südost' ~ '02',
                                   Gebietsname == 'Zentrum Süd' ~ '03',
                                   Gebietsname == 'Zentrum-West' ~ '04',
                                   Gebietsname == 'Zentrum-Nordwest' ~ '05',
                                   Gebietsname == 'Zentrum-Nord' ~ '06',
                                   Gebietsname == 'Schönefeld-Abtnaundorf' ~ '10',
                                   Gebietsname == 'Schönefeld-Ost' ~ '11',
                                   Gebietsname == 'Mockau-Süd' ~ '12',
                                   Gebietsname == 'Mockau-Nord' ~ '13',
                                   Gebietsname == 'Thekla' ~ '14',
                                   Gebietsname == 'Plaußig-Portitz' ~ '15',
                                   Gebietsname == 'Neustadt-Neuschönefeld' ~ '20',
                                   Gebietsname == 'Volksmarsdorf' ~ '21',
                                   Gebietsname == 'Anger-Crottendorf' ~ '22',
                                   Gebietsname == 'Sellerhausen-Stünz' ~ '23',
                                   Gebietsname == 'Paunsdorf' ~ '24',
                                   Gebietsname == 'Heiterblick' ~ '25',
                                   Gebietsname == 'Mölkau' ~ '26',
                                   Gebietsname == 'Engelsdorf' ~ '27',
                                   Gebietsname == 'Baalsdorf' ~ '28',
                                   Gebietsname == 'Althen-Kleinpösna' ~ '29',
                                   Gebietsname == 'Reudnitz-Thonberg' ~ '30',
                                   Gebietsname == 'Stötteritz' ~ '31',
                                   Gebietsname == 'Probstheida' ~ '32',
                                   Gebietsname == 'Meusdorf' ~ '33',
                                   Gebietsname == 'Liebertwolkwitz' ~ '34',
                                   Gebietsname == 'Holzhausen' ~ '35',
                                   Gebietsname == 'Südvorstadt' ~ '40',
                                   Gebietsname == 'Connewitz' ~ '41',
                                   Gebietsname == 'Marienbrunn' ~ '42',
                                   Gebietsname == 'Lößnig' ~ '43',
                                   Gebietsname == 'Dölitz-Dösen' ~ '44',
                                   Gebietsname == 'Schleußig' ~ '50',
                                   Gebietsname == 'Plagwitz' ~ '51',
                                   Gebietsname == 'Kleinzschocher' ~ '52',
                                   Gebietsname == 'Großzschocher' ~ '53',
                                   Gebietsname == 'Knautkleeberg-Knauthain' ~ '54',
                                   Gebietsname == 'Hartmannsdorf-Knautnaundorf' ~ '55',
                                   Gebietsname == 'Schönau' ~ '60',
                                   Gebietsname == 'Grünau-Ost' ~ '61',
                                   Gebietsname == 'Grünau-Mitte' ~ '62',
                                   Gebietsname == 'Grünau-Siedlung' ~ '63',
                                   Gebietsname == 'Lausen-Grünau' ~ '64',
                                   Gebietsname == 'Grünau-Nord' ~ '65',
                                   Gebietsname == 'Miltitz' ~ '66',
                                   Gebietsname == 'Lindenau' ~ '70',
                                   Gebietsname == 'Altlindenau' ~ '71',
                                   Gebietsname == 'Neulindenau' ~ '72',
                                   Gebietsname == 'Leutzsch' ~ '73',
                                   Gebietsname == 'Böhlitz-Ehrenberg' ~ '74',
                                   Gebietsname == 'Burghausen-Rückmarsdorf' ~ '75',
                                   Gebietsname == 'Möckern' ~ '80',
                                   Gebietsname == 'Wahren' ~ '81',
                                   Gebietsname == 'Lützschena-Stahmeln' ~ '82',
                                   Gebietsname == 'Lindenthal' ~ '83',
                                   Gebietsname == 'Gohlis-Süd' ~ '90',
                                   Gebietsname == 'Gohlis-Mitte' ~ '91',
                                   Gebietsname == 'Gohlis-Nord' ~ '92',
                                   Gebietsname == 'Eutritzsch' ~ '93',
                                   Gebietsname == 'Seehausen' ~ '94',
                                   Gebietsname == 'Wiederitzsch' ~ '95',
                                   TRUE ~ "") #,
#         stadtbezirknr = substr(Gebietsnummer,1,1),
#         stadtbezirk = case_when(stadtbezirknr == 0 ~ "Mitte",
#                                 stadtbezirknr == 1 ~ "Nordost",
#                                 stadtbezirknr == 2 ~ "Ost",
#                                 stadtbezirknr == 3 ~ "Südost",
#                                 stadtbezirknr == 4 ~ "Süd",
#                                 stadtbezirknr == 5 ~ "Südwest",
#                                 stadtbezirknr == 6 ~ "West",
#                                 stadtbezirknr == 7 ~ "Alt-West",
#                                 stadtbezirknr == 8 ~ "Nordwest",
#                                 stadtbezirknr == 9 ~ "Nord",
#                                 TRUE ~ "")
         ) %>%
  group_by(Gebietsname,Stimme) %>%
  mutate(Prozent = Anzahl/sum(Anzahl,na.rm = TRUE)*100) %>%
  ungroup() %>%
  group_by(Gebietsnummer,Stimme) %>%  # identifiziere pro Wahlkreis und Stimme (Erst, Zweit)
  mutate(Rang = rank(desc(Anzahl), ties.method = "first")) %>%
  select(Gebietsart,everything())
  #select(Gebietsart,stadtbezirknr,stadtbezirk, everything())



# Datensatz Wahlkreisgewinner ---------------------------------------------

BTW21LRang <- BTW21LParteien %>%   # den Rang nach Stimmenanzahl
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

BTW21Lwk_shapes <- readOGR( 
  dsn= "Daten/Leipzig" , 
  layer="ot",
  verbose=FALSE) %>% 
  st_as_sf() %>%
  st_transform(4326) %>%
  #rename(Gebietsnummer = OT) %>%
  mutate(WKR_NR = Gebietsnummer,
         stadtbezirknr = substr(Gebietsnummer,1,1),
         stadtbezirk = case_when(stadtbezirknr == 0 ~ "Mitte",
                                 stadtbezirknr == 1 ~ "Nordost",
                                 stadtbezirknr == 2 ~ "Ost",
                                 stadtbezirknr == 3 ~ "Südost",
                                 stadtbezirknr == 4 ~ "Süd",
                                 stadtbezirknr == 5 ~ "Südwest",
                                 stadtbezirknr == 6 ~ "West",
                                 stadtbezirknr == 7 ~ "Alt-West",
                                 stadtbezirknr == 8 ~ "Nordwest",
                                 stadtbezirknr == 9 ~ "Nord",
                                 TRUE ~ "")
         ) %>%
  #select(stadtbezirknr,stadtbezirk, WKR_NR,everything()) %>% 
  select(WKR_NR,everything()) %>% 
  as_Spatial()
class(BTW21Lwk_shapes)

names(BTW21Lwk_shapes)


# abspeichern -------------------------------------------------------------

save(BTW21LParteien,BTW21LRang,BTW21Lwk_shapes, file= "Daten/BTW21Leipzig.RData")  

names(BTW21LParteien)
names(BTW21JParteien)


names(BTW21LRang)
names(BTW21JRang)

names(BTW21Lwk_shapes)
names(BTW21Jwk_shapes)

  
  
# Test only  
#  filter(Gebietsnummer == 1 & Stimme == 1)

