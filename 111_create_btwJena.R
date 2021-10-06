# 111_create_btwJena.R

require(readxl)
library(stringr)
BTW21Jall <- read_xlsx("Daten/Jena/BW21191E_VH.xlsx",skip = 13)

names(BTW21Jall)
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

names(BTW21JParteien)

names(table(BTW21JParteien$Gebietsnummer))

BTW21J2nd <- BTW21Jall %>% select(WBZnr,WBZName,Wahlberechtigte,Wähler,Wahlbeteiligung,starts_with("1st"))



BTW21J <- BTW21JParteien %>%   # den Rang nach Stimmenanzahl
  filter(Rang < 3)  %>% # nur Platz 1 und 2
  mutate(Gruppenname = factor(Gruppenname,levels = c("AfD","CDU","DIE LINKE","FDP","GRÜNE","SPD"))) %>%
  arrange(Gebietsnummer,Stimme, Rang) %>% # nach Rang sortieren
  select(Gebietsnummer,Gebietsname,Gruppenname,Rang,Stimme,Anzahl,Prozent) %>% # nur interessierende Spalten
  pivot_wider(id_cols = c(Gebietsnummer,Gebietsname,Stimme),  # Pivotoere Tabelle um einfacher Differenzen zu bilden
              names_from = Rang, 
              values_from = c(Gruppenname,Anzahl,Prozent)) %>%
  mutate(Anzahl_Diff = Anzahl_1 - Anzahl_2,    # Differenz zwischen Platz 1 und 2 nach Stimmen
         Prozent_Diff = Prozent_1 - Prozent_2) # Differenz nach Prozent 


table(BTW21J$Gruppenname_2)

  
  
  
# Test only  
#  filter(Gebietsnummer == 1 & Stimme == 1)

