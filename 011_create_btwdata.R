# 011_create_brwdata.R


BTW21all <- read.csv("Daten/btw21_kerg2.csv",skip = 9,sep = "\t")

#names(BTW21)
BTW21BUND <- BTW21all %>% filter(Gebietsart == "Bund")


BTW21Parteien <- BTW21all %>%
  filter(Gebietsart == "Wahlkreis")  %>% # nur Wahlkreise
  #droplevels() %>%
  filter(Gruppenart != "System-Gruppe") %>% # nicht gültige ungütige Stimmen
  droplevels() %>%
  mutate(Prozent = as.numeric(str_replace(Prozent,",","."))) %>% # Prozentzahl mit R Dezimalpunkt 
  group_by(Gebietsnummer,Stimme) %>%  # identifiziere pro Wahlkreis und Stimme (Erst, Zweit)
  mutate(Rang = rank(desc(Anzahl), ties.method = "first"))

BTW21 <- BTW21Parteien %>%   # den Rang nach Stimmenanzahl
  filter(Rang < 3)  %>% # nur Platz 1 und 2
  arrange(Gebietsnummer,Stimme, Rang) %>% # nach Rang sortieren
  select(Gebietsnummer,Gebietsname,Gruppenname,Rang,Stimme,Anzahl,Prozent) %>% # nur interessierende Spalten
  pivot_wider(id_cols = c(Gebietsnummer,Gebietsname,Stimme),  # Pivotoere Tabelle um einfacher Differenzen zu bilden
              names_from = Rang, 
              values_from = c(Gruppenname,Anzahl,Prozent)) %>%
  mutate(Anzahl_Diff = Anzahl_1 - Anzahl_2,    # Differenz zwischen Platz 1 und 2 nach Stimmen
         Prozent_Diff = Prozent_1 - Prozent_2) # Differenz nach Prozent 


  
  
  
# Test only  
#  filter(Gebietsnummer == 1 & Stimme == 1)

