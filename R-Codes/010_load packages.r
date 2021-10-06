## 010_load packages.r

library(tidyverse) # R-Welt
library(readr) # ladt csv-dateien
library(rgdal) # lädt Shapefiles
library(sp) # für Geoaten
library(sf) # simple feature Geodaten
library(leaflet) # für Karten
library(cartogram) # Verzerrte Karten
#library(mapview) # zum Abspeichern von statischen Karten
library(htmlwidgets) # zum Abspeichern von dynamischen Karten

library(plotly) # Interactive Plots
library(rgeos) 
library(scales)
library(flexdashboard)

#library(Rdatacheck)


# Muss in Paket Rdatacheck integriert werden
mkr.formatwithdigits<-function(val,digits=3){
  fval<-rep(NA,length(val))
  for(i in (1:length(val))){
    fval[i]<-formatC(val[i],digits=digits,format="f")
  }
  return(fval)
}

