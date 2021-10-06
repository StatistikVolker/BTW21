#merge WK Data to geometric data
btw1st_spdf <- merge(BTW21wk_shapesfile, BTW21Rang %>% filter(Stimme == 1), by.x="WKR_NR", by.y="Gebietsnummer")
btw2st_spdf <- merge(BTW21wk_shapesfile, BTW21Rang %>% filter(Stimme == 2), by.x="WKR_NR", by.y="Gebietsnummer")


# Allgemein:
shapedf <- BTW21wk_shapesfile # (bzw. BTW21Jwk_shapesfile, BTW21Lwk_shapesfile - J,L Jena, Leipzig)
rangdf <-BTW21Rang            # (bzw. BTW21JRang,BTW21LRang - J,L Jena, Leipzig)
vote <- 1 # (oder 2)
btw21stimme <- merge(shapedf, Rangdf %>% filter(Stimme == vote), by.x="WKR_NR", by.y="Gebietsnummer")