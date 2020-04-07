########## Nuova sessione: telerilevamento ###########
#installa e carica pacchetti
install.packages()
library()

#stabilire la cartella di lavoro
setwd("/Users/jen/Desktop/lab")

#importare immagine satellitare
p224r63_2011 <- brick("p224r63_2011_masked.grd", header = TRUE)

#grafico dell'immagine
plot(p224r63_2011) #si vede un paesaggio in 7 bande, ognuna corrispondente a diversi sensori connessi a diversi colori 
