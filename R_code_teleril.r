########## Nuova sessione: telerilevamento ###########
#installare e caricare pacchetti
install.packages()
library()

#stabilire la cartella di lavoro
setwd("/Users/jen/Desktop/lab")

#importare immagine satellitare
p224r63_2011 <- brick("p224r63_2011_masked.grd", header = TRUE)

#grafico dell'immagine
plot(p224r63_2011) #si vede un paesaggio in 7 bande, ognuna corrispondente a diversi sensori connessi a diversi colori 
cl <- colorRampPalette(c("black","grey","light grey"))(100) #il numero si riferisce al numero di gamme di colore
plot(p224r63_2011, col=cl) #nero=pixel con bassa riflettanza nel colore;grigio chiaro=pixel con alta riflettanza nel colore

#visualizzare le diverse bande
names(p224r63_2011) #B1:blue-B2:green-B3:red-B4:near infrared-B5:medium infrared-B6:thermal infrared-B7:medium infrared

#visualizzare la banda del blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)  
#siccome la funzione attach() non funziona con il pacchetto raster
#si deve utilizzare il $ per collegare la colonna al dataset
plot(p224r63_2011$B1_sre, col=clb) #la parte colorata è relativa ai valori della riflettanza di questo colore
#valori molto alti=molte piante

#esercizio: grafico della banda dell'infrarosso vicino con colori che variano dal rosso al giallo
cliv <- colorRampPalette(c("red","orange","yellow"))(100)  
plot(p224r63_2011$B4_sre, col=cliv)

#grafico multiframe
par(mfrow=c(2,2))

#blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre, col=clb) 

#verde
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre, col=clg) 

#rosso
clr <- colorRampPalette(c("dark red","red","pink"))(100)
plot(p224r63_2011$B3_sre, col=clr) 

#nir
cliv <- colorRampPalette(c("red","orange","yellow"))(100)  
plot(p224r63_2011$B4_sre, col=cliv)

#chiudere la finestra grafica e quindi la successione di immagini 
dev.off()

#vedere l'immagine come la vedrebbe l'occhio umano
#ad ogni componente RGB si associa una banda del database
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") #stretch dissocia le sfumature di colori
#quindi qui si nota come la parte scura corrisponda alla parte forestata

#vedere l'immagine con false colours
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
#tutte le zone dove c'è una pianta sono ora colorate di rosso perchè le piante riflettono 
#molto l'infrarosso che in questo caso è montato sul rosso mentre il suolo nudo risulta celeste, 
#circondato da zone agricole visualizzate in rosa

#multiframe dei due grafici
par(mfrow=c(1,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

#esercizio: nir nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") 

########### Nuova sessione: progressione telerilevamento #############

#importare altra immagine satellitare
p224r63_1988 <- brick("p224r63_1988_masked.grd", header = TRUE)

#grafico multiframe
par(mfrow=c(2,2))

#blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_1988$B1_sre, col=clb) 

#verde
clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_1988$B2_sre, col=clg) 

#rosso
clr <- colorRampPalette(c("dark red","red","pink"))(100)
plot(p224r63_1988$B3_sre, col=clr) 

#nir
cliv <- colorRampPalette(c("red","orange","yellow"))(100)  
plot(p224r63_1988$B4_sre, col=cliv)
#chiudere sequenza grafica
dev.off()

#vedere l'immagine con false colours
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin") 

#esercizio: nir nella componente rossa
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

#multiframe dei due grafici dello stesso tipo di cui uno riferito al 1988 e l'altro al 2011
par(mfrow=c(1,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
#chiudere sequenza grafica
dev.off()

#indice di vegetazione per il 1988, per verificare le condizioni effettive della vegetazione
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
#grafico dell'indice per il 1988
plot(dvi1988)

#esercizio:indice di vegetazione per il 2011
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
#grafico dell'indice per il 2011
plot(dvi2011)

#differenza nel tempo tra i due indici
difdvi <- dvi2011 - dvi1988
#grafico di questa differenza
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi)
#le piante che stanno meglio sono nel colore blu e viceversa nel colore rosso, 
#mentre quelle stabili nel colore bianco

#multiframe tra 1988, 2011 e l'indice di vegetazione
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
#chiudere sequenza grafica
dev.off()

#cambiare la risoluzione dei grafici
p224r63_2011lr <- aggregate(p224r63_2011, fact=10) #immagine con pixel di dieci volte più grande
p224r63_2011
p224r63_2011lr

#multiframe tra immagini ad alta e a bassa risoluzione
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
#chiudere sequenza grafica
dev.off()

#cambiare la risoluzione: ancora più bassa
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50

#multiframe tra le immagini a diverse risoluzioni
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")
#chiudere sequenza grafica
dev.off()

#indice di vegetazione per il 2011 ad una risoluzione peggiore
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
#grafico dell'indice
plot(dvi2011lr50)

#cambiare la risoluzione per il 1988
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
#indice di vegetazione per il 1988 ad una risoluzione peggiore
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

#indice di vegetazione
difdvilr50 <- dvi2011lr50 - dvi1988lr50
#grafico dell'indice
plot(difdvilr50,col=cldifdvi)

#confronto tra immagine finale a bassa e ad alta risoluzione
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)


