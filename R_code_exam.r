### R code first

install.packages("sp")

library(sp) #require(sp) è un altro comando

#importo dataset
data("meuse")
head(meuse) #per visualizzare le prime righe, molto potente
names(meuse) #altra funzione interessante, stavolta per visualizzare nomi variabili

#statistiche di base
summary(meuse)

#grafico sulle correlazioni tra tutte le variabili 
pairs(meuse) #ci saranno zone più inquinate in base al contenuto in metalli
pairs(~ cadmium + copper + lead , data = meuse) #tilde significa uguale mentre la virgola è un separatore degli argomenti in funzione

#primo esercizio, aggiungi alla funzione precedente zinco
pairs(~ cadmium + copper + lead + zinc, data = meuse)

#metodo per fare un subset di undatabase selezionando solo alcune V
pairs(meuse[,3:6]) #dove i numeri si riferiscon alle colonne e le parentesi quadre indicano il subset 
pairs(meuse[,3:6], col="green") #dove si aggiunge una funzione relativa al colore
pairs(meuse[,3:6], col="green", pch=19) #pch=point character per cambiare il carattere dei punti, in questo caso i punti si colorano
pairs(meuse[,3:6], col="green", pch=19, cex=0.5) #per cambiare la dimensione dei punti
pairs(meuse[,3:6], col="green", pch=19, cex=0.5, main="Primo pairs") #per aggiungere un titolo al grafico

#secondo esercizio, aggiungi al grafico elevation
pairs(meuse[,3:7], col="green", pch=19, cex=0.5, main="Primo pairs")

#per utilizzare una funzione/codice esterna/o
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
} #x e y indica qualsiasi due variabili

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                             cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}#lowess è uno smoother locale 

panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms) 
#lower.panel indica la parte inferiore del grafico per cui qui si decide di inserire la funzione relativa alle correlazioni
#upper.panel indica la parte superiore del grafico per cui qui si decide di inserire la funzione relativa allo smoothing
#diag.panel indica la parte diagonale per cui si inserisce la funzione relativa agli istogrammi

#terzo esercizio, scambia la disposizione delle funzioni tra upper e lower
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms) 
 
#altra funzione per produrre grafici
plot(meuse$cadmium, meuse$copper) #il dollaro serve per collegare la colonna col proprio dataset

attach(meuse) #funzione per inserire il dataset all'interno di R se non già fatto e spiega a R 
#che utilizzeremo sempre questo dataset per tutte le funzioni da qui in avanti

#quindi rifacendo il plot senza il dollaro ora vedrai che funzionerà comunque grazie ad "attach"
plot(cadmium, copper, pch=17) 
plot(cadmium, copper, pch=17, xlab="cadmio", ylab="rame") #per cambiare i nomi relativi agli assi
plot(cadmium, copper, pch=17, xlab="cadmio", ylab="rame", cex.lab=2) #per cambiare la grandezza dei nomi relativi agli assi

########################################################

### R code spatial

#caricare pacchetto sp
library(sp)

#richiamare dati caricati in precedenza
data(meuse)

#breve riepilogo delle operazioni effettuate in precedenza

#allegare il dataframe per poi disegnare grafici sul rapporto cadmio/piombo
attach(meuse)
plot(cadmium, lead, col="brown", pch=8, cex=2)

#esercizio: grafico sul rapporto tra rame e zinco con come simbolo il triangolo e come colore il verde
plot(copper, zinc, col="darkgreen", pch=17, cex=2)

#cambiare le etichette
plot(copper, zinc, col="darkgreen", pch=17, cex=2, xlab="rame", ylab="zinco") #ciò che è relazionato ad un testo va messo sotto virgolette

#inserire più di un grafico all'interno della stessa finestra-> funzione IMPORTANTISSIMA!
par(mfrow=c(1,2)) #c si usa quando c'è una serie di numeri o caratteri
plot(cadmium, lead, col="brown", pch=8, cex=2)
plot(copper, zinc, col="darkgreen", pch=17, cex=2)

#invertire i grafici per riga e colonna 
par(mfrow=c(2,1)) 
plot(cadmium, lead, col="brown", pch=8, cex=2)
plot(copper, zinc, col="darkgreen", pch=17, cex=2)

#multiframe automatico 
install.packages("GGally") #siccome lo installiamo dall'esterno ci vogliono le virgolette
library(GGally)

#grafico di correlazioni
ggpairs(meuse[,3:6]) #la virgola significa "a partire da"
ggpairs #mostra come è stata calcolata la funzione

########### Analisi spaziale ###########
head(meuse) #per visualizzare le coordinate, in questo caso sotto il nome di x e y
coordinates(meuse)=~x+y

#grafico con le coordinate
plot(meuse)

#grafico spaziale, per analizzare la variazione delle variabili nello spazio
spplot(meuse,"zinc") #questo grafico mostra i valori della variabile scelta suddivisi in classi
#in relazione alla loro posizione nello spazio

########### Nuova sessione spaziale ###########
#caricare pacchetto sp
library(sp)

#caricare dati
data(meuse)

#inserire coordinate del dataset 
coordinates(meuse)=~x+y

#grafico spaziale
spplot(meuse,"zinc")

#esercizio: ssplot con rame
spplot(meuse,"copper")

#funzione bubble
bubble(meuse,"zinc") #stessa rappresentazione di prima ma con infografico per cui le concentrazioni 
#dell'elemento son rappresentate in funzione della dimensione delle bolle

#esercizio: bubble con rame ma con bolle rosse
bubble(meuse,"copper",col="red")

###########################################################

### R code spatial 2 (da confrontare per suddivisione con l'altro spatial)

########### Creazione oggetti ###########
#creazione array per foraminiferi 
foram <- c(10,20,35,55,67,80)

#creazione array per Ca capture
carbon <- c(5,15,30,70,85,99)

#grafico sulla relazione tra i due vettori come variabili 
plot(foram, carbon, col="cornflowerblue", cex=2, pch=19)

#################################################

### R code point pattern 

#installare pacchetti
install.packages("ggplot2")
install.packages("spatstat")
install.packages("rgdal")

#carica pacchetti 
library(ggplot2)
library(spatstat)
library(rgdal)

#stabilire la cartella di lavoro
setwd("/Users/jen/Desktop/lab")

#importare la tabella dati
covid <- read.table("covid_agg.csv", header = TRUE)

#lista files salvati nelle sessioni precedenti
ls()

#visualizzare le prime righe del dataset
head(covid)

#versioni del grafico del numero di infetti per paese differenti per posizione delle etichette
attach(covid)
plot(country, cases, las=0) #le etichette son parallele agli assi
plot(country, cases, las=1) #le etichette sono orizzontali
plot(country, cases, las=2) #le etichette son perpendicolari
plot(country, cases, las=3) #le etichette son tutte verticali

#grafico del numero di infetti per paese definitivo
plot(country, cases, las=3, cex.lab=0.5, cex.axis=0.5)

#esempi delle possibilità di ggplot2 sul dataset mpg
data(mpg)
head(mpg)

#grafico esempio con il dataset mpg
ggplot(mpg, aes(x=displ,y=hwy)) + geom_point() #aes sono le variabili che comporanno il grafico
ggplot(mpg, aes(x=displ,y=hwy)) + geom_line() #cambio del tipo di geometria

#applicazione di ggplot al dataset covid
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()

#creare il dataset per l'applicazione di spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))

#funzione di densità
d <- density(covids)

#grafico della mappa di densità
plot(d)
points(covids) #mostra i paesi sopra la mappa della densità

#cambiare la palette di colori del  grafico
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100) #quest'ultima parentesi si riferisce al numero delle gradazioni di colori
plot(d, col=cl)
 
#esercizio: mappa di densità dal verde al blu
cl2 <- colorRampPalette(c('darkgreen', 'lightgreen', 'blue')) (100) 
plot(d, col=cl2)

#carica shapefile
coastlines <- readOGR("ne_10m_coastline.shp")

#aggiungere il nuovo dataset al grafico precedente
plot(coastlines, add=T) #questa mappa rappresenta quanto densi sono i punti nel mondo e si nota
#la d massima registrata in Febbraio è localizzata in Europa

#esercizio: mappa di densità con nuova pallette e aggiunta delle coastlines
cl3 <- colorRampPalette(c('darkcyan', 'purple', 'red')) (200) 
plot(d, col=cl3)
points(covids)
plot(coastlines, add=T) 

#guardare il dataset
head(covid)

#associare i valori dei point patterns alla funzione cases
marks(covids) <-covid$cases

#interpolazione dei valori
s <- Smooth(covids)

#mappa dei casi
plot(s)

#esercizio: mappa di interpolazione del numero dei casi relativo alle coste
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)
#densità più alta in Europa ma una stima dei casi più alta in Asia in quel periodo

#mappa finale di tutti i diversi grafici finora visualizzati
par(mfrow=c(2,1))
#mappa di densità dei punti 
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)
#mappa di interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)
#chiudi la sequenza grafica 
dev.off()

#caricare il nuovo dataset
load("Tesi.RData")

#visualizzare il dataset 
head(Tesi)
attach(Tesi)

#visualizzare le statistiche per individuare i valori minimi e massimi
summary(Tesi) 
#x varia da 12.42 a 12.46
#y varia da 43.91 a 43.94

#creare il dataset per l'applicazione spatstat
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9,43.95))
#ossia point pattern: x,y,c(xmin, xmax), c(ymin, ymax), e ricorda di arrontare per eccesso i margini

#funzione di densità per individuare quanto son densi i punti nello spazioe
dT <- density(Tesippp)

#grafico di densità
plot(dT)
points(Tesippp)

# Esercizio: identifica i dati precedentemente creati
ls()
#dT = funzione di densità; Tesi = dataset ; Tesippp = point pattern 

#associare ad ogni punto spaziale il valore della variabile da interpolare, in questo caso la ricchezza specifica
marks(Tesippp) <- Tesi$Species_richness

#interpolazione
interpol <- Smooth(Tesippp)

#grafico della mappa con interpolazione
plot(interpol)
points(Tesippp, col="green") #si può notare come i valori più bassi di ricchezza specifica siano nella parte a N e S-O mentre i valori più alti nella parte ad O e S-E

#aggiungere il confine di San Marino
sanmarino <- readOGR("San_Marino.shp")

#aggiungere San Marino alla mappa di interpolazione
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp, col="green")
plot(sanmarino, add=T)

# Esercizio: grafico multiframe di densità e interpolazione 
par(mfrow=c(2,1))
plot(dT)
points(Tesippp, col="green")
plot(interpol)
points(Tesippp, col="green")
dev.off()

#####################################################

### R code teleril

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

#############################################################

### R code landcover

#scaricare pacchetto
install.packages("RStoolbox")

#caricare pacchetti
library(raster)
library(RStoolbox)

#stabilire la cartella di lavoro
setwd("/Users/jen/Desktop/lab") 

#caricare immagine
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#grafico RGB, dove le bande del landsat sono:1=b, 2=v, 3=r, 4=nir
plotRGB(p224r63_2011, r=4, g=2, b=2, stretch="Lin") 

#classificazione tramite algoritmo
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

#grafico della mappa definita dall'algoritmo
plot(p224r63_2011c$map)

#cambiare i colori della mappa
clclass <- colorRampPalette(c('blue', 'red', 'green', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

#classificazione tramite algoritmo alternativa 
p224r63_2011c2 <- unsuperClass(p224r63_2011, nClasses=2)

#cambiare i colori della mappa alternativa
clclass2 <- colorRampPalette(c('red', 'blue', 'black', 'green'))(100)
plot(p224r63_2011c2$map, col=clclass2)
#si può qui notare come in funzione del numero di classi aumenta l'incertezza automatico di classificazione

#############################################################

### R code multitemp

#caricare pacchetti
library(sp)
library(raster)
library(RStoolbox)
library(ggplot2)

#impostare la directory
setwd("/Users/jen/Desktop/lab")

#caricare i dati
defor1 <- brick("defor1_.jpg.png")
defor2 <- brick("defor2_.jpg.png")

#visualizzare immagine 1
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

defor1
#nomi: defor1_.1, defor1_.2, defor1_.3
#defor1_.1 = NIR, defor1_.2 = rosso, defor1_.3 = verde

#visualizzare immagine 2
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin") #parte più utilizzata dall'uomo

defor2

#visualizzare immagini insieme
par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")
dev.off()

#classificazione non supervisionata di defor1 (ossia non conferiamo dei training set al computer cioè non gli indichiamo cosa è foresta e cosa non è foresta)
d1c <- unsuperClass(defor1, nClasses=2)
d1c

#grafico della classficazione
par(mfrow=c(2,1))
plot(d1c$map)
cl <- colorRampPalette(c('black', 'green'))(100)
plot(d1c$map, col=cl)

#classificazione non supervisionata di defor1 
d2c <- unsuperClass(defor2, nClasses=2)
d2c

#grafico della classficazione
par(mfrow=c(2,1))
plot(d2c$map)
cl <- colorRampPalette(c('black', 'green'))(100)
plot(d2c$map, col=cl)
dev.off()

#confrontare le due mappe 
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
#si può notare come la zona deforestata si stia ingrandendo
dev.off()

#calcolare la frequenza dei pixel differenziati per classe in defor1
freq(d1c$map) #dove classe1=aree aperte e classe2=aree forestate

#calcolare la proporzione dei pixel differenziati per classe in defor1
totd1 <- 35239 + 306053 #genera il totale dei numero di pixel nella mappa
totd1
percent1 <- freq(d1c$map) * 100/totd1
percent1

#calcolare la frequenza dei pixel differenziati per classe in defor2
freq(d2c$map) #dove classe1=aree aperte e classe2=aree forestate

#calcolare la proporzione dei pixel differenziati per classe in defor2
totd2 <- 164095 + 178631 #genera il totale dei numero di pixel nella mappa
totd2
percent2 <- freq(d2c$map) * 100/totd2
percent2

#creare un dataframe generale per le due mappe e le corrispettive frequenze
cover <- c("Agriculture", "Forest")
before <- c(10.3, 89.7)
after <- c(47.9, 52.1)
output <- data.frame(cover, before, after)
View(output)

########### Nuova sessione ###########

#installare pacchetti
install.packages("gridExtra")

#caricare pacchetti
library(gridExtra)

#impostare la directory
setwd("/Users/jen/Desktop/lab")

#caricare dati
load("defor.RData")

#grafico sulla % di copertura forestale prima della deforestazione
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white") #fill si riferisce al colore da utilizzare per l'interno delle barre

#Esercizio: grafico sulla % di copertura forestale dopo la deforestazione
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white") 

#unire i grafici all'interno di un'unica finestra grafica
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white") + ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white") + ylim(0, 100)

grid.arrange(grafico1, grafico2, nrow=1) 

#################################################

### R code multitemp NO2

#caricare pacchetti
library(sp)
library(raster)

#impostare la directory
setwd("/Users/jen/Desktop/lab")

#caricare le prima immagine
EN01 <- raster("EN_0001.png") #utilizziamo solo una banda, quella riguardante l'azoto

#visualizzare l'immagine come grafico
plot(EN01)

#caricare il resto delle immagini
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

#cambiare la palette di colori per i grafici
cl <- colorRampPalette(c('red', 'orange', 'yellow')) (100) 

#visualizzare il grafico iniziale e finale insieme
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)
dev.off()

#effettuare la differenza tra i valori di NO2 delle due situazioni temporali
difno2 <- EN13 - EN01
cldif<- colorRampPalette(c('blue', 'black', 'yellow')) (100)
plot(difno2, col=cldif) #zone gialle=alta differenza mentre zone blu=bassa differenza

#visualizzare simultaneamente in modo manuale tutte le diverse immagini
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)
dev.off()

############ Nuova sessione ###########

#caricare pacchetti
library(sp)
library(raster)

#impostare la directory
setwd("/Users/jen/Desktop/lab/esa_no2")

#importare tutte le immagini in una volta sola le quali però devono essere immesse entro una unica cartella 
rlist <- list.files(pattern=".png", full.names=T) #creare lista delle immagini
rlist
listafinale <- lapply(rlist, raster) #associare la lista alla funzione raster e quindi importarla
listafinale

#unire tutte le immagini in un'unica immagine
EN <- stack(listafinale)

#visualizzare i grafici di tutte le immagini
cldif<- colorRampPalette(c('red', 'orange', 'yellow')) (100)
plot(EN, col=cl)

########## Nuova sessione ##########

#caricare pacchetti
library(sp)
library(raster)

#impostare la directory
setwd("/Users/jen/Desktop/lab/esa_no2")

#importare tutte le immagini in una volta sola le quali però devono essere immesse entro una unica cartella 
rlist <- list.files(pattern=".png", full.names=T) #creare lista delle immagini
rlist
listafinale <- lapply(rlist, raster) #associare la lista alla funzione raster e quindi importarla
listafinale

#unire tutte le immagini in un'unica immagine
EN <- stack(listafinale)

#differenza tra i valori di NO2 delle due situazioni temporali (immagine finale e immagine iniziale)
difEN <- EN$EN_0013 - EN$EN_0001

#grafici 
cld <- colorRampPalette(c('blue', 'white', 'red')) (100)
plot(difEN, col=cld) 

#grafico boxplot
boxplot(EN,horizontal=T, outline=F) 
#il cambiamento più grande che ci è stato è sui massimi valori; in generale passando da il primo anno al secondo vi è una diminuzione dei valori massimi degli ossidi di azoto atmosferico

################################################################

### R code snow

#impostare la directory
setwd("/Users/jen/Desktop/lab")

#installare pacchetti
install.packages("ncdf4")

#caricare pacchetti
library(raster)
library(sp)
library(ncdf4)

#importare le immagini
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

#grafico delle immagini sulla copertura in neve
cl <- colorRampPalette(c('darkblue', 'blue', 'light blue'))(100)
plot(snowmay, col=cl)

########### Nuova importazione ###########
#impostare la directory
setwd("/Users/jen/Desktop/lab/snow")

#importare un nuovo set di dati
rlist <- list.files(pattern=".tif")
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

#grafico dei dati importati
plot(snow.multitemp, col=cl)

#confrontare situazioni temporali
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))
dev.off()

#grafico della differenza tra le due situazioni temporali
difsnow=snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue', 'white', 'red'))(100)
#rosso=differenza più alta; blu=differenza più bassa
plot(difsnow, col=cldiff)

#modello previsionale (scenario) per stimare il valore per il 2025
source("prediction.r") #source serve per caricare un codice già pronto dall'esterno 
#interrompere il comando
^C

#visualizzare la mappa della previsione (in questo caso abbiamo interrotto il comando e importiamo un nuovo raster già pronto)
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)

#######################################################

### R code patches

#impostare la directory
setwd("/Users/jen/Desktop/lab")

#installare libreria
install.packages("igraph")

#caricare librerie
library(sp)
library(raster)
library(igraph)
library(ggplot2)

#caricare le immagini
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

#grafico di comparazione tra le due situazioni temporali
cl <- colorRampPalette(c('black', 'green'))(100)
par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d2c, col=cl) #la foresta è la classe numero due mentre la zona coltivata è la classe numero uno
dev.off()

#estrarre solamente la zona forestata per la prima immagine
d1c.for <- reclassify(d1c, cbind(1, NA))  

#grafico di comparazione tra la situazione con solo la zona forestata e quella con entrambe le zone 
cl <- colorRampPalette(c('black', 'green'))(100)
par(mfrow=c(1,2))
plot(d1c.for, col=cl)
plot(d1c, col=cl) 
dev.off()

#estrarre solamente la zona forestata per la seconda immagine
d2c.for <- reclassify(d2c, cbind(1, NA))

#grafico di comparazione tra le due situazioni temporali con solo la zona forestata  
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)
dev.off()

#creare patch
d1c.for.patches <- clump(d1c.for) 
d2c.for.patches <- clump(d2c.for) 

#salvare i dati verso l'esterno (esportarli)
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

#importare immagini con la funzione raster
d1c.for.patches <- raster("d1c.for.patches.tif")
d2c.for.patches <- raster("d2c.for.patches.tif")

#grafico delle due immagini di patch
clp <- colorRampPalette(c('dark blue', 'blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)
dev.off()

#definire quantitatviamente quante patch son state create
cellStats(d1c.for.patches, max) 
cellStats(d2c.for.patches, max)

#creare dataframe definito dal numero massimo di patch in relazione alle due diverse immagini
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)

#grafico della variazione del numero di patch nel tempo
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

##########################################################

### R code crop

#impostare la directory
setwd("/Users/jen/Desktop/snow") 

#installare pacchetto
install.packages("ncdf4")

#importare i pacchetti
library(raster)
library(ncdf4) #questa libreria serve per poter caricare dati in formato come lo trovi sul sito copernicus

#importare un nuovo set di dati
rlist <- list.files(pattern="snow") #creare una lista di file
list_rast=lapply(rlist, raster) #applicare la funzione raster sulla lista di file
snow.multitemp <- stack(list_rast) #per accorparli tutti in un unico file, così definendo una serie multitemporale 

#grafico 
clb <- colorRampPalette(c('dark blue', 'blue', 'light blue'))(100)
plot(snow.multitemp, col=clb)
plot(snow.multitemp$snow2010r, col=clb) #grafico di un'unica immagine entro la serie multitemporale 

#definire una certa estensione per un'immagine 
extension <- c(6, 20, 35, 50) #dove i numeri tra parentesi si riferiscono, in ordine, a: xmin; xmax

#effettuare lo zoom sull'immagine scelta secondo l'estensione specificata, in questo caso a livello della penisola italiana
zoom(snow.multitemp$snow2010r, ext=extension)

#effettuare direttamente lo zoom definendo un rettangolo nell'immagine originale
plot(snow.multitemp$snow2010r, col=clb) #immagine originale
zoom(snow.multitemp$snow2010r, ext=drawExtent())

#funzione crop invece che zoom, pur mantenendo la stessa estensione definita in precedenza 
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension) #la funzione crop quindi non è semplicemente uno zoom dell'immagine originale ma bensì una vera e propria nuova immagine

#grafico dell'immagine snow2010r così ritagliata
plot(snow2010r.italy, col=clb)

#funzione crop per l'intera seria multitemporale
extension <- c(6, 20, 35, 50)
snow.italy <- crop(snow.multitemp, extension)

#grafico di tutte le immagini così ritagliate
plot(snow.italy, col=clb, zlim=c(20,200)) #zlim serve per regolare le legende delle immagini in modo che riportino tutte lo stesso range di valori, da minimo a massimo

boxplot(snow.italy, horizontal=T, outline=F) #dove outline si riferisce ai valori outliers, se F li esclude 

##########################################################

### R species modeling distribution 

#installare pacchetto
install.packages("sdm")

#caricare pacchetti
library(sdm)
library(raster)
library(rgdal)

#caricare il file relativo alle specie
file <- system.file("external/species.shp", package = "sdm")
species <- shapefile(file) #per caricare tutta la parte grafica del file

#visualizzare le info del file
species
specie$Occurrence #visualizzo le occorrenze di specie

#grafico dei punti relativi alla distribuzione di specie 
plot(species)

#grafico con i punti delle occorrenze differenziati tra presente e assente
plot(species[species$Occurrence==1,], col=blue, pch=16) #formula condizionale che in questo caso trattiene solo le occorrenze uguali a 1 
points(species[species$Occurrence==0,], col=red, pch=16) #la funzione points aggiunge dei punti al grafico precedente, questa volta la funzione riguarda le occorrenze pari a 0

#importare i predittori, ossia delle variabili ambientali che servono a prevedere quale sarà la distribuzione delle specie nello spazio
path <- system.file("external", package="sdm") #imposta il percorso dal quale importare i files
lst <- list.files(path=path, pattern="asc$",  full.names=T)
preds <- stack(lst) 
cl <- colorRampPalette(c('blue', 'orange', 'red', 'yellow'))(100)
#T più alta nella parte centrale .......

#come la specie si distribuisce in funzione della variabile abiotica elevation
plot(preds%elevation, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che la specie stia bene a basse quote

#come la specie si distribuisce in funzione della variabile abiotica temperature
plot(preds%temperature, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che alla specie piacciano le T alte 

#come la specie si distribuisce in funzione della variabile abiotica precipitation
plot(preds%precipitation, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che ci sia una situazione intermedia per la precipitazione 

#come la specie si distribuisce in funzione della variabile vegetation
plot(preds%vegetation, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che alla specie piaccia la situazione ombreggiata 

#GLM che unisce tutte le variabili ambientali in modeo da avere la probabilità distributiva della specie 
d <- sdmData(train=species, predictors=preds) #unisce i dati a cui si è interessati per l'analisi
m1 <- sdm(Occurrence = elevation + precipitation + temperature + vegetation, data=d, methods="glm") #formula GLM
p1 <- predict(m1, newdata=preds) #mappa previsionale della distribuzione di specie in funzione dei predittori ambientali 

#grafico della mappa previsionale 
plot(p1, col=cl)
points(species[species$Occurrence==1,], pch=16)


 
 

























https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home



