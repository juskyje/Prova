### R code first

#installo il pacchetto
install.packages("sp") #install.packages() installa i pacchetti esterni dalle repositories
#"sp" è un pacchetto per l'analisi e per le funzioni associate ai dati di tipo spaziale

#carico il pacchetto
library(sp) #library() importa i pacchetti esterni dapprima installati

##carico il dataset meuse
data("meuse") #data() carica datasets 
#in questo caso il dataset è già incluso nel pacchetto sp 

head(meuse) #con head() visualizzo le prime righe di un dataset
names(meuse) #con names() visualizzo i nomi delle variabili del dataset

#visualizzo le statistiche di base
summary(meuse) #summary() produce le statiche di base per ogni variabile del dataset

#grafico sulle correlazioni tra le variabili 
pairs(meuse) #pairs() produce una matrice di scatterplots 

#grafico sulle correlazioni tra le sole variabili cadmio, rame e piombo
pairs(~ cadmium + copper + lead , data = meuse) #la tilde significa uguale mentre la virgola è usata come separatore degli argomenti 

#primo esercizio: aggiungo al grafico precedente la variabile zinco
pairs(~ cadmium + copper + lead + zinc, data = meuse)

#grafico sulle correlazioni tra un subset di variabili scelte 
pairs(meuse[,3:6]) #i numeri dopo la virgola si riferiscono alle colonne del dataset e le parentesi quadre indicano che la scelta riguarda un suo specifico subset di dati 
pairs(meuse[,3:6], col="green") #l'argomento col aggiunge uno specifico colore al grafico
pairs(meuse[,3:6], col="green", pch=19) #l'argomento pch permette di cambiare il carattere dei punti del grafico 
pairs(meuse[,3:6], col="green", pch=19, cex=0.5) #l'argomento cex permette di cambiare la dimensione dei punti del grafico
pairs(meuse[,3:6], col="green", pch=19, cex=0.5, main="Primo pairs") #l'argomento main permette di aggiungere un titolo al grafico

#secondo esercizio: aggiungo al grafico precedente la variabile elevazione
pairs(meuse[,3:7], col="green", pch=19, cex=0.5, main="Primo pairs")

#utilizzo una funzione esterna
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
  } #quella così definita è una funzione relativa alle correlazioni tra x e y, i quali indicano due variabili qualsiasi

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
  cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
  {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
} #quella così definita è una funzione relativa allo smoothing, dove lowess è uno smoother locale 

panel.histograms <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
} #quella così definita è una funzione relativa alla produzione di istogrammi

pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms) #matrice di grafici dove l'argomento lower.panel indica la parte inferiore del grafico, upper.panel indica la parte superiore del grafico, e diag.panel indica la parte diagonale 
#in questo caso si associa la parte inferiore del grafico alla funzione sulle correlazioni, la diagonale alla funzione sugli istogrammi e la superiore alla funzione smoothing

#terzo esercizio: scambio la disposizione delle funzioni tra upper e lower
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms) 

#grafico generico sulle relazioni tra le variabili cadmio e rame
plot(meuse$cadmium, meuse$copper) #il dollaro serve per collegare la colonna variabile col dataset di provenienza

#allego il dataframe
attach(meuse) #attach() permette di spiegare a R che utilizzeremo sempre questo dataset per tutte le funzioni da qui in avanti

#grafico generico sulle relazioni tra le due variabili cadmium e copper
plot(cadmium, copper, pch=17) 
plot(cadmium, copper, pch=17, xlab="cadmio", ylab="rame") #gli argomenti xlab e ylab modificano i nomi relativi all'asse x e all'asse y, rispettivamente
plot(cadmium, copper, pch=17, xlab="cadmio", ylab="rame", cex.lab=2) #l'argomento cex.lab permette di cambiare la grandezza dei nomi relativi agli assi

###########################################################

### R code spatial

#installo i pacchetti 
install.packages("GGally")
#"GGally" è un'estensione del pacchetto "ggplot2" per cui ne semplifica le funzioni

#carico i pacchetto
library(sp)
library(GGally)

##carico il dataset meuse
data(meuse)

#allego il dataframe
attach(meuse)

#primo esercizio: grafico sul rapporto tra rame e zinco con come simbolo il triangolo e come colore il verde
plot(copper, zinc, col="darkgreen", pch=17, cex=2)

#secondo esercizio: cambio le etichette al grafico
plot(copper, zinc, col="darkgreen", pch=17, cex=2, xlab="rame", ylab="zinco") #ciò che è relazionato ad un testo va sempre messo tra virgolette

#multiframe: confronto tra il grafico cadmio-piombo e il grafico rame-zinco
par(mfrow=c(1,2)) #par(mfrow) permette di inserire più di un grafico all'interno della stessa finestra grafica, dove i numeri tra parentesi si riferiscono il primo al numero di righe della finestra grafica e il secondo al suo numero di colonne
plot(cadmium, lead, col="brown", pch=8, cex=2)
plot(copper, zinc, col="darkgreen", pch=17, cex=2)
dev.off() #dev.off() permette di chiudere la sequenza grafica iniziata col comando par()

#terzo esercizio: inverto i grafici per riga-colonna 
par(mfrow=c(2,1)) 
plot(cadmium, lead, col="brown", pch=8, cex=2)
plot(copper, zinc, col="darkgreen", pch=17, cex=2)
dev.off()

#grafico ggpairs delle correlazioni tra le variabili scelte 
ggpairs(meuse[,3:6]) #ggpairs() crea una matrice di grafici suddivisi in tre diversi set di panelli, ciascuno definito da una diversa funzione   
ggpairs #richiamando la funzione ggpairs viene mostrato come è stata calcolata la funzione

###########################################################

### R code spatial 2 

#carico il pacchetto
library(sp)

##carico il dataset meuse
data(meuse)

#visualizzo le coordinate del dataset
head(meuse) 
#in questo caso le coordinate sono riportate come x e y
coordinates(meuse)=~x+y #coordinates() riferisce a R quali sono le coordinate del dataset 

#grafico spaziale con spplot
spplot(meuse,"zinc") #spplot() crea un grafico spaziale che mostra i valori della variabile scelta suddivisi in classi
#in relazione alla loro posizione nello spazio

#primo esercizio: grafico spplot con rame
spplot(meuse,"copper")

#grafico spaziale con la funzione bubble
bubble(meuse,"zinc") #bubble() crea un grafico spaziale con infografico per cui le concentrazioni dell'elemento son rappresentate in funzione della dimensione delle bolle

#secondo esercizio: grafico bubble con rame e bolle di colore rosso
bubble(meuse,"copper",col="red")

#creazione di array di dati 
foram <- c(10,20,35,55,67,80)
carbon <- c(5,15,30,70,85,99) #Ca capture

#grafico sulle relazioni tra i due vettori  
plot(foram, carbon, col="cornflowerblue", cex=2, pch=19)

###########################################################

### R code point pattern 

#installo i pacchetti
install.packages("ggplot2")
#"ggplot2" è un pacchetto atto alla creazione di grafici
install.packages("spatstat")
#"spatspat" è un pacchetto atto principalmente all'analisi dei modelli puntuali a due o tre dimensioni ma che include anche 
#molte altre funzioni comunque legate a dinamiche e modelli spaziali
install.packages("rgdal")
#"rgdal" è un pacchetto che implementa le funzioni del pacchetto "sp" e permette inoltre di lavorare su file raster e vettoriali

#carico i pacchetti 
library(ggplot2)
library(spatstat)
library(rgdal)

#stabilisco la cartella di lavoro
setwd("/Users/jen/Desktop/lab") #setwd() stabilisce il percorso e quindi la posizione della directory di lavoro

##carico il dataset covid
covid <- read.table("covid_agg.csv", header = TRUE) #read.table() permette di importare i dati come formato excel

#visualizzo i precedenti di lavoro
ls() #ls() permette di visualizzare come lista i files e le funzioni salvati nelle sessioni di lavoro precedenti

#visualizzo il dataset
head(covid)

#allego il dataset
attach(covid)

#grafico del numero di infetti per paese 
plot(country, cases, las=0) #l'argomento las si riferisce all'orientamento delle etichette
#in questo primo caso, con il valore di las pari a 0, le etichette di entrambi gli assi risultano parallele agli assi
plot(country, cases, las=1) 
#con las pari a 1 le etichette sono orizzontali
plot(country, cases, las=2) 
#con las pari a 2 le etichette son perpendicolari tra loro
plot(country, cases, las=3) 
#con las pari a 3 le etichette sono verticali

#grafico del numero di infetti per paese definitivo
plot(country, cases, las=3, cex.lab=0.5, cex.axis=1) #l'argomento cex.axis permette di cambiare la grandezza delle etichette degli assi 

#grafico ggplot sul dataset covid
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point() #dove l'argomento aes indica le variabili da utilizzare nella costruzione del grafico e geom_() il tipo di geometria
#in questo caso la geometria è puntuale 

#creo il dataset relativo ai casi di covid
covids <- ppp(lon, lat, c(-180,180), c(-90,90)) #ppp() permette di creare un dataset puntuale, ossia a point pattern 

#creo la funzione di densità
d <- density(covids) #density() computa la funzione di densità la quale informa sulla densità dei punti nello spazio

#mappa di densità con i punti
plot(d)
points(covids) #points() aggiunge i punti sopra la mappa di densità

#cambio i colori del  grafico
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100) #colorRampPalette() permette di cambiare la palette di colori scelti per il grafico, dove il valore riportato dall'ultima parentesi si riferisce al numero delle gradazioni di colori
plot(d, col=cl)

#primo esercizio: mappa di densità con palette di colori dal verde al blu
cl2 <- colorRampPalette(c('darkgreen', 'lightgreen', 'blue')) (100) 
plot(d, col=cl2)

##carico l'immagine relativa alle coste
coastlines <- readOGR("ne_10m_coastline.shp") #readOGR() permette di caricare dati in formato shapefile

#aggiungo le coste al grafico precedente
plot(coastlines, add=T) 
#questa mappa rappresenta quanto densi sono i punti nel mondo 
#e si nota che la densità massima registrata in Febbraio è localizzata in Europa

#secondo esercizio: mappa di densità con i punti, le coste, e con nuova palette di colori 
cl3 <- colorRampPalette(c('darkcyan', 'purple', 'red')) (200) 
plot(d, col=cl3)
points(covids)
plot(coastlines, add=T) 

#visualizzo il dataset covid
head(covid)

#associo ai point pattern il valore della variabile cases 
marks(covids) <-covid$cases #marks() permette di associare un dataset a point pattern ai valori della variabile da interpolare
#in questo caso cases

#effettuo l'interpolazione dei valori
s <- Smooth(covids) #Smooth() performa lo smoothing del dataset a point pattern
#ossia permette di stimare i valori nelle zone dove non è stato effettuato il campionamento

#mappa di interpolazione
plot(s)

#terzo esercizio: mappa di interpolazione con i punti, le coste, e nuova palette di colori
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
plot(coastlines, add=T)
#la densità più alta si riscontra in Europa ma una stima dei casi più alta in Asia 

#grafico multiframe finale della mappa di densità e della mappa di interpolazione
par(mfrow=c(2,1)) 
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
plot(coastlines, add=T)
 
plot(s, col=cl5, main="estimate of cases")
points(covids)
plot(coastlines, add=T)
dev.off()

##carico il dataset Tesi
load("Tesi.RData") #load() carica un oggetto salvato come RData

#visualizzo il dataset 
head(Tesi)

#allego il dataset
attach(Tesi)

#visualizzo le statistiche per individuare i valori minimi e massimi delle coordinate
summary(Tesi) 
#x varia da 12.42 a 12.46
#y varia da 43.91 a 43.94

#creo il dataset puntuale
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9,43.95))

#creo la funzione di densità 
dT <- density(Tesippp)

#mappa di densità
plot(dT)
points(Tesippp)

#associare ai point pattern il valore della variabile ricchezza specifica
marks(Tesippp) <- Tesi$Species_richness

#effettuo l'interpolazione dei valori
interpol <- Smooth(Tesippp)

#mappa di interpolazione
plot(interpol)
points(Tesippp, col="green") 
#si può notare come i valori più bassi di ricchezza specifica si trovino nella parte a N e S-O mentre i valori più alti nella parte ad O e S-E

##carico i confini di San Marino
sanmarino <- readOGR("San_Marino.shp")

#aggiungo i confini di San Marino alla mappa di interpolazione
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp, col="green")
plot(sanmarino, add=T)

#quarto esercizio: grafico multiframe della mappa di densità e di interpolazione 
par(mfrow=c(2,1))
plot(dT)
points(Tesippp, col="green")
plot(interpol)
points(Tesippp, col="green")
dev.off()

###########################################################

### R code teleril

#installo i pacchetti
install.packages("raster")
#"raster" è un pacchetto che consente la lettura, l'analisi e la manipolazione di dati spaziali come raster e vettori

#carico i pacchetti
library(raster)
library(sp)

#stabilire la cartella di lavoro
setwd("/Users/jen/Desktop/lab")

##importo l'immagine satellitare p224r63_2011
p224r63_2011 <- brick("p224r63_2011_masked.grd", header = TRUE) #brick() permette di importare un oggetto multi-livello in formato raster 

#visualizzo l'immagine a più bande
plot(p224r63_2011) 
#si vede un paesaggio a 7 bande, ognuna corrispondente a diversi sensori, riferiti a loro volta a diversi colori 

#cambio i colori del grafico multiframe
cl <- colorRampPalette(c("black","grey","light grey"))(100) 
plot(p224r63_2011, col=cl) 
#nero=pixel con bassa riflettanza nel colore associato alla banda;grigio chiaro=pixel con alta riflettanza nel colore associato alla banda

#visualizzo i nomi associati alle diverse bande
names(p224r63_2011) #names() riporta i nomi associati alle variabili interrogate
#B1:blue; B2:green; B3:red; B4:near infrared; B5:medium infrared;
#B6:thermal infrared; B7:medium infrared

#visualizzo la banda del blu
clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)  
plot(p224r63_2011$B1_sre, col=clb) #il $ viene usato per collegare la banda all'immagine poichè la funzione attach() non funziona con il pacchetto raster
#valori di riflettanza indicano un'ampia copertura vegetativa

#primo esercizio: visualizzo la banda dell'infrarosso vicino con colori che variano dal rosso al giallo
cliv <- colorRampPalette(c("red","orange","yellow"))(100)  
plot(p224r63_2011$B4_sre, col=cliv)

#grafico multiframe con palette di colori specifica per ogni banda
par(mfrow=c(2,2))

clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_2011$B1_sre, col=clb) 
#banda del blu

clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_2011$B2_sre, col=clg) 
#banda del verde 

clr <- colorRampPalette(c("dark red","red","pink"))(100)
plot(p224r63_2011$B3_sre, col=clr) 
#banda del rosso

cliv <- colorRampPalette(c("red","orange","yellow"))(100)  
plot(p224r63_2011$B4_sre, col=cliv)
#banda del nir

dev.off()

#visualizzo l'immagine come la vedrebbe l'occhio umano
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") #plotRGB associa ad ogni banda del dataset una componente RGB, dove l'argomento stretch dissocia o meno le sfumature di colori
#qui si nota come la parte scura corrisponda alla parte forestata

#visualizzo l'immagine RGB con falsi colori del 2011
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
#tutte le zone dove c'è una pianta sono ora colorate di rosso perchè le piante #riflettono molto l'infrarosso che in questo caso è montato sul rosso mentre il #suolo nudo risulta celeste, circondato da zone agricole visualizzate in rosa

#multiframe delle due immagini RGB del 2011
par(mfrow=c(1,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

#secondo esercizio: visualizzo l'immagine RGB del 2011 con la banda nir nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") 

##importo l'immagine satellitare p224r63_1988
p224r63_1988 <- brick("p224r63_1988_masked.grd", header = TRUE)

#grafico multiframe con palette di colori specifica per ogni banda
par(mfrow=c(2,2))

clb <- colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(p224r63_1988$B1_sre, col=clb) 
#blu

clg <- colorRampPalette(c("dark green","green","light green"))(100)
plot(p224r63_1988$B2_sre, col=clg) 
#verde

clr <- colorRampPalette(c("dark red","red","pink"))(100)
plot(p224r63_1988$B3_sre, col=clr) 
#rosso

cliv <- colorRampPalette(c("red","orange","yellow"))(100)  
plot(p224r63_1988$B4_sre, col=cliv)
#nir

dev.off()

#visualizzo l'immagine RGB del 1988
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin") 

#terzo esercizio: visualizzo l'immagine RGB del 1988 con la banda nir nella componente rossa
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

#multiframe delle due immagini RGB di cui una riferita al 1988 e l'altra al 2011
par(mfrow=c(1,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

#calcolo l'indice di vegetazione per il 1988
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
#l'indice è definito da nir-red

#visualizzo l'immagine relativa all'indice di vegetazione per il 1988
plot(dvi1988)

#quarto esercizio: calcolo l'indice di vegetazione per il 2011
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre 

#visualizzo l'immagine relativa all'indice di vegetazione per il 2011
plot(dvi2011)

#calcolo la differenza tra i due indici di vegetazione riferiti alle due diverse finestre temporali
difdvi <- dvi2011 - dvi1988

#visualizzo l'immagine della differenza tra i due indici di vegetazione riferiti alle due diverse finestre temporali
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi)
#le piante che stanno meglio sono di colore blu e viceversa di colore rosso, 
#mentre quelle stabili di colore bianco

#multiframe tra le due immagini RGB temporalmente distinte e qula differenza degli indici di vegetazione
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()

#cambio la risoluzione dell'immagine p224r63_2011
p224r63_2011lr <- aggregate(p224r63_2011, fact=10) #aggregate() genera statistiche divise per le componenti del dataset
#in questo caso la risoluzione scelta è di dieci volte più grande rispetto all'originale 

#visualizzo il confronto tra la risoluzione dell'immagine originale rispetto a quella modificata 
p224r63_2011
p224r63_2011lr

#multiframe tra le immagini RGB del 2011 ad alta e a bassa risoluzione
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
dev.off()

#cambio la risoluzione dell'immagine p224r63_2011 diminuendola ulteriormente
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50

#multiframe tra le immagini RGB del 2011 a diverse risoluzioni
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")
dev.off()

#calcolo l'indice di vegetazione per il 2011 ad una risoluzione peggiore rispetto all'originale
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre

#visualizzo l'immagine relativa all'indice di vegetazione per il 2011 ad una bassa risoluzione
plot(dvi2011lr50)

#cambio la risoluzione dell'immagine p224r63_1988
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)

#calcolo l'indice di vegetazione per il 1988 ad una risoluzione peggiore rispetto all'originale
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

#calcolo la differenza tra i due indici di vegetazione a bassa risoluzione riferiti alle due diverse finestre temporali 
difdvilr50 <- dvi2011lr50 - dvi1988lr50

#visualizzo l'immagine della differenza tra i due indici di vegetazione a bassa risoluzione riferiti alle due diverse finestre temporali
plot(difdvilr50,col=cldifdvi)

#visualizzo il confronto tra l'immagine finale a bassa risoluzione e quella ad alta risoluzione
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

###########################################################

### R code landcover

#installo il pacchetto
install.packages("RStoolbox")
#"RStoolbox" consente l'analisi delle immagini da sensore remoto 

#carico i pacchetti
library(raster)
library(RStoolbox)

#stabilisco la cartella di lavoro
setwd("/Users/jen/Desktop/lab") 

##carico l'immagine p224r63_2011
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#visualizzo l'immagine RGB del 2011 (dove le bande del landsat sono:1=blu, 2=verde, 3=rosso, 4=nir)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 

#classifico l'immagine del 2011 tramite algoritmo non supervisionato
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4) #unsuperClass() ragguppa i dati di un raster in classi in maniera non supervisionata ossia in una maniera che non conferisce i training set al computer per la distinzione
#cioè in questo caso non indica cosa è foresta e cosa non è foresta

#visualizzo l'immagine del 2011 definita dall'algoritmo 
plot(p224r63_2011c$map)

#cambio i colori dell'immagine del 2011 definita dall'algoritmo e la visualizzo
clclass <- colorRampPalette(c('blue', 'red', 'green', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

#classifico l'immagine del 2011 tramite algoritmo non supervisionato con due classi  
p224r63_2011c2 <- unsuperClass(p224r63_2011, nClasses=2)

#cambio i colori dell'immagine del 2011 definita dall'algoritmo con due classi e la visualizzo
clclass2 <- colorRampPalette(c('red', 'blue', 'black', 'green'))(100)
plot(p224r63_2011c2$map, col=clclass2)
#si può qui notare come in funzione del numero di classi aumenti l'incertezza dell'algoritmo automatico di classificazione

###########################################################

### R code multitemp

#installo i pacchetti
install.packages("gridExtra")
#"gridExtra" abilita delle funzioni che consentono di gestire spazialmente i grafici 

#carico i pacchetti
library(raster)
library(RStoolbox)
library(gridExtra)

#imposto la directory
setwd("/Users/jen/Desktop/lab")

##carico le immagini defor1 e defor2
defor1 <- brick("defor1_.jpg.png")
defor2 <- brick("defor2_.jpg.png")

#visualizzo l'immagine RGB di defor1
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

#visualizzo le specifiche dell'immagine defor1
defor1
#defor1_.1 = NIR, defor1_.2 = rosso, defor1_.3 = verde

#visualizzare l'immagine RGB di defor2
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin") #parte più utilizzata dall'uomo

#visualizzo le specifiche dell'immagine defor2
defor2

#multiframe tra le immagini RGB di defor1 e defor2
par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")
dev.off()

#classifico l'immagine defor1 tramite algoritmo non supervisionato 
d1c <- unsuperClass(defor1, nClasses=2)

#visualizzo l'immagine defor1 definita tramite l'algoritmo non supervisionato con e senza mofifica della palette di colori
par(mfrow=c(2,1))
plot(d1c$map)
cl <- colorRampPalette(c('black', 'green'))(100)
plot(d1c$map, col=cl)
dev.off()

#classifico l'immagine defor2 tramite algoritmo non supervisionata  
d2c <- unsuperClass(defor2, nClasses=2)

#visualizzo l'immagine defor2 definita tramite l'algoritmo non supervisionato con e senza modifica della palette di colori
par(mfrow=c(2,1))
plot(d2c$map)
cl <- colorRampPalette(c('black', 'green'))(100)
plot(d2c$map, col=cl)
dev.off()

#visualizzo il confronto tra l'immagine defor1 e l'immagine defor2 definite secondo l'algoritmo non supervisionato
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
#si può notare come la zona deforestata si stia ingrandendo nel tempo
dev.off()

#calcolo le frequenze dei pixel differenziati per classe in defor1
freq(d1c$map) #freq() calcola la tavola delle frequenze per un dato oggetto 
#classe1=aree aperte e classe2=aree forestate

#calcolo le proporzioni dei pixel differenziati per classe in defor1
totd1 <- 35239 + 306053 
#in questo modo si genera il totale dei numero di pixel nella mappa
totd1
percent1 <- freq(d1c$map) * 100/totd1
percent1

#calcolo le frequenze dei pixel differenziati per classe in defor2
freq(d2c$map) 

#calcolo le proporzioni dei pixel differenziati per classe in defor2
totd2 <- 164095 + 178631 #genera il totale dei numero di pixel nella mappa
totd2
percent2 <- freq(d2c$map) * 100/totd2
percent2

#creo un dataframe generale per le due mappe e le corrispettive frequenze
cover <- c("Agriculture", "Forest")
before <- c(10.3, 89.7)
after <- c(47.9, 52.1)
output <- data.frame(cover, before, after)

#visualizzo il dataframe delle mappe con frequenze associate
View(output) #View() permette la visualizzazione dei dati matriciali sotto forma di foglio di calcolo

#grafico ggplot sulla percentuale di copertura forestale prima della deforestazione
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white") #l'argomento fill si riferisce al colore da utilizzare per il riempimento interno delle barre del grafico 

#primo esercizio: grafico ggplot sulla percentuale di copertura forestale dopo la deforestazione
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white") 

#multiframe tra i due grafici ggplot riferiti uno alla copertura forestale prima della deforestazione e l'altro alla copertura forestale dopo la deforestazione 

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white") + ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white") + ylim(0, 100)

grid.arrange(grafico1, grafico2, nrow=1) #grid.arrange organizza la disposizione spaziale dei grafici all'interno della finestra grafica 

###########################################################

### R code multitemp NO2

#carico i pacchetti
library(raster)

#imposto la directory
setwd("/Users/jen/Desktop/lab")

##carico l'immagine EN01
EN01 <- raster("EN_0001.png") #raster() permette di importare file in formato raster
#utilizziamo solo una banda, quella riguardante l'azoto

#visualizzo l'immagine EN01
plot(EN01)

##carico singolarmente le altre immagini "EN" 
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

#cambio la palette di colori alle immagini 
cl <- colorRampPalette(c('red', 'orange', 'yellow')) (100) 

#multiframe tra l'immagine iniziale e l'immagine finale
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)
dev.off()

#calcolo la differenza dei valori di NO2 tra l'immagine iniziale e l'immagine finale
difno2 <- EN13 - EN01

#visualizzo la differenza dei valori di NO2 tra l'immagine iniziale e l'immagine finale
cldif<- colorRampPalette(c('red', 'orange', 'yellow')) (100)
plot(difno2, col=cldif) 
#zone gialle=molta differenza; zone blu=poca differenza

#multiframe di tutte le diverse immagini "EN" impostate singolarmente
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

#impostare la directory
setwd("/Users/jen/Desktop/lab/esa_no2")

##carico tutte le immagini "EN" in una volta 
rlist <- list.files(pattern=".png", full.names=T) #list.files() crea una lista di immagini
listafinale <- lapply(rlist, raster) #lapply() applica una data funzione ad una lista di file 
#in questo caso la funzione è raster 
EN <- stack(listafinale) #stack() unisce tutti file come unico dataset

#visualizzo tutte le immagini "EN" in una volta 
cldif<- colorRampPalette(c('red', 'orange', 'yellow')) (100)
plot(EN, col=cl)

#boxplot di tutte le immagini "EN"
boxplot(EN,horizontal=T, axes=T, outline=F) #boxplot() produce un grafico a scatola e #baffi che riporta i valori della mediana e dei quartili, dove l'argomento #horizontal si riferisce all'orientamento dei box e outline alla ritenzione o #eliminazione dei valori outlier 
#in questo caso il boxplot è orientato orizzontalmente e gli outliers vengono eliminati
#il cambiamento più grande che ci è stato è relativo ai massimi valori; in #generale passando dal primo anno al secondo vi è una diminuzione dei valori #massimi degli ossidi di azoto atmosferico

################################################################

### R code snow

#installo i pacchetti
install.packages("ncdf4")
#"ncdf4" consente di importare e aprire files del formato netCDF  

#carico i pacchetti
library(raster)
library(ncdf4)
library(rgdal)

#imposto la directory
setwd("/Users/jen/Desktop/lab")

##carico l'immagine della copertura nevosa
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

#visualizzo l'immagine della copertura nevosa
cl <- colorRampPalette(c('darkblue', 'blue', 'light blue'))(100)
plot(snowmay, col=cl)

#imposto una nuova directory
setwd("/Users/jen/Desktop/lab/snow")

##carico un nuovo set di immagini multitemporali
rlist <- list.files(pattern="snow")
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

#visualizzo il set di immagini multitemporali
plot(snow.multitemp, col=cl)

#multiframe del confronto tra la copertura nevosa nel 2000 e la copertura nevosa nel 2020
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))
dev.off()

#multiframe della differenza tra la copertura nevosa nel 2000 e la copertura nevosa nel 2020
difsnow=snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue', 'white', 'red'))(100)
plot(difsnow, col=cldiff)
#rosso=differenza più grande; blu=differenza minore

#calcolo il modello previsionale per stimare il valore della copertura nevosa nel 2025
source("prediction.r") #source() permette di caricare un codice già pronto dall'esterno 
^C #^C termina il comando

#visualizzo l'immagine relativa al modello previsionale (in questo caso importo il raster già pronto)
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)

################################################################

### R code patches

#installo i pacchetti
install.packages("igraph")
#"igraph" consente di operare nell'ambito nella network analysis 

#carico i pacchetti
library(raster)
library(igraph)
library(ggplot2)

#imposto la directory
setwd("/Users/jen/Desktop/lab")

##carico le immagini d1c e d2c
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

#multiframe del confronto tra d1c e d2c
cl <- colorRampPalette(c('black', 'green'))(100)
par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d2c, col=cl) 
#la foresta è associata alla classe numero due mentre la zona coltivata alla classe numero uno
dev.off()

#estraggo solamente la zona forestata per d1c
d1c.for <- reclassify(d1c, cbind(1, NA))  

#multiframe del confronto tra d1c con solo la zona forestata e d1c con entrambe le zone 
cl <- colorRampPalette(c('black', 'green'))(100)
par(mfrow=c(1,2))
plot(d1c.for, col=cl)
plot(d1c, col=cl) 
dev.off()

#estraggo solamente la zona forestata per d2c
d2c.for <- reclassify(d2c, cbind(1, NA))

#multiframe di confronto tra d1c e d2c entrambe con solo la zona forestata  
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)
dev.off()

#suddivido le immagini in patches
d1c.for.patches <- clump(d1c.for) #clump() suddivide le immagini in patches raggruppando i pixels secondo il patch di appartenenza
d2c.for.patches <- clump(d2c.for) 

#esporto le immagini così suddivise in patches
writeRaster(d1c.for.patches, "d1c.for.patches.tif") #writeRaster() esporta file creati all'interno dell'ambiente R in formato raster
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

#multiframe di confronto tra d1c e d2c suddivise in patches 
clp <- colorRampPalette(c('dark blue', 'blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)
dev.off()

#definisco quante patch son state create
cellStats(d1c.for.patches, max) #cellStats() computa le statistiche per le celle di ciascun livello di un oggetto in formato raster
#il valore massimo delle patches per d1c coincide con 301
cellStats(d2c.for.patches, max)
#il valore massimo delle patches per d2c coincide con 1212

#creo un dataframe definito dal numero massimo di patch in relazione alle due diverse immagini d1c e d2c
time <- c("Before deforestation","After deforestation") 
npatches <- c(301,1212) 
output <- data.frame(time,npatches)
attach(output)

#grafico della variazione del numero di patch nel tempo
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

################################################################

### R code crop

#importo i pacchetti
library(raster)
library(ncdf4) 

#imposto la directory
setwd("/Users/jen/Desktop/snow") 

##carico un set di immagini multitemporali
rlist <- list.files(pattern="snow") 
list_rast=lapply(rlist, raster) 
snow.multitemp <- stack(list_rast)  

#visualizzo il set di immagini della serie multitemporale 
clb <- colorRampPalette(c('dark blue', 'blue', 'light blue'))(100)
plot(snow.multitemp, col=clb)
 
#definisco una certa estensione per le immagini della serie multitemporale 
extension <- c(6, 20, 35, 50) #i numeri tra parentesi si riferiscono, in ordine, a: xmin e xmax

#effettuo lo zoom a livello della penisola italiana sull'immagine relativa al 2010 secondo l'estensione specificata
zoom(snow.multitemp$snow2010r, ext=extension) #zoom() effettua lo zoom su un'area di una data immagine scelta secondo una certa estensione

#effettuo manualmente lo zoom a livello della penisola italiana sull'immagine relativa al 2010
plot(snow.multitemp$snow2010r, col=clb) 
zoom(snow.multitemp$snow2010r, ext=drawExtent())
#in questo caso lo zoom viene effetuato definendo un rettangolo nell'immagine scelta originale

#ritaglio l'immagine relativa al 2010 a livello della penisola italiana 
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension) #crop() crea una nuova immagine in funzione del ritaglio di un'altra scelto secondo una certa estensione 

#visualizzo l'immagine relativa al 2010 così ritagliata
plot(snow2010r.italy, col=clb)

#primo esercizio: ritaglio l'intero set multitemporale a livello della penisola italiana
extension <- c(6, 20, 35, 50)
snow.italy <- crop(snow.multitemp, extension)

#visualizzo tutte le immagini della serie multitemporale così ritagliate
plot(snow.italy, col=clb, zlim=c(20,200)) #l'argomento zlim permette di regolare le legende delle immagini 
#in modo che riportino tutte lo stesso range di valori, da minimo a massimo

#boxplot di tutte le immagini della serie multitemporale così ritagliate
boxplot(snow.italy, horizontal=T, outline=F) #dove outline si riferisce ai valori outliers, se F li esclude 

################################################################

### R species modeling distribution 

#installo i pacchetti
install.packages("sdm")
#"sdm" consente di sviluppare modelli per la distribuzione delle specie 

#carico i pacchetti
library(sdm)
library(raster)
library(rgdal)

##carico il file relativo alle specie
file <- system.file("external/species.shp", package = "sdm") #system.file() permette di identificare dei file presenti all'interno di pacchetti
species <- shapefile(file) #shapefile() permette di importare dati di tipo spaziale come shapefile

#visualizzo le informazioni relative alle occorrenze di specie del file
species
specie$Occurrence 

#grafico relativo alla distribuzione di specie 
plot(species)

#grafico puntuale differenziato per presenza e assenza di specie
plot(species[species$Occurrence==1,], col=blue, pch=16) #formula condizionale #che in questo caso trattiene solo le occorrenze uguali a 1 
points(species[species$Occurrence==0,], col=red, pch=16) #points() aggiunge dei punti al grafico 
#questa volta la funzione riguarda le occorrenze pari a 0

##carico i predittori, ossia le variabili ambientali che servono a prevedere quale sarà la distribuzione delle specie nello spazio
path <- system.file("external", package="sdm") 
lst <- list.files(path=path, pattern="asc$",  full.names=T)
preds <- stack(lst) #stack() permette di unire più file come singolo 

#imposto la palette di colori per i grafici
cl <- colorRampPalette(c('blue', 'orange', 'red', 'yellow'))(100)

#grafico di come la specie si distribuisce in funzione della variabile elevazione
plot(preds%elevation, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che la specie si distribuisca preferibilmente a basse quote

#grafico di come la specie si distribuisce in funzione della variabile temperatura
plot(preds%temperature, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che la specie si distribuisca preferibilmente in aree con T alte 

#grafico di come la specie si distribuisce in funzione della variabile  precipitazione
plot(preds%precipitation, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che ci sia una situazione intermedia per la precipitazione 

#come la specie si distribuisce in funzione della variabile vegetazione
plot(preds%vegetation, col=cl)
points(species[species$Occurrence==1,], pch=16) 
#sembra che la specie si distribuisca preferibilmente in aree ombreggiate

#GLM che unisce tutte le variabili ambientali in modeo da avere la probabilità distributiva della specie 
d <- sdmData(train=species, predictors=preds) #sdmData() permette di unire le variabili dipendenti con quelle indipendenti 
#ossia in questo caso il file relativo alla specie con quello relativo alle condizioni ambientali
m1 <- sdm(Occurrence = elevation + precipitation + temperature + vegetation, data=d, methods="glm") #formulazione del modello lineare generalizzato
p1 <- predict(m1, newdata=preds) #predict() produce una mappa previsionale
#in questo caso della distribuzione di specie in funzione dei predittori ambientali 

#grafico della mappa previsionale 
plot(p1, col=cl)
points(species[species$Occurrence==1,], pch=16)

################################################################

#scarica dati da:
https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home

################################################################

### R_code_Exam

#carico i pacchetti
library(sp)
library(ncdf4)
library(raster)
library(rgdal)
library(RStoolbox)
library(ggplot2)
library(gridExtra)

#stabilisco la cartella di lavoro
setwd("/Users/jen/Desktop/Esame/LST")

#carico i dati relativi a LST
lst_list <- list.files(pattern="LST_", full.names=T) #creo una lista di dati da importare
nc <- nc_open(lst_list) #nc_open() permette di aprire il contenuto e le informazioni relative ai file ntCDF 
names(nc[['var']]) #visualizzo le variabili dei dati caricati
surface.temp.median <- stack(stack(lst_list,varname="MEDIAN")) #carico i dati relativi ai valori mediani (definiti in Kelvin) della variabile LST 
surface.temp.max <- stack(stack(lst_list,varname="MAX")) #carico i dati relativi ai valori massimi (definiti in Kelvin) della variabile LST

#carico i dati relativi a NDVI 
list <- list.files(pattern="NDVI_", full.names=T) 
ndvi_list <- lapply(list, raster)
ndvi <- stack(ndvi_list)

###Primo obiettivo: analisi multitemporale della LST media e massima 

#scelgo una palette di colori per visualizzare le immagini LST 
colors() #colors() permette di visualizzare i nomi relativi ai colori disponibili all'uso
cl1 <- colorRampPalette(c('darkslateblue', 'blue1', 'cyan', 'yellow', 'red')) (100)
cld <- colorRampPalette(c('blue', 'white', 'red'))(100)

#visualizzo l'immagine multiframe relativa ai dati sulla LST media 
plot(surface.temp.median, col=cl1, zlim=c(250,330))
#le immagini sono plottate in ordine cronologico, per cui 1=2017; 2=2018; 3=2019; 4=2020
#i valori non sembrano essersi discostati di molto nel corso degli anni, fattasi eccezione per l’Europa e alcune porzioni del nord America

#visualizzo l'immagine multiframe relativa ai dati sulla LST massima 
plot(surface.temp.max, col=cl1, zlim=c(250,360))
#i valori mostrano una variabilità maggiore negli anni rispetto ai valori relativi alla LST media

#boxplot del discostamento dei valori della LST media per ogni anno preso in considerazione
par(mar=c(4,8,3,2)) #regolo la visualizzazione dei margini del grafico rispetto ai bordi della figura
bmediana <- boxplot(surface.temp.median, horizontal=T, axes=T, outline=F, las=1)
#i valori ricadono in media all’interno dell’intervallo 255-325, 
#dove nell’anno 2018 si riscontra un restringimento del range dei valori mentre nell’anno 2019 se ne riscontra un allargamento
bmediana$stats #questa funzione permette di visualizzare i valori delle statistiche associate al boxplot 
#(e quindi i quartili, la mediana e i baffi)

#boxplot del discostamento dei valori della LST massima per ogni anno preso in considerazione
par(mar=c(4,8,3,2))
bmassima <- boxplot(surface.temp.max, horizontal=T, axes=T, outline=F, las=1)
#stesso trend riferibile alla LST media ma in questo caso i valori oscillano tra 275 e 345
bmassima$stats

#visualizzo l'immagine multiframe relativa alla comparazione tra la LST media nel 2017 e la LST media nel 2020
par(mfrow=c(1,2)) 
plot(surface.temp.median$LST.Median.1, col=cl1, zlim=c(250,330), main="LST.Median.2017")
plot(surface.temp.median$LST.Median.4, col=cl1, zlim=c(250,330), main="LST.Median.2020")
dev.off()
#si nota una variazione localizzata più che altro nell’emisfero boreale, 
#per cui il Canada sembra essere interessato da un raffreddamento mentre l’Europa da un riscaldamento

#visualizzo l'immagine multiframe relativa alla comparazione tra la LST massima nel 2017 e la LST massima nel 2020
par(mfrow=c(1,2)) 
plot(surface.temp.max$LST.Maximum.1, col=cl1, zlim=c(250,360), main="LST.Maximum.2017")
plot(surface.temp.max$LST.Maximum.4, col=cl1, zlim=c(250,360), main="LST.Maximum.2020")
dev.off()
#il Canada sembra essere interessato da un abbassamento nella LST massima mentre l’Europa da un suo innalzamento

#creo un dataframe per il confronto dei valori minimi e massimi di LST media tra il 2017 e il 2020
year.med <- c("2017", "2020")
min.med <- c(259.1, 257.6)
max.med <- c(319.22, 321.8)
output1 <- data.frame(year.med, min.med, max.med)
 #multiframe tra i due grafici ggplot riferiti uno ai valori minimi di LST media e l'altro ai suoi valori massimi 
grafico11 <- ggplot(output1, aes(x=year.med, y=min.med, color=year.med)) + 
geom_bar(stat="identity", fill=c("darkgoldenrod1", "darkorange")) + ylim(0, 350)

grafico21 <- ggplot(output1, aes(x=year.med, y=max.med, color=year.med)) +
geom_bar(stat="identity", fill=c("darkgoldenrod1", "darkorange")) + ylim(0, 350)

grid.arrange(grafico11, grafico21, nrow=1)
#i valori minimi risultano essere diminuiti, mentre i valori massimi risultano essere accresciuti

#creo un dataframe per il confronto dei valori minimi e massimi di LST massima tra il 2017 e il 2020
year.max <- c("2017", "2020")
min.max <- c(277.2, 276.5)
max.max <- c(340.4, 343.8)
output2 <- data.frame(year.max, min.max, max.max)
 #multiframe tra i due grafici ggplot riferiti uno ai valori minimi di LST massima e l'altro ai suoi valori massimi
grafico12 <- ggplot(output2, aes(x=year.max, y=min.max, color=year.max)) + 
  geom_bar(stat="identity", fill=c("brown1", "brown4")) + ylim(0, 350)

grafico22 <- ggplot(output2, aes(x=year.max, y=max.max, color=year.max)) +
  geom_bar(stat="identity", fill=c("brown1", "brown4")) + ylim(0, 350)

grid.arrange(grafico12, grafico22, nrow=1)
#stesso trend riferibile ai valori della variabile mediana 
#per cui in generale sembra esserci un aumento di circa 3 unità a livello di valori massimi e di circa 1 a livello di valori minimi

#computo la differenza tra la LST media nel 2020 e la LST media nel 2017
diflst.median = surface.temp.median$LST.Median.1 - surface.temp.median$LST.Median.4
#visualizzo l'immagine relativa alla differenza 
plot(diflst.median, col=cld, main="Median")
#ogni continente (fattasi eccezione per l’unione europea e la Cina, che non mostrano variazioni allarmanti apprezzabili) 
#è un mosaico di diverse zone: alcune hanno mantenuto gli stessi valori, altre hanno subito variazioni  minime e altre ancora hanno subito variazioni drastiche

#computo la differenza tra la LST massima nel 2020 e la LST massima nel 2017
diflst.max = surface.temp.max$LST.Maximum.1 - surface.temp.max$LST.Maximum.4
#rosso=differenza più alta; blu=differenza più bassa, bianco=nessuna differenza
#visualizzo l'immagine relativa alla differenza 
plot(diflst.max, col=cld, main="Maximum")
#solo poche zone mostrano forti variazioni 
#e queste zone corrispondono a: Canada, Messico, parte del Brasile, Kenya-Etiopia, anatolia, India e parte della Russia. 

#predico i valori della LST media relativi al 2021 
 #costruisco una variabile temporale 
time <- 1:nlayers(surface.temp.median)
 #eseguo la regressione lineare
fun <- function(x) {if (is.na(x[1])){ NA } else {lm(x ~ time)$coefficients[2] }} 
predicted.surface.temp.median2021 <- calc(surface.temp.median, fun) 
predicted.surface.temp.median2021.norm <- predicted.surface.temp.median2021*255/53.90828
 #visualizzo l'immagine relativa al modello previsionale 
plot(predicted.surface.temp.median2021.norm, main="Median", col=cl1)
#previsione catastrofica non attendibile

#predico i valori della LST massima relativi al 2021
 #costruisco una variabile temporale 
time <- 1:nlayers(surface.temp.max)
 #eseguo la regressione lineare
fun <- function(x) {if (is.na(x[1])){ NA } else {lm(x ~ time)$coefficients[2] }} 
predicted.surface.temp.max2021 <- calc(surface.temp.max, fun) 
predicted.surface.temp.max2021.norm <- predicted.surface.temp.max2021*255/53.90828
 #visualizzo l'immagine relativa al modello previsionale 
plot(predicted.surface.temp.max2021.norm, main="Maximum", col=cl1)
#previsione catastrofica non attendibile

###Secondo obiettivo: confronto tra i valori di NDVI e LST media a livello del territorio malgascio 

#ricavo l'estensione per tagliare sul Madagascar
plot(surface.temp.median$LST.Median.1)
d <- zoom(surface.temp.median$LST.Median.1, ext=drawExtent())

#definisco l'estensione relativa alla posizione del Madagascar nella mappa 
extension <- c(41, 52, -26, -10)

#ritaglio  le immagini LST a livello del Madagascar 
s.t.median.madagascar <- crop(surface.temp.median, extension)
 #visualizzo le immagini LST così ritagliate 
plot(s.t.median.madagascar, col=cl1)
#si nota una variazione nei valori che interessa maggiormente la punta nord, la zona sud, e il centro nord

#ritaglio le immagini NDVI a livello del Madagascar
ndvi.madagascar <- crop(ndvi, extension)
 #configuro la palette di colori per le immagini NDVI
cl2 <- colorRampPalette(c('darkgoldenrod3', 'brown', 'darkseagreen4', 'darkgreen', 'white')) (100)
 #visualizzo le immagini NDVI così ritagliate 
plot(ndvi.madagascar, col=cl2, main=c("NDVI.1", "NDVI.2", "NDVI.3", "NDVI.4"))
#si nota una variazione nei valori che interessa per lo più la costa est, il centro e il centro-sud 

#visualizzo l'immagine multiframe relativa alla comparazione tra i valori di NDVI e LST media riferiti al Madagascar e agli anni 2017 e 2020
par(mfrow=c(2,2)) 
plot(s.t.median.madagascar$LST.Median.1, main="LST.2017", col=cl1)
plot(ndvi.madagascar$Normalized.Difference.Vegetation.Index.333M.1, main= "NDVI.2017", col=cl2)
plot(s.t.median.madagascar$LST.Median.4, main="LST.2020", col=cl1)
plot(ndvi.madagascar$Normalized.Difference.Vegetation.Index.333M.4, main= "NDVI.2020", col=cl2)
dev.off()
#Sapendo che i valori di LST e NDVI son in genere tra loro inversamente proporzionali e dal momento che valori elevati di NDVI (da 0,6 a 0,9) suggeriscono un'alta densità di vegetazione 
#mentre bassi valori (da 0 a 0,3) suggeriscono la presenza di terreno nudo o superfici artificiali, mi aspettavo che alle zone più verdi (come identificate da un alto NDVI)
#corrispondessero valori più bassi di LST e viceversa. Questa relazione è visivamente riscontrata solo a livello della costa est e parte del centro, 
#mentre per altre zone si riscontrerebbe addirittura il trend opposto, come ben visibile per la zona nord-ovest.

#####################################################################################################







