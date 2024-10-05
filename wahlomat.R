
library(rstudioapi)
library(network)
library(igraph)

#setze das workingdir auf das verzeichnis, das den code enthaelt
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# im working-directory muss eine csv-datei mit den wahlomat-daten und eine mit der abgeordnetenzahl (+ parteifarben) vorhanden sein
# anpassungen erforderlich beim clustering, bei der berechnung von mehrheitskoalitionen und beim treshhold bezgl. der uebereinstimmung zwischen den parteien

wahlomat <- read.csv2("europa-2024.csv", header=TRUE, sep=";")
#wahlomat  <- read.csv2("hessen-2023.csv", header=TRUE, sep=";")
anzahlparteien <- length(unique(wahlomat$Partei..Name))
anzahlfragen <- max(wahlomat$These..Nr.)

# Matrix für die Antworten erzeugen
tabelle <- data.frame(matrix(ncol=(anzahlfragen+1), nrow=0))

# Parteikurzbezeichnungen eintragen
for (i in c(1:anzahlparteien))
{ tabelle[i,1] <- wahlomat$Partei..Kurzbezeichnung[i]}

# Fragen und bei den jeweiligen Parteien eintragen
for (i in c(1:anzahlparteien))
{
for (y in c(1:anzahlfragen))
{tabelle[i, y+1] <- wahlomat$Position..Position[((y-1)*anzahlparteien)+i]}
}

# zahlencodes definieren
klartext <- c("stimme nicht zu", "neutral", "stimme zu")
kodiert <- c(-1,0,1)

# ersetze den Klartext durch Zahlencodes
for (i in c(1:3))
{
tabelle[tabelle==klartext[i]] <- kodiert[i]
}

# matrix für die übereinstimmungen kreieren
uebereinstimmung1 <- data.frame(matrix(ncol=anzahlparteien, nrow=anzahlparteien))
# matrix mit Nullen vorbelegen
uebereinstimmung1[,] <- 0

# für jede übereinstimmung den wert in der matrix eins hochzaehlen
for (y in c(2:(anzahlfragen+1)))
  {for (i in c(1:anzahlparteien))
    {for (j in c(1:anzahlparteien))
      if (tabelle[i,y] == tabelle[j,y])
      { uebereinstimmung1[i,j] <- uebereinstimmung1[i,j]+1}
      }
}

# die diagonale der matrix auf null setzen
#es ist klar, dass jede partei in jeder frage mit sich selbst übereinstimmt
diag(uebereinstimmung1)<- 0

## die parteinamen als benennung der kolumnen und zeilen hinzufügen
zeilenvektor <- t(tabelle[,1])
colnames(uebereinstimmung1) <- zeilenvektor
rownames(uebereinstimmung1) <- zeilenvektor

uebereinstimmunga <- as.matrix(uebereinstimmung1)

# die matrix in einen graph umwandeln und die uebereinstimmungen an den kanten als gewichte hinzufuegen
gr <- graph_from_adjacency_matrix(uebereinstimmunga, weighted = TRUE, mode = "lower")
plot(gr, edge.label = E(gr)$weight)

#datei mit anzahl der abgeordneten einlesen und den parteien zugeordneten farben
abgeordnete <- read.csv2("abgeordnete-europa-2024.csv", header=TRUE, sep=";")
#abgeordnete <- read.csv2("abgeordnete-hessen-2023.csv", header=TRUE, sep=";")
anzahlen <- abgeordnete$anzahlabgeordnete
farbe <- abgeordnete$farbe
parteien <- abgeordnete$Partei..Kurzbezeichnung

# den knoten entsprechend der anzahl der abgeordneten gewicht geben
gr <- set_vertex_attr(gr, "weight", value=anzahlen)
gr <- set_vertex_attr(gr, "color" , value=farbe)

# einen treshhold definieren und alle kanten weglassen, die kleiner sind
# hier 23 da es noch einen einigermassen zusammenhaengenden graph ergibt
tresh <- 23
subgraph_kanten <- which(E(gr)$weight > tresh)
xgr <- subgraph.edges(gr, subgraph_kanten, delete.vertices = FALSE)
tkplot(xgr, edge.label = E(xgr)$weight, vertex.color = E(xgr)$farbe, layout=layout_with_fr(xgr, dim=2), vertex.size= V(xgr)$weight, edge.width = (E(xgr)$weight) - tresh, rescale = TRUE, canvas.width = 800, canvas.height = 800)

# nur noch die parteien anzeigen, die abgeordnete bekommen haben
subgraph_knoten <- which(V(xgr)$weight > 0)
fgr <- subgraph(xgr, subgraph_knoten) 
tkplot(fgr, edge.label = E(fgr)$weight, vertex.color = E(fgr)$farbe, layout=layout_with_fr(fgr, dim=2), vertex.size= V(fgr)$weight, edge.width = (E(fgr)$weight) - tresh, rescale = TRUE, canvas.width = 800, canvas.height = 800)

# tabelle2 erzeugen, die nur parteien mit sitzen enthält und trage die anzahl der sitze in die letzte spalte ein
tabelle2 <- data.frame(matrix(ncol=(anzahlfragen+2), nrow=0))
index <- 0
for (i in c(1:anzahlparteien))
{ if (abgeordnete$anzahlabgeordnete[i] > 0)
  { index <- index +1
  tabelle2[index,] <- tabelle[i,]
  tabelle2[index,(anzahlfragen+2)] <- abgeordnete$anzahlabgeordnete[i]
  }
}

# die anzahl der parteien mit mehr als 0 abgeodneten in einer variable speichern
anzahlparteienmitabgeordneten <- index

# character in integer werte umwandeln in tabelle2, damit man damit rechnen kann
for (i in c(2:(anzahlfragen+2)))
   {tabelle2[,i] <- as.integer(tabelle2[,i])
    }

# ordne tabelle2 nach sitzen absteigend
library(dplyr)
tabelle3 <- tabelle2 %>% arrange(desc(tabelle2[,(anzahlfragen+2)]))

# cdu/csu und/oder afd und/oder spd müssen in der "koalition" sein, da sie zusammen mehr als die hälfte der abgeordneten stellen
# es wird eine der drei ausgewählt und davon ausgehend die übereinstimmung von "mehrheitskoalitionen" ausgerechnet

# es werden nur dreierkoalitionen berechnet - da im fall der europawahl 2024 mindestens drei parteien zusammen ueber eine mehrheit der abgeordneten verfuegen
# fuer andere konstellationen muessen die werte angepasst werden!

# funktion berechnet eine Masszahl der Uebereinstimmungen
zeigeuebereinstimmungen <- function (coll)
  {
  ergebnis <- 0
  for (a in c(2:(anzahlfragen+1)))
  {
    ergebnis <- ergebnis + abs(tabelle3[coll[1],a] + tabelle3[coll[2],a] + tabelle3[coll[3],a])}
  return(ergebnis)
  }

koalitionstabelle <- data.frame(matrix(ncol=5, nrow=0))
colnames(koalitionstabelle) <- c("partei1","partei2","partei3","stimmen","uebereinstimmungswert")

# ganzzahlige haelfte der abgeordneten berechnen, so dass die zahl + 1 immer die mehrheit ist
gesamtzahlabgeordnete = sum(tabelle2$X40)
haelfte <- (gesamtzahlabgeordnete %/% 2)

for (i in c(1:3))
  { 
  #die erste partei in die koalition eintragen
  for (y in c(1:i))
  {
  for (j in c((i+1):anzahlparteienmitabgeordneten))
  { 
    for (k in c((j+1):anzahlparteienmitabgeordneten))
    {
      if (k<15)
      stimmen <- (tabelle3[i,(anzahlfragen+2)] + tabelle3[j,(anzahlfragen+2)] + tabelle3[k,(anzahlfragen+2)]) 
      {if (stimmen > haelfte)
         {koalitionstabelle <- rbind(koalitionstabelle, data.frame(partei1 = tabelle3[i,1], partei2 = tabelle3[j,1], partei3= tabelle3[k,1], stimmen = stimmen, uebereinstimmungswert = zeigeuebereinstimmungen(c(i,j,k))))
         }
    }
  }
  }
  }
}
koalitionstabelle

# cluster bilden und in datei speichern

library(cluster)
library(fpc)

# die tabelle nehmen, die die kodierten antworten auf die fragen enthaelt

clustertabelle <- tabelle2
# letzte spalte mit der anzahl der abgeordneten entfernen
clustertabelle <- clustertabelle[,-(anzahlfragen+2)]
# parteienamen aus der tabelle rauswerfen und als zu vergleichende objekte nehmen
row.names(clustertabelle) <- clustertabelle[,1]
clustertabelle <- clustertabelle[,-1]
# in der tabelle ueberall den wert 1 addieren: damit ist jetzt ablehnung = 0, neutral = 1 und zustimmung = 2
clustertabelle <- clustertabelle + konstante
# absolute werte durch prozentwerte (zwischen 0 und 1) ersetzen
zeilenzahl <- 1
for (i in row.names(clustertabelle))
{ gesamtzahl <- 0
spaltenzahl <- 1
for (a in names(clustertabelle))
{
  gesamtzahl <- gesamtzahl + clustertabelle[zeilenzahl,spaltenzahl]
  spaltenzahl <- spaltenzahl + 1
}
if (gesamtzahl > 0) 
{ spaltenzahl <- 1
for (b in names(clustertabelle))
{
  if (clustertabelle[zeilenzahl,spaltenzahl] > 0)
  {
    clustertabelle[zeilenzahl,spaltenzahl] <- clustertabelle[zeilenzahl,spaltenzahl] / gesamtzahl
  }
  spaltenzahl <- spaltenzahl + 1
}
}
zeilenzahl <- zeilenzahl + 1
}
# hierachische Clusteranalyse plotten
plot(hclust(dist(scale(clustertabelle), method="euclid"), method="complete"), main="Complete")

# Clusteranalyse mit vorgegebener Anzahl an Clustern
# optimale anzahl der Cluster feststellen
# das folgende muss individuell angepasst werden, wenn andere daten verwendet werden

km2 <- kmeans(clustertabelle, 2, nstart=20)
km3 <- kmeans(clustertabelle, 3, nstart=20)
km4 <- kmeans(clustertabelle, 4, nstart=20)
km5 <- kmeans(clustertabelle, 5, nstart=20)
km6 <- kmeans(clustertabelle, 6, nstart=20)
km7 <- kmeans(clustertabelle, 7, nstart=20)
wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss), sum(km5$withinss), sum(km6$withinss), sum(km7$withinss))
names(wss) <- 2:7
# barplot(wss)

# fehlersumme nimmt relativ gleichmaessig ab - auswahl 4 daher eher willkuerlich

anzahlcluster <- 4
parteicluster <- kmeans(clustertabelle, anzahlcluster, nstart = 4)
eintraege <- parteicluster$cluster
for (a in 1:anzahlcluster)
{
  write(a, file = "output.txt",
        append = TRUE, sep = " ")
  for (i in 1:length(eintraege))
  {
    element <- eintraege[i]
    if (eintraege[i] == a)
    {
      write(names(element[1]), file = "output.txt",
            append = TRUE, sep = " ")
      print(names(element[1]))
    }
  }
}

# medianantworten suchen
# welche antwort ist in abhaengigkeit der anzahl der abgeordneten der median
# ab hier sollte es wieder ohne anpassung gehen

tabelle4 <- tabelle3
for (j in c(1:anzahlparteienmitabgeordneten))
{
  for (i in c(2:(anzahlfragen+1)))
  { tabelle4[j,i] <- (tabelle4[j,i]* tabelle4[j,(anzahlfragen+2)])}
}

# mit der anzahl der abgeodneten gewichtet wird eine medianantwort ermittelt

medianantwort <- numeric(0)
for (i in c(2:(anzahlfragen + 1)))
{  positiv <- 0
   negativ <- 0
   wert <-0
  for (j in c(1:anzahlparteienmitabgeordneten))
  {
    wert <- wert + tabelle4[j,i] 
    if (tabelle4[j,i] > 0)
    {
      positiv <- positiv + tabelle4[j,i]
    }
    else if (tabelle4[j,i] <0 )
    {
      negativ <- negativ + abs(tabelle4[j,i])
    }
  }
   if (wert > 0)
   {
      if (positiv > haelfte)
      {
         wert <- 1
      }
      else
      {  wert <- 0
      }
   }
   else if (wert < 0)
   {
     if (negativ > haelfte)
     {
       wert <- -1
     }
     else
     { wert <- 0}   
   }
   else
   {
     wert <- 0
   }
   medianantwort <- c(medianantwort,wert)
}

# die medianantwort wird mit der tatsaechlichen antwort der parteien verglichen und die anzahl der uebereinstimmungen wird abgespeichert

medianliste <- list()
for (j in (1:anzahlparteienmitabgeordneten))
{
uebereinstimmung <- 0
   for (i in c(1:anzahlfragen))
   { if (medianantwort[i] == tabelle2[j,i+1])
     {
       uebereinstimmung <- uebereinstimmung + 1
     }
   }
    medianliste[[tabelle2[j,1]]] <- uebereinstimmung
}

medianframe <- as.data.frame(medianliste)

# welche parteien haben in ihren aussagen wieviele uebereinstimmungen mit dem medianwaehler
print(medianframe)

library(centiserve)

# Zentralitaetsmasse berechnen

# Matrix für die Zentralitaetsmasse erzeugen
centralitytabelle <- data.frame(matrix(ncol=1, nrow=0))

# Parteikurzbezeichnungen eintragen
for (i in c(1:anzahlparteienmitabgeordneten))
    {centralitytabelle[i,1] <- tabelle2[i,1]
}
# die verschiedenen Zentralitaetsmasse als Funktionen aufrufen und die ergebnisse eintragen

position <- 2
zentralitaetsmasse <- c("degree", "closeness", "betweenness", "eigen_centrality", "entropy")
for (zentralitaet in zentralitaetsmasse)
   {
    funktion <- get(zentralitaet)
    centralitytabelle[,position] <- funktion(fgr)
    position <- position + 1
   }
tabellenueberschrift <- c("partei", zentralitaetsmasse)
colnames(centralitytabelle) <- tabellenueberschrift
centralitytabelle
