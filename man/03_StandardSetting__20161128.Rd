%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel  3}
\alias{Kapitel 3}
\encoding{UTF-8}
\title{Kapitel 3: Standard-Setting}

\description{
Das ist die Nutzerseite zum Kapitel 3, \emph{Standard-Setting}, im 
Herausgeberband Large-Scale Assessment mit \R: Methodische Grundlagen der 
österreichischen Bildungsstandardüberprüfung. 
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. 
Im Abschnitt \strong{Examples} werden die \R-Syntaxen des Kapitels vollständig 
wiedergegeben und gegebenenfalls erweitert.
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Luger-Bazinger, C., Freunberger, R. & Itzlinger-Bruneforth, U. (2016). 
  Standard-Setting. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 83--110). Wien: facultas.
}

% \note{
% % wenn es Hinweise gibt
% }

\author{
  Claudia Luger-Bazinger, Roman Freunberger, Ursula Itzlinger-Bruneforth
}

\seealso{
%Verweis zu den Daten im Kapitel
Zu \code{\link{datenKapitel03}}, den im Kapitel verwendeten Daten.\cr
%Verweis zum nächsten und zum vorigen Kapitel
Zurück zu \code{\link{Kapitel 2}}, Stichprobenziehung.\cr
Zu \code{\link{Kapitel 4}}, Differenzielles Itemfunktionieren in Subgruppen.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach 
%Übersicht einfügen)
%Zur Hilfeseite von fit.r \code{\link{fit.r}}
}

%% ----------------------------------------------------------------------------
%% Details
\details{

\subsection{Übersicht über die verwendeten Daten}{
Für dieses Kapitel werden drei Datensätze verwendet. 
Der Datensatz \code{ratings} ist das Ergebnis der IDM-Methode, darin enthalten 
sind für alle Items die Einstufung jedes Raters auf eine der drei 
Kompetenzstufen (1, 2, 3), sowie Item-Nummer und Schwierigkeit. 
Der Datensatz \code{bookmarks} ist das Ergebnis der Bookmark-Methode, darin 
enthalten sind pro Rater und pro Cut-Score jeweils die gewählte Bookmark als 
Seitenzahl im OIB (die ein bestimmtes Item repräsentiert). 
In \code{sdat} sind Personenparameter von 3500 Schülerinnen und Schülern 
enthalten, diese dienen zur Schätzung von Impact Data. 
Der Datensatz \code{productive} ist für die Illustration der 
Contrasting-Groups-Methode gedacht: Dieser enthält die Ratings aus der 
Contrasting-Groups-Methode, pro Rater die Information, ob der entsprechende 
Text auf die Stufe unter- oder oberhalb des Cut-Scores eingeteilt wurde, sowie 
Nummer des Textes und Personenfähigkeit. 
}%end Subsection Übersicht über die verwendeten Daten


\subsection{Abschnitt 3.2.2: Daten aus der IDM-Methode}{
\subsection{Listing 1: Feedback}{
Hier wird der Datensatz \code{ratings} verwendet. Er ist das Ergebnis der 
IDM-Methode, darin enthalten sind für alle Items die Einstufung jedes Raters 
auf eine der drei Kompetenzstufen (1, 2, 3). Zunächst werden die Rater und die 
Items aus dem Datensatz ausgewählt, dann wird pro Item die prozentuelle 
Verteilung der Ratings auf die drei Stufen berechnet. 

\ifelse{latex}{%start latex
\code{}\cr
\code{> raterID <- grep("R", colnames(ratings), value = TRUE)}\cr
\code{> nraters <- length(raterID) }\cr
\code{> nitems <- nrow(ratings) }\cr
\code{> itemID <- ratings[, 1] }\cr
\code{> itemdiff <- ratings[, 2]}\cr
\code{> stufen <- c(1, 2, 3) # Anzahl der Kompetenzstufen}\cr
\code{> item.freq <- data.frame() }\cr
\code{> # Berechne Prozentuelle Zuteilungen auf Stufen pro Item}\cr
\code{> tabelle.ii <- data.frame()}\cr
\code{> for(ii in 1:nitems)\{   }\cr
\code{+   tabelle.ii <- round(table(factor(as.numeric(ratings[ii,}\cr
\code{+     raterID]), levels = stufen)) / nraters * 100, digits = 2)      }\cr
\code{+   item.freq <- rbind(item.freq, tabelle.ii) \}}\cr
\code{> colnames(item.freq) <- paste0("Level_", stufen)}\cr
\code{> item.freq <- data.frame(ratings[, 1:2], item.freq)}\cr
\code{> head(item.freq, 3)}\cr
\code{> # Anmerkung: Item 3 zu 100\% auf Stufe 1, Item 2 aufgeteilt }\cr
\code{> # auf Stufe 1 und 2}\cr
}{\verb{raterID <- grep("R", colnames(ratings), value = TRUE)
nraters <- length(raterID) 
nitems <- nrow(ratings) 
itemID <- ratings[, 1] 
itemdiff <- ratings[, 2]
stufen <- c(1, 2, 3) # Anzahl der Kompetenzstufen
item.freq <- data.frame() 
# Berechne Prozentuelle Zuteilungen auf Stufen pro Item
tabelle.ii <- data.frame()
for(ii in 1:nitems){   
  tabelle.ii <- round(table(factor(as.numeric(ratings[ii,
    raterID]), levels = stufen)) / nraters * 100, digits = 2)      
  item.freq <- rbind(item.freq, tabelle.ii) }
colnames(item.freq) <- paste0("Level_", stufen)
item.freq <- data.frame(ratings[, 1:2], item.freq)
head(item.freq, 3)
# Anmerkung: Item 3 zu 100\% auf Stufe 1, Item 2 aufgeteilt 
# auf Stufe 1 und 2
}
}%end else / end ifelse latex

}%end Subsection Listing 1: Feedback

\subsection{Listing 1a: Ergänzung zum Buch}{
Hier wird eine Grafik erzeugt, in der das Rating-Verhalten sichtbar wird: Pro 
Item wird angezeigt, wieviele Prozent der Raters es auf eine der drei Stufen 
eingeteilt haben. 
Zunächst werden drei verschiedene Farben definiert, anschließend werden drei 
Barplots erstellt, die zusammen auf einer Seite dargestellt werden. 
Die Grafik wird zur Orientierung bei Diskussionen verwendet, da so schnell 
ersichtlich ist, bei welchen Items sich das Experten-Panel einig oder uneinig
war. 
Für die Grafik gibt es die Möglichkeit, diese in Schwarz-Weiss zu halten oder 
in Farbe zu gestalten.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> # Farben für die Grafik definieren - falls eine bunte Grafik gewünscht ist, }\cr   
\code{> # kann barcol <- c(c1, c2, c3) definiert werden}\cr   
\code{> c1 <- rgb(239/255, 214/255, 67/255)  }\cr   
\code{> c2 <- rgb(207/255, 151/255, 49/255)  }\cr   
\code{> c3 <- rgb(207/255, 109/255, 49/255)}\cr   
\code{> }\cr   
\code{> # Aufbereitung Tabelle für Grafik}\cr   
\code{> freq.dat <- t(as.matrix(item.freq[1:nitems,(3:(2+length(stufen)))]))}\cr   
\code{> barcol <- c("black", "gray", "white") }\cr   
\code{> }\cr   
\code{> #Grafik wird erzeugt}\cr   
\code{> par(mfcol=c(3,1), oma=c(0,0,3,0)) # Angeben der Plot-Anzahl      }\cr   
\code{> perplot <- round(nitems/3)    }\cr   
\code{> a <- perplot + 1   }\cr   
\code{> b <- perplot*2  }\cr   
\code{> c <- b + 1     }\cr   
\code{> d <- perplot*3}\cr   
\code{> barplot(freq.dat[,1 : perplot], col = barcol, beside = T, }\cr   
\code{+          names.arg = seq(1 , perplot), xlab = "Itemnummer (Seitenzahl im OIB)", }\cr   
\code{+          ylab = "\% Zuteilung auf Stufe", horiz = F, ylim = range(1:100))}\cr   
\code{> barplot(freq.dat[, a:b], col = barcol, beside = T, names.arg = seq(a, b), }\cr   
\code{+          xlab = "Itemnummer (Seitenzahl im OIB)", }\cr   
\code{+          ylab = "\% Zuteilung auf Stufe", }\cr   
\code{+          horiz = F, ylim = range(1:100))}\cr   
\code{> barplot(freq.dat[, c:d], col = barcol, beside = T, names.arg = seq(c, d), }\cr   
\code{+          xlab = "Itemnummer (Seitenzahl im OIB)", }\cr   
\code{+          ylab = "\% Zuteilung auf Stufe", }\cr   
\code{+          horiz = F, ylim = range(1:100))}\cr   
\code{> title("Feedback für das Experten-Panel aus der IDM-Methode", outer = T)}\cr   
}{\verb{# Farben für die Grafik definieren - falls eine bunte Grafik gewünscht ist, 
# kann barcol <- c(c1, c2, c3) definiert werden
c1 <- rgb(239/255, 214/255, 67/255)  
c2 <- rgb(207/255, 151/255, 49/255)  
c3 <- rgb(207/255, 109/255, 49/255)

# Aufbereitung Tabelle für Grafik
freq.dat <- t(as.matrix(item.freq[1:nitems,(3:(2+length(stufen)))]))
barcol <- c("black", "gray", "white") 

#Grafik wird erzeugt
par(mfcol=c(3,1), oma=c(0,0,3,0)) # Angeben der Plot-Anzahl      
perplot <- round(nitems/3)    
a <- perplot + 1   
b <- perplot*2  
c <- b + 1     
d <- perplot*3
barplot(freq.dat[,1 : perplot], col = barcol, beside = T, 
        names.arg = seq(1 , perplot), xlab = "Itemnummer (Seitenzahl im OIB)", 
        ylab = "\% Zuteilung auf Stufe", horiz = F, ylim = range(1:100))
barplot(freq.dat[, a:b], col = barcol, beside = T, names.arg = seq(a, b), 
        xlab = "Itemnummer (Seitenzahl im OIB)", 
        ylab = "\% Zuteilung auf Stufe", 
        horiz = F, ylim = range(1:100))
barplot(freq.dat[, c:d], col = barcol, beside = T, names.arg = seq(c, d), 
        xlab = "Itemnummer (Seitenzahl im OIB)", 
        ylab = "\% Zuteilung auf Stufe", 
        horiz = F, ylim = range(1:100))
title("Feedback für das Experten-Panel aus der IDM-Methode", outer = T)
}
}%end else / end ifelse latex

}%end Subsection Listing 1a: Ergänzung zum Buch



\subsection{Listing 2: Cut-Score Berechnung}{
Hier wird der Cut-Score aus den Daten der IDM-Methode mithilfe logistischer 
Regression für den ersten Rater im Experten-Panel berechnet. 
Dafür wird der zweite Cut-Score herangezogen. Zunächst müssen die 
entsprechenden Ratings für die logistische Regression umkodiert werden 
(2 = 0, 3 = 1).
Anschließend wird die logistische Regression berechnet, als unabhängige Variable 
dient die Einstufung durch den jeweiligen Experten (0, 1), als abhängige 
Variable die Itemschwierigkeit. 
Anhand der erhaltenen Koeffizienten kann der Cut-Score berechnet werden.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> library(car)}\cr   
\code{> # Rekodieren}\cr   
\code{> rate.i <- ratings[which(ratings$R01 \%in\% c(2, 3)), }\cr   
\code{+                    c("MB_Norm_rp23", "R01")] }\cr   
\code{> rate.i$R01 <-  recode(rate.i$R01, "2=0; 3=1")}\cr   
\code{> coef(cut.i <- glm(rate.i$R01  ~ rate.i$MB_Norm_rp23 , }\cr   
\code{+                family = binomial(link="logit")))}\cr   
\code{> # Berechnung des Cut-Scores laut Formel}\cr   
\code{> cut.R01 <- (-cut.i$coefficients[1])/ cut.i$coefficients[2]}\cr   
}{\verb{library(car)
# Rekodieren
rate.i <- ratings[which(ratings$R01 \%in\% c(2, 3)), 
                  c("MB_Norm_rp23", "R01")] 
rate.i$R01 <-  recode(rate.i$R01, "2=0; 3=1")
coef(cut.i <- glm(rate.i$R01  ~ rate.i$MB_Norm_rp23 , 
              family = binomial(link="logit")))
# Berechnung des Cut-Scores laut Formel
cut.R01 <- (-cut.i$coefficients[1])/ cut.i$coefficients[2]
}
}%end else / end ifelse latex

}%end Subsection Listing 2: Cut-Score Berechnung


\subsection{Listing 3: Rater-Analysen}{
Als ersten Schritt in den Rater-Analysen wird das mittlere Cohen's Kappa eines 
Raters mit allen anderen Raters berechnet. Dafür werden zunächst die Ratings 
ausgewählt und dann für jeden Rater die Übereinstimmung mit jedem anderen Rater 
paarweise berechnet. 
Anschließend werden diese Werte gemittelt und auch die Standard-Abweichung 
berechnet. 

\ifelse{latex}{%start latex
\code{}\cr   
\code{> library(irr)}\cr   
\code{> # Auswahl der Ratings}\cr   
\code{> rater.dat <- ratings[ ,grep("R", colnames(ratings))]}\cr   
\code{> # Kappa von jeder Person mit allen anderen Personen wird berechnet}\cr   
\code{> kappa.mat <- matrix(NA, nraters, nraters) }\cr   
\code{> for(ii in 1:nraters)\{  }\cr   
\code{+    rater.eins <- rater.dat[, ii]      }\cr   
\code{+    for(kk in 1:nraters)\{    }\cr   
\code{+      rater.zwei <- rater.dat[ ,kk]}\cr   
\code{+      dfr.ii <- cbind(rater.eins, rater.zwei)}\cr   
\code{+      kappa.ik <- kappa2(dfr.ii)       }\cr   
\code{+      kappa.mat[ii, kk] <- kappa.ik$value }\cr     
\code{+        \}\} }\cr   
\code{> diag(kappa.mat) <- NA }\cr   
\code{> # Berechne Mittleres Kappa für jede Person}\cr   
\code{> MW_Kappa <- round(colMeans(kappa.mat, na.rm=T), digits=2) }\cr   
\code{> SD_Kappa <- round(apply(kappa.mat, 2, sd, na.rm=T), digits=2)}\cr    
\code{> (Kappa.Stat <- data.frame("Person"= raterID, MW_Kappa, SD_Kappa))}\cr   
}{\verb{library(irr)
# Auswahl der Ratings
rater.dat <- ratings[ ,grep("R", colnames(ratings))]
# Kappa von jeder Person mit allen anderen Personen wird berechnet
kappa.mat <- matrix(NA, nraters, nraters) 
for(ii in 1:nraters){  
  rater.eins <- rater.dat[, ii]      
  for(kk in 1:nraters){    
    rater.zwei <- rater.dat[ ,kk]
    dfr.ii <- cbind(rater.eins, rater.zwei)
    kappa.ik <- kappa2(dfr.ii)       
    kappa.mat[ii, kk] <- kappa.ik$value   
      }} 
diag(kappa.mat) <- NA 
# Berechne Mittleres Kappa für jede Person
MW_Kappa <- round(colMeans(kappa.mat, na.rm=T), digits=2) 
SD_Kappa <- round(apply(kappa.mat, 2, sd, na.rm=T), digits=2) 
(Kappa.Stat <- data.frame("Person"= raterID, MW_Kappa, SD_Kappa))
}
}%end else / end ifelse latex

}%end Subsection Listing 3: Rater-Analysen


\subsection{Listing 4: Berechnung Fleiss' Kappa}{
Fleiss' Kappa gibt die Übereinstimmung innerhalb des gesamten Experten-Panels 
an. 
Wird das Standard-Setting über mehrere Runden durchgeführt, kann Fleiss' Kappa 
auch für jede Runde berechnet werden. 

\ifelse{latex}{%start latex
\code{}\cr   
\code{> kappam.fleiss(rater.dat)}\cr   
}{\verb{kappam.fleiss(rater.dat)
}
}%end else / end ifelse latex

}%end Subsection Listing 4: Berechnung Fleiss' Kappa


\subsection{Listing 5: Modalwerte}{
Auch die Korrelation zwischen dem Modalwert jedes Items (d.h., ob es am 
häufigsten auf Stufe 1, 2 oder 3 eingeteilt wurde) und der inviduellen 
Zuordnung durch einen Rater kann zu Rater-Analysen herangezogen werden. 
Zunächst wird der Modal-Wert eines jeden Items berechnet. 
Hat ein Item zwei gleich häufige Werte, gibt es eine Warnmeldung und es wird 
für dieses Item NA anstatt eines Wertes vergeben (für diese Analyse sind aber 
nur Items von Interesse, die einen eindeutigen Modalwert haben). 
Danach wird pro Rater die Korrelation zwischen dem Modalwert eines Items und 
der entsprechenden Einteilung durch den Rater berechnet, und dann in 
aufsteigender Höhe ausgegeben.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> library(prettyR)}\cr   
\code{> # Berechne Modalwert}\cr   
\code{> mode <- as.numeric(apply(rater.dat, 1, Mode))}\cr   
\code{> # Korrelation für die Ratings jeder Person im Panel mit den }\cr   
\code{> # Modalwerten der Items}\cr   
\code{> corr <- data.frame()}\cr   
\code{> for(z in raterID)\{}\cr   
\code{+    rater.ii <- rater.dat[, (grep(z, colnames(rater.dat)))]}\cr   
\code{+    cor.ii <- round(cor(mode, rater.ii, use = "pairwise.complete.obs", }\cr   
\code{+      method = "spearman"), digits = 2)}\cr   
\code{+    corr <- rbind(corr, cor.ii)}\cr   
\code{+ \}}\cr   
\code{> corr[, 2] <- raterID}\cr   
\code{> colnames(corr) <- c("Korrelation", "Rater")}\cr   
\code{> # Aufsteigende Reihenfolge }\cr   
\code{> (corr <- corr[order(corr[, 1]),])}\cr   
}{\verb{library(prettyR)
# Berechne Modalwert
mode <- as.numeric(apply(rater.dat, 1, Mode))
# Korrelation für die Ratings jeder Person im Panel mit den 
# Modalwerten der Items
corr <- data.frame()
for(z in raterID){
  rater.ii <- rater.dat[, (grep(z, colnames(rater.dat)))]
  cor.ii <- round(cor(mode, rater.ii, use = "pairwise.complete.obs", 
    method = "spearman"), digits = 2)
  corr <- rbind(corr, cor.ii)
}
corr[, 2] <- raterID
colnames(corr) <- c("Korrelation", "Rater")
# Aufsteigende Reihenfolge 
(corr <- corr[order(corr[, 1]),])
}
}%end else / end ifelse latex

}%end Subsection Listing 5: Modalwerte


\subsection{Listing 5a: Ergänzung zum Buch}{
Die Korrelation zwischen Modalwerten und individueller Zuordnung kann auch zur 
besseren Übersicht graphisch gezeigt werden. 
Dabei werden die Korrelationen der Raters aufsteigend dargestellt.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> # Grafik}\cr   
\code{> plot(corr$Korrelation, xlab = NA, ylab = "Korrelation",   }\cr   
\code{+       ylim = c(0.5, 1), xaxt = "n", main = \"Korrelation zwischen }\cr   
\code{+       Modalwert und individueller Zuordnung der Items pro Rater/in\")}\cr   
\code{> text(seq(1:nraters), corr$Korrelation - 0.02, labels = corr[, 2], }\cr   
\code{+       offset = 1, cex = 1)}\cr   
\code{> title(xlab = "Raters nach aufsteigender Korrelation gereiht")}\cr   
}{\verb{# Grafik
plot(corr$Korrelation, xlab = NA, ylab = "Korrelation",   
     ylim = c(0.5, 1), xaxt = "n", main = "Korrelation zwischen 
     Modalwert und individueller Zuordnung der Items pro Rater/in")
text(seq(1:nraters), corr$Korrelation - 0.02, labels = corr[, 2], 
     offset = 1, cex = 1)
title(xlab = "Raters nach aufsteigender Korrelation gereiht")
}
}%end else / end ifelse latex

}%end Subsection Listing 5a: Ergänzung zum Buch


\subsection{Listing 6: ICC}{
Hier wird der ICC als Ausdruck der Übereinstimmung (d.h., Items werden auf 
dieselbe Stufe eingeteilt) und der Konsistenz (d.h., Items werden in dieselbe 
Reihenfolge gebracht) zwischen Raters berechnet. 
Falls es mehrere Runden gibt, kann der ICC auch wiederholt berechnet und 
verglichen werden.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> library(irr)}\cr   
\code{> (iccdat.agree <- icc(rater.dat, model = "twoway", type = "agreement", }\cr   
\code{+                       unit = "single", r0 = 0, conf.level=0.95))}\cr   
\code{> (iccdat.cons <- icc(rater.dat, model = "twoway", type = "consistency", }\cr   
\code{+                      unit = "single", r0 = 0, conf.level=0.95))}\cr   
}{\verb{library(irr)
(iccdat.agree <- icc(rater.dat, model = "twoway", type = "agreement", 
                     unit = "single", r0 = 0, conf.level=0.95))
(iccdat.cons <- icc(rater.dat, model = "twoway", type = "consistency", 
                    unit = "single", r0 = 0, conf.level=0.95))
}
}%end else / end ifelse latex

}%end Subsection Listing 6: ICC
}%end Subsection Abschnitt 3.2.2: Daten aus der IDM-Methode


 
\subsection{Abschnitt 3.2.3: Daten aus der Bookmark-Methode}{

\subsection{Listing 1: Feedback}{
Auch in der Bookmark-Methode wird dem Experten-Panel Feedback angeboten, um die 
Diskussion zu fördern. 
Hier wird pro Cut-Score Median, Mittelwert und Standard-Abweichung der 
Bookmarks (Seitenzahl im OIB) im Experten-Panel berechnet.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> head(bookmarks)}\cr   
\code{> statbookm <- data.frame("Stats"=c("Md","Mean","SD"), }\cr   
\code{+                          "Cut1"=0, "Cut2"=0)}\cr   
\code{> statbookm[1,2] <- round(median(bookmarks$Cut1), digits=2)}\cr   
\code{> statbookm[1,3] <- round(median(bookmarks$Cut2), digits=2)}\cr   
\code{> statbookm[2,2] <- round(mean(bookmarks$Cut1), digits=2)}\cr   
\code{> statbookm[2,3] <- round(mean(bookmarks$Cut2), digits=2)}\cr   
\code{> statbookm[3,2] <- round(sd(bookmarks$Cut1), digits=2)}\cr   
\code{> statbookm[3,3] <- round(sd(bookmarks$Cut2), digits=2)}\cr   
\code{> (statbookm)}\cr   
\code{> table(bookmarks$Cut1)}\cr   
\code{> table(bookmarks$Cut2)}\cr   
}{\verb{head(bookmarks)
statbookm <- data.frame("Stats"=c("Md","Mean","SD"), 
                        "Cut1"=0, "Cut2"=0)
statbookm[1,2] <- round(median(bookmarks$Cut1), digits=2)
statbookm[1,3] <- round(median(bookmarks$Cut2), digits=2)
statbookm[2,2] <- round(mean(bookmarks$Cut1), digits=2)
statbookm[2,3] <- round(mean(bookmarks$Cut2), digits=2)
statbookm[3,2] <- round(sd(bookmarks$Cut1), digits=2)
statbookm[3,3] <- round(sd(bookmarks$Cut2), digits=2)
(statbookm)
table(bookmarks$Cut1)
table(bookmarks$Cut2)
}
}%end else / end ifelse latex

}%end Subsection Listing 1: Feedback

\subsection{Listing 2: Cut-Score Berechnung}{
Jede Bookmark repräsentiert ein Item, das eine bestimmte Itemschwierigkeit hat. 
Die Cut-Scores lassen sich berechnen, in dem man die unterliegenden 
Itemschwierigkeiten der Bookmarks mittelt.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> bm.cut <- NULL }\cr   
\code{> bm.cut$cut1 <- mean(ratings$MB_Norm_rp23[bookmarks$Cut1])  }\cr   
\code{> bm.cut$cut2 <- mean(ratings$MB_Norm_rp23[bookmarks$Cut2])  }\cr   
\code{> bm.cut$cut1sd <- sd(ratings$MB_Norm_rp23[bookmarks$Cut1])  }\cr   
\code{> bm.cut$cut2sd <- sd(ratings$MB_Norm_rp23[bookmarks$Cut2])}\cr   
}{\verb{bm.cut <- NULL 
bm.cut$cut1 <- mean(ratings$MB_Norm_rp23[bookmarks$Cut1]) 
bm.cut$cut2 <- mean(ratings$MB_Norm_rp23[bookmarks$Cut2]) 
bm.cut$cut1sd <- sd(ratings$MB_Norm_rp23[bookmarks$Cut1]) 
bm.cut$cut2sd <- sd(ratings$MB_Norm_rp23[bookmarks$Cut2]) 
}
}%end else / end ifelse latex

}%end Subsection Listing 2: Cut-Score Berechnung


\subsection{Listing 3: Standardfehler des Cut-Scores}{
Der Standardfehler wird berechnet, um eine mögliche Streuung des Cut-Scores 
zu berichten. 

\ifelse{latex}{%start latex
\code{}\cr   
\code{> se.cut1 <- bm.cut$cut1sd/sqrt(nraters)}\cr   
\code{> se.cut2 <- bm.cut$cut2sd/sqrt(nraters)}\cr   
}{\verb{se.cut1 <- bm.cut$cut1sd/sqrt(nraters)
se.cut2 <- bm.cut$cut2sd/sqrt(nraters)
}
}%end else / end ifelse latex

}%end Subsection Listing 3: Standardfehler des Cut-Scores


\subsection{Listing 4: Impact Data}{
Mithilfe von Impact Data wird auf Basis von pilotierten Daten geschätzt, welche 
Auswirkungen die Cut-Scores auf die Schülerpopulation hätten (d.h., wie sich die
Schülerinnen und Schüler auf die Stufen verteilen würden).
Für diese Schätzung werden die Personenparameter herangezogen.
Anschließend wird die Verteilung der Personenparameter entsprechend der 
Cut-Scores unterteilt. 
Die Prozentangaben der Schülerinnen und Schüler, die eine bestimmte Stufe 
erreichen, dienen dem Experten-Panel als Diskussionsgrundlage.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> Pers.Para <- sdat[, "TPV1"]}\cr   
\code{> cuts <- c(bm.cut$cut1, bm.cut$cut2)}\cr   
\code{> # Definiere Bereiche: Minimaler Personenparameter bis Cut-Score 1, }\cr   
\code{> #   Cut-Score 1 bis Cut-Score 2, Cut-Score 2 bis maximaler }\cr   
\code{> #   Personenparameter}\cr   
\code{> Cuts.Vec <- c(min(Pers.Para)-1, cuts, max(Pers.Para)+1)}\cr   
\code{> # Teile Personenparameter in entsprechende Bereiche auf}\cr   
\code{> Kum.Cuts <- cut(Pers.Para, breaks = Cuts.Vec)}\cr   
\code{> # Verteilung auf die einzelnen Bereiche}\cr   
\code{> Freq.Pers.Para <- xtabs(~ Kum.Cuts)}\cr   
\code{> nstud <- nrow(sdat)}\cr   
\code{> # Prozent-Berechnung}\cr   
\code{> prozent <- round(as.numeric(Freq.Pers.Para / nstud * 100), }\cr   
\code{+    digits = 2) }\cr   
\code{> (Impact.Data <- data.frame("Stufe" = c("A1", "A2", "B1"), }\cr   
\code{+    "Prozent" = prozent))}\cr   
}{\verb{Pers.Para <- sdat[, "TPV1"]
cuts <- c(bm.cut$cut1, bm.cut$cut2)
# Definiere Bereiche: Minimaler Personenparameter bis Cut-Score 1, 
#   Cut-Score 1 bis Cut-Score 2, Cut-Score 2 bis maximaler 
#   Personenparameter
Cuts.Vec <- c(min(Pers.Para)-1, cuts, max(Pers.Para)+1)
# Teile Personenparameter in entsprechende Bereiche auf
Kum.Cuts <- cut(Pers.Para, breaks = Cuts.Vec)
# Verteilung auf die einzelnen Bereiche
Freq.Pers.Para <- xtabs(~ Kum.Cuts)
nstud <- nrow(sdat)
# Prozent-Berechnung
prozent <- round(as.numeric(Freq.Pers.Para / nstud * 100), 
  digits = 2) 
(Impact.Data <- data.frame("Stufe" = c("A1", "A2", "B1"), 
  "Prozent" = prozent))
}
}%end else / end ifelse latex
}%end Subsection Listing 4: Impact Data
}%end Subsection Abschnitt 3.2.3: Daten aus der Bookmark-Methode


\subsection{Abschnitt 3.3.3: Daten aus der Contrasting-Groups-Methode}{

\subsection{Listing 1: Cut-Scores}{
Hier wird der Cut-Score für den produktiven Bereich Schreiben berechnet, die 
Basis ist dabei die Personenfähigkeeit. 
Dabei wird pro Rater vorgegangen. Für jeden Rater werden dabei zwei Gruppen 
gebildet - Texte, die auf die untere Stufe eingeteilt wurden und Texte, die auf 
die obere Stufe eingeteilt wurden. 
Von beiden Gruppen wird jeweils der Mittelwert der Personenfähigkeit berechnet 
und anschließend der Mittelwert zwischen diesen beiden Gruppen. 
Wurde das für alle Raters durchgeführt, können die individuell gesetzten 
Cut-Scores wiederum gemittelt werden und die Standard-Abweichung sowie der 
Standardfehler berechnet werden.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> raterID <- grep("R", colnames(productive), value = TRUE) }\cr   
\code{> nraters <- length(raterID)  }\cr   
\code{> nscripts <- nrow(productive) }\cr   
\code{> # Berechne Cut-Score für jeden Rater}\cr   
\code{> cutscore <- data.frame("rater"=raterID, "cut1.ges"=NA)}\cr   
\code{> for(ii in 1:length(raterID))\{ }\cr   
\code{+    rater <- raterID[ii]   }\cr   
\code{+    rates.ii <-  productive[ ,grep(rater, colnames(productive))]   }\cr   
\code{+    mean0.ii <- mean(productive$Performance[rates.ii == 0], na.rm = T)}\cr      
\code{+    mean1.ii <- mean(productive$Performance[rates.ii == 1], na.rm = T)   }\cr   
\code{+    mean.ii <- mean(c(mean1.ii, mean0.ii), na.rm = T)   }\cr   
\code{+    cutscore[ii, "cut1.ges"] <- mean.ii \}}\cr   
\code{> # Finaler Cut-Score}\cr   
\code{> cut1 <- mean(cutscore$cut1.ges)}\cr   
\code{> sd.cut1 <- sd(cutscore$cut1.ges)}\cr   
\code{> se.cut1 <- sd.cut1/sqrt(nraters)}\cr   
}{\verb{raterID <- grep("R", colnames(productive), value = TRUE) 
nraters <- length(raterID)  
nscripts <- nrow(productive) 
# Berechne Cut-Score für jeden Rater
cutscore <- data.frame("rater"=raterID, "cut1.ges"=NA)
for(ii in 1:length(raterID)){ 
  rater <- raterID[ii]   
  rates.ii <-  productive[ ,grep(rater, colnames(productive))]   
  mean0.ii <- mean(productive$Performance[rates.ii == 0], na.rm = T)   
  mean1.ii <- mean(productive$Performance[rates.ii == 1], na.rm = T)   
  mean.ii <- mean(c(mean1.ii, mean0.ii), na.rm = T)   
  cutscore[ii, "cut1.ges"] <- mean.ii }
# Finaler Cut-Score
cut1 <- mean(cutscore$cut1.ges)
sd.cut1 <- sd(cutscore$cut1.ges)
se.cut1 <- sd.cut1/sqrt(nraters)
}
}%end else / end ifelse latex

}%end Subsection Listing 1: Cut-Scores
}%end Subsection Abschnitt 3.3.3: Daten aus der Contrasting-Groups-Methode


\subsection{Appendix: Abbildungen im Buch}{
Hier ist der R-Code für die im Buch abgedruckten Grafiken zu finden.

\subsection{Abbildung 3.1}{
In einem nächsten Schritt wird anhand des mittleren Kappa und der dazugehörigen 
Standard-Abweichung eine Grafik erstellt, um die Übereinstimmung eines Raters 
mit allen anderen Ratern dazustellen. 
Dafür wird zunächst ein Boxplot des mittleren Kappa pro Rater erzeugt. 
In einem zweiten Schritt werden die mittleren Kappas mit der dazugehörigen 
Standard-Abweichung abgetragen. 
Linien markieren 1.5 Standard-Abweichungen vom Mittelwert. 
Raters, die über oder unter dieser Grenze liegen, werden gekennzeichnet.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> # GRAFIK}\cr   
\code{> # 1. Grafik}\cr   
\code{> par(fig=c(0, 1, 0, 0.35), oma=c(0,0,3,0), cex = 0.85) }\cr   
\code{> boxplot(Kappa.Stat$MW_Kappa, horizontal = T, ylim=c(0.42,0.66), }\cr   
\code{+          axes = F, xlab = "MW Kappa")}\cr   
\code{> # 2. Grafik wird hinzugefügt}\cr   
\code{> par(fig=c(0, 1, 0.2, 1), new=TRUE)}\cr   
\code{> sd.factor <- 1.5 }\cr   
\code{> mmw <- mean(Kappa.Stat$MW_Kappa)}\cr   
\code{> sdmw <- sd(Kappa.Stat$MW_Kappa)}\cr   
\code{> #Grenzwerte für MW und SD werden festgelegt}\cr   
\code{> mwind <- c(mmw-(sd.factor*sdmw), mmw+(sd.factor*sdmw))}\cr   
\code{> plot(Kappa.Stat$MW_Kappa, Kappa.Stat$SD_Kappa, xlab = "",}\cr   
\code{+       ylab = "SD Kappa", type = "n", xlim = c(0.42, 0.66), }\cr   
\code{+       ylim = c(0, 0.2))}\cr   
\code{> abline(v = mwind, col="grey", lty = 2)}\cr   
\code{> # Rater mit 1.5 SD Abweichung vom MW werden grau markiert }\cr   
\code{> abw.rater <- which(Kappa.Stat$MW_Kappa < mwind[1] | }\cr   
\code{+                       Kappa.Stat$MW_Kappa > mwind[2])}\cr   
\code{> points(Kappa.Stat$MW_Kappa[-abw.rater], }\cr   
\code{+         Kappa.Stat$SD_Kappa[-abw.rater], }\cr   
\code{+         pch = 19)}\cr   
\code{> points(Kappa.Stat$MW_Kappa[abw.rater], }\cr   
\code{+         Kappa.Stat$SD_Kappa[abw.rater], }\cr   
\code{+         pch = 25, bg = "grey")}\cr   
\code{> text(Kappa.Stat$MW_Kappa[abw.rater], }\cr   
\code{+       Kappa.Stat$SD_Kappa[abw.rater], }\cr   
\code{+       Kappa.Stat$Person[abw.rater], }\cr   
\code{+       pos = 3) }\cr   
\code{> title("Rater-Analysen: MW und SD Kappa aus der IDM-Methode", }\cr   
\code{+        outer = TRUE)}\cr   
}{\verb{# GRAFIK
# 1. Grafik
par(fig=c(0, 1, 0, 0.35), oma=c(0,0,3,0), cex = 0.85) 
boxplot(Kappa.Stat$MW_Kappa, horizontal = T, ylim=c(0.42,0.66), 
        axes = F, xlab = "MW Kappa")
# 2. Grafik wird hinzugefügt
par(fig=c(0, 1, 0.2, 1), new=TRUE)
sd.factor <- 1.5 
mmw <- mean(Kappa.Stat$MW_Kappa)
sdmw <- sd(Kappa.Stat$MW_Kappa)
#Grenzwerte für MW und SD werden festgelegt
mwind <- c(mmw-(sd.factor*sdmw), mmw+(sd.factor*sdmw))
plot(Kappa.Stat$MW_Kappa, Kappa.Stat$SD_Kappa, xlab = "",
     ylab = "SD Kappa", type = "n", xlim = c(0.42, 0.66), 
     ylim = c(0, 0.2))
abline(v = mwind, col="grey", lty = 2)
# Rater mit 1.5 SD Abweichung vom MW werden grau markiert 
abw.rater <- which(Kappa.Stat$MW_Kappa < mwind[1] | 
                     Kappa.Stat$MW_Kappa > mwind[2])
points(Kappa.Stat$MW_Kappa[-abw.rater], 
       Kappa.Stat$SD_Kappa[-abw.rater], 
       pch = 19)
points(Kappa.Stat$MW_Kappa[abw.rater], 
       Kappa.Stat$SD_Kappa[abw.rater], 
       pch = 25, bg = "grey")
text(Kappa.Stat$MW_Kappa[abw.rater], 
     Kappa.Stat$SD_Kappa[abw.rater], 
     Kappa.Stat$Person[abw.rater], 
     pos = 3) 
title("Rater-Analysen: MW und SD Kappa aus der IDM-Methode", 
      outer = TRUE)
}
}%end else / end ifelse latex
}%end Subsection Abbildung 3.1

\subsection{Abbildung 3.2}{
Um das Feedback über die Setzung der Bookmarks an das Experten-Panel einfacher 
zu gestalten, wird eine Grafik erstellt. 
Darin sieht man pro Cut-Score, wo die Raters ihre Bookmarks (d.h. Seitenzahl 
im OIB) gesetzt haben, sowie Info über den Mittelwert dieser Bookmarks. 
Diese Grafik soll die Diskussion fördern.

\ifelse{latex}{%start latex
\code{}\cr   
\code{> nitems <- 60}\cr   
\code{> }\cr   
\code{> library(lattice)}\cr   
\code{> library(gridExtra)}\cr   
\code{> #Erster Plot mit Mittelwert}\cr   
\code{> plot.Cut1 <- dotplot(bookmarks$Rater ~ bookmarks$Cut1, col = "black", }\cr   
\code{+                       panel = function(...)\{}\cr   
\code{+                         panel.dotplot(...)}\cr   
\code{+                         panel.abline(v = mean(bookmarks$Cut1), lty = 5)}\cr   
\code{+                        \}, }\cr   
\code{+                       xlab = "Bookmarks für Cut-Score 1 (Seite im OIB)",}\cr   
\code{+                       ylab = "Raters", cex = 1.3)}\cr   
\code{> #Zweiter Plot mit Mittelwert}\cr   
\code{> plot.Cut2 <- dotplot(bookmarks$Rater ~ bookmarks$Cut2, col = "black", }\cr   
\code{+                       panel = function(...)\{}\cr   
\code{+                         panel.dotplot(...)}\cr   
\code{+                         panel.abline(v = mean(bookmarks$Cut2), lty = 5)}\cr   
\code{+                        \}, }\cr   
\code{+                       xlab = "Bookmarks für Cut-Score 2 (Seite im OIB)", }\cr   
\code{+                       ylab = "Raters", cex = 1.3)}\cr   
\code{> #Plots nebeneinander anordnen}\cr   
\code{> grid.arrange(plot.Cut1, plot.Cut2, nrow = 1, top = "Bookmarks pro Rater/in")}\cr   
}{\verb{nitems <- 60

library(lattice)
library(gridExtra)
#Erster Plot mit Mittelwert
plot.Cut1 <- dotplot(bookmarks$Rater ~ bookmarks$Cut1, col = "black", 
                     panel = function(...){
                       panel.dotplot(...)
                       panel.abline(v = mean(bookmarks$Cut1), lty = 5)
                      }, 
                     xlab = "Bookmarks für Cut-Score 1 (Seite im OIB)",
                     ylab = "Raters", cex = 1.3)
#Zweiter Plot mit Mittelwert
plot.Cut2 <- dotplot(bookmarks$Rater ~ bookmarks$Cut2, col = "black", 
                     panel = function(...){
                       panel.dotplot(...)
                       panel.abline(v = mean(bookmarks$Cut2), lty = 5)
                      }, 
                     xlab = "Bookmarks für Cut-Score 2 (Seite im OIB)", 
                     ylab = "Raters", cex = 1.3)
#Plots nebeneinander anordnen
grid.arrange(plot.Cut1, plot.Cut2, nrow = 1, top = "Bookmarks pro Rater/in")
}
}%end else / end ifelse latex

}%end Subsection Abbildung 3.2
}%end Subsection Appendix: Abbildungen im Buch
}%end Details


%% ----------------------------------------------------------------------------
%% Examples
\examples{
library(car)
library(irr)
library(prettyR)
library(lattice)
library(gridExtra)

data(datenKapitel03)
ratings <- datenKapitel03$ratings
bookmarks <- datenKapitel03$bookmarks
sdat <- datenKapitel03$sdat
productive <- datenKapitel03$productive

\dontrun{
## -------------------------------------------------------------
## Abschnitt 3.2.2: Daten aus der IDM-Methode
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 1: Feedback
#

raterID <- grep("R", colnames(ratings), value = TRUE)
nraters <- length(raterID) 
nitems <- nrow(ratings) 
itemID <- ratings[, 1] 
itemdiff <- ratings[, 2]
stufen <- c(1, 2, 3) # Anzahl der Kompetenzstufen
item.freq <- data.frame() 
# Berechne Prozentuelle Zuteilungen auf Stufen pro Item
tabelle.ii <- data.frame()
for(ii in 1:nitems){   
  tabelle.ii <- round(table(factor(as.numeric(ratings[ii, 
    raterID]), levels = stufen)) / nraters * 100, digits = 2)      
  item.freq <- rbind(item.freq, tabelle.ii) }
colnames(item.freq) <- paste0("Level_", stufen)
item.freq <- data.frame(ratings[, 1:2], item.freq)
head(item.freq, 3)
# Anmerkung: Item 3 zu 100\% auf Stufe 1, Item 2 aufgeteilt 
# auf Stufe 1 und 2

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 1a: Ergänzung zum Buch
# GRAFIK-Erzeugung
#

# Farben für die Grafik definieren
c1 <- rgb(239/255, 214/255, 67/255)  
c2 <- rgb(207/255, 151/255, 49/255)  
c3 <- rgb(207/255, 109/255, 49/255)

# Aufbereitung Tabelle für Grafik
freq.dat <- t(as.matrix(item.freq[1:nitems,(3:(2+length(stufen)))]))
barcol <- c("black", "gray", "white") 

#Grafik wird erzeugt
par(mfcol=c(3,1), oma=c(0,0,3,0)) # Angeben der Plot-Anzahl      
perplot <- round(nitems/3)    
a <- perplot + 1   
b <- perplot*2  
c <- b + 1     
d <- perplot*3
barplot(freq.dat[,1 : perplot], col = barcol, beside = T, 
        names.arg = seq(1 , perplot), xlab = "Itemnummer (Seitenzahl im OIB)", 
        ylab = "\% Zuteilung auf Stufe", horiz = F, ylim = range(1:100))
barplot(freq.dat[, a:b], col = barcol, beside = T, names.arg = seq(a, b), 
        xlab = "Itemnummer (Seitenzahl im OIB)", 
        ylab = "\% Zuteilung auf Stufe", 
        horiz = F, ylim = range(1:100))
barplot(freq.dat[, c:d], col = barcol, beside = T, names.arg = seq(c, d), 
        xlab = "Itemnummer (Seitenzahl im OIB)", 
        ylab = "\% Zuteilung auf Stufe", 
        horiz = F, ylim = range(1:100))
title("Feedback für das Experten-Panel aus der IDM-Methode", outer = T)

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 2: Cut-Score Berechnung
#

library(car)
# Rekodieren
rate.i <- ratings[which(ratings$R01 \%in\% c(2, 3)), 
                  c("Norm_rp23", "R01")] 
rate.i$R01 <-  recode(rate.i$R01, "2=0; 3=1")
coef(cut.i <- glm(rate.i$R01  ~ rate.i$Norm_rp23 , 
                  family = binomial(link="logit")))
# Berechnung des Cut-Scores laut Formel
cut.R01 <- (-cut.i$coefficients[1])/ cut.i$coefficients[2]

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 3: Rater-Analysen
# 

library(irr)
# Auswahl der Ratings
rater.dat <- ratings[ ,grep("R", colnames(ratings))]
# Berechne Kappa von jeder Person mit allen anderen Personen
kappa.mat <- matrix(NA, nraters, nraters) 
for(ii in 1:nraters){  
  rater.eins <- rater.dat[, ii]      
  for(kk in 1:nraters){    
    rater.zwei <- rater.dat[ ,kk]
    dfr.ii <- cbind(rater.eins, rater.zwei)
    kappa.ik <- kappa2(dfr.ii)       
    kappa.mat[ii, kk] <- kappa.ik$value }} 
diag(kappa.mat) <- NA 
# Berechne Mittleres Kappa für jede Person
MW_Kappa <- round(colMeans(kappa.mat, na.rm=T), digits=2) 
SD_Kappa <- round(apply(kappa.mat, 2, sd, na.rm=T), digits=2) 
(Kappa.Stat <- data.frame("Person"= raterID, MW_Kappa, 
  SD_Kappa))

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 4: Berechnung Fleiss' Kappa
# 

kappam.fleiss(rater.dat)

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 5: Modalwerte
# 

library(prettyR)
# Berechne Modalwert
mode <- as.numeric(apply(rater.dat, 1, Mode))
# Korrelation für die Ratings jeder Person im Panel mit den 
# Modalwerten der Items
corr <- data.frame()
for(z in raterID){
  rater.ii <- rater.dat[, (grep(z, colnames(rater.dat)))]
  cor.ii <- round(cor(mode, rater.ii, method = "spearman",
    use = "pairwise.complete.obs"), digits = 2)
  corr <- rbind(corr, cor.ii)
}
corr[, 2] <- raterID
colnames(corr) <- c("Korrelation", "Rater")
# Aufsteigende Reihenfolge 
(corr <- corr[order(corr[, 1]),])

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 5: Ergänzung zum Buch
# GRAFIK-Erzeugung und ICC
#

# Grafik
plot(corr$Korrelation, xlab = NA, ylab = "Korrelation",   
     ylim = c(0.5, 1), xaxt = "n", main = "Korrelation zwischen 
     Modalwert und individueller Zuordnung der Items pro Rater/in")
text(seq(1:nraters), corr$Korrelation - 0.02, labels = corr[, 2], 
     offset = 1, cex = 1)
title(xlab = "Raters nach aufsteigender Korrelation gereiht")

# -------------------------------------------------------------
# Abschnitt 3.2.2, Listing 6: ICC
# 

library(irr)
(iccdat.agree <- icc(rater.dat, model = "twoway", 
  type = "agreement", unit = "single", r0 = 0, conf.level=0.95))
(iccdat.cons <- icc(rater.dat, model = "twoway", 
  type = "consistency", unit = "single", r0 = 0, conf.level=0.95))


## -------------------------------------------------------------
## Abschnitt 3.2.3: Daten aus der Bookmark-Methode
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 3.2.3, Listing 1: Feedback
# 

head(bookmarks)
statbookm <- data.frame("Stats"=c("Md","Mean","SD"), 
                        "Cut1"=0, "Cut2"=0)
statbookm[1,2] <- round(median(bookmarks$Cut1), digits=2)
statbookm[1,3] <- round(median(bookmarks$Cut2), digits=2)
statbookm[2,2] <- round(mean(bookmarks$Cut1), digits=2)
statbookm[2,3] <- round(mean(bookmarks$Cut2), digits=2)
statbookm[3,2] <- round(sd(bookmarks$Cut1), digits=2)
statbookm[3,3] <- round(sd(bookmarks$Cut2), digits=2)
(statbookm)
table(bookmarks$Cut1)
table(bookmarks$Cut2)

# -------------------------------------------------------------
# Abschnitt 3.2.3, Listing 2: Cut-Score Berechnung
# 

bm.cut <- NULL 
bm.cut$cut1 <- mean(ratings$Norm_rp23[bookmarks$Cut1]) 
bm.cut$cut2 <- mean(ratings$Norm_rp23[bookmarks$Cut2]) 
bm.cut$cut1sd <- sd(ratings$Norm_rp23[bookmarks$Cut1]) 
bm.cut$cut2sd <- sd(ratings$Norm_rp23[bookmarks$Cut2]) 

# -------------------------------------------------------------
# Abschnitt 3.2.3, Listing 3: Standardfehler des Cut-Scores
# 

se.cut1 <- bm.cut$cut1sd/sqrt(nraters)
se.cut2 <- bm.cut$cut2sd/sqrt(nraters)

# -------------------------------------------------------------
# Abschnitt 3.2.3, Listing 4: Impact Data
#

Pers.Para <- sdat[, "TPV1"]
cuts <- c(bm.cut$cut1, bm.cut$cut2)
# Definiere Bereiche: Minimaler Personenparameter bis Cut-Score 1, 
#   Cut-Score 1 bis Cut-Score 2, Cut-Score 2 bis maximaler 
#   Personenparameter
Cuts.Vec <- c(min(Pers.Para)-1, cuts, max(Pers.Para)+1)
# Teile Personenparameter in entsprechende Bereiche auf
Kum.Cuts <- cut(Pers.Para, breaks = Cuts.Vec)
# Verteilung auf die einzelnen Bereiche
Freq.Pers.Para <- xtabs(~ Kum.Cuts)
nstud <- nrow(sdat)
# Prozent-Berechnung
prozent <- round(as.numeric(Freq.Pers.Para / nstud * 100), 
                 digits = 2) 
(Impact.Data <- data.frame("Stufe" = c("A1", "A2", "B1"), 
                           "Prozent" = prozent))


## -------------------------------------------------------------
## Abschnitt 3.3.2: Daten aus der Contrasting-Groups-Methode
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 3.3.2, Listing 1: Cut-Scores
#

raterID <- grep("R", colnames(productive), value = TRUE) 
nraters <- length(raterID)  
nscripts <- nrow(productive) 
# Berechne Cut-Score für jeden Rater
cutscore <- data.frame("rater"=raterID, "cut1.ges"=NA)
for(ii in 1:length(raterID)){ 
  rater <- raterID[ii]   
  rates.ii <- productive[ ,grep(rater, colnames(productive))]   
  mean0.ii <- mean(productive$Performance[rates.ii == 0], 
    na.rm = TRUE)   
  mean1.ii <- mean(productive$Performance[rates.ii == 1], 
    na.rm = TRUE)   
  mean.ii <- mean(c(mean1.ii, mean0.ii), na.rm = TRUE)   
  cutscore[ii, "cut1.ges"] <- mean.ii }
# Finaler Cut-Score
cut1 <- mean(cutscore$cut1.ges)
sd.cut1 <- sd(cutscore$cut1.ges)
se.cut1 <- sd.cut1/sqrt(nraters)


## -------------------------------------------------------------
## Appendix: Abbildungen
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abbildung 3.1
#

# 1. Grafik
par(fig=c(0, 1, 0, 0.35), oma=c(0,0,3,0), cex = 0.85) 
boxplot(Kappa.Stat$MW_Kappa, horizontal = T, ylim=c(0.42,0.66), 
        axes = F, xlab = "MW Kappa")
# 2. Grafik wird hinzugefügt
par(fig=c(0, 1, 0.2, 1), new=TRUE)
sd.factor <- 1.5 
mmw <- mean(Kappa.Stat$MW_Kappa)
sdmw <- sd(Kappa.Stat$MW_Kappa)
#Grenzwerte für MW und SD werden festgelegt
mwind <- c(mmw-(sd.factor*sdmw), mmw+(sd.factor*sdmw))
plot(Kappa.Stat$MW_Kappa, Kappa.Stat$SD_Kappa, xlab = "",
     ylab = "SD Kappa", type = "n", xlim = c(0.42, 0.66), 
     ylim = c(0, 0.2))
abline(v = mwind, col="grey", lty = 2)
# Rater mit 1.5 SD Abweichung vom MW werden grau markiert 
abw.rater <- which(Kappa.Stat$MW_Kappa < mwind[1] | 
                     Kappa.Stat$MW_Kappa > mwind[2])
points(Kappa.Stat$MW_Kappa[-abw.rater], 
       Kappa.Stat$SD_Kappa[-abw.rater], 
       pch = 19)
points(Kappa.Stat$MW_Kappa[abw.rater], 
       Kappa.Stat$SD_Kappa[abw.rater], 
       pch = 25, bg = "grey")
text(Kappa.Stat$MW_Kappa[abw.rater], 
     Kappa.Stat$SD_Kappa[abw.rater], 
     Kappa.Stat$Person[abw.rater], 
     pos = 3) 
title("Rater-Analysen: MW und SD Kappa aus der IDM-Methode", 
      outer = TRUE)

# -------------------------------------------------------------
# Abbildung 3.2
#

nitems <- 60

library(lattice)
library(gridExtra)
#Erster Plot mit Mittelwert
plot.Cut1 <- dotplot(bookmarks$Rater ~ bookmarks$Cut1, col = "black", 
                     panel = function(...){
                       panel.dotplot(...)
                       panel.abline(v = mean(bookmarks$Cut1), lty = 5)
                     }, 
                     xlab = "Bookmarks für Cut-Score 1 (Seite im OIB)",
                     ylab = "Raters", cex = 1.3)
#Zweiter Plot mit Mittelwert
plot.Cut2 <- dotplot(bookmarks$Rater ~ bookmarks$Cut2, col = "black", 
                     panel = function(...){
                       panel.dotplot(...)
                       panel.abline(v = mean(bookmarks$Cut2), lty = 5)
                     }, 
                     xlab = "Bookmarks für Cut-Score 2 (Seite im OIB)", 
                     ylab = "Raters", cex = 1.3)
#Plots nebeneinander anordnen
grid.arrange(plot.Cut1, plot.Cut2, nrow = 1, top = "Bookmarks pro Rater/in")

}
}
