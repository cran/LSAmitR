%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel  7}
\alias{Kapitel 7}
\encoding{UTF-8}
\title{Kapitel 7: Statistische Analysen produktiver Kompetenzen}

\description{
Das ist die Nutzerseite zum Kapitel 7, \emph{Statistische Analysen produktiver 
Kompetenzen}, im Herausgeberband Large-Scale Assessment mit \R: Methodische 
Grundlagen der österreichischen Bildungsstandardüberprüfung. 
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. 
Im Abschnitt \strong{Examples} werden die \R-Syntaxen des Kapitels vollständig 
wiedergegeben und gegebenenfalls erweitert.
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Freunberger, R., Robitzsch, A. & Luger-Bazinger, C. (2016). Statistische 
  Analysen produktiver Kompetenzen. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 225--258). Wien: facultas.
}

%\note{
% wenn es Hinweise gibt
%}

\author{
  Roman Freunberger, Alexander Robitzsch, Claudia Luger-Bazinger
}

\seealso{
%Verweis zu den Daten im Kapitel
Zu \code{\link{datenKapitel07}}, den im Kapitel verwendeten Daten.\cr
%Verweis zum nächsten und zum vorigen Kapitel
Zurück zu \code{\link{Kapitel 6}}, Skalierung und Linking.\cr
Zu \code{\link{Kapitel 8}}, Fehlende Daten und Plausible Values.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.\cr
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen.
Zur Hilfeseite von \code{\link[TAM:TAM-package]{TAM}}
}

%% ----------------------------------------------------------------------------
%% Details
\details{
\subsection{Abschnitt 1: Beispieldatensätze}{

Der zur Illustration verwendete Datensatz \code{prodRat} beinhaltet die 
beurteilten Schreibkompetenzen im Fach Englisch auf der 8. Schulstufe von 9836 
Schüler/innen (\code{idstud}) die von insgesamt 41 Ratern (\code{rater}) 
beurteilt wurden. 
Die sechs Schreibaufgaben (\code{aufgabe}) wurden auf sechs Testhefte 
(\code{th}) aufgeteilt, wobei jede Aufgabe in genau zwei Testheften vorkommt.

Zur weiteren Analyse verwenden wir auch den Datensatz \code{prodPRat} mit 
sogenannten Pseudoratern.

Für die Analyse von Varianzkomponenten mittels Linear Mixed Effects (LME) 
Modellen verwenden wir den ursprünglichen Datensatz im Long Format 
(\code{prodRatL}). 
} % end Abschnitt 1


\subsection{Abschnitt 2: Beurteilerübereinstimmung}{

\subsection{Listing 1: Berechnen von Häufigkeitstabellen}{
Hier werden die Datensätze \code{prodRat} und \code{prodPRat} verwendet. 
Die R-Funktion \code{apply()} ermöglicht eine Anwendung einer beliebigen 
Funktion z.B. \code{prop.table()} über alle Zeilen (1) oder Spalten (2) eines 
\code{data.frame}.

\ifelse{latex}{%start latex
\code{}\cr
\code{> library(irr)}\cr
\code{> data(datenKapitel07)}\cr
\code{> prodRat <- datenKapitel07$prodRat}\cr
\code{>}\cr
\code{> # Items auswählen}\cr
\code{> items <- c("TA", "CC", "GR", "VO")}\cr
\code{> # Tabelle erzeugen}\cr
\code{> tab <- apply(prodRat[, items], 2,}\cr
\code{+                      FUN=function(x)\{}\cr
\code{+                        prop.table(table(x))*100\})}\cr
\code{> print(tab, digits = 2)}\cr
\code{> }\cr
\code{> # Mittelwert der Ratings berechnen}\cr
\code{> round(apply(prodRat[, items], 2, mean), 2)}\cr
}{\verb{library(irr)
data(datenKapitel07)
prodRat <- datenKapitel07$prodRat

# Items auswählen
items <- c("TA", "CC", "GR", "VO")
# Tabelle erzeugen
tab <- apply(prodRat[, items], 2,
                     FUN=function(x){
                       prop.table(table(x))*100})
print(tab, digits = 2)

# Mittelwert der Ratings berechnen
round(apply(prodRat[, items], 2, mean), 2)
}
}%end else / end ifelse latex

} % end Listing 1

\subsection{Listing 2: Beurteilerübereinstimmung berechnen}{
Wir verwenden den Datensatz mit Pseudoratern \code{prodPRat}. 
Die Analysen werden mit dem Paket \code{irr} durchgeführt. 

\ifelse{latex}{%start latex
\code{}\cr
\code{> prodRat <- datenKapitel07$prodRat}\cr
\code{> }\cr
\code{> items <- c("TA", "CC", "GR", "VO")}\cr
\code{> dfr <- data.frame(items, agree = NA, }\cr
\code{+                   kappa = NA, wkappa = NA, korr = NA)}\cr
\code{> for(i in 1:length(items))\{}\cr
\code{+   dat.i <- prodPRat[, grep(items[i], colnames(prodPRat))]}\cr
\code{+   dfr[i, "agree"] <- agree(dat.i, tolerance = 1)["value"]}\cr
\code{+   dfr[i, "kappa"] <- kappa2(dat.i)["value"]}\cr
\code{+   dfr[i, "wkappa"] <- kappa2(dat.i, weight = "squared")["value"]}\cr
\code{+   dfr[i, "korr"] <- cor(dat.i[,1], dat.i[,2])}\cr
\code{+   dfr[i, "icc"] <- icc(dat.i, model = "twoway")["value"]}\cr
\code{+ \}}\cr
\code{> print(dfr, digits = 3)}\cr
}{\verb{prodRat <- datenKapitel07$prodRat

items <- c("TA", "CC", "GR", "VO")
dfr <- data.frame(items, agree = NA, 
                  kappa = NA, wkappa = NA, korr = NA)
for(i in 1:length(items)){
  dat.i <- prodPRat[, grep(items[i], colnames(prodPRat))]
  dfr[i, "agree"] <- agree(dat.i, tolerance = 1)["value"]
  dfr[i, "kappa"] <- kappa2(dat.i)["value"]
  dfr[i, "wkappa"] <- kappa2(dat.i, weight = "squared")["value"]
  dfr[i, "korr"] <- cor(dat.i[,1], dat.i[,2])
  dfr[i, "icc"] <- icc(dat.i, model = "twoway")["value"]
}
print(dfr, digits = 3)
}
}%end else / end ifelse latex

} % end Listing 2
} % end Abschnitt 2


\subsection{Abschnitt 3: Skalierungsmodelle}{

\subsection{Listing 1: Skalierungsmodell mit TAM}{

Der Funktion \code{tam.mm.mfr()} muss ein \code{data.frame} für die Facetten 
übergeben werden. 
Zusätzlich können Einstellungen in einer Liste für das Argument 
\code{control = list()} übergeben werden. 
Hier verwenden wir die Einstellung \code{xsi.start0 = 1}, was dazu führt, dass 
alle Startwerte auf 0 gesetzt werden. 
Mit \code{fac.oldxsi = 0.1} setzen wir das Gewicht der Parameterwerte aus der 
vorigen Iteration etwas über 0. 
Damit kann der Algorithmus stabilisiert und Konvergenzprobleme (deviance 
increase) verhindert werden. Wir definieren noch \code{increment.factor = 1.05} 
etwas über dem default-Wert von 1 um mögliche Konvergenzprobleme abzufangen. 
Dieser Wert definiert das Ausmaß der Abnahme des maximalen Zuwachs der 
Parameter pro Iteration (s. TAM-Hilfe).

Die Personenparameter werden mit der Funktion \code{tam.wle()} geschätzt.

Gibt man in der Funktion \code{summary()} das Argument \code{file} an, so wird 
der Output direkt in ein Textfile geschrieben.

\ifelse{latex}{%start latex
\code{}\cr
\code{> set.seed(1234)}\cr
\code{> library(TAM)}\cr
\code{> }\cr
\code{> prodRat <- datenKapitel07$prodRat}\cr
\code{> }\cr
\code{> # Rater-Facette definieren}\cr
\code{> facets <- prodRat[, "rater", drop = FALSE] }\cr
\code{> }\cr
\code{> # Response Daten definieren}\cr
\code{> vars <- c("TA", "CC", "GR", "VO")}\cr
\code{> resp <- prodRat[, vars] }\cr
\code{> }\cr
\code{> # Personen-ID definieren}\cr
\code{> pid <- prodRat$idstud }\cr
\code{> }\cr
\code{> # Formel für Modell}\cr
\code{> formulaA <- ~item*step+item*rater}\cr
\code{> }\cr
\code{> # Modell berechnen}\cr
\code{> mod <- tam.mml.mfr(resp = resp, facets = facets, formulaA = formulaA,   }\cr
\code{+                    pid = pid, control=list(xsi.start0 = 1, }\cr
\code{+                                            fac.oldxsi = 0.1, }\cr
\code{+                                            increment.factor = 1.05))}\cr
}{\verb{set.seed(1234)
library(TAM)

prodRat <- datenKapitel07$prodRat

# Rater-Facette definieren
facets <- prodRat[, "rater", drop = FALSE] 

# Response Daten definieren
vars <- c("TA", "CC", "GR", "VO")
resp <- prodRat[, vars] 

# Personen-ID definieren
pid <- prodRat$idstud 

# Formel für Modell
formulaA <- ~item*step+item*rater

# Modell berechnen
mod <- tam.mml.mfr(resp = resp, facets = facets, formulaA = formulaA,   
                   pid = pid, control=list(xsi.start0 = 1, 
                                           fac.oldxsi = 0.1, 
                                           increment.factor = 1.05))
summary(mod, file="TAM_MFRM")

# Personenparameter und Rohscores
persons.mod <- tam.wle(mod)
persons.mod$raw.score <- persons.mod$PersonScores / (persons.mod$N.items) 
}
}%end else / end ifelse latex

} % end Listing 1


\subsection{Listing 1b (Ergänzung zum Buch): Skalierungsmodell mit TAM}{

Hier werden alle im Buch besprochenen Modelle berechnet und anschließend ein 
Modellvergleich durchgeführt.

\ifelse{latex}{%start latex
\code{}\cr
\code{> f1 <- ~item * rater * step}\cr
\code{> mod1 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f1,  }\cr 
\code{+                     pid = pid, control=list(xsi.start0 = 1, }\cr
\code{+                                             fac.oldxsi = 0.1,}\cr 
\code{+                                             increment.factor = 1.05))}\cr
\code{> f2 <- ~item*step+item*rater}\cr 
\code{> mod2 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f2,   }\cr 
\code{+                     pid = pid, control=list(xsi.start0 = 1, }\cr 
\code{+                                             fac.oldxsi = 0.1, }\cr 
\code{+                                             increment.factor = 1.05))}\cr 
\code{> f3 <- ~item * step + rater}\cr 
\code{> mod3 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f3,   }\cr 
\code{+                     pid = pid, control=list(xsi.start0 = 1, }\cr 
\code{+                                             fac.oldxsi = 0.1, }\cr 
\code{+                                             increment.factor = 1.05))}\cr 
\code{> f4 <- ~item + step + rater}\cr 
\code{> mod4 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f4,   }\cr 
\code{+                     pid = pid, control=list(xsi.start0 = 1, }\cr 
\code{+                                             fac.oldxsi = 0.1, }\cr 
\code{+                                             increment.factor = 1.05))}\cr 
\code{> mod4$xsi.facets}\cr 
\code{> IRT.compareModels(mod1, mod2, mod3, mod4)}\cr
}{\verb{f1 <- ~item * rater * step
mod1 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f1,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
f2 <- ~item*step+item*rater
mod2 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f2,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
f3 <- ~item * step + rater
mod3 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f3,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
f4 <- ~item + step + rater
mod4 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f4,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
mod4$xsi.facets
IRT.compareModels(mod1, mod2, mod3, mod4)
}
}%end else / end ifelse latex

}

\subsection{Listing 1c (Ergänzung zum Buch): Wright-Map}{

Mit dem Paket \code{WrightMap} können die Ergebnisse für die einzelnen Facetten 
dargestellt werden. Wir machen dies für Items und Rater.

\ifelse{latex}{%start latex
\code{}\cr
\code{> library(WrightMap)}\cr
\code{> }\cr
\code{> item.labs <- vars}\cr
\code{> rater.labs <- unique(prodRat$rater)}\cr
\code{> item.labs <- c(item.labs, rep(NA, length(rater.labs) - }\cr
\code{+                                 length(item.labs)))}\cr
\code{> }\cr
\code{> pars <- mod$xsi.facets$xsi}\cr
\code{> facet <- mod$xsi.facets$facet}\cr
\code{> item.par <- pars[facet == "item"]}\cr
\code{> rater.par <- pars[facet == "rater"]}\cr
\code{> item_rat <- pars[facet == "item:rater"]}\cr
\code{> len <- length(item_rat)}\cr
\code{> item.long <- c(item.par, rep(NA, len - length(item.par)))}\cr
\code{> rater.long <- c(rater.par, rep(NA, len - length(rater.par)))}\cr
\code{> }\cr
\code{> wrightMap(persons.mod$theta, rbind(item.long, rater.long), }\cr
\code{+           label.items = c("Items",  "Rater"), }\cr
\code{+           thr.lab.text = rbind(item.labs, rater.labs), }\cr
\code{+           axis.items = "", min.l=-3, max.l=3,}\cr
\code{+           axis.persons = "Personen")}\cr
}{\verb{library(WrightMap)

item.labs <- vars
rater.labs <- unique(prodRat$rater)
item.labs <- c(item.labs, rep(NA, length(rater.labs) - 
                                length(item.labs)))

pars <- mod$xsi.facets$xsi
facet <- mod$xsi.facets$facet
item.par <- pars[facet == "item"]
rater.par <- pars[facet == "rater"]
item_rat <- pars[facet == "item:rater"]
len <- length(item_rat)
item.long <- c(item.par, rep(NA, len - length(item.par)))
rater.long <- c(rater.par, rep(NA, len - length(rater.par)))

wrightMap(persons.mod$theta, rbind(item.long, rater.long), 
          label.items = c("Items",  "Rater"), 
          thr.lab.text = rbind(item.labs, rater.labs), 
          axis.items = "", min.l=-3, max.l=3,
          axis.persons = "Personen")
}
}%end else / end ifelse latex

}


\subsection{Listing 2: Fit-Indices berechnen}{

Die Fit-Indices werden mit der Funktion \code{msq.itemfitWLE} für die 
Raterparameter und Itemparameter gesondert  berechnet. 
Der Funktion muss ein Vektor mit Parameterbezeichnungen übergeben werden so wie 
sie im Modell-Objekt vorkommen. 
Im Paket \code{TAM} gibt es noch die Funktion \code{tam.fit()}, diese basiert 
auf einer Simulation der individuellen Posterior-Verteilung. 
Die Funktion \code{msq.itemfitWLE} wertet dagegen die individuelle 
Posterior-Verteilung direkt aus (s. \code{TAM}-Hilfe für weitere Beispiele) und 
führt keine Simulation durch.

\ifelse{latex}{%start latex
\code{}\cr
\code{> # Infit/Outfit berechnen}\cr
\code{> pseudo_items <- colnames(mod$resp)}\cr
\code{> pss <- strsplit(pseudo_items , split="-")}\cr
\code{> item_parm <- unlist(lapply(pss, FUN = function(ll){ll[1]}))}\cr
\code{> rater_parm <- unlist(lapply(pss, FUN = function(ll){ll[2]}))}\cr
\code{> }\cr
\code{> # Fit Items}\cr
\code{> res.items <- msq.itemfitWLE(mod, item_parm)}\cr
\code{> summary(res.items)}\cr
\code{> }\cr
\code{> # Fit Rater}\cr
\code{> res.rater <- msq.itemfitWLE(mod, rater_parm)}\cr
\code{> summary(res.rater)}\cr
}{\verb{# Infit/Outfit berechnen
pseudo_items <- colnames(mod$resp)
pss <- strsplit(pseudo_items , split="-")
item_parm <- unlist(lapply(pss, FUN = function(ll){ll[1]}))
rater_parm <- unlist(lapply(pss, FUN = function(ll){ll[2]}))

# Fit Items
res.items <- msq.itemfitWLE(mod, item_parm)
summary(res.items)

# Fit Rater
res.rater <- msq.itemfitWLE(mod, rater_parm)
summary(res.rater)
}
}%end else / end ifelse latex

}

\subsection{Listing 2a (Ergänzung zum Buch): Abbildung Fit-Indices}{
\ifelse{latex}{%start latex
\code{}\cr
\code{> # Abbildung: Histogramm, Rohscores}\cr
\code{> }\cr
\code{> par(mfcol=c(1,2))}\cr
\code{> }\cr
\code{> hist(persons.mod$theta, col="grey", breaks=40, }\cr
\code{+      main = "",}\cr
\code{+      xlab = "Theta (logits)",}\cr
\code{+      ylab = "Häufigkeit")}\cr
\code{> with(persons.mod, scatter.smooth(raw.score, theta, }\cr
\code{+     pch = 1, cex = .6, xlab = "Roscores",}\cr
\code{+     ylab = "Theta (logits)", }\cr
\code{+     lpars = list(col = "darkgrey", lwd = 2, lty = 1)))}\cr
\code{> }\cr
\code{> # Abbildung: Fit-Statistik}\cr
\code{> par(mfcol=c(1,2))}\cr
\code{> fitdat <- res.rater$fit_data}\cr
\code{> fitdat$var <- factor(substr(fitdat$item, 1, 2))}\cr
\code{> boxplot(Outfit~var, data=fitdat, }\cr
\code{+         ylim=c(0,2), main="Outfit")}\cr
\code{> boxplot(Infit~var, data=fitdat, }\cr
\code{+         ylim=c(0,2), main="Infit")}\cr
}{\verb{# Abbildung: Histogramm, Rohscores

par(mfcol=c(1,2))

hist(persons.mod$theta, col="grey", breaks=40, 
     main = "",
     xlab = "Theta (logits)",
     ylab = "Häufigkeit")
with(persons.mod, scatter.smooth(raw.score, theta, 
    pch = 1, cex = .6, xlab = "Roscores",
    ylab = "Theta (logits)", 
    lpars = list(col = "darkgrey", lwd = 2, lty = 1)))

# Abbildung: Fit-Statistik
par(mfcol=c(1,2))
fitdat <- res.rater$fit_data
fitdat$var <- factor(substr(fitdat$item, 1, 2))
boxplot(Outfit~var, data=fitdat, 
        ylim=c(0,2), main="Outfit")
boxplot(Infit~var, data=fitdat, 
        ylim=c(0,2), main="Infit")
}
}%end else / end ifelse latex

}

\subsection{Listing 2b (Ergänzung zum Buch): Korrelationen}{

Pearson und Spearman Korrelationskoeffizient wird zwischen Rohscores und Theta 
berechnet.

\ifelse{latex}{%start latex
\code{}\cr
\code{> korr <- c(with(persons.mod, cor(raw.score, theta, }\cr
\code{+                                 method = "pearson")),}\cr
\code{+           with(persons.mod, cor(raw.score, theta, }\cr
\code{+                                 method = "spearman")))}\cr
\code{> print(korr)}\cr
}{\verb{korr <- c(with(persons.mod, cor(raw.score, theta, 
                                method = "pearson")),
          with(persons.mod, cor(raw.score, theta, 
                                method = "spearman")))
print(korr)
}
}%end else / end ifelse latex

}

\subsection{Listing 3: Q3-Statistik berechnen}{

Die Q3-Statistik für lokale stochastische Unabhängigkeit wird mit der Funktion 
\code{tam.modelfit()} berechnet. 
Der Output enthält eine Vielzahl an Fit-Statistiken, für weitere Details sei 
hier auf die \code{TAM}-Hilfeseite verwiesen. 
Die adjustierte aQ3-Statistik berechnet sich aus den Q3-Werten abzüglich des 
Gesamtmittelwerts von allen Q3-Werten. 

Mit \code{tam.modelfit()} werden Fit-Statistiken für alle Rater x Item 
Kombinationen berechnet. 
Diese werden im Code unten anschließend aggregiert um eine Übersicht zu 
erhalten. 
Dazu werden zuerst nur Paare gleicher Rater ausgewählt, somit wird die 
aggregierte Q3-Statistik nur Rater-spezifisch berechnet. 
Das Objekt \code{rater.q3} beinhaltet eine Zeile pro Rater x Item Kombination. 
Kombinationen ergeben sich nur für einen Rater, nicht zwischen unterschiedlichen 
Ratern.

Anschließend kann man mit \code{aggregate()} separat über Rater und 
Kombinationen mitteln und diese als Dotplot darstellen (Paket \code{lattice}).

\ifelse{latex}{%start latex
\code{}\cr
\code{> # Q3 Statistik}\cr
\code{> mfit.q3 <- tam.modelfit(mod)}\cr
\code{> rater.pairs <- mfit.q3$stat.itempair}\cr
\code{> }\cr
\code{> # Nur Paare gleicher Rater wählen}\cr
\code{> unique.rater <- which(substr(rater.pairs$item1, 4,12) == }\cr
\code{+                       substr(rater.pairs$item2, 4,12))}\cr
\code{> rater.q3 <- rater.pairs[unique.rater, ]}\cr
\code{> }\cr
\code{> # Spalten einfügen: Rater, Kombinationen}\cr
\code{> rater.q3$rater <- substr(rater.q3$item1, 4, 12)}\cr
\code{> rater.q3 <- rater.q3[order(rater.q3$rater),]}\cr
\code{> rater.q3$kombi <- as.factor(paste(substr(rater.q3$item1, 1, 2), }\cr
\code{+                                   substr(rater.q3$item2, 1, 2), sep="_"))}\cr
\code{> }\cr
\code{> # Statistiken aggregieren: Rater, Kombinationen}\cr
\code{> dfr.raterQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$rater), mean)}\cr
\code{> colnames(dfr.raterQ3) <- c("Rater", "Q3")}\cr
\code{> dfr.itemsQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$kombi), mean)}\cr
\code{> colnames(dfr.itemsQ3) <- c("Items", "Q3")}\cr
\code{> dfr.itemsQ3}\cr
}{\verb{# Q3 Statistik
mfit.q3 <- tam.modelfit(mod)
rater.pairs <- mfit.q3$stat.itempair

# Nur Paare gleicher Rater wählen
unique.rater <- which(substr(rater.pairs$item1, 4,12) == 
                      substr(rater.pairs$item2, 4,12))
rater.q3 <- rater.pairs[unique.rater, ]

# Spalten einfügen: Rater, Kombinationen
rater.q3$rater <- substr(rater.q3$item1, 4, 12)
rater.q3 <- rater.q3[order(rater.q3$rater),]
rater.q3$kombi <- as.factor(paste(substr(rater.q3$item1, 1, 2), 
                                  substr(rater.q3$item2, 1, 2), sep="_"))

# Statistiken aggregieren: Rater, Kombinationen
dfr.raterQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$rater), mean)
colnames(dfr.raterQ3) <- c("Rater", "Q3")
dfr.itemsQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$kombi), mean)
colnames(dfr.itemsQ3) <- c("Items", "Q3")
dfr.itemsQ3
}
}%end else / end ifelse latex

}

\subsection{Listing 3 (Ergänzung zum Buch): Lattice Dotplot}{
\ifelse{latex}{%start latex
\code{}\cr
\code{> library(lattice)}\cr
\code{> library(grid)}\cr
\code{> }\cr
\code{> # Lattice Dotplot}\cr
\code{> mean.values <- aggregate(rater.q3$aQ3, list(rater.q3$kombi), mean)[["x"]]}\cr
\code{> dotplot(aQ3~kombi, data=rater.q3, main="Q3-Statistik", ylab="Q3 (adjustiert)",}\cr
\code{+         col="darkgrey", }\cr
\code{+         panel = function(x,...)\{}\cr
\code{+           panel.dotplot(x,...)}\cr
\code{+           panel.abline(h = 0, col.line = "grey", lty=3)}\cr
\code{+           grid.points(1:6, mean.values, pch=17)}\cr
\code{+         \})}\cr
}{\verb{library(lattice)
library(grid)

# Lattice Dotplot
mean.values <- aggregate(rater.q3$aQ3, list(rater.q3$kombi), mean)[["x"]]
dotplot(aQ3~kombi, data=rater.q3, main="Q3-Statistik", ylab="Q3 (adjustiert)",
        col="darkgrey", 
        panel = function(x,...){
          panel.dotplot(x,...)
          panel.abline(h = 0, col.line = "grey", lty=3)
          grid.points(1:6, mean.values, pch=17)
        })
}
}%end else / end ifelse latex

}
} % end Abschnitt 3


\subsection{Abschnitt 4: Generalisierbarkeitstheorie}{

\subsection{Listing 1: Varianzkomponenten mit lme4 berechnen}{

Mit der Funktion \code{lmer()} aus dem Paket \code{lme4} schätzen wir die 
Varianzkomponenten. 
In der Formel definieren wir dabei die Facetten als random effects.

\ifelse{latex}{%start latex
\code{}\cr
\code{> library(lme4)}\cr
\code{> }\cr
\code{> prodRatL <- datenKapitel07$prodRatL}\cr
\code{> }\cr
\code{> # Formel definieren}\cr
\code{> formula1 <- response ~ (1|idstud) + (1|item) + (1|rater) +}\cr
\code{+                        (1|rater:item) + (1|idstud:rater) + }\cr
\code{+                        (1|idstud:item)}\cr
\code{> # Modell mit Interaktionen}\cr
\code{> mod.vk <- lmer(formula1, data=prodRatL)}\cr
\code{> }\cr
\code{> # Zusammenfassung der Ergebnisse}\cr
\code{> summary(mod.vk)}\cr
}{\verb{library(lme4)

prodRatL <- datenKapitel07$prodRatL

# Formel definieren
formula1 <- response ~ (1|idstud) + (1|item) + (1|rater) +
                       (1|rater:item) + (1|idstud:rater) + 
                       (1|idstud:item)
# Modell mit Interaktionen
mod.vk <- lmer(formula1, data=prodRatL)

# Zusammenfassung der Ergebnisse
summary(mod.vk)
}
}%end else / end ifelse latex

}

\subsection{Listing 1a (Ergänzung zum Buch): Summary-Funktion für 
Varianzkomponenten}{

Wir generieren eine Funktion \code{summary.VarComp()}, die den Output des 
Modells \code{mod.vk} in einen ansprechenden \code{data.frame} schreibt. 
Hier werden auch die prozentualen Anteile der Varianzkomponenten berechnet.

\ifelse{latex}{%start latex
\code{}\cr
\code{> # Helper-Function um die Varianzkomponenten zu extrahieren}\cr
\code{> summary.VarComp <- function(mod)\{ }\cr
\code{+   var.c <- VarCorr(mod)}\cr
\code{+   var.c <- c(unlist(var.c) , attr(var.c , "sc")^2)}\cr
\code{+   names(var.c)[length(var.c)] <- "Residual"}\cr
\code{+   dfr1 <- data.frame(var.c)}\cr
\code{+   colnames(dfr1) <- "Varianz"}\cr
\code{+   dfr1 <- rbind(dfr1, colSums(dfr1))}\cr
\code{+   rownames(dfr1)[nrow(dfr1)] <- "Total"}\cr
\code{+   dfr1$prop.Varianz <- 100 * (dfr1$Varianz / dfr1$Varianz[nrow(dfr1)])}\cr
\code{+   dfr1 <- round(dfr1,2)}\cr
\code{+   return(dfr1)}\cr
\code{+ \}}\cr
\code{> summary.VarComp(mod.vk)}\cr
}{\verb{# Helper-Function um die Varianzkomponenten zu extrahieren
summary.VarComp <- function(mod){ 
  var.c <- VarCorr(mod)
  var.c <- c(unlist(var.c) , attr(var.c , "sc")^2)
  names(var.c)[length(var.c)] <- "Residual"
  dfr1 <- data.frame(var.c)
  colnames(dfr1) <- "Varianz"
  dfr1 <- rbind(dfr1, colSums(dfr1))
  rownames(dfr1)[nrow(dfr1)] <- "Total"
  dfr1$prop.Varianz <- 100 * (dfr1$Varianz / dfr1$Varianz[nrow(dfr1)])
  dfr1 <- round(dfr1,2)
  return(dfr1)
}
summary.VarComp(mod.vk)
}
}%end else / end ifelse latex

}

\subsection{Listing 2: Berechnung des G-Koeffizienten}{

Den G-Koeffizienten berechnen wir nach der Formel im Buch.

\ifelse{latex}{%start latex
\code{}\cr
\code{> vk <- summary.VarComp(mod.vk)}\cr
\code{> n.p <- length(unique(prodRatL$idstud)) # Anzahl Schüler}\cr
\code{> n.i <- 4  # Anzahl Items}\cr
\code{> n.r <- c(1,2,5) # Anzahl Rater}\cr
\code{> }\cr
\code{> # Varianzkomponenten extrahieren}\cr
\code{> sig2.p <- vk["idstud", "Varianz"]}\cr
\code{> sig2.i <- vk["item", "Varianz"]}\cr
\code{> sig2.r <- vk["rater", "Varianz"]}\cr
\code{> sig2.ri <- vk["rater:item", "Varianz"]}\cr
\code{> sig2.pr <- vk["idstud:rater", "Varianz"]}\cr
\code{> sig2.pi <- vk["idstud:item", "Varianz"]}\cr
\code{> sig2.pir <- vk["Residual", "Varianz"]}\cr
\code{> }\cr
\code{> # Fehlervarianz berechnen}\cr
\code{> sig2.delta <- sig2.pi/n.i + sig2.pr/n.r + sig2.pir/(n.i*n.r) }\cr
\code{> }\cr
\code{> # G-Koeffizient berechnen}\cr
\code{> g.koeff <- sig2.p / (sig2.p + sig2.delta)}\cr
\code{> print(data.frame(n.r, g.koeff), digits = 3)}\cr
}{\verb{vk <- summary.VarComp(mod.vk)
n.p <- length(unique(prodRatL$idstud)) # Anzahl Schüler
n.i <- 4  # Anzahl Items
n.r <- c(1,2,5) # Anzahl Rater

# Varianzkomponenten extrahieren
sig2.p <- vk["idstud", "Varianz"]
sig2.i <- vk["item", "Varianz"]
sig2.r <- vk["rater", "Varianz"]
sig2.ri <- vk["rater:item", "Varianz"]
sig2.pr <- vk["idstud:rater", "Varianz"]
sig2.pi <- vk["idstud:item", "Varianz"]
sig2.pir <- vk["Residual", "Varianz"]

# Fehlervarianz berechnen
sig2.delta <- sig2.pi/n.i + sig2.pr/n.r + sig2.pir/(n.i*n.r) 

# G-Koeffizient berechnen
g.koeff <- sig2.p / (sig2.p + sig2.delta)
print(data.frame(n.r, g.koeff), digits = 3)
}
}%end else / end ifelse latex

}

\subsection{Listing 2a (Ergänzung zum Buch): Phi-Koeffizient berechnen}{

\ifelse{latex}{%start latex
\code{}\cr
\code{> sig2.D <- sig2.r/n.r + sig2.i/n.i + sig2.pi/n.i + sig2.pr/n.r + }\cr
\code{+           sig2.ri/(n.i*n.r) + sig2.pir/(n.i*n.r) }\cr
\code{> phi.koeff <- sig2.p / (sig2.p + sig2.D)}\cr
\code{> print(data.frame(n.r, phi.koeff), digits = 3)}\cr
\code{> }\cr
\code{> # Konfidenzintervalle}\cr
\code{> 1.96*sqrt(sig2.D)}\cr
}{\verb{sig2.D <- sig2.r/n.r + sig2.i/n.i + sig2.pi/n.i + sig2.pr/n.r + 
          sig2.ri/(n.i*n.r) + sig2.pir/(n.i*n.r) 
phi.koeff <- sig2.p / (sig2.p + sig2.D)
print(data.frame(n.r, phi.koeff), digits = 3)

# Konfidenzintervalle
1.96*sqrt(sig2.D)
}
}%end else / end ifelse latex

}

\subsection{Listing 2c (Ergänzung zum Buch): Variable Rateranzahl}{

Für eine variable Rateranzahl (hier 1 bis 10 Rater) werden die G-Koeffizienten 
berechnet.

\ifelse{latex}{%start latex
\code{}\cr
\code{> n.i <- 4  # Anzahl Items}\cr
\code{> dn.r <- seq(1,10) # 1 bis 10 mögliche Rater}\cr
\code{> delta.i <- sig2.pi/n.i + sig2.pr/dn.r + sig2.pir/(n.i*dn.r)}\cr
\code{> g.koeff <- sig2.p / (sig2.p + delta.i)}\cr
\code{> names(g.koeff) <- paste("nR", dn.r, sep="_") }\cr
\code{> print(g.koeff[1:4])}\cr
\code{> }\cr
\code{> plot(g.koeff, type = "b", pch = 19, lwd = 2, bty = "n",}\cr
\code{+      main = "G-Koeffizient: Raters",}\cr
\code{+      ylab = "G-Koeffizient",}\cr
\code{+      xlab = "Anzahl Raters",  xlim = c(0,10))}\cr
\code{> abline(v=2, col="darkgrey")}\cr
}{\verb{n.i <- 4  # Anzahl Items
dn.r <- seq(1,10) # 1 bis 10 mögliche Rater
delta.i <- sig2.pi/n.i + sig2.pr/dn.r + sig2.pir/(n.i*dn.r)
g.koeff <- sig2.p / (sig2.p + delta.i)
names(g.koeff) <- paste("nR", dn.r, sep="_") 
print(g.koeff[1:4])

plot(g.koeff, type = "b", pch = 19, lwd = 2, bty = "n",
     main = "G-Koeffizient: Raters",
     ylab = "G-Koeffizient",
     xlab = "Anzahl Raters",  xlim = c(0,10))
abline(v=2, col="darkgrey")
}
}%end else / end ifelse latex

}
} % end Abschnitt 4


\subsection{Abschnitt 5: Strukturgleichungsmodelle}{

In R setzen wir das Struktugleichungsmodell mit dem Paket \code{lavaan} um. 
Das Modell wird als Textvariable definiert, welche anschließend der Funktion 
\code{sem()} übergeben wird. 
Latente Variablen im Messmodell werden in \code{lavaan} mit der Form 
\code{latente Variable =~ manifeste} \code{Variable(n)} definiert, die Ladungen werden 
dabei auf den Wert 1 fixiert, was mittels der Multiplikation der Variable mit 
dem Wert 1 umgesetzt werden kann (z.B. \code{1*Variable}). 
Varianzen und Kovarianzen werden mit \code{Variable ~~ Variable} gebildet, 
wobei hier die Multiplikation mit einem Label einerseits den berechneten 
Parameter benennt, andererseits, bei mehrmaligem Auftreten des Labels, 
Parameterschätzungen von verschiedenen Variablen restringiert bzw. gleichstellt 
(z.B. wird für die Within-Varianz von \code{TA} über beide Rater nur ein 
Parameter geschätzt, nämlich \code{Vta_R12}). 
Die ICC wird für jede Dimension separat direkt im Modell spezifiziert, dies 
geschieht durch abgeleitete Variablen mit der Schreibweise 
\code{Variable := Berechnung}. 
Die Modellspezifikation und der Aufruf der Funktion \code{sem()} ist wie folgt 
definiert:

\subsection{Listing 1 (mit Ergänzung zum Buch): SEM}{
\ifelse{latex}{%start latex
\code{}\cr
\code{> library(lavaan)}\cr
\code{> }\cr
\code{> prodPRat <- datenKapitel07$prodPRat}\cr
\code{> }\cr
\code{> # SEM Modell definieren}\cr
\code{> lv.mod <- \" }\cr
\code{+   # Messmodell}\cr
\code{+   TA =~ 1*TA_R1 + 1*TA_R2}\cr
\code{+   CC =~ 1*CC_R1 + 1*CC_R2}\cr
\code{+   GR =~ 1*GR_R1 + 1*GR_R2}\cr
\code{+   VO =~ 1*VO_R1 + 1*VO_R2}\cr
\code{+   }\cr
\code{+   # Varianz Between (Personen)}\cr
\code{+   TA ~~ Vta * TA}\cr
\code{+   CC ~~ Vcc * CC}\cr
\code{+   GR ~~ Vgr * GR}\cr
\code{+   VO ~~ Vvo * VO}\cr
\code{+   }\cr
\code{+   # Varianz Within (Rater X Personen)}\cr
\code{+   TA_R1 ~~ Vta_R12 * TA_R1}\cr
\code{+   TA_R2 ~~ Vta_R12 * TA_R2}\cr
\code{+   CC_R1 ~~ Vcc_R12 * CC_R1}\cr
\code{+   CC_R2 ~~ Vcc_R12 * CC_R2}\cr
\code{+   GR_R1 ~~ Vgr_R12 * GR_R1}\cr
\code{+   GR_R2 ~~ Vgr_R12 * GR_R2}\cr
\code{+   VO_R1 ~~ Vvo_R12 * VO_R1}\cr
\code{+   VO_R2 ~~ Vvo_R12 * VO_R2}\cr
\code{+   }\cr
\code{+   # Kovarianz Within}\cr
\code{+   TA_R1 ~~ Kta_cc * CC_R1}\cr
\code{+   TA_R2 ~~ Kta_cc * CC_R2}\cr
\code{+   TA_R1 ~~ Kta_gr * GR_R1}\cr
\code{+   TA_R2 ~~ Kta_gr * GR_R2}\cr
\code{+   TA_R1 ~~ Kta_vo * VO_R1}\cr
\code{+   TA_R2 ~~ Kta_vo * VO_R2}\cr
\code{+   CC_R1 ~~ Kcc_gr * GR_R1}\cr
\code{+   CC_R2 ~~ Kcc_gr * GR_R2}\cr
\code{+   CC_R1 ~~ Kcc_vo * VO_R1}\cr
\code{+   CC_R2 ~~ Kcc_vo * VO_R2}\cr
\code{+   GR_R1 ~~ Kgr_vo * VO_R1}\cr
\code{+   GR_R2 ~~ Kgr_vo * VO_R2}\cr
\code{+   }\cr
\code{+   # ICC berechnen}\cr
\code{+   icc_ta := Vta / (Vta + Vta_R12)}\cr
\code{+   icc_cc := Vcc / (Vcc + Vcc_R12)}\cr
\code{+   icc_gr := Vgr / (Vgr + Vgr_R12)}\cr
\code{+   icc_vo := Vvo / (Vvo + Vvo_R12)}\cr
\code{+ \"}\cr
\code{> # Schätzung des Modells}\cr
\code{> mod1 <- sem(lv.mod, data = prodPRat)}\cr
\code{> summary(mod1, standardized = TRUE)}\cr
\code{> # Inspektion der Ergebnisse}\cr
\code{> show(mod1)}\cr
\code{> fitted(mod1)}\cr
\code{> inspect(mod1,"cov.lv")}\cr
\code{> inspect(mod1, "free")}\cr
}{\verb{library(lavaan)

prodPRat <- datenKapitel07$prodPRat

# SEM Modell definieren
lv.mod <- " 
  # Messmodell
  TA =~ 1*TA_R1 + 1*TA_R2
  CC =~ 1*CC_R1 + 1*CC_R2
  GR =~ 1*GR_R1 + 1*GR_R2
  VO =~ 1*VO_R1 + 1*VO_R2
  
  # Varianz Between (Personen)
  TA ~~ Vta * TA
  CC ~~ Vcc * CC
  GR ~~ Vgr * GR
  VO ~~ Vvo * VO
  
  # Varianz Within (Rater X Personen)
  TA_R1 ~~ Vta_R12 * TA_R1
  TA_R2 ~~ Vta_R12 * TA_R2
  CC_R1 ~~ Vcc_R12 * CC_R1
  CC_R2 ~~ Vcc_R12 * CC_R2
  GR_R1 ~~ Vgr_R12 * GR_R1
  GR_R2 ~~ Vgr_R12 * GR_R2
  VO_R1 ~~ Vvo_R12 * VO_R1
  VO_R2 ~~ Vvo_R12 * VO_R2
  
  # Kovarianz Within
  TA_R1 ~~ Kta_cc * CC_R1
  TA_R2 ~~ Kta_cc * CC_R2
  TA_R1 ~~ Kta_gr * GR_R1
  TA_R2 ~~ Kta_gr * GR_R2
  TA_R1 ~~ Kta_vo * VO_R1
  TA_R2 ~~ Kta_vo * VO_R2
  CC_R1 ~~ Kcc_gr * GR_R1
  CC_R2 ~~ Kcc_gr * GR_R2
  CC_R1 ~~ Kcc_vo * VO_R1
  CC_R2 ~~ Kcc_vo * VO_R2
  GR_R1 ~~ Kgr_vo * VO_R1
  GR_R2 ~~ Kgr_vo * VO_R2
  
  # ICC berechnen
  icc_ta := Vta / (Vta + Vta_R12)
  icc_cc := Vcc / (Vcc + Vcc_R12)
  icc_gr := Vgr / (Vgr + Vgr_R12)
  icc_vo := Vvo / (Vvo + Vvo_R12)
"
# Schätzung des Modells
mod1 <- sem(lv.mod, data = prodPRat)
summary(mod1, standardized = TRUE)

# Inspektion der Ergebnisse
show(mod1)
fitted(mod1)
inspect(mod1,"cov.lv")
inspect(mod1, "free")
}
}%end else / end ifelse latex

}

\subsection{Listing 2: Kompakte Darstellung der Ergebnisse}{

\ifelse{latex}{%start latex
\code{}\cr
\code{> parameterEstimates(mod1, ci = FALSE, }\cr
\code{+                    standardized = TRUE)}\cr
}{\verb{parameterEstimates(mod1, ci = FALSE, 
                   standardized = TRUE)
}
}%end else / end ifelse latex

}

\subsection{Listing 2a (Ergänzung zum Buch): Schreibe Ergebnisse in 
Latex-Tabelle}{

\ifelse{latex}{%start latex
\code{}\cr
\code{> library(xtable)}\cr
\code{> }\cr
\code{> xtable(parameterEstimates(mod1, ci = FALSE, }\cr
\code{+                           standardized = TRUE), digits = 3)}\cr
}{\verb{library(xtable)

xtable(parameterEstimates(mod1, ci = FALSE, 
                          standardized = TRUE), digits = 3)
}
}%end else / end ifelse latex

} 
} % end Abschnitt 5

\subsection{Abschnitt 7: Übungen}{

\subsection{Übung 1: MFRM M3 und M4 umsetzen und Vergleichen}{
Wir setzen die Modelle separat in TAM um und lassen uns mit \code{summary()} 
die Ergebnisse anzeigen.
Einen direkten Zugriff auf die geschätzen Parameter bekommt man mit 
\code{mod$xsi.facets}. 
Dabei sieht man, dass im Modell 4 keine generalized items gebildet werden, da 
hier kein Interaktionsterm vorkommt. 
Den Modellvergleich machen wir mit \code{IRT.compareModels(mod3, mod4)}. 
Modell 3 weist hier kleinere AIC-Werte auf und scheint etwas besser auf die 
Daten zu passen als Modell 4. 
Dies zeigt auch der Likelihood-Ratio Test, demnach sich durch das Hinzufügen von 
Parametern die Modellpassung verbessert.

\ifelse{latex}{%start latex
\code{}\cr
\code{> library(TAM)}\cr
\code{> prodRatEx <- datenKapitel07$prodRatEx}\cr
\code{> }\cr
\code{> # Rater-Facette definieren}\cr
\code{> facets <- prodRatEx[, "rater", drop = FALSE] }\cr
\code{> }\cr
\code{> # Response Daten definieren}\cr
\code{> vars <- c("TA", "CC", "GR", "VO")}\cr
\code{> resp <- prodRatEx[, vars] }\cr
\code{>}\cr
\code{> # Personen-ID definieren}\cr
\code{> pid <- prodRatEx$idstud }\cr
\code{> }\cr
\code{> # Modell 3}\cr
\code{> f3 <- ~item * step + rater}\cr
\code{> mod3 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f3,   }\cr
\code{+                     pid = pid, control=list(xsi.start0 = 1, }\cr
\code{+                                             fac.oldxsi = 0.1, }\cr
\code{+                                             increment.factor = 1.05))}\cr
\code{> # Modell 4}\cr
\code{> f4 <- ~item + step + rater}\cr
\code{> mod4 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f4,   }\cr
\code{+                     pid = pid, control=list(xsi.start0 = 1, }\cr
\code{+                                             fac.oldxsi = 0.1, }\cr
\code{+                                             increment.factor = 1.05))}\cr
\code{> summary(mod3, file = "TAM_MFRM")}\cr
\code{> summary(mod4, file = "TAM_MFRM")}\cr
\code{> }\cr
\code{> }\cr
\code{> mod3$xsi.facets}\cr
\code{> mod4$xsi.facets}\cr
\code{> }\cr
\code{> IRT.compareModels(mod3, mod4)}\cr
\code{$IC}\cr
\code{' Model   loglike Deviance Npars Nobs      AIC      BIC     AIC3     AICc     CAIC'}\cr
\code{1  mod3 -60795.38 121590.8    69 9748 121728.8 122224.5 121797.8 121729.8 122293.5}\cr
\code{2  mod4 -61041.46 122082.9    51 9748 122184.9 122551.3 122235.9 122185.5 122602.3}\cr
\code{}\cr
\code{$LRtest}\cr
\code{' Model1 Model2     Chi2 df p'}\cr
\code{1   mod4   mod3 492.1549 18 0}\cr
\code{}\cr
\code{attr(,"class")}\cr
\code{[1] "IRT.compareModels"}\cr
}{\verb{library(TAM)
prodRatEx <- datenKapitel07$prodRatEx

# Rater-Facette definieren
facets <- prodRatEx[, "rater", drop = FALSE] 

# Response Daten definieren
vars <- c("TA", "CC", "GR", "VO")
resp <- prodRatEx[, vars] 

# Personen-ID definieren
pid <- prodRatEx$idstud 

# Modell 3
f3 <- ~item * step + rater
mod3 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f3,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
# Modell 4
f4 <- ~item + step + rater
mod4 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f4,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
summary(mod3, file = "TAM_MFRM")
summary(mod4, file = "TAM_MFRM")


mod3$xsi.facets
mod4$xsi.facets

IRT.compareModels(mod3, mod4)

$IC
  Model   loglike Deviance Npars Nobs      AIC      BIC     AIC3     AICc     CAIC
1  mod3 -60795.35 121590.7    69 9748 121728.7 122224.5 121797.7 121729.7 122293.5
2  mod4 -61041.47 122082.9    51 9748 122184.9 122551.4 122235.9 122185.5 122602.4

$LRtest
  Model1 Model2     Chi2 df p
1   mod4   mod3 492.2264 18 0
}
}%end else / end ifelse latex

}

\subsection{Übung 2: Varianzkomponentenmodell}{
Das Varianzkomponentenmodell setzen wir für die short prompts nach den Vorgaben 
im Buchkapitel um. 
Dabei verändern wir die Anzahl der möglichen Rater durch 
\code{n.r <- c(2,10,15)}. 
Der Phi-Koeffizient kann laut Gleichung 6.9 und 6.10 berechnet werden. 
Die Ergebnisse zeigen einen prozentuellen Anteil der Interaktion Person und 
Rater von ca. 15\%, dieser scheint auf Halo-Effekte hinzuweisen.

\ifelse{latex}{%start latex
\code{}\cr
\code{> # Formel definieren}\cr
\code{> formula1 <- response ~ (1|idstud) + (1|item) + (1|rater) +}\cr
\code{+                        (1|rater:item) + (1|idstud:rater) + }\cr
\code{+                        (1|idstud:item)}\cr
\code{> # Modell mit Interaktionen}\cr
\code{> mod.vk <- lmer(formula1, data=prodRatLEx)}\cr
\code{>}\cr
\code{> summary(mod.vk)}\cr
\code{> print(vk <- summary.VarComp(mod.vk))}\cr
\code{'           Varianz prop.Varianz'}\cr
\code{idstud:item     0.10         2.45}\cr
\code{idstud:rater    0.64        15.21}\cr
\code{idstud          2.88        67.94}\cr
\code{rater:item      0.01         0.22}\cr
\code{rater           0.19         4.39}\cr
\code{item            0.00         0.02}\cr
\code{Residual        0.41         9.78}\cr
\code{Total           4.24       100.00}\cr
\code{> }\cr
\code{>}\cr 
\code{> # Verändern der Rateranzahl}\cr
\code{> n.p <- length(unique(prodRatLEx$idstud)) # Anzahl Schüler}\cr
\code{> n.i <- 4  # Anzahl Items}\cr
\code{> n.r <- c(2,10,15) # Anzahl Rater}\cr
\code{> }\cr
\code{> # Varianzkomponenten extrahieren}\cr
\code{> sig2.p <- vk["idstud", "Varianz"]}\cr
\code{> sig2.i <- vk["item", "Varianz"]}\cr
\code{> sig2.r <- vk["rater", "Varianz"]}\cr
\code{> sig2.ri <- vk["rater:item", "Varianz"]}\cr
\code{> sig2.pr <- vk["idstud:rater", "Varianz"]}\cr
\code{> sig2.pi <- vk["idstud:item", "Varianz"]}\cr
\code{> sig2.pir <- vk["Residual", "Varianz"]}\cr
\code{> }\cr
\code{> # Phi-Koeffizient berechnen}\cr
\code{> sig2.D <- sig2.r/n.r + sig2.i/n.i + sig2.pi/n.i + sig2.pr/n.r + }\cr
\code{+           sig2.ri/(n.i*n.r) + sig2.pir/(n.i*n.r) }\cr
\code{> phi.koeff <- sig2.p / (sig2.p + sig2.D)}\cr
\code{> print(data.frame(n.r, phi.koeff), digits = 3)}\cr
\code{> }\cr
\code{> # Konfidenzintervalle}\cr
\code{> 1.96*sqrt(sig2.D)}\cr
}{\verb{library(lme4)
prodRatLEx <- datenKapitel07$prodRatLEx

# Formel definieren
formula1 <- response ~ (1|idstud) + (1|item) + (1|rater) +
                       (1|rater:item) + (1|idstud:rater) + 
                       (1|idstud:item)
# Modell mit Interaktionen
mod.vk <- lmer(formula1, data=prodRatLEx)

# Zusammenfassung der Ergebnisse
summary(mod.vk)
print(vk <- summary.VarComp(mod.vk))

             Varianz prop.Varianz
idstud:item     0.10         2.45
idstud:rater    0.64        15.21
idstud          2.88        67.94
rater:item      0.01         0.22
rater           0.19         4.39
item            0.00         0.02
Residual        0.41         9.78
Total           4.24       100.00


# Verändern der Rateranzahl
n.p <- length(unique(prodRatLEx$idstud)) # Anzahl Schüler
n.i <- 4  # Anzahl Items
n.r <- c(2,10,15) # Anzahl Rater

# Varianzkomponenten extrahieren
sig2.p <- vk["idstud", "Varianz"]
sig2.i <- vk["item", "Varianz"]
sig2.r <- vk["rater", "Varianz"]
sig2.ri <- vk["rater:item", "Varianz"]
sig2.pr <- vk["idstud:rater", "Varianz"]
sig2.pi <- vk["idstud:item", "Varianz"]
sig2.pir <- vk["Residual", "Varianz"]

# Phi-Koeffizient berechnen
sig2.D <- sig2.r/n.r + sig2.i/n.i + sig2.pi/n.i + sig2.pr/n.r + 
          sig2.ri/(n.i*n.r) + sig2.pir/(n.i*n.r) 
phi.koeff <- sig2.p / (sig2.p + sig2.D)
print(data.frame(n.r, phi.koeff), digits = 3)

# Konfidenzintervalle
1.96*sqrt(sig2.D)
}
}%end else / end ifelse latex

}
} % end Abschnitt 7

} % end details

%% ----------------------------------------------------------------------------
%% Examples
\examples{
\dontrun{
library(irr)
library(TAM)
library(WrightMap)
library(lattice)
library(grid)
library(lme4)
library(lavaan)
library(xtable)

summary.VarComp <- function(mod){ 
  var.c <- VarCorr(mod)
  var.c <- c(unlist(var.c) , attr(var.c , "sc")^2)
  names(var.c)[length(var.c)] <- "Residual"
  dfr1 <- data.frame(var.c)
  colnames(dfr1) <- "Varianz"
  dfr1 <- rbind(dfr1, colSums(dfr1))
  rownames(dfr1)[nrow(dfr1)] <- "Total"
  dfr1$prop.Varianz <- 100 * (dfr1$Varianz / dfr1$Varianz[nrow(dfr1)])
  dfr1 <- round(dfr1,2)
  return(dfr1)
}

data(datenKapitel07)
prodRat <- datenKapitel07$prodRat
prodRatL <- datenKapitel07$prodRatL
prodPRat <- datenKapitel07$prodPRat 

## -------------------------------------------------------------
## Abschnitt 7.2: Beurteilerübereinstimmung
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 7.2, Listing 1: Berechnen der Häufigkeitstabellen
#

# Items auswählen
items <- c("TA", "CC", "GR", "VO")
# Tabelle erzeugen
tab <- apply(prodRat[, items], 2,
             FUN=function(x){
               prop.table(table(x))*100})
print(tab, digits = 2)

# Mittelwert der Ratings berechnen
round(apply(prodRat[, items], 2, mean), 2)

# -------------------------------------------------------------
# Abschnitt 7.2, Listing 2: Beurteilerübereinstimmung berechnen
#

items <- c("TA", "CC", "GR", "VO")
dfr <- data.frame(items, agree = NA, 
                  kappa = NA, wkappa = NA, korr = NA)
for(i in 1:length(items)){
  dat.i <- prodPRat[, grep(items[i], colnames(prodPRat))]
  dfr[i, "agree"] <- agree(dat.i, tolerance = 1)["value"]
  dfr[i, "kappa"] <- kappa2(dat.i)["value"]
  dfr[i, "wkappa"] <- kappa2(dat.i, weight = "squared")["value"]
  dfr[i, "korr"] <- cor(dat.i[,1], dat.i[,2])
  dfr[i, "icc"] <- icc(dat.i, model = "twoway")["value"]
}
print(dfr, digits = 3)


## -------------------------------------------------------------
## Abschnitt 7.3: Skalierungsmodelle
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 1: Skalierungsmodell mit TAM
#

set.seed(1234)

# Rater-Facette definieren
facets <- prodRat[, "rater", drop = FALSE] 
# Response Daten definieren
vars <- c("TA", "CC", "GR", "VO")
resp <- prodRat[, vars] 
# Personen-ID definieren
pid <- prodRat$idstud 

# Formel für Modell
formulaA <- ~item*step+item*rater
# Modell berechnen
mod <- tam.mml.mfr(resp = resp, facets = facets, formulaA = formulaA,   
                   pid = pid, control=list(xsi.start0 = 1, 
                                           fac.oldxsi = 0.1, 
                                           increment.factor = 1.05))
summary(mod, file="TAM_MFRM")

# Personenparameter und Rohscores
persons.mod <- tam.wle(mod)
persons.mod$raw.score <- persons.mod$PersonScores / (persons.mod$N.items) 

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 1b: Ergänzung zum Buch
# Modellvergleich aller besprochenen Modelle
#

f1 <- ~item * rater * step
mod1 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f1,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
f2 <- ~item*step+item*rater
mod2 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f2,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
f3 <- ~item * step + rater
mod3 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f3,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
f4 <- ~item + step + rater
mod4 <- tam.mml.mfr(resp = resp, facets = facets, formulaA = f4,   
                    pid = pid, control=list(xsi.start0 = 1, 
                                            fac.oldxsi = 0.1, 
                                            increment.factor = 1.05))
mod4$xsi.facets
IRT.compareModels(mod1, mod2, mod3, mod4)

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 1c: Ergänzung zum Buch
# Wright-Map: Items und Rater
#

item.labs <- vars
rater.labs <- unique(prodRat$rater)
item.labs <- c(item.labs, rep(NA, length(rater.labs) - 
                                length(item.labs)))

pars <- mod$xsi.facets$xsi
facet <- mod$xsi.facets$facet
item.par <- pars[facet == "item"]
rater.par <- pars[facet == "rater"]
item_rat <- pars[facet == "item:rater"]
len <- length(item_rat)
item.long <- c(item.par, rep(NA, len - length(item.par)))
rater.long <- c(rater.par, rep(NA, len - length(rater.par)))

wrightMap(persons.mod$theta, rbind(item.long, rater.long), 
          label.items = c("Items",  "Rater"), 
          thr.lab.text = rbind(item.labs, rater.labs), 
          axis.items = "", min.l=-3, max.l=3,
          axis.persons = "Personen")

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 2: Fit-Indices berechnen
#

# Infit/Outfit berechnen
pseudo_items <- colnames(mod$resp)
pss <- strsplit(pseudo_items , split="-")
item_parm <- unlist(lapply(pss, FUN = function(ll){ll[1]}))
rater_parm <- unlist(lapply(pss, FUN = function(ll){ll[2]}))

# Fit Items
res.items <- msq.itemfitWLE(mod, item_parm)
summary(res.items)

# Fit Rater
res.rater <- msq.itemfitWLE(mod, rater_parm)
summary(res.rater)

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 2a: Ergänzung zum Buch
# Abbildung: Histogramm, Rohscores
#

dev.off()
par(mfcol=c(1,2))

hist(persons.mod$theta, col="grey", breaks=40, 
     main = "",
     xlab = "Theta (logits)",
     ylab = "Häufigkeit")
with(persons.mod, scatter.smooth(raw.score, theta, 
                                 pch = 1, cex = .6, xlab = "Rohscores",
                                 ylab = "Theta (logits)", 
                                 lpars = list(col = "darkgrey", lwd = 2, 
                                              lty = 1)))

# Abbildung: Fit-Statistik
par(mfcol=c(1,2))
fitdat <- res.rater$fit_data
fitdat$var <- factor(substr(fitdat$item, 1, 2))
boxplot(Outfit~var, data=fitdat, 
        ylim=c(0,2), main="Outfit")
boxplot(Infit~var, data=fitdat, 
        ylim=c(0,2), main="Infit")

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 2b: Ergänzung zum Buch
# Korrelationen
#

korr <- c(with(persons.mod, cor(raw.score, theta, 
                                method = "pearson")),
          with(persons.mod, cor(raw.score, theta, 
                                method = "spearman")))
print(korr)

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 3: Q3-Statistik berechnen
#

# Q3 Statistik
mfit.q3 <- tam.modelfit(mod)
rater.pairs <- mfit.q3$stat.itempair

# Nur Paare gleicher Rater wählen
unique.rater <- which(substr(rater.pairs$item1, 4,12) == 
                        substr(rater.pairs$item2, 4,12))
rater.q3 <- rater.pairs[unique.rater, ]

# Spalten einfügen: Rater, Kombinationen
rater.q3$rater <- substr(rater.q3$item1, 4, 12)
rater.q3 <- rater.q3[order(rater.q3$rater),]
rater.q3$kombi <- as.factor(paste(substr(rater.q3$item1, 1, 2), 
                                  substr(rater.q3$item2, 1, 2), sep="_"))

# Statistiken aggregieren: Rater, Kombinationen
dfr.raterQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$rater), mean)
colnames(dfr.raterQ3) <- c("Rater", "Q3")
dfr.itemsQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$kombi), mean)
colnames(dfr.itemsQ3) <- c("Items", "Q3")
dfr.itemsQ3

# -------------------------------------------------------------
# Abschnitt 7.3, Listing 3a: Ergänzung zum Buch
# Lattice Dotplot
#

# Lattice Dotplot
mean.values <- aggregate(rater.q3$aQ3, list(rater.q3$kombi), mean)[["x"]]
dotplot(aQ3~kombi, data=rater.q3, main="Q3-Statistik", ylab="Q3 (adjustiert)",
        col="darkgrey", 
        panel = function(x,...){
          panel.dotplot(x,...)
          panel.abline(h = 0, col.line = "grey", lty=3)
          grid.points(1:6, mean.values, pch=17)
        })


## -------------------------------------------------------------
## Abschnitt 7.4: Generalisierbarkeitstheorie
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 7.4, Listing 1: Varianzkomponenten mit lme4 berechnen
#

# Formel definieren
formula1 <- response ~ (1|idstud) + (1|item) + (1|rater) +
  (1|rater:item) + (1|idstud:rater) + 
  (1|idstud:item)
# Modell mit Interaktionen
mod.vk <- lmer(formula1, data=prodRatL)

# Zusammenfassung der Ergebnisse
summary(mod.vk)

# -------------------------------------------------------------
# Abschnitt 7.4, Listing 1a: Ergänzung zum Buch
# Helper-Function um die Varianzkomponenten zu extrahieren
#

summary.VarComp <- function(mod){ 
  var.c <- VarCorr(mod)
  var.c <- c(unlist(var.c) , attr(var.c , "sc")^2)
  names(var.c)[length(var.c)] <- "Residual"
  dfr1 <- data.frame(var.c)
  colnames(dfr1) <- "Varianz"
  dfr1 <- rbind(dfr1, colSums(dfr1))
  rownames(dfr1)[nrow(dfr1)] <- "Total"
  dfr1$prop.Varianz <- 100 * (dfr1$Varianz / dfr1$Varianz[nrow(dfr1)])
  dfr1 <- round(dfr1,2)
  return(dfr1)
}
summary.VarComp(mod.vk)

# -------------------------------------------------------------
# Abschnitt 7.4, Listing 2: Berechnung des G-Koeffizienten
#

vk <- summary.VarComp(mod.vk)
n.p <- length(unique(prodRatL$idstud)) # Anzahl Schüler
n.i <- 4  # Anzahl Items
n.r <- c(1,2,5) # Anzahl Rater

# Varianzkomponenten extrahieren
sig2.p <- vk["idstud", "Varianz"]
sig2.i <- vk["item", "Varianz"]
sig2.r <- vk["rater", "Varianz"]
sig2.ri <- vk["rater:item", "Varianz"]
sig2.pr <- vk["idstud:rater", "Varianz"]
sig2.pi <- vk["idstud:item", "Varianz"]
sig2.pir <- vk["Residual", "Varianz"]

# Fehlervarianz berechnen
sig2.delta <- sig2.pi/n.i + sig2.pr/n.r + sig2.pir/(n.i*n.r) 

# G-Koeffizient berechnen
g.koeff <- sig2.p / (sig2.p + sig2.delta)
print(data.frame(n.r, g.koeff), digits = 3)

# -------------------------------------------------------------
# Abschnitt 7.4, Listing 2a: Ergänzung zum Buch
# Phi-Koeffizient berechnen
#

sig2.D <- sig2.r/n.r + sig2.i/n.i + sig2.pi/n.i + sig2.pr/n.r + 
  sig2.ri/(n.i*n.r) + sig2.pir/(n.i*n.r) 
phi.koeff <- sig2.p / (sig2.p + sig2.D)
print(data.frame(n.r, phi.koeff), digits = 3)

# Konfidenzintervalle
1.96*sqrt(sig2.D)

# -------------------------------------------------------------
# Abschnitt 7.4, Listing 2c: Ergänzung zum Buch
# Variable Rateranzahl
#

dev.off()
n.i <- 4  # Anzahl Items
dn.r <- seq(1,10)# 1 bis 10 mögliche Rater
delta.i <- sig2.pi/n.i + sig2.pr/dn.r + sig2.pir/(n.i*dn.r)
g.koeff <- sig2.p / (sig2.p + delta.i)
names(g.koeff) <- paste("nR", dn.r, sep="_") 
print(g.koeff[1:4])

# Abbildung variable Rateranzahl
plot(g.koeff, type = "b", pch = 19, lwd = 2, bty = "n",
     main = "G-Koeffizient: Raters",
     ylab = "G-Koeffizient",
     xlab = "Anzahl Raters",  xlim = c(0,10))
abline(v=2, col="darkgrey")


## -------------------------------------------------------------
## Abschnitt 7.5: Strukturgleichungsmodelle
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 7.5, Listing 1: SEM
#

# SEM Modell definieren
lv.mod <- " 
  # Messmodell
  TA =~ 1*TA_R1 + 1*TA_R2
  CC =~ 1*CC_R1 + 1*CC_R2
  GR =~ 1*GR_R1 + 1*GR_R2
  VO =~ 1*VO_R1 + 1*VO_R2
  
  # Varianz Personen
  TA ~~ Vta * TA
  CC ~~ Vcc * CC
  GR ~~ Vgr * GR
  VO ~~ Vvo * VO
  
  # Varianz Rater X Personen
  TA_R1 ~~ Vta_R12 * TA_R1
  TA_R2 ~~ Vta_R12 * TA_R2
  CC_R1 ~~ Vcc_R12 * CC_R1
  CC_R2 ~~ Vcc_R12 * CC_R2
  GR_R1 ~~ Vgr_R12 * GR_R1
  GR_R2 ~~ Vgr_R12 * GR_R2
  VO_R1 ~~ Vvo_R12 * VO_R1
  VO_R2 ~~ Vvo_R12 * VO_R2
  
  # Kovarianz
  TA_R1 ~~ Kta_cc * CC_R1
  TA_R2 ~~ Kta_cc * CC_R2
  TA_R1 ~~ Kta_gr * GR_R1
  TA_R2 ~~ Kta_gr * GR_R2
  TA_R1 ~~ Kta_vo * VO_R1
  TA_R2 ~~ Kta_vo * VO_R2
  CC_R1 ~~ Kcc_gr * GR_R1
  CC_R2 ~~ Kcc_gr * GR_R2
  CC_R1 ~~ Kcc_vo * VO_R1
  CC_R2 ~~ Kcc_vo * VO_R2
  GR_R1 ~~ Kgr_vo * VO_R1
  GR_R2 ~~ Kgr_vo * VO_R2
  
  # ICC berechnen
  icc_ta := Vta / (Vta + Vta_R12)
  icc_cc := Vcc / (Vcc + Vcc_R12)
  icc_gr := Vgr / (Vgr + Vgr_R12)
  icc_vo := Vvo / (Vvo + Vvo_R12)
  "
# Schätzung des Modells
mod1 <- sem(lv.mod, data = prodPRat)
summary(mod1, standardized = TRUE)

# Inspektion der Ergebnisse
show(mod1)
fitted(mod1)
inspect(mod1,"cov.lv")
inspect(mod1, "free")

# -------------------------------------------------------------
# Abschnitt 7.5, Listing 2: Kompakte Darstellung der Ergebnisse
#

parameterEstimates(mod1, ci = FALSE, 
                   standardized = TRUE)

# -------------------------------------------------------------
# Abschnitt 7.5, Listing 2a: Ergänzung zum Buch
# Schreibe Ergebnisse in Latex-Tabelle:
#

xtable(parameterEstimates(mod1, ci = FALSE, 
                          standardized = TRUE), digits = 3)
}
} % end Examples
