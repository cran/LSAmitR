%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel 7}
\alias{Kapitel 7}
\encoding{UTF-8}
\title{Kapitel 7: Statistische Analysen produktiver Kompetenzen}

\description{
Das ist die Nutzerseite zum Kapitel 7, Statistische Analysen produktiver 
Kompetenzen, im Herausgeberband \emph{Large-Scale Assessment mit \R}:  
Methodische Grundlagen der österreichischen Bildungsstandard-Überprüfung.\cr
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. Im Abschnitt
\strong{Examples} werden die \R-Syntaxen des Kapitels vollständig widergegeben und 
gegebenefalls erweitert. 
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Freunberger, R., Robitzsch, A. & Luger-Bazinger, C. (2016). Statistische 
  Analysen produktiver Kompetenzen. In C. Schreiner & S. Breit (Hrsg.): 
  \emph{Large-Scale Assessment mit \R}: Methodische Grundlagen der 
  österreichischen Bildungsstandard-Überprüfung (pp. xx-yy). Wien: facultas 
  wuv.
}

%\note{
% wenn es Hinweise gibt
%}

\author{
  Roman Freunberger, Alexander Robitzsch, Claudia Luger-Bazinger
}

\seealso{
Zurück zu \code{\link{Kapitel 6}}, Skalierung und Linking.\cr
Zu \code{\link{Kapitel 8}}, Behandlung fehlender Daten und PV-Ziehung.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.\cr
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen.
Zur Hilfeseite von \code{\link[TAM:TAM-package]{TAM}}
}

%% ----------------------------------------------------------------------------
%% Details
\details{
\subsection{Abschnitt 1: Beispieldatensätze}{

Der zur Illustration verwendete Datensatz \code{prodRat} beinhaltet die beurteilten Schreibkompetenzen im Fach Englisch auf der 8.  Schulstufe von 9836 Schüler/innen (\code{idstud}) die von insgesamt 41 Ratern (\code{rater}) beurteilt wurden. Die sechs Schreibaufgaben (\code{aufgabe}) wurden auf sechs Testhefte (\code{th}) aufgeteilt, wobei jede Aufgabe in genau zwei Testheften vorkommt.

Zur weiteren Analyse verwenden wir auch den Datensatz \code{prodPRat} mit sogenannten Pseudoratern.

Für die Analyse von Varianzkomponenten mittels Linear Mixed Effects (LME) Modellen verwenden wir den ursprünglichen Datensatz im Long Format (\code{prodRatL}). 
} % end Abschnitt 1


\subsection{Abschnitt 2: Beurteilerübereinstimmung}{

\subsection{Listing 1: Berechnen von Häufigkeitstabellen}{
Hier werden die Datensätze \code{prodRat} und \code{prodPRat} verwendet. Die R-Funktion \code{apply()} ermöglicht eine Anwendung einer beliebigen Funktion z.B. \code{prop.table()} über alle Zeilen (1) oder Spalten (2) eines \code{data.frame}.

\verb{
library(irr)
load(file = "prodRat.RData")

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

} % end Listing 1

\subsection{Listing 2: Beurteilerübereinstimmung berechnen}{
Wir verwenden den Datensatz mit Pseudoratern \code{prodPRat}. Die Analysen werden mit dem Paket \code{irr} durchgeführt. 

\verb{
load(file = "prodPRat.RData")

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

} % end Listing 2
} % end Abschnitt 2


\subsection{Abschnitt 3: Skalierungsmodelle}{

\subsection{Listing 1: Skalierungsmodell mit TAM}{

Der Funktion \code{tam.mm.mfr()} muss ein \code{data.frame} für die Facetten übergeben werden. Zusätzlich können Einstellungen in einer Liste für das Argument \code{control = list()} übergeben werden. Hier verwenden wir die Einstellung \code{xsi.start0 = 1}, was dazu führt, dass alle Startwerte auf 0 gesetzt werden. Mit \code{fac.oldxsi = 0.1} setzen wir das Gewicht der Parameterwerte aus der vorigen Iteration etwas über 0. Damit kann der Algorithmus stabilisiert und Konvergenzprobleme (deviance increase) verhindert werden. Wir definieren noch \code{increment.factor = 1.05} etwas über dem default-Wert von 1 um mögliche Konvergenzprobleme abzufangen. Dieser Wert definiert das Ausmaß der Abnahme des maximalen Zuwachs der Parameter pro Iteration (s. TAM-Hilfe).

Die Personenparameter werden mit der Funktion \code{tam.wle()} geschätzt.

Gibt man in der Funktion \code{summary()} das Argument \code{file} an, so wird der Output direkt in ein Textfile geschrieben.

\verb{
set.seed(1234)
library(TAM)

load(file = "prodRat.RData")

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
} % end Listing 1


\subsection{Listing 1b (Ergänzung zum Buch): Skalierungsmodell mit TAM}{

Hier werden alle im Buch besprochenen Modelle berechnet und anschließend ein Modellvergleich durchgeführt.

\verb{
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
}
}

\subsection{Listing 1c (Ergänzung zum Buch): Wright-Map}{

Mit dem Paket \code{WrightMap} können die Ergebnisse für die einzelnen Facetten dargestellt werden. Wir machen dies für Items und Rater.

\verb{
library(WrightMap)

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
}


\subsection{Listing 2: Fit-Indices berechnen}{

Die Fit-Indices werden mit der Funktion \code{msq.itemfitWLE} für die Raterparameter und Itemparameter gesondert  berechnet. Der Funktion muss ein Vektor mit Parameterbezeichnungen übergeben werden so wie sie im Modell-Objekt vorkommen. Im Paket \code{TAM} gibt es noch die Funktion \code{tam.fit()}, diese basiert auf einer Simulation der individuellen Posterior-Verteilung. Die Funktion \code{msq.itemfitWLE} wertet dagegen die individuelle Posterior-Verteilung direkt aus (s. \code{TAM}-Hilfe für weitere Beispiele) und führt keine Simulation durch.

\verb{
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
}
}

\subsection{Listing 2a (Ergänzung zum Buch): Abbildung Fit-Indices}{
\verb{
# Abbildung: Histogramm, Rohscores

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
}

\subsection{Listing 2b (Ergänzung zum Buch): Korrelationen}{

Pearson und Spearman Korrelationskoeffizient wird zwischen Rohscores und Theta berechnet.

\verb{
korr <- c(with(persons.mod, cor(raw.score, theta, 
                                method = "pearson")),
          with(persons.mod, cor(raw.score, theta, 
                                method = "spearman")))
print(korr)
}
}

\subsection{Listing 3: Q3-Statistik berechnen}{

Die Q3-Statistik für lokale stochastische Unabhängigkeit wird mit der Funktion \code{tam.modelfit()} berechnet. Der Output enthält eine Vielzahl an Fit-Statistiken, für weitere Details sei hier auf die \code{TAM}-Hilfeseite verwiesen. Die adjustierte aQ3-Statistik berechnet sich aus den Q3-Werten abzüglich des Gesamtmittelwerts von allen Q3-Werten. 

Mit \code{tam.modelfit()} werden Fit-Statistiken für alle Rater x Item Kombinationen berechnet. Diese werden im Code unten anschließend aggregiert um eine Übersicht zu erhalten. Dazu werden zuerst nur Paare gleicher Rater ausgewählt, somit wird die aggregierte Q3-Statistik nur Rater-spezifisch berechnet. Das Objekt \code{rater.q3} beinhaltet eine Zeile pro Rater x Item Kombination. Kombinationen ergeben sich nur für einen Rater nicht zwischen unterschiedlichen Ratern.

Anschließend kann man mit \code{aggregate()} separat über Rater und Kombinationen mitteln und diese als Dotplot darstellen (Paket \code{lattice}).

\verb{
# Q3 Statistik
mfit.q3 <- tam.modelfit(mod)
rater.pairs <- mfit.q3$stat.itempair

# Nur Paare gleicher Rater wählen
unique.rater <- which(substr(rater.pairs$item1, 4,12) == substr(rater.pairs$item2, 4,12))
rater.q3 <- rater.pairs[unique.rater, ]

# Spalten einfügen: Rater, Kombinationen
rater.q3$rater <- substr(rater.q3$item1, 4, 12)
rater.q3 <- rater.q3[order(rater.q3$rater),]
rater.q3$kombi <- as.factor(paste(substr(rater.q3$item1, 1, 2), substr(rater.q3$item2, 1, 2), sep="_"))

# Statistiken aggregieren: Rater, Kombinationen
dfr.raterQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$rater), mean)
colnames(dfr.raterQ3) <- c("Rater", "Q3")
dfr.itemsQ3 <- aggregate(rater.q3$aQ3, by = list(rater.q3$kombi), mean)
colnames(dfr.itemsQ3) <- c("Items", "Q3")
dfr.itemsQ3

}
}

\subsection{Listing 3 (Ergänzung zum Buch): Lattice Dotplot}{
\verb{
library(lattice)
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
}
} % end Abschnitt 3


\subsection{Abschnitt 4: Generalisierbarkeitstheorie}{

\subsection{Listing 1: Varianzkomponenten mit lme4 berechnen}{

Mit der Funktion \code{lmer()} aus dem Paket \code{lme4} schätzen wir die Varianzkomponenten. In der Formel definieren wir dabei die Facetten als random effects.

\verb{
library(lme4)

load(file="prodRatL.RData")

# Formel definieren
formula1 <- response ~ (1|idstud) + (1|item) + (1|rater) +
                       (1|rater:item) + (1|idstud:rater) + 
                       (1|idstud:item)
# Modell mit Interaktionen
mod.vk <- lmer(formula1, data=prodRatL)

# Zusammenfassung der Ergebnisse
summary(mod.vk)
}
}

\subsection{Listing 1a (Ergänzung zum Buch): Summary-Funktion für Varianzkomponenten}{

Wir generieren eine Funktion \code{summary.VarComp()} die den Output des Modells \code{mod.vk} in einen ansprechenden \code{data.frame} schreibt. Hier werden auch die prozentualen Anteile der Varianzkomponenten berechnet.

\verb{
# Helper-Function um die Varianzkomponenten zu extrahieren
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
}

\subsection{Listing 2: Berechnung des G-Koeffizienten}{

Den G-Koeffizienten berechnen wir nach der Formel im Buch.

\verb{
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
}
}

\subsection{Listing 2a (Ergänzung zum Buch): Phi-Koeffizient berechnen}{

\verb{
sig2.D <- sig2.r/n.r + sig2.i/n.i + sig2.pi/n.i + sig2.pr/n.r + sig2.ri/(n.i*n.r) + sig2.pir/(n.i*n.r) 
phi.koeff <- sig2.p / (sig2.p + sig2.D)
print(data.frame(n.r, phi.koeff), digits = 3)

# Konfidenzintervalle
1.96*sqrt(sig2.D)
}
}

\subsection{Listing 2c (Ergänzung zum Buch): Variable Rateranzahl}{

Für eine variable Rateranzahl (hier 1 bis 10 Rater) werden die G-Koeffizienten berechnet.

\verb{
n.i <- 4  # Anzahl Items
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
}
} % end Abschnitt 4


\subsection{Abschnitt 5: Strukturgleichungsmodelle}{

In R setzen wir das Struktugleichungsmodell mit dem Paket \code{lavaan} um. Das Modell wird als Textvariable definiert, welche anschließend der Funktion \code{sem()} übergeben wird. Latente Variablen im Messmodell werden in \code{lavaan} mit der Form \code{latente Variable =~ manifeste Variable(n)} definiert, die Ladungen werden dabei auf den Wert 1 fixiert, was mittels der Multiplikation der Variable mit dem Wert 1 umgesetzt werden kann (z.B. \code{1*Variable}). Varianzen und Kovarianzen werden mit \code{Variable ~~ Variable} gebildet, wobei hier die Multiplikation mit einem Label einerseits den berechneten Parameter benennt, andererseits, bei mehrmaligem Auftreten des Labels, Parameterschätzungen von verschiedenen Variablen restringiert bzw. gleichstellt (z.B. wird für die Within-Varianz von \code{TA} über beide Rater nur ein Parameter geschätzt, nämlich \code{Vta_R12}). Die ICC wird für jede Dimension separat direkt im Modell spezifiziert, dies geschieht durch abgleitete Variablen mit der Schreibweise \code{Variable := Berechnung}. Die Modellspezifikation und der Aufruf der Funktion \code{sem()} ist wie folgt definiert:

\subsection{Listing 1 (mit Ergänzung zum Buch): SEM}{
\verb{
library(lavaan)

load(file = "prodPRat.RData")

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
}

\subsection{Listing 2: Kompakte Darstellung der Ergebnisse}{

\verb{
parameterEstimates(mod1, ci = FALSE, 
                   standardized = TRUE)
}
}

\subsection{Listing 2a (Ergänzung zum Buch): Schreibe Ergebnisse in Latex-Tabelle}{

\verb{
library(xtable)

xtable(parameterEstimates(mod1, ci = FALSE, 
                          standardized = TRUE), digits = 3)
}
} 
} % end Abschnitt 5

\subsection{Abschnitt 7: Übungen}{

\subsection{Übung 1: MFRM M3 und M4 umsetzen und Vergleichen}{
Wir setzen die Modelle separat in TAM um und lassen uns mit \code{summary()} die Ergebnisse anzeigen. Einen direkten Zugriff auf die geschätzen Parameter bekommt man mit \code{mod$xsi.facets}. Dabei sieht man, dass im Modell 4 keine generalized items gebildet werden da hier kein Interaktionsterm vorkommt. Den Modellvergleich machen wir mit \code{IRT.compareModels(mod3, mod4)}. Modell 3 weist hier kleinere AIC-Werte auf und scheint etwas besser auf die Daten zu passen als Modell 4. Dies zeigt auch der Likelihood-Ratio Test demnach sich durch das Hinzufügen von Parametern die Modellpassung verbessert.

\verb{
library(TAM)
load(file = "prodRatEx.RData")

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
}

\subsection{Übung 2: Varianzkomponentenmodell}{
Das Varianzkomponentenmodell setzen wir für die short prompts nach den Vorgaben im Buchkapitel um. Dabei verändern wir die Anzahl der möglichen Rater durch \code{n.r <- c(2,10,15)}. Der Phi-Koeffizient kann laut Gleichung 6.9 und 6.10 berechnet werden. Die Ergebnisse zeigen einen prozentuellen Anteile der Interaktion Person und Rater von ca. 15\%, diese scheint auf Halo-Effekte hinzuweisen.

\verb{
library(lme4)
load(file="prodRatLEx.RData")

# Formel definieren
formula1 <- response ~ (1|idstud) + (1|item) + (1|rater) +
                       (1|rater:item) + (1|idstud:rater) + 
                       (1|idstud:item)
# Model mit Interaktionen
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
sig2.D <- sig2.r/n.r + sig2.i/n.i + sig2.pi/n.i + sig2.pr/n.r + sig2.ri/(n.i*n.r) + sig2.pir/(n.i*n.r) 
phi.koeff <- sig2.p / (sig2.p + sig2.D)
print(data.frame(n.r, phi.koeff), digits = 3)

# Konfidenzintervalle
1.96*sqrt(sig2.D)
}
}

} % end Abschnitt 7

} % end details

%% ----------------------------------------------------------------------------
%% Examples
\examples{
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


\dontrun{
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
# Model mit Interaktionen
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