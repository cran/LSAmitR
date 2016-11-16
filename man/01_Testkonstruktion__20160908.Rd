%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel  1}
\alias{Kapitel 1}
\encoding{UTF-8}
\title{Kapitel 1: Testkonstruktion}

\description{
Das ist die Nutzerseite zum Kapitel 1, \emph{Testkonstruktion}, im 
Herausgeberband Large-Scale Assessment mit \R: Methodische Grundlagen der 
österreichischen Bildungsstandardüberprüfung. 
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. 
Im Abschnitt \strong{Examples} werden die \R-Syntaxen des Kapitels vollständig 
wiedergegeben und gegebenfalls erweitert.
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Itzlinger-Bruneforth, U., Kuhn, J.-T. & Kiefer, T. (2016). Testkonstruktion. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 21--50). Wien: facultas.
}

% \note{
% % wenn es Hinweise gibt
% }

\author{
  Ursula Itzlinger-Bruneforth, Jörg-Tobias Kuhn, und Thomas Kiefer
}

\seealso{
%Verweis zu den Daten im Kapitel
Zu \code{\link{datenKapitel01}}, den im Kapitel verwendeten Daten.\cr
%Verweis zum nächsten und zum vorigen Kapitel
Zurück zu \code{\link{Kapitel 0}}, Konzeption.\cr
Zu \code{\link{Kapitel 2}}, Stichprobenziehung.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
% Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach 
% Übersicht einfügen)
}

% %% ----------------------------------------------------------------------------
% %% Details
% \details{
% \subsection{Vorbereitungen}{
% ..
% \verb{
% ..
% }
% }%end Subsection Vorbereitungen
% 
% 
% \subsection{Abschnitt x.1: KTT}{
% \subsection{Listing 1: Skalierung der Rekodierten Daten}{
% ..
% \verb{
% ..
% }
% }%end Subsection Listing 1: Skalierung der Rekodierten Daten
% }%end Subsection Abschnitt x.1: KTT
% }%end Details

%% ----------------------------------------------------------------------------
%% Examples
\examples{
library(TAM)
library(miceadds)
library(irr)
library(gtools)
library(car)

set.seed(1337)
data(datenKapitel01)
pilotScored <- datenKapitel01$pilotScored
pilotItems <- datenKapitel01$pilotItems
pilotRoh <- datenKapitel01$pilotRoh
pilotMM <- datenKapitel01$pilotMM

\dontrun{
## -------------------------------------------------------------
## Abschnitt 1.5.5: Aspekte empirischer Güteüberprüfung 
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 1: Vorbereitung
#

# Rekodierter Datensatz pilotScored
dat <- pilotScored
items <- grep("E8R", colnames(dat), value = TRUE)
dat[items] <- recode(dat[items], "9=0;8=0")
# Itembank im Datensatz pilotItems
dat.ib <- pilotItems
items.dich <- dat.ib$item[dat.ib$maxScore == 1]

# Berechne erreichbare Punkte je TH
# aus Maximalscore je Item in Itembank
ind <- match(items, dat.ib$item)
testlets.ind <- ! items \%in\% items.dich
ind[testlets.ind] <- match(items[testlets.ind], dat.ib$testlet)
maxscores <- dat.ib$maxScore[ind]
max.form <- 1 * (!is.na(dat[, items])) \%*\% maxscores

# Erzielter Score ist der Summenscore dividiert durch 
# Maximalscore
sumscore <- rowSums(dat[, items], na.rm = TRUE)
relscore <- sumscore/max.form
mean(relscore)

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 2: Omitted Response
#

library(TAM)
# Bestimme absolute und relative Häufigkeit der Kategorie 9 (OR)
ctt.omit <- tam.ctt2(pilotScored[, items])
ctt.omit <- ctt.omit[ctt.omit$Categ == 9, ]
# Übersicht der am häufigsten ausgelassenen Items
tail(ctt.omit[order(ctt.omit$RelFreq), -(1:4)])

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 3: Not Reached
#

not.reached <- rep(0, length(items))
names(not.reached) <- items

# Führe die Bestimmung in jedem Testheft durch
forms <- sort(unique(dat$form))
for(ff in forms){ 
  # (1) Extrahiere Itempositionen
  order.ff <- order(dat.ib[, ff], na.last = NA, 
                    decreasing = TRUE)
  items.ff <- dat.ib$item[order.ff]
  testlets.ff <- dat.ib$testlet[order.ff]
  
  # (2) Sortiere Items und Testlets nach den Positionen
  testlets.ind <- ! items.ff \%in\% items.dich
  items.ff[testlets.ind] <- testlets.ff[testlets.ind]
  items.order.ff <- unique(items.ff)
  
  # (3) Bringe Testhefte in Reihenfolge und
  #     zähle von hinten aufeinanderfolgende Missings
  ind.ff <- pilotScored$form == ff
  dat.order.ff <- pilotScored[ind.ff, items.order.ff]  
  dat.order.ff <- dat.order.ff == 9
  dat.order.ff <- apply(dat.order.ff, 1, cumsum)
  
  # (4) Vergleiche letzteres mit theoretisch möglichem 
  #     vollständigen NR
  vergleich <- cumsum(rep(1, length(items.order.ff)))
  dat.order.ff[dat.order.ff != vergleich] <- 0
  
  # (5) Erstes NR kann auch OR sein
  erstes.NR <- apply(dat.order.ff, 2, which.max)
  ind <- cbind(erstes.NR, 1:ncol(dat.order.ff))
  dat.order.ff[ind] <- 0
  
  # (6) Zähle, wie oft für ein Item NR gilt
  not.reached.ff <- rowSums(dat.order.ff > 0)
  not.reached[items.order.ff] <- not.reached.ff[items.order.ff] + 
    not.reached[items.order.ff]
}

tail(not.reached[order(not.reached)])

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 4: Itemschwierigkeit
#

# Statistik der relativen Lösungshäufigkeiten
p <- colMeans(dat[, items], na.rm = TRUE) / maxscores
summary(p)

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 5: Trennschärfe
#

discrim <- sapply(items, FUN = function(ii){ 
  if(var(dat[, ii], na.rm = TRUE) == 0) 0 else
    cor(dat[, ii], relscore, use = "pairwise.complete.obs") 
}) 

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 6: Eindeutigkeit der Lösung
#

dat.roh <- pilotRoh
items <- grep("E8R", colnames(dat.roh), value = TRUE)
vars <- c("item", "Categ", "AbsFreq", "RelFreq", "rpb.WLE")

# Wähle nur geschlossene Items (d. h., nicht Open gap-fill)
items.ogf <- dat.ib$item[dat.ib$format == "Open gap-fill"]
items <- setdiff(items, items.ogf)

# Bestimme absolute und relative Häufigkeit der Antwortoptionen 
# und jeweilige punktbiseriale Korrelationen mit dem Gesamtscore
ctt.roh <- tam.ctt2(dat.roh[, items], wlescore = relscore)

# Indikator der richtigen Antwort
match.item <- match(ctt.roh$item, dat.ib$item)
rohscore <- 1 * (ctt.roh$Categ == dat.ib$key[match.item])

# Klassifikation der Antwortoptionen 
ist.antwort.option <- (!ctt.roh$Categ \%in\% c(8,9))
ist.distraktor <- rohscore == 0 & ist.antwort.option
ist.pos.korr <- ctt.roh$rpb.WLE > 0.05
ist.bearb <- ctt.roh$AbsFreq >= 10

# Ausgabe
ctt.roh[ist.distraktor & ist.pos.korr & ist.bearb, vars]

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 7: Plausible Distraktoren
#

# Ausgabe
head(ctt.roh[ist.distraktor & ctt.roh$RelFreq < 0.05, vars],4)

# -------------------------------------------------------------
# Abschnitt 1.5.5, Listing 8: Kodierbarkeit
#

library(irr)
dat.mm <- pilotMM

# Bestimme Modus der Berechnung: bei 3 Kodierern
# gibt es 3 paarweise Vergleiche
vars <- grep("Coder", colnames(dat.mm))
n.vergleiche <- choose(length(vars), 2)
ind.vergleiche <- upper.tri(diag(length(vars)))

# Berechne Statistik für jedes Item
coder <- NULL
for(ii in unique(dat.mm$item)){
  dat.mm.ii <- dat.mm[dat.mm$item == ii, vars]
  
  # Relative Häufigkeit der paarweisen Übereinstimmung
  agreed <- apply(dat.mm.ii, 1, function(dd){
    sum(outer(dd, dd, "==")[ind.vergleiche]) / n.vergleiche
  })
  
  # Fleiss Kappa
  kappa <- kappam.fleiss(dat.mm.ii)$value
  
  # Ausgabe
  coderII <- data.frame("item" = ii,
                        "p_agreed" = mean(agreed),
                        "kappa" = round(kappa, 4))
  coder <- rbind(coder, coderII)
}

}%end notrun
}