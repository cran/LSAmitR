%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel  4}
\alias{Kapitel 4}
\encoding{UTF-8}
\title{Kapitel 4: Differenzielles Itemfunktionieren in Subgruppen}

\description{
Das ist die Nutzerseite zum Kapitel 4, \emph{Differenzielles Itemfunktionieren 
in Subgruppen}, im Herausgeberband Large-Scale Assessment mit \R: Methodische 
Grundlagen der österreichischen Bildungsstandardüberprüfung. 
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. 
Im Abschnitt \strong{Examples} werden die \R-Syntaxen des Kapitels vollständig 
wiedergegeben und gegebenenfalls erweitert.
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Trendtel, M., Schwabe, F. & Fellinger, R. (2016). Differenzielles 
  Itemfunktionieren in Subgruppen. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 111--147). Wien: facultas.
}

% \note{
% % wenn es Hinweise gibt
% }

\author{
  Matthias Trendtel, Franziska Schwabe, Robert Fellinger
}

\seealso{
%Verweis zu den Daten im Kapitel
Zu \code{\link{datenKapitel04}}, den im Kapitel verwendeten Daten.\cr
%Verweis zum nächsten und zum vorigen Kapitel
Zurück zu \code{\link{Kapitel 3}}, Standard-Setting.\cr
Zu \code{\link{Kapitel 5}}, Testdesign.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach 
%Übersicht einfügen)
%Zur Hilfeseite von fit.r \code{\link{fit.r}}
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
\dontrun{
library(difR)
library(mirt)
library(sirt)
library(TAM)
set.seed(12345)

data(datenKapitel04)
dat <- datenKapitel04$dat
dat.th1 <- datenKapitel04$dat.th1
ibank <- datenKapitel04$ibank

## -------------------------------------------------------------
## Abschnitt 4.4.1 DIF-Analysen für vollständige Daten
## -------------------------------------------------------------

items.th1 <- grep("E8R", colnames(dat.th1), value=T)
resp <- dat.th1[, items.th1]
AHS <- dat.th1$AHS

# -------------------------------------------------------------
# Abschnitt 4.4.1, Listing 1: Mantel-Haenszel
#

difMH(Data = resp, group = AHS, correct = F, focal.name = 0)

# -------------------------------------------------------------
# Abschnitt 4.4.1, Listing 2: Standardisierte p-Wert Differenzen
#

difStd(Data = resp, group = AHS, focal.name = 0)

# -------------------------------------------------------------
# Abschnitt 4.4.1, Listing 3: SIBTEST
#

SIBTEST(dat = resp, group = AHS, focal_name = 0, 
        focal_set = grep("E8RS03131", items.th1))
SIBTEST(dat = resp, group = AHS, focal_name=0,
        focal_set = grep("E8RS15621", items.th1))

# -------------------------------------------------------------
# Abschnitt 4.4.1, Listing 4: Methode nach Lord
#

difLord(Data = resp, group = AHS, focal.name = 0,
        model = "1PL")

# -------------------------------------------------------------
# Abschnitt 4.4.1, Listing 5: Zusammenschau
#

dichoDif(Data = resp, group = AHS, correct = F, focal.name = 0, 
         method = c("MH", "Std", "Lord"), model = "1PL")


## -------------------------------------------------------------
## Abschnitt 4.4.2 DIF-Analysen für unvollständige Daten
## -------------------------------------------------------------

items <- grep("E8R", colnames(dat), value = T)
resp <- dat[ ,items]
AHS <- dat$AHS

# -------------------------------------------------------------
# Abschnitt 4.4.2, Listing 1: Matching-Variable setzen
#

score <- rowSums(resp, na.rm=T)

# -------------------------------------------------------------
# Abschnitt 4.4.2, Listing 2: Durchführung Logistische Regression
#

difLR <- dif.logistic.regression(resp, group = AHS, score = score)

# -------------------------------------------------------------
# Abschnitt 4.4.2, Listing 3: Durchführung Logistische Regression
#                             mit angepasster Referenzgruppe
#

difLR <- dif.logistic.regression(resp, AHS==0, score)

# -------------------------------------------------------------
# Abschnitt 4.4.2, Listing 4: Ausgabe erster Teil
#

cbind(item = difLR$item, round(difLR[, 4:13], 3))

# -------------------------------------------------------------
# Abschnitt 4.4.2, Listing 5: Ausgabe zweiter Teil
#

cbind(difLR[, c(3,14:16)], sign = difLR[, 17], ETS = difLR[, 18]) 

# -------------------------------------------------------------
# Abschnitt 4.4.2, Listing 6: DIF-Größen
#

table(difLR[, 17], difLR[, 18])

difLR[c(10, 18), c(3, 14, 17:18)]

# -------------------------------------------------------------
# Abschnitt 4.4.2, Listing 7: Ausgabe dritter Teil
#

cbind(difLR[, c(3, 21:23)], sign=difLR[, 24])


## -------------------------------------------------------------
## Abschnitt 4.4.3 Hypothesenprüfung mit GLMM
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 1: Itemauswahl
#

HO.items <- ibank[ibank$format == "ho", "task"]

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 2: Facettenidentifikation
#

facets <- data.frame(AHS = dat$AHS)
form <- formula( ~ item * AHS)

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 3: Initiierung des Designs
#

design <- designMatrices.mfr(resp = dat[, items], 
                             formulaA = form, facets = facets)

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 4: Übergabe der Designmatrix und des
#                             erweiterten Responsepatterns
#

A <- design$A$A.3d[, , 1:(length(items) + 2)]
dimnames(A)[[3]] <- c(items, "AHS", "HO:AHS")
resp <- design$gresp$gresp.noStep

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 5: Ausgabe der ersten Zeilen des 
#                             Responsepatterns
#

head(resp)

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 6: Identifikation Itemformat X Gruppe
#

HO.AHS0 <- paste0(HO.items, "-AHS0")
HO.AHS1 <- paste0(HO.items, "-AHS1")

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 7: Spezifizierung des Designs
#

A[, , "HO:AHS"] <- 0
A[HO.AHS0, 2, "HO:AHS"] <- -1; A[HO.AHS1, 2, "HO:AHS"] <-  1

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 8: Ausgabe der Designmatrix für 
#                             Itemkategorie 'richtig beantwortet'
#

A[,2,c("AHS", "HO:AHS")]

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 9: Schätzen des Modells
#

mod <- tam.mml(resp = resp, A=A)

# -------------------------------------------------------------
# Abschnitt 4.4.3, Listing 10: Ausgabe der Parameterschätzer
#

summary(mod)
}
}
