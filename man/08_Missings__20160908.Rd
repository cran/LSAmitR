%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel 8}
\alias{Kapitel 8}
\encoding{UTF-8}
\title{Kapitel 8: Behandlung fehlender Daten und Ziehung von Plausible Values}

\description{
Das ist die Nutzerseite zum Kapitel 8, Behandlung fehlender Daten und Ziehung von 
Plausible Values, im Herausgeberband \emph{Large-Scale Assessment mit \R}:  
Methodische Grundlagen der österreichischen Bildungsstandard-Überprüfung.\cr
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. Im Abschnitt
\strong{Examples} werden die \R-Syntaxen des Kapitels vollständig widergegeben und 
gegebenefalls erweitert. 
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Robitzsch, Pham, G. & Yanagida, T. (2016). Behandlung fehlender Daten und 
  Ziehung von Plausible Values. In C. Schreiner & S. Breit (Hrsg.): 
  \emph{Large-Scale Assessment mit \R}: Methodische Grundlagen der 
  österreichischen Bildungsstandard-Überprüfung (pp. xx-yy). Wien: facultas wuv.
}

% \note{
% % wenn es Hinweise gibt
% }

\author{
  Alexander Robitzsch, Giang Pham, Takuya Yanagida
}

\seealso{
Zurück zu \code{\link{Kapitel 7}}, Statistische Analysen produktiver Kompetenzen .\cr
Zu \code{\link{Kapitel 9}}, Fairer Vergleich in der Rückmeldung.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach Übersicht einfügen)
%Zur Hilfeseite von \code{\link[TAM:TAM-package]{TAM}}
}

%% ----------------------------------------------------------------------------
%% Details
\details{
\subsection{Vorbereitungen}{
Zur Illustration der Konsequenzen fehlender Daten und der Messfehlerbehaftetheit von Variablen soll zunächst ein Illustrationsdatensatz (\code{data08I}) mit N=1500 simuliert werden. Dabei sollen zwei Variablen vorliegen: Der Sozialstatus \code{X} soll teilweise fehlende Werte aufweisen und die zu erfassende Kompetenz liegt sowohl als wahrer Wert \eqn{\theta}  als auch als messfehlerbehaftete Variable \eqn{\hat{\theta}} vor. Im Datensatz \code{data08I} liegt sowohl der vollständig beobachtete Sozialstatus (\code{x}) als auch derselbe Sozialstatus mit teilweise fehlenden Variablen (\code{X}) vor.
Neben dem Illustrationsdatensatz werden in diesem Kapitel Datensätze der österreichischen Bildungsstandards im Fach Englisch verwendet. Der Datensatz \code{data08H} enthält Kovariaten (d.h. Variablen aus Fragebögen oder administrative Daten) auf Ebene der Schüler (Ebene 1) und auf Ebene der Schulen (Ebene 2). Variablen beider Ebenen können dabei fehlende Werte besitzen. Im Datensatz \code{data08J} sind fehlende Werte des Datensatzes \code{data08H} durch eine Ersetzung von Werten bereits aufgefüllt. Außerdem liegen Item Responses der Schüler für den Bereich Hörverstehen (Listening, L) im Datensatz \code{data08K} vor.
Folgende R-Pakete werden in diesem Kapitel verwendet: \code{mice},\code{miceadds},\code{TAM},\code{pls}.
\verb{
library(miceadds)
library(mice)
library(TAM)
library(pls)
}
}%end Subsection Vorbereitungen


\subsection{Abschnitt 8.1.1: Konsequenzen fehlender Daten und messfehlerbehafteter Variablen}{
\subsection{Listing 1: Deskriptive Statistiken des Datensatzes}{
Mit folgendem R-Code werden deskriptive Statistiken des Datensatzes \code{data08I} ermittelt, an denen die Bedeutung der geeigneten Behandlung fehlender Werte und von Messfehlern herausgearbeitet werden soll.
\verb{
data(data08I)
dat <- data08I[,-1]
#*** Missinganteile
round( colMeans( is.na(dat), na.rm=TRUE) , 2 )     
#*** Mittelwerte
round( apply( dat , 2 , mean , na.rm=TRUE ) , 2 )   
#*** Zusammenhang von Missingindikator und Variablen 
round( miceadds::mi_dstat( dat[,c("WLE","X")] ) , 2 )
#*** Varianzen
round( apply( dat , 2 , var , na.rm=TRUE ) , 2 ) 
#*** Korrelationsmatrix
round( cor( dat , use = "pairwise.complete.obs") , 2 )
}
}%end Subsection Listing 1: Deskriptive Statistiken des Datensatzes
}%end Subsection Abschnitt 8.1.1: Konsequenzen fehlender Daten und messfehlerbehafteter Variablen


\subsection{Abschnitt 8.2.5: Durchführung der multiplen Imputation in R}{
\subsection{Listing 2: Variablenauswahl und leere Imputation}{
In diesem Abschnitt wird die multiple Imputation basiert auf den MICE-Ansatz im Paket \code{mice} in R umgesetzt. Als Datensatz soll \code{data08H} verwendet werden. Zur Vereinfachung der Darstellung wählen wir auf der Ebene der Schüler die Variablen Sozialstatus (\code{HISEI}), Anzahl der Bücher zu Hause (\code{buch}) und den WLE der Hörverstehenskompetenz (\code{E8LWLE}) sowie einen auf der Schulebene erfassten Sozialstatus (\code{SES_Schule}) aus.
\verb{
set.seed(56) 
data(data08H)
dat <- data08H
# waehle Variablen aus 
dat1 <- dat[ , c("MB_idschool", "HISEI", "buch", "E8LWLE",
                 "SES_Schule") ]
colMeans(is.na(dat1)) 
# fuehre leere Imputation durch
imp0 <- mice::mice(dat1, m=0, maxit=0)
}
}%end Subsection Listing 2: Variablenauswahl und leere Imputation

\subsection{Listing 3: Spezifikation der Imputationsmethoden}{
Die nachfolgende Syntax zeigt die Spezifikation der Imputationsmethoden im Vektor \code{impMethod} in \code{mice} für unser Datenbeispiel.
\verb{
impMethod <- imp0$method 
impMethod["HISEI"] <- "2l.continuous" 
# [...]  weitere Spezifikationen
impMethod["SES_Schule"] <- "2lonly.norm" 
impMethod
}
}%end Subsection Listing 3: Spezifikation der Imputationsmethoden

\subsection{Listing 4: Definition der Prädiktormatrix für die Imputation in mice}{
Nachfolgender R-Code zeigt die Definition der Prädiktormatrix (Matrix \code{predMatrix}) für die Imputation in \code{mice}.
\verb{
predMatrix <- imp0$predictorMatrix 
predMatrix[-1,"MB_idschool"] <- -2 
# [...]
predMatrix
}
}%end Subsection Listing 4: Definition der Prädiktormatrix für die Imputation in mice

\subsection{Listing 5: Datenimputation}{
Die Imputation kann nun unter dem Aufruf der Funktion \code{mice} unter Übergabe der Imputationsmethoden und der Prädiktormatrix erfolgen. Für das PMM werden 5 Donoren gewählt. Insgesamt sollen 10 imputierte Datensätze generiert werden, wobei der Algorithmus 7 Iterationen durchlaufen soll.
\verb{
imp1 <- mice::mice( dat1, imputationMethod=impMethod, 
                    predictorMatrix=predMatrix, donors=5,
                    m=10, maxit=7 )
}
}%end Subsection Listing 5: Datenimputation
}%end Subsection Abschnitt 8.2.5: Durchführung der multiplen Imputation in R

\subsection{Abschnitt 8.3.2: Dimensionsreduzierende Verfahren für Kovariaten im latenten Regressionsmodell}{
\subsection{Listing 6: Kovariatenauswahl, Interaktionsbildung und Bestimmung PLS-Faktoren}{
Die Methode des Partial Least Squares soll für den Datensatz \code{data08J} illustriert werden. Es wird zum Auffüllen der Kovariaten mit fehlenden Werten nur ein imputierter Datensatz erstellt. Danach wird eine PLS-Regression des WLE der Hörverstehenskompetenz \code{E8LWLE} auf Kovariaten und deren Interaktionen bestimmt. Für die Bestimmung der PLS-Faktoren wird das R-Paket \code{pls} verwendet.Die nachfolgende R-Syntax zeigt die Kovariatenauswahl, die Bildung der Interaktionen und die Bestimmung der PLS-Faktoren. Insgesamt entstehen durch Aufnahme der Haupteffekte und Interaktionen 55 Kovariaten.
\verb{
data(data08J)
dat <- data08J
#*** Kovariatenauswahl
kovariaten <- scan(what="character", nlines=2) 
X <- scale( dat[, kovariaten ] )
V <- ncol(X) 
# bilde alle Zweifachinteraktionen 
c2 <- combinat::combn(V,2) 
X2 <- apply( c2 , 2 , FUN = function(cc){ 
             X[,cc[1]] * X[,cc[2]] } ) 
X0 <- cbind( X , X2 )
mod1 <- pls::plsr( dat$E8LWLE ~ X0 , ncomp=55  ) 
summary(mod1)
}
}%end Subsection Listing 6: Kovariatenauswahl, Interaktionsbildung und Bestimmung PLS-Faktoren
}%end Subsection Abschnitt 8.3.2: Dimensionsreduzierende Verfahren für Kovariaten im latenten Regressionsmodell

\subsection{Abschnitt 8.3.3: Ziehung von Plausible Values in R}{
In diesem Abschnitt soll die Ziehung von Plausible Values mit dem R-Paket \code{TAM} illustriert werden. Dabei beschränken wir uns auf den Kompetenzbereich des Hörverstehens. 
\subsection{Listing 7: PLS-Faktoren auswählen}{
In Abschnitt 8.3.2 wurden dabei die Kovariaten auf PLS-Faktoren reduziert. Für die Ziehung von Plausible Values werden nachfolgend 10 PLS-Faktoren verwendet.
\verb{
facs <- mod1$scores[,1:10]
}
}%end Subsection Listing 7: PLS-Faktoren auswählen

\subsection{Listing 8: Rasch-Skalierung}{
Für die Hörverstehenskompetenz im Datensatz \code{data08K} wird nachfolgend das Messmodell als Rasch-Modell geschätzt. Dabei werden Stichprobengewichte für die Bestimmung der Itemparameter verwendet.
\verb{
data(data08K)
dat2 <- data08K
items <- grep("E8L", colnames(dat2), value=TRUE)
# Schaetzung des Rasch-Modells in TAM
mod11 <- TAM::tam.mml( resp= dat2[,items ] , 
                       pid = dat2$MB_idstud,  
                       pweights = dat2$wgtstud )
}
}%end Subsection Listing 8: Rasch-Skalierung

\subsection{Listing 9: Individuelle Likelihood, latente Regressionsmodell und PV-Ziehung}{
Bei einer Fixierung von Itemparametern ist die bedingte Verteilung \eqn{P(\mathbf{X}|\boldsymbol{\theta})} des Messmodells konstant. Die Schätzung von Item-Response-Modellen erfolgt in \code{TAM} unter Verwendung eines diskreten Gitters von \eqn{\boldsymbol{\theta}}-Werten. Während der Anpassung des Rasch-Modells in \code{mod11} liegt daher die auf diesem Gitter ausgewertete sog. individuelle Likelihood \eqn{P(\mathbf{X}|\boldsymbol{\theta})} vor, die mit der Funktion \code{IRT.likelihood} aus dem Objekt \code{mod11} extrahiert werden kann. Das latente Regressionsmodell kann unter Rückgriff auf die individuelle Likelihood mit der Funktion \code{tam.latreg} angepasst werden. Die Ziehung der Plausible Values erfolgt anschließend mit der Funktion \code{tam.pv}.
\verb{
#*** extrahiere individuelle Likelihood
lmod11 <- IRT.likelihood(mod11) 
#*** schaetze latentes Regressionsmodell
mod12 <- TAM::tam.latreg( like = lmod11 , Y = facs )
#*** ziehe Plausible Values 
pv12 <- TAM::tam.pv(mod12, normal.approx=TRUE, 
                    samp.regr=TRUE , ntheta=400)
}
}%end Subsection Listing 9: Individuelle Likelihood, latente Regressionsmodell und PV-Ziehung

\subsection{Listing 10: Plausible Values extrahieren}{
Die imputierten Plausible Values lassen sich im Element \code{$pv} des Ergebnisobjekts aus \code{tam.pv} extrahieren.
\verb{
#*** Plausible Values fuer drei verschiedene Schueler
round( pv12$pv[c(2,5,9),] , 3 )
}
}%end Subsection Listing 10: Plausible Values extrahieren
}%end Subsection Abschnitt 8.3: Plausible Value Imputation

}%end Details

%% ----------------------------------------------------------------------------
%% Examples
\examples{
library(TAM)
library(mice)
library(miceadds)
library(pls)
library(combinat)
library(mitml)

data(datenKapitel08)
data08H <- datenKapitel08$data08H
data08I <- datenKapitel08$data08I
data08J <- datenKapitel08$data08J
data08K <- datenKapitel08$data08K

\dontrun{
## -------------------------------------------------------------
## Abschnitt 8.1.1: Konsequenzen fehlender Daten und 
##                  messfehlerbehafteter Variablen
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 8.1.1, Listing 1: Deskriptive Statistiken des 
#                             Illustrationsdatensatzes
#

data(datenKapitel08)
dat <- datenKapitel08$data08I[,-1]
#*** Missinganteile
round( colMeans( is.na(dat), na.rm=TRUE) , 2 )     
#*** Mittelwerte
round( apply( dat , 2 , mean , na.rm=TRUE ) , 2 )   
#*** Zusammenhang von Missingindikator und Variablen 
round( miceadds::mi_dstat( dat[,c("WLE","X")] ) , 2 )
#*** Varianzen
round( apply( dat , 2 , var , na.rm=TRUE ) , 2 ) 
#*** Korrelationsmatrix
round( cor( dat , use = "pairwise.complete.obs") , 2 )


## -------------------------------------------------------------
## Abschnitt 8.2: Multiple Imputation
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 8.2.5, Listing 1: Variablenauswahl und leere 
#                             Imputation
#

set.seed(56) 
data(datenKapitel08)
dat <- datenKapitel08$data08H
# wähle Variablen aus 
dat1 <- dat[ , c("idschool", "HISEI", "buch", "E8LWLE",
                 "SES_Schule") ]
colMeans(is.na(dat1)) 
# führe leere Imputation durch
imp0 <- mice::mice(dat1, m=0, maxit=0)

# -------------------------------------------------------------
# Abschnitt 8.2.5, Listing 2: Spezifikation der Imputations-
#                             methoden
#

impMethod <- imp0$method 
impMethod["HISEI"] <- "2l.continuous" 
# [...]  weitere Spezifikationen
impMethod["SES_Schule"] <- "2lonly.norm" 
impMethod     

# -------------------------------------------------------------
# Abschnitt 8.2.5, Listing 2b: Ergänzung zum Buch
#

# [...]  weitere Spezifikationen
impMethod["buch"]  <- "2l.pmm"
impMethod

# -------------------------------------------------------------
# Abschnitt 8.2.5, Listing 3: Definition der Prädiktormatrix 
#                             für die Imputation in mice
#

predMatrix <- imp0$predictorMatrix 
predMatrix[-1,"idschool"] <- -2 
# [...]
predMatrix    

# -------------------------------------------------------------
# Abschnitt 8.2.5, Listing 3b: Ergänzung zum Buch
#

# [...]
predMatrix[2:4,2:4] <- 3*predMatrix[2:4,2:4]
predMatrix

# -------------------------------------------------------------
# Abschnitt 8.2.5, Listing 4: Führe Imputation durch
#

imp1 <- mice::mice( dat1, imputationMethod=impMethod, 
  predictorMatrix=predMatrix, donors=5, m=10, maxit=7)

# -------------------------------------------------------------
# Abschnitt 8.2.5, Listing 4b: Ergänzung zum Buch
#

#-- Mittelwert HISEI
wmod1 <- with( imp1 , lm(HISEI ~ 1))
summary( mice::pool( wmod1 ) )

#-- lineare Regression HISEI auf Büchervariable
wmod2 <- with( imp1 , lm(E8LWLE ~ HISEI) )
summary( mice::pool( wmod2 ))

#-- Inferenz Mehrebenenmodelle mit Paket mitml
imp1b <- mitml::mids2mitml.list(imp1)
wmod3 <- with(imp1b, lme4::lmer( HISEI ~ (1|idschool)) )
mitml::testEstimates(wmod3, var.comp=TRUE)

## ------------------------------------------------------------
## Abschnitt 8.3.2: Dimensionsreduzierende Verfahren für 
## Kovariaten im latenten Regressionsmodell
## ------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 8.3.2, Listing 1: Kovariatenauswahl, Interaktions- 
#                         bildung und Bestimmung PLS-Faktoren
#

set.seed(56)
data(datenKapitel08)
dat <- datenKapitel08$data08J

#*** Kovariatenauswahl
kovariaten <- scan(what="character", nlines=2) 
  female migrant HISEI  eltausb buch  
  SK LF NSchueler NKlassen SES_Schule

X <- scale( dat[, kovariaten ] )
V <- ncol(X) 
# bilde alle Zweifachinteraktionen 
c2 <- combinat::combn(V,2) 
X2 <- apply( c2 , 2 , FUN = function(cc){ 
            X[,cc[1]] * X[,cc[2]] } ) 
X0 <- cbind( X , X2 )
# Partial Least Squares Regression
mod1 <- pls::plsr( dat$E8LWLE ~ X0 , ncomp=55  ) 
summary(mod1)

# -------------------------------------------------------------
# Abschnitt 8.3.2, Listing 1b: Ergänzung zum Buch
# Abbildung: Aufgeklärter Varianzanteil
#

# Principal Component Regression (Extraktion der Hauptkomponenten)
mod2 <- pls::pcr(  dat$E8LWLE ~ X0 , ncomp=55 )
summary(mod2)

#*** extrahierte Varianzen mit PLS-Faktoren und PCA-Faktoren
res <- mod1
R2 <- base::cumsum(res$Xvar) / res$Xtotvar
ncomp <- 55
Y <- dat$E8LWLE
R21 <- base::sapply( 1:ncomp , FUN = function(cc){
  1 - stats::var( Y -  res$fitted.values[,1,cc] ) / stats::var( Y )
} )
dfr <- data.frame("comp" = 1:ncomp , "PLS" = R21 )

res <- mod2
R2 <- base::cumsum(res$Xvar) / res$Xtotvar
ncomp <- 55
Y <- dat$E8LWLE
R21 <- base::sapply( 1:ncomp , FUN = function(cc){
           1 - stats::var( Y -  res$fitted.values[,1,cc] ) / stats::var( Y )
} )
dfr$PCA <- R21


plot( dfr$comp , dfr$PLS , type="l" , xlab="Anzahl Faktoren" , 
      ylab="Aufgeklärter Varianzanteil" ,
      ylim=c(0,.3) )
points( dfr$comp , dfr$PLS , pch=16 )        
points( dfr$comp , dfr$PCA , pch=17 )        
lines( dfr$comp , dfr$PCA , lty=2 )        
legend( 45 , .15 , c("PLS" , "PCA") , pch=c(16,17) , lty=c(1,2))

## ------------------------------------------------------------
## Abschnitt 8.3.3: Ziehung von Plausible Values in R
## ------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 8.3.3, Listing 1: PLS-Faktoren auswählen
#

facs <- mod1$scores[,1:10]

# -------------------------------------------------------------
# Abschnitt 8.3.3, Listing 1b: Ergänzung zum Buch
#
set.seed(98766)

# -------------------------------------------------------------
# Abschnitt 8.3.3, Listing 2: Anpassung kognitive Daten
#

data(datenKapitel08)
dat2 <- datenKapitel08$data08K
items <- grep("E8L", colnames(dat2), value=TRUE)
# Schätzung des Rasch-Modells in TAM
mod11 <- TAM::tam.mml( resp= dat2[,items ] , 
       pid = dat2$idstud, pweights = dat2$wgtstud ) 

# -------------------------------------------------------------
# Abschnitt 8.3.3, Listing 3: Individuelle Likelihood, latentes 
#                             Regressionsmodell und PV-Ziehung
#

#*** extrahiere individuelle Likelihood
lmod11 <- IRT.likelihood(mod11) 
#*** schaetze latentes Regressionsmodell
mod12 <- TAM::tam.latreg( like = lmod11 , Y = facs )
#*** ziehe Plausible Values 
pv12 <- TAM::tam.pv(mod12, normal.approx=TRUE, 
               samp.regr=TRUE , ntheta=400)

# -------------------------------------------------------------
# Abschnitt 8.3.3, Listing 4: Plausible Values extrahieren
#

#*** Plausible Values für drei verschiedene Schüler
round( pv12$pv[c(2,5,9),] , 3 )


# -------------------------------------------------------------
# Abschnitt 8.3.3, Listing 4b: Ergänzung zum Buch
#

hist( pv12$pv$PV1.Dim1 )

# Korrelation mit Kovariaten
round( cor( pv12$pv$PV1.Dim1 , dat[,kovariaten] , 
            use="pairwise.complete.obs") , 3 )
round( cor( dat$E8LWLE , dat[,kovariaten] , 
            use="pairwise.complete.obs" ) , 3 )
}%end dontrun
}