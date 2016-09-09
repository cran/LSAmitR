%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel 9}
\alias{Kapitel 9}
\encoding{UTF-8}
\title{Kapitel 9: Fairer Vergleich in der Rückmeldung}

\description{
Das ist die Nutzerseite zum Kapitel 9, Fairer Vergleich in der Rückmeldung, im 
Herausgeberband \emph{Large-Scale Assessment mit \R}:  Methodische Grundlagen der 
österreichischen Bildungsstandard-Überprüfung.\cr
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. Im Abschnitt
\strong{Examples} werden die \R-Syntaxen des Kapitels vollständig widergegeben und 
gegebenefalls erweitert. 
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Pham, G., Robitzsch, A., George, A. C. & Freunberger, R. (2016). Fairer 
  Vergleich in der Rückmeldung. In C. Schreiner & S. Breit (Hrsg.): 
  \emph{Large-Scale Assessment mit \R}: Methodische Grundlagen der 
  österreichischen Bildungsstandard-Überprüfung (pp. xx-yy). Wien: facultas wuv.
}

% \note{
% % wenn es Hinweise gibt
% }

\author{
  Giang Pham, Alexander Robitzsch, Ann Cathrice George, Roman Freunberger
}

\seealso{
Zurück zu \code{\link{Kapitel 8}}, Behandlung fehlender Daten und PV-Ziehung.\cr
Zu \code{\link{Kapitel 10}}, Reporting und Analysen.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach Übersicht einfügen)
%Zur Hilfeseite von \code{\link[TAM:TAM-package]{TAM}}
}

%% ----------------------------------------------------------------------------
%% Details
\details{
\subsection{Vorbereitungen}{
Der zur Illustration verwendete Datensatz \code{dat} beinhaltet (imputierte) aggregierte Leistungs- und Hintergrunddaten von 244 Schulen, bestehend aus 74 ländlichen allgemeinbildenden Pflichtschulen (APS, Stratum 1), 69 städtischen APS (Stratum 2), 52 ländlichen allgemeinbildenden höheren Schulen (AHS, Stratum 3) und 49 städtischen AHS (Stratum 4). 
Im Kapitel wird zur Bildung von Interaktionseffekten und quadratischen Termen der Kovariaten eine neue Funktion \code{covainteraction} verwendet.
\verb{
data(datenKapitel09)
dat <- datenKapitel09

covainteraction <- function(dat,covas,nchar){
  for(ii in 1:(length(covas))){
    vv1 <- covas[ii]
    # Interaktion von vv1 mit sich selbst
    subname1 <- substr(vv1,1,nchar)
    intvar <- paste0(subname1, subname1)
    if(vv1 == covas[1]){
      dat.int <- dat[,vv1]*dat[,vv1];
      newvars <- intvar } else {
        dat.int <- cbind(dat.int,dat[,vv1]*dat[,vv1]); 
        newvars <- c(newvars,intvar) 
      }
    # Interaktion von vv1 mit restlichen Variablen
    if(ii < length(covas)){
      for(jj in ((ii+1):length(covas))){
        vv2 <- covas[jj]
        subname2 <- substr(vv2,1,nchar)
        intvar <- paste0(subname1, subname2)
        newvars <- c(newvars, intvar)
        dat.int <- cbind(dat.int,dat[,vv1]*dat[,vv2])
      }
    }
    
  }
  dat.int <- data.frame(dat.int)
  names(dat.int) <- newvars
  return(dat.int)
}


}
}%end Subsection Vorbereitungen


\subsection{Abschnitt 9.2.5.1: Kovariaten und Interaktionsterme}{
\subsection{Listing 1: Kovariatenauswahl und z-Standardisierung}{
Als Variablen zur Beschreibung von Kontext und Schülerzusammensetzung in den Schulen werden in diesem Beispiel die logarithmierte Schulgröße \code{groesse}, der Anteil an Mädchen \code{female}, der Anteil an Schülerinnen und Schülern mit Migrationshintergrund \code{mig} und der mittlere sozioökonomische Status (SES) \code{sozstat} eingeführt. Die abhängige Variable ist die aggregierte Schülerleistung der Schule \code{TWLE}. Alle Kovariaten (\code{vars}) werden zunächst z-standardisiert (\code{zvars}).
\verb{
vars <- c("groesse","female","mig","sozstat")
zvars <- paste0("z",vars)
dat[,zvars] <- scale(dat[,vars],scale = T)
}
}%end Subsection Listing 1: Kovariatenauswahl und z-Standardisierung

\subsection{Listing 2: Interaktionen bilden, z-standardisieren}{
Zur Optimierung der Modellspezifikation werden Interaktionseffekte und quadratische Terme der Kovariaten gebildet, dann z-standardisiert und in den Gesamtdatensatz hinzugefügt. Die neuen Variablennamen sind in der Liste \code{intvars} aufgelistet.
\verb{
dat1 <- covainteraction(dat = dat,covas = zvars,nchar = 4)
intvars <- names(dat1) # Interaktionsvariablen
dat1[,intvars] <- scale(dat1[,intvars],scale = T)
dat <- cbind(dat,dat1)
}
}%end Subsection Listing 2: Interaktionen bilden, z-standardisieren

\subsection{Listing 3: Haupt- und Interaktionseffekte}{
\verb{
maineff <- zvars # Haupteffekte 
alleff <- c(zvars,intvars) # Haupt- und Interaktionseffekte
}
}%end Subsection Listing 3: Haupt- und Interaktionseffekte
}%end Subsection Abschnitt 9.2.5.1: Kovariaten und Interaktionsterme


\subsection{Abschnitt 9.2.5.2: OLS-Regression}{
\subsection{Listing 4: OLS-Regression mit Haupteffekten}{
Das OLS-Regressionsmodell mit den Haupteffekten als Modellprädiktoren (\code{ols.mod1}) für Schulen im Stratum (\code{st}) 4
\verb{
fm.ols1 <- paste0("TWLE ~ ",paste(maineff,collapse=" + "))
fm.ols1 <- as.formula(fm.ols1) # Modellgleichung
st <- 4
pos <- which(dat$stratum == st) # Schulen im Stratum st
ols.mod1 <- lm(formula = fm.ols1,data = dat[pos,]) # Regression
}
}%end Subsection Listing 4: OLS-Regression mit Haupteffekten
}%end Subsection Abschnitt 9.2.5.2: OLS-Regression

\subsection{Abschnitt 9.2.5.3: Lasso-Regression}{
\subsection{Listing 5: Datenaufbereitung}{
Für die Durchführung der Lasso-Regression wird das R-Paket glmnet (Friedman et al., 2010) eingesetzt. Die Kovariatenmatrix (\code{Z}) sowie der Vektor der abhängigen Leistungswerte (\code{Y}) müssen definiert werden.
\verb{
library(glmnet)
Z <- as.matrix(dat[pos,alleff]) # Kovariatenmatrix
Y <- dat$TWLE[pos] # Abhängige Variable
}
}%end Subsection Listing 5: Datenaufbereitung

\subsection{Listing 6: Bestimmung Teilmengen für Kreuzvalidierung, Lasso-Regression}{
Das Lasso-Verfahren wird mit der Funktion \code{cv.glmnet()} durchgeführt. Zur Auswahl eines optimalen shrinkage \eqn{\lambda} wird das Verfahren der K-fachen Kreuzvalidierung verwendet. Die Schulstichprobe wird durch ID-Zuweisung (\code{foldid}) verschiedenen Teilmengen zugewiesen.
\verb{
nid <- floor(length(pos)/3) # Teilmengen definieren 
foldid <- rep(c(1:nid),3,length.out=length(pos)) # Zuweisung
lasso.mod2 <- glmnet::cv.glmnet(x=Z,y=Y,alpha = 1, foldid = foldid)
}
}%end Subsection Listing 6: Bestimmung Teilmengen für Kreuzvalidierung, Lasso-Regression

\subsection{Listing 7: Erwartungswerte der Schulen}{
Entsprechend \code{lambda.min} werden die Regressionskoeffizienten und die entsprechenden Erwartungswerte der Schulen bestimmt.
\verb{
lasso.pred2 <- predict(lasso.mod2,newx = Z,s="lambda.min")
dat$expTWLE.Lasso2[pos] <- as.vector(lasso.pred2)
}
}%end Subsection Listing 7: Erwartungswerte der Schulen

\subsection{Listing 8: Bestimmung R^2}{
Bestimmung des aufgeklärten Varianzanteils der Schulleistung \code{R^2}.
\verb{
varY <- var(dat$TWLE[pos])
varY.lasso.mod2 <- var(dat$expTWLE.Lasso2[pos])
R2.lasso.mod2 <- varY.lasso.mod2/varY
}
}%end Subsection Listing 8: Bestimmung R^2
}%end Subsection Abschnitt 9.2.5.3: Lasso-Regression


\subsection{Abschnitt 9.2.5.4: Nichtparametrische Regression}{
\subsection{Listing 9: Distanzberechnung}{
Der erste Schritt zur Durchführung einer nichtparametrischen Regression ist die Erstellung der Distanzmatrix zwischen Schulen. In diesem Beispiel wird die euklidische Distanz als Distanzmaß berechnet, alle standardisierten Haupteffekte sind eingeschlossen. Außerdem setzen wir die Gewichte von allen Kovariaten (\code{gi}) auf 1. \code{dfr.i} in diesem Beispiel ist die Distanzmatrix für erste Schule im Stratum.
\verb{
N <- length(pos) # Anzahl Schulen im Stratum
schools <- dat$idschool[pos] # Schulen-ID
i <- 1
# Teildatensatz von Schule i
dat.i <- dat[pos[i],c("idschool","TWLE",maineff)]
names(dat.i) <- paste0(names(dat.i),".i")
# Daten der Vergleichsschulen
dat.vgl <- dat[pos[-i],c("idschool","TWLE",maineff)]
index.vgl <- match(dat.vgl$idschool,schools)
# Daten zusammenfuegen
dfr.i <- data.frame("index.i"=i,dat.i,"index.vgl"=index.vgl,
                    dat.vgl, row.names=NULL)
# Distanz zur Schule i
dfr.i$dist <- 0
gi <- c(1,1,1,1)
for(ii in 1:length(maineff)){
  vv <- maineff[ii]
  pair.vv <- grep(vv, names(dfr.i), value=T)
  dist.vv <- gi[ii]*((dfr.i[,pair.vv[1]]-dfr.i[,pair.vv[2]])^2)
  dfr.i$dist <- dfr.i$dist + dist.vv }
}
}%end Subsection Listing 9: Distanzberechnung

\subsection{Listing 10: H initiieren}{
Die Gewichte w_{ik} für jedes Paar (i, k) von Schulen werden mithilfe der Distanz, der Gauß’schen Kernfunktion (\code{dnorm}) als Transformationsfunktion sowie einer schulspezifischen Bandweite h_i berechnet. Die Auswahl des optimalen Werts \eqn{\hat{h_i}} für jede Schule i erfolgt nach Vieu (1991). Zunächst wird ein Vektor \code{H} so gewählt, dass der optimale Wert \eqn{\hat{h_i}} größer als der kleinste und kleiner als der größte Wert in \code{H} ausfällt. Je kleiner das Intervall zwischen den Werten in \code{H} ist, desto wahrscheinlicher ist, dass ein Listenelement den optimalen Wert erlangt. Auf der anderen Seite korrespondiert die Rechenzeit mit der Länge von \code{H}. Gemäß der Größe der Vergleichsgruppe wählen wir eine Länge von 30 für \code{H}, zusätzlich wird ein sehr großer Wert (100000) für die Fälle hinzugefügt, bei denen alle Gewichte beinahe gleich sind.
\verb{
d.dist <- max(dfr.i$dist)-min(dfr.i$dist)
H <- c(seq(d.dist/100,d.dist,length=30),100000)
V1 <- length(H) 
# Anzahl Vergleichsschulen
n <- nrow(dfr.i) 
}
}%end Subsection Listing 10: H initiieren

\subsection{Listing 11: Leave-One-Out-Schätzer der jeweiligen Vergleichsschule k nach h in H}{
Auf Basis aller Werte in \code{H} und dem jeweils entsprechenden Gewicht w_{ik} (\code{wgt.h}) werden die Leave-One-Out-Schätzer der jeweiligen Vergleichsschule (\code{pred.k}) berechnet.
\verb{
sumw <- 0*H # Vektor w_{ik} initiieren, h in H
av <- "TWLE"
dfr0.i <- dfr.i[,c("idschool",av)]
# Schleife ueber alle h-Werte
for (ll in 1:V1 ){
  h <- H[ll]
  # Gewicht w_{ik} bei h
  dfr.i$wgt.h <- dnorm(sqrt(dfr.i$dist), mean=0, sd=sqrt(h))
  # Summe von w_{ik} bei h
  sumw[which(H==h)] <- sum(dfr.i$wgt.h)
  # Leave-one-out-Schätzer von Y_k
  for (k in 1:n){
    # Regressionsformel
    fm <- paste0(av,"~",paste0(maineff,collapse="+"))
    fm <- as.formula(fm)
    # Regressionsanalyse ohne Beitrag von Schule k
    dfr.i0 <- dfr.i[-k,]
    mod.k <- lm(formula=fm,data=dfr.i0,weights=dfr.i0$wgt.h)
    # Erwartungswert anhand Kovariaten der Schule k berechnen
    pred.k <- predict(mod.k, dfr.i)[k]
    dfr0.i[k,paste0( "h_",h) ] <- pred.k
}}
# Erwartungswerte auf Basis verschiedener h-Werte
dfr1 <- data.frame("idschool.i"=dfr.i$idschool.i[1],"h"=H )
}
}%end Subsection Listing 11: Leave-One-Out-Schätzer der jeweiligen Vergleichsschule k nach h in H

\subsection{Listing 12: Kreuzvalidierungskriterium nach h in H}{
Zur Berechnung der Kreuzvalidierungskriterien (\code{CVh}, je kleiner, desto reliabler sind die Schätzwerte) für alle Werte h in \code{H} verwenden wir in diesem Beispiel die Plug-in-Bandweite nach Altman und Leger (1995) (\code{hAL}), die mit der Funktion \code{ALbw()} des R-Pakets \code{kerdiest} aufrufbar ist.
\verb{
library(kerdiest)
hAL <- ALbw("n",dfr.i$dist) # Plug-in Bandweite
dfr.i$cross.h <- hAL
dfr.i$crosswgt <- dnorm( sqrt(dfr.i$dist), mean=0, sd = sqrt(hAL) ) 
# Kreuzvalidierungskriterium CVh
vh <- grep("h_",colnames(dfr0.i),value=TRUE)
for (ll in 1:V1){
  dfr1[ll,"CVh"] <- sum( (dfr0.i[,av] - dfr0.i[,vh[ll]])^2 * 
                           dfr.i$crosswgt) / n}

}
}%end Subsection Listing 12: Kreuzvalidierungskriterium nach h in H

\subsection{Listing 13: Bestimmung optimales Wertes von h}{
Der optimale Wert von h in \code{H} (\code{h.min}) entspricht dem mit dem kleinsten resultierenden \code{CVh}.
\verb{
dfr1$min.h.index <- 0
ind <-  which.min( dfr1$CVh )
dfr1$min.h.index[ind ] <- 1
dfr1$h.min <- dfr1$h[ind]
}
}%end Subsection Listing 13: Bestimmung optimales Wertes von h

\subsection{Listing 14: Kleinste Quadratsumme der Schätzfehler}{
Kleinste Quadratsumme der Schätzfehler der nichtparametrischen Regression mit h=\code{h.min}.
\verb{
dfr1$CVhmin <- dfr1[ ind , "CVh" ]
}
}%end Subsection Listing 14: Kleinste Quadratsumme der Schätzfehler

\subsection{Listing 15: Effizienzsteigerung}{
Die Effizienz (Steigerung der Schätzungsreliabilität) der nichtparametrischen Regression gegenüber der linearen Regression (äquivalent zu \code{CVh} bei h=100000).
\verb{
dfr1$eff_gain <-  100 * ( dfr1[V1,"CVh"] / dfr1$CVhmin[1] - 1 )
}
}%end Subsection Listing 15: Effizienzsteigerung

\subsection{Listing 16: Durchführung der nichtparametrischen Regression}{
\verb{
h <- dfr1$h.min[1]  # h.min
dfr.i$wgt.h <- dnorm(sqrt(dfr.i$dist),sd=sqrt(h))/
  dnorm(0,sd= sqrt(h)) # w_{ik} bei h.min      
dfr.i0 <- dfr.i
# Lokale Regression    
mod.ii <- lm(formula=fm,data=dfr.i0,weights=dfr.i0$wgt.h)
# Kovariaten Schule i
predM <- data.frame(dfr.i[1,paste0(maineff,".i")])    
names(predM) <- maineff
pred.ii <- predict(mod.ii, predM) # Schätzwert Schule i
dat[match(dfr1$idschool.i[1],dat$idschool), "expTWLE.np"] <- pred.ii
}
}%end Subsection Listing 16: Durchführung der nichtparametrischen Regression
}%end Subsection Abschnitt 9.2.5.4: Nichtparametrische Regression


\subsection{Abschnitt 9.2.7, Berücksichtigung der Schätzfehler}{
Der Erwartungsbereich wird nach der im Buch beschriebenen Vorgehensweise bestimmt.
\subsection{Listing 17: Bestimmung des Erwartungsbereichs}{
Bestimmung der Breite des Erwartungsbereichs aller Schulen auf Basis der Ergebnisse der OLS-Regression mit Haupteffekten.
\verb{
vv <- "expTWLE.OLS1" # Variablenname
mm <- "OLS1" # Kurzname des Modells
dfr <- NULL # Ergebnistabelle
# Schleife über alle möglichen Breite von 10 bis 60
for(w in 10:60){
  # Variablen für Ergebnisse pro w
  var <- paste0(mm,".pos.eb",w) # Position der Schule
  var.low <- paste0(mm,".eblow",w) # Untere Grenze des EBs
  var.upp <- paste0(mm,".ebupp",w) # Obere Grenze des EBs
  # Berechnen
  dat[,var.low] <- dat[,vv]-w/2 # Untere Grenze des EBs
  dat[,var.upp] <- dat[,vv]+w/2 # Obere Grenze des EBs 
  # Position: -1=unterhalb, 0=innerhalb, 1=oberhalb des EBs 
  dat[,var] <- -1*(dat$TWLE < dat[,var.low]) + 1*(dat$TWLE > dat[,var.upp])
  # Verteilung der Schulpositionen
  tmp <- data.frame(t(matrix(prop.table(table(dat[,var])))))
  names(tmp) <- c("unterhalb","innerhalb","oberhalb")
  tmp <- data.frame("ModellxBereich"=var,tmp); dfr <- rbind(dfr,tmp) }

# Abweichung zur Wunschverteilung 25-50-25 
dfr1 <- dfr 
dfr1[,c(2,4)] <- (dfr1[,c(2,4)] - .25)^2 
dfr1[,3] <- (dfr1[,3] - .5)^2 
dfr1$sumquare <- rowSums(dfr1[,-1]) 
# Auswahl markieren 
dfr$Auswahl <- 1*(dfr1$sumquare == min(dfr1$sumquare) )
}
}%end Subsection Listing 17: Bestimmung des Erwartungsbereichs
}%end Subsection Abschnitt 9.2.7, Berücksichtigung der Schätzfehler
}%end Details

%% ----------------------------------------------------------------------------
%% Examples
\examples{
library(miceadds)
library(glmnet)
library(kerdiest)

covainteraction <- function(dat,covas,nchar){
  for(ii in 1:(length(covas))){
    vv1 <- covas[ii]
    # Interaktion von vv1 mit sich selbst
    subname1 <- substr(vv1,1,nchar)
    intvar <- paste0(subname1, subname1)
    if(vv1 == covas[1]){
      dat.int <- dat[,vv1]*dat[,vv1];
      newvars <- intvar } else {
        dat.int <- cbind(dat.int,dat[,vv1]*dat[,vv1]); 
        newvars <- c(newvars,intvar) 
      }
    # Interaktion von vv1 mit restlichen Variablen
    if(ii < length(covas)){
      for(jj in ((ii+1):length(covas))){
        vv2 <- covas[jj]
        subname2 <- substr(vv2,1,nchar)
        intvar <- paste0(subname1, subname2)
        newvars <- c(newvars, intvar)
        dat.int <- cbind(dat.int,dat[,vv1]*dat[,vv2])
      }
    }
    
  }
  dat.int <- data.frame(dat.int)
  names(dat.int) <- newvars
  return(dat.int)
}

data(datenKapitel09)
dat <- datenKapitel09

\dontrun{
## -------------------------------------------------------------
## Abschnitt 9.2.5, Umsetzung in R
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 9.2.5.1, Listing 1: Kovariatenauswahl und
#                               z-Standardisierung
#

vars <- c("groesse","female","mig","sozstat")
zvars <- paste0("z",vars)
dat[,zvars] <- scale(dat[,vars],scale = T)

# -------------------------------------------------------------
# Abschnitt 9.2.5.1, Listing 2: 
#

# Interaktionen bilden, z-standardisieren  
dat1 <- covainteraction(dat = dat,covas = zvars,nchar = 4)
intvars <- names(dat1) # Interaktionsvariablen
dat1[,intvars] <- scale(dat1[,intvars],scale = T)
dat <- cbind(dat,dat1)

# -------------------------------------------------------------
# Abschnitt 9.2.5.1, Listing 3: Modellprädiktoren: Haupt- und
#                               Interaktionseffekte
#

maineff <- zvars # Haupteffekte 
alleff <- c(zvars,intvars) # Haupt- und Interaktionseffekte

# -------------------------------------------------------------
# Abschnitt 9.2.5.2, Listing 4: OLS-Regression mit Haupteffekten
# 

fm.ols1 <- paste0("TWLE ~ ",paste(maineff,collapse=" + "))
fm.ols1 <- as.formula(fm.ols1) # Modellgleichung
st <- 4
pos <- which(dat$stratum == st) # Schulen im Stratum st
ols.mod1 <- lm(formula = fm.ols1,data = dat[pos,]) # Regression

# -------------------------------------------------------------
# Abschnitt 9.2.5.3, Listing 5: Lasso-Regression
# Datenaufbereitung
#

library(glmnet)
Z <- as.matrix(dat[pos,alleff]) # Kovariatenmatrix
Y <- dat$TWLE[pos] # Abhängige Variable

# -------------------------------------------------------------
# Abschnitt 9.2.5.3, Listing 6: Lasso-Regression
# Bestimmung Teilmengen für Kreuzvalidierung, Lasso-Regression
#

nid <- floor(length(pos)/3) # Teilmengen definieren 
foldid <- rep(c(1:nid),3,length.out=length(pos)) # Zuweisung
lasso.mod2 <- glmnet::cv.glmnet(x=Z,y=Y,alpha = 1, foldid = foldid)

# -------------------------------------------------------------
# Abschnitt 9.2.5.3, Listing 7: Lasso-Regression
# Erwartungswerte der Schulen
#

lasso.pred2 <- predict(lasso.mod2,newx = Z,s="lambda.min")
dat$expTWLE.Lasso2[pos] <- as.vector(lasso.pred2)

# -------------------------------------------------------------
# Abschnitt 9.2.5.3, Listing 8: Lasso-Regression
# Bestimmung R^2
#

varY <- var(dat$TWLE[pos])
varY.lasso.mod2 <- var(dat$expTWLE.Lasso2[pos])
R2.lasso.mod2 <- varY.lasso.mod2/varY

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 9: Nichtparametrische Regression
# Distanzberechnung zur Schule i (Stratum st)
#

N <- length(pos) # Anzahl Schulen im Stratum
schools <- dat$idschool[pos] # Schulen-ID
i <- 1
# Teildatensatz von Schule i
dat.i <- dat[pos[i],c("idschool","TWLE",maineff)]
names(dat.i) <- paste0(names(dat.i),".i")
# Daten der Vergleichsschulen
dat.vgl <- dat[pos[-i],c("idschool","TWLE",maineff)]
index.vgl <- match(dat.vgl$idschool,schools)
# Daten zusammenfuegen
dfr.i <- data.frame("index.i"=i,dat.i,"index.vgl"=index.vgl,
                    dat.vgl, row.names=NULL)
# Distanz zur Schule i
dfr.i$dist <- 0
gi <- c(1,1,1,1)
for(ii in 1:length(maineff)){
  vv <- maineff[ii]
  pair.vv <- grep(vv, names(dfr.i), value=T)
  dist.vv <- gi[ii]*((dfr.i[,pair.vv[1]]-dfr.i[,pair.vv[2]])^2)
  dfr.i$dist <- dfr.i$dist + dist.vv }

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 10: Nichtparametrische Regression
#

# H initiieren
d.dist <- max(dfr.i$dist)-min(dfr.i$dist)
H <- c(seq(d.dist/100,d.dist,length=30),100000)
V1 <- length(H) 
# Anzahl Vergleichsschulen
n <- nrow(dfr.i) 

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 11: Nichtparametrische Regression
# Berechnung der Leave-One-Out-Schätzer der jeweiligen 
# Vergleichsschule k nach h in H
#

sumw <- 0*H # Vektor w_{ik} initiieren, h in H
av <- "TWLE"
dfr0.i <- dfr.i[,c("idschool",av)]
# Schleife ueber alle h-Werte
for (ll in 1:V1 ){
  h <- H[ll]
  # Gewicht w_{ik} bei h
  dfr.i$wgt.h <- dnorm(sqrt(dfr.i$dist), mean=0, sd=sqrt(h))
  # Summe von w_{ik} bei h
  sumw[which(H==h)] <- sum(dfr.i$wgt.h)
  # Leave-one-out-Schätzer von Y_k
  for (k in 1:n){
    # Regressionsformel
    fm <- paste0(av,"~",paste0(maineff,collapse="+"))
    fm <- as.formula(fm)
    # Regressionsanalyse ohne Beitrag von Schule k
    dfr.i0 <- dfr.i[-k,]
    mod.k <- lm(formula=fm,data=dfr.i0,weights=dfr.i0$wgt.h)
    # Erwartungswert anhand Kovariaten der Schule k berechnen
    pred.k <- predict(mod.k, dfr.i)[k]
    dfr0.i[k,paste0( "h_",h) ] <- pred.k
}}
# Erwartungswerte auf Basis verschiedener h-Werte
dfr1 <- data.frame("idschool.i"=dfr.i$idschool.i[1],"h"=H )

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 12: Nichtparametrische Regression
# Berechnung des Kreuzvalidierungskriteriums nach h in H
#

library(kerdiest)
hAL <- ALbw("n",dfr.i$dist) # Plug-in Bandweite
dfr.i$cross.h <- hAL
dfr.i$crosswgt <- dnorm( sqrt(dfr.i$dist), mean=0, sd = sqrt(hAL) ) 
# Kreuzvalidierungskriterium CVh
vh <- grep("h_",colnames(dfr0.i),value=TRUE)
for (ll in 1:V1){
  dfr1[ll,"CVh"] <- sum( (dfr0.i[,av] - dfr0.i[,vh[ll]])^2 * 
                           dfr.i$crosswgt) / n}

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 13: Nichtparametrische Regression
# Bestimmung optimales Wertes von h (h.min)
#

dfr1$min.h.index <- 0
ind <-  which.min( dfr1$CVh )
dfr1$min.h.index[ind ] <- 1
dfr1$h.min <- dfr1$h[ind]

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 14: Nichtparametrische Regression
# Kleinste Quadratsumme der Schätzfehler
#

dfr1$CVhmin <- dfr1[ ind , "CVh" ]

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 15: Nichtparametrische Regression
# Effizienzsteigerung berechnen
#

dfr1$eff_gain <-  100 * ( dfr1[V1,"CVh"] / dfr1$CVhmin[1] - 1 )

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Listing 16: Nichtparametrische Regression
# Durchführung der nichtparametrischen Regression bei h=h.min
#

h <- dfr1$h.min[1]  # h.min
dfr.i$wgt.h <- dnorm(sqrt(dfr.i$dist),sd=sqrt(h))/
  dnorm(0,sd= sqrt(h)) # w_{ik} bei h.min      
dfr.i0 <- dfr.i
# Lokale Regression    
mod.ii <- lm(formula=fm,data=dfr.i0,weights=dfr.i0$wgt.h)
# Kovariaten Schule i
predM <- data.frame(dfr.i[1,paste0(maineff,".i")])    
names(predM) <- maineff
pred.ii <- predict(mod.ii, predM) # Schätzwert Schule i
dat[match(dfr1$idschool.i[1],dat$idschool), "expTWLE.np"] <- pred.ii   

## -------------------------------------------------------------
## Abschnitt 9.2.5, Umsetzung in R, Ergänzung zum Buch
## -------------------------------------------------------------

# Korrelationen zwischen Haupteffekten
cor(dat[,maineff]) # gesamt
# Pro Stratum
for(s in 1:4) print(cor(dat[which(dat$stratum == s),maineff]))

# -------------------------------------------------------------
# Abschnitt 9.2.5.2, Ergänzung zum Buch
# OLS-Regression
#

# Modellgleichung nur mit Haupteffekten
fm.ols1 <- paste0("TWLE ~ ",paste(maineff,collapse=" + "))
fm.ols1 <- as.formula(fm.ols1)

# Modellgleichung mit Haupteffekten ohne zgroesse
fm.ols1a <- paste0("TWLE ~ ",paste(setdiff(maineff,c("zgroesse")),collapse=" + "))
fm.ols1a <- as.formula(fm.ols1a)

# Modellgleichung mit Haupt- und Interaktionseffekten
fm.ols2 <- paste0("TWLE ~ ",paste(alleff,collapse=" + "))
fm.ols2 <- as.formula(fm.ols2)

# Leistungsschätzwerte der 2 OLS-Modelle
dat$expTWLE.OLS2 <- dat$expTWLE.OLS1 <- NA

# Ergebnistabelle über 4 Strata hinweg vorbereiten
tab1 <- data.frame("Variable"=c("(Intercept)",maineff))
tab2 <- data.frame("Variable"=c("(Intercept)",alleff))

# Durchführung: Schleife über vier Strata
for(st in 1:4){
  # st <- 4
  # Position Schulen des Stratums st im Datensatz
  pos <- which(dat$stratum == st)
  
  #---------------------------------
  # OLS-Modell 1
  
  # Durchführung
  ols.mod1 <- lm(formula = fm.ols1,data = dat[pos,])
  ols.mod1a <- lm(formula = fm.ols1a,data = dat[pos,])
  
  # Modellergebnisse anzeigen
  summary(ols.mod1)
  summary(ols.mod1a)
  
  # Erwartungswerte der Schulen 
  dat$expTWLE.OLS1[pos] <- fitted(ols.mod1)
  
  # Ergebnisse in Tabelle speichern
  par <- summary(ols.mod1)
  tab.s <- data.frame(par$coef,R2=par$r.squared,R2.adj=par$adj.r.squared)
  names(tab.s) <- paste0("stratum",st,c("_coef","_SE","_t","_p","_R2","_R2.adj"))
  tab1 <- cbind(tab1, tab.s)
  
  # Durchführung OLS-Modell 2
  ols.mod2 <- lm(formula = fm.ols2,data = dat[pos,])
  
  # Modellergebnisse anzeigen
  summary(ols.mod2)
  
  # Erwartungswerte der Schulen
  dat$expTWLE.OLS2[pos] <- fitted(ols.mod2)
  
  # Ergebnisse in Tabelle speichern
  par <- summary(ols.mod2)
  tab.s <- data.frame(par$coef,R2=par$r.squared,R2.adj=par$adj.r.squared)
  names(tab.s) <- paste0("stratum",st,c("_coef","_SE","_t","_p","_R2","_R2.adj"))
  tab2 <- cbind(tab2, tab.s) 
  
}

# Daten Schule 1196 ansehen
dat[which(dat$idschool == 1196),]

# Schätzwerte nach ols.mod1 und ols.mod2 vergleichen
summary(abs(dat$expTWLE.OLS1 - dat$expTWLE.OLS2))
cor.test(dat$expTWLE.OLS1,dat$expTWLE.OLS2)

# Grafische Darstellung des Vergleich (Schule 1196 rot markiert)
plot(dat$expTWLE.OLS1,dat$expTWLE.OLS2,xlim=c(380,650),ylim=c(380,650),
     col=1*(dat$idschool == 1196)+1,pch=15*(dat$idschool == 1196)+1)
abline(a=0,b=1)

# -------------------------------------------------------------
# Abschnitt 9.2.5.3, Ergänzung zum Buch
# Lasso-Regression
#

library(glmnet)

# Variablen für Erwartungswerte
dat$expTWLE.Lasso2 <- dat$expTWLE.Lasso1 <- NA

# Tabelle für Modellergebnisse
tab3 <- data.frame("Variable"=c("(Intercept)",maineff))
tab4 <- data.frame("Variable"=c("(Intercept)",alleff))

for(st in 1:4){
  # st <- 4
  
  # Position Schulen des Stratums st im Datensatz
  pos <- which(dat$stratum == st)
  
  #------------------------------------------------------------#
  # Lasso-Regression mit den Haupteffekten
  
  # Kovariatenmatrix
  Z <- as.matrix(dat[pos,maineff])
  # Abhängige Variable
  Y <- dat$TWLE[pos]
  
  # Kreuzvalidierung: Teilmengen definieren
  nid <- floor(length(pos)/3)
  # Schulen zu Teilmengen zuordnen
  foldid <- rep(c(1:nid),3,length.out=length(pos))
  
  # Regression
  lasso.mod1 <- cv.glmnet(x=Z,y=Y,alpha = 1, foldid = foldid)
  
  # Ergebnisse ansehen
  print(lasso.mod1)
  
  # Lasso-Koeffizienten bei lambda.min
  print(lasso.beta <- coef(lasso.mod1,s="lambda.min"))
  
  # Erwartungswerte der Schulen
  lasso.pred1 <- predict(lasso.mod1,newx = Z,s="lambda.min")
  dat$expTWLE.Lasso1[pos] <- as.vector(lasso.pred1)
  
  # R2 bestimmen
  varY <- var(dat$TWLE[pos])
  varY.lasso.mod1 <- var(dat$expTWLE.Lasso1[pos])
  print(R2.lasso.mod1 <- varY.lasso.mod1/varY)
  
  # Ergebnistabelle
  vv <- paste0("coef.stratum",st); tab3[,vv] <- NA
  tab3[lasso.beta@i+1,vv] <- lasso.beta@x
  vv <- paste0("lambda.stratum",st); tab3[,vv] <- lasso.mod1$lambda.min
  vv <- paste0("R2.stratum",st); tab3[,vv] <- R2.lasso.mod1
  
  #------------------------------------------------------------#
  # Lasso-Regression mit Haupt- und Interaktionseffekten
  
  # Kovariatenmatrix
  Z <- as.matrix(dat[pos,alleff])
  
  # Regression
  lasso.mod2 <- cv.glmnet(x=Z,y=Y,alpha = 1, foldid = foldid)
  
  # Ergebnisausdruck
  print(lasso.mod2)
  
  # Lasso-Koeffizienten bei lambda.min
  print(lasso.beta <- coef(lasso.mod2,s="lambda.min"))
  
  # Erwartungswerte der Schulen
  lasso.pred2 <- predict(lasso.mod2,newx = Z,s="lambda.min")
  dat$expTWLE.Lasso2[pos] <- as.vector(lasso.pred2)
  
  # R2 bestimmen
  varY.lasso.mod2 <- var(dat$expTWLE.Lasso2[pos])
  R2.lasso.mod2 <- varY.lasso.mod2/varY
  R2.lasso.mod2
  
  # Ergebnistabelle
  vv <- paste0("coef.stratum",st); tab4[,vv] <- NA
  tab4[lasso.beta@i+1,vv] <- lasso.beta@x
  vv <- paste0("lambda.stratum",st); tab4[,vv] <- lasso.mod2$lambda.min
  vv <- paste0("R2.stratum",st); tab4[,vv] <- R2.lasso.mod2
  
  
}

# Regressionresiduen = Schätzung von SChul- und Unterrichtseffekt
dat$resTWLE.Lasso1 <- dat$TWLE - dat$expTWLE.Lasso1
dat$resTWLE.Lasso2 <- dat$TWLE - dat$expTWLE.Lasso2

# -------------------------------------------------------------
# Abschnitt 9.2.5.4, Ergänzung zum Buch
# Nichtparametrische Regression
#

av <- "TWLE" # Abhängige Variable
dfr3 <- NULL # Ergebnistabelle

# Variable für Leistungsschätzwerte
dat$expTWLE.np <- NA

# Schleife über 4 Strata
for(st in 1:4){
  # st <- 1
  pos <- which(dat$stratum == st)
  N <- length(pos)
  schools <- dat$idschool[pos]
  
  ###
  # Distanzmatrix dfr für alle Schulen im Stratum erstellen
  dfr <- NULL
  
  for (i in 1:N){
    # i <- 1
    # Teildatensatz von Schule i
    dat.i <- dat[pos[i],c("idschool","TWLE",maineff)]
    # Daten der Vergleichsgruppe
    dat.vgl <- dat[pos[-i],c("idschool","TWLE",maineff)]
    # Variablennamen von dat.vgl umbenennen
    # names(dat.vgl) <- paste0("vgl.",names(dat.vgl))
    # Variablennamen von dat.i umbenennen
    names(dat.i) <- paste0(names(dat.i),".i")
    
    # Daten zusammenfuegen
    index.vgl <- match(dat.vgl$idschool,schools)
    dfr.i <- data.frame("index.i"=i,dat.i,
                        "index.vgl"=index.vgl,dat.vgl,
                        row.names=NULL)
    
    # Distanz zur i
    dfr.i$dist <- 0
    gi <- c(1,1,1,1)
    for(ii in 1:length(maineff)){
      vv <- maineff[ii]
      pair.vv <- grep(vv, names(dfr.i), value=T)
      dist.vv <- gi[ii]*((dfr.i[,pair.vv[1]]-dfr.i[,pair.vv[2]])^2)
      dfr.i$dist <- dfr.i$dist + dist.vv
    }
    
    print(i) ; flush.console()
    dfr <- rbind( dfr , dfr.i )
  }
  
  dfr1 <- index.dataframe( dfr , systime=TRUE )
  
  ###
  # h-Auswahl und Nichtparametrische Regression pro Schule i
  dfr1.list <- list()
  for (i in 1:N){
    # i <- 1
    dfr.i <- dfr[ dfr$index.i == i , ]
    n <- nrow(dfr.i)
    
    # Startwertliste fuer h initiieren
    d.dist <- max(dfr.i$dist)-min(dfr.i$dist)
    H <- c(seq(d.dist/100,d.dist,length=30),100000)
    V1 <- length(H) # Anzahl der Startwerte in H
    
    # Startwerte: Summe von w_ik
    sumw <- 0*H
    dfr0.i <- dfr.i[,c("idschool",av)]
    # Schleife ueber alle h-Werte
    for (ll in 1:V1 ){
      h <- H[ll]
      # Gewicht w_ik bei h
      dfr.i$wgt.h <- dnorm(sqrt(dfr.i$dist), mean=0, sd=sqrt(h))
      # Summe von w_ik bei h
      sumw[which(H==h)] <- sum(dfr.i$wgt.h)
      # Leave-one-out-Schätzer von Y_k
      for (k in 1:n){
        # Regressionsformel
        fm <- paste0(av,"~",paste0(maineff,collapse="+"))
        fm <- as.formula(fm)
        # Regressionsanalyse ohne Beitrag von Schule k
        dfr.i0 <- dfr.i[-k,]
        mod.k <- lm(formula=fm,data=dfr.i0,weights=dfr.i0$wgt.h)
        # Erwartungswert anhand Kovariaten der Schule k berechnen
        pred.k <- predict(mod.k, dfr.i)[k]
        dfr0.i[k,paste0( "h_",h) ] <- pred.k
      }
      print(paste0("i=",i,", h_",ll))
    }
    # Erwartungswerte auf Basis verschiedener h-Werte
    dfr1 <- data.frame("idschool.i"=dfr.i$idschool.i[1],"h"=H )
    
    # Berechnung des Kreuzvalidierungskriteriums
    library(kerdiest)
    hAL <- ALbw("n",dfr.i$dist) # Plug-in Bandbreite nach Altman ud Leger
    name <- paste0( "bandwidth_choice_school" , dfr.i$idschool.i[1] ,  "_cross.h_" , round2(hAL,1) )
    # Regressionsgewichte auf Basis cross.h
    dfr.i$cross.h <- hAL
    dfr.i$crosswgt <- dnorm( sqrt(dfr.i$dist), mean=0, sd = sqrt(hAL) ) 
    
    dfr.i <- index.dataframe( dfr.i , systime=TRUE )

    # Kreuzvalidierungskriterium CVh
    vh <- grep("h_",colnames(dfr0.i),value=TRUE)
    for (ll in 1:V1){
      # ll <- 5
      dfr1[ll,"CVh"] <- sum( (dfr0.i[,av] - dfr0.i[,vh[ll]])^2 * 
                               dfr.i$crosswgt) / n
      print(ll)
    }
    
    # Bestimmung h.min
    dfr1$min.h.index <- 0
    ind <-  which.min( dfr1$CVh )
    dfr1$min.h.index[ind ] <- 1
    dfr1$h.min <- dfr1$h[ind]
    # Kleinste Quadratsumme der Schätzfehler
    dfr1$CVhmin <- dfr1[ ind , "CVh" ]
    
    # Effizienzsteigerung berechnen
    dfr1$eff_gain <-  100 * ( dfr1[V1,"CVh"] / dfr1$CVhmin[1] - 1 )
    
    # h auswählen
    h <- dfr1$h.min[1]
    
    # Gewichte anhand h berechnen
    dfr.i$wgt.h <- dnorm( sqrt( dfr.i$dist ) , sd = sqrt( h) ) / dnorm( 0 , sd = sqrt( h) )     
    dfr.i0 <- dfr.i
    mod.ii <- lm(formula = fm,data = dfr.i0,weights = dfr.i0$wgt.h)
    
    # Leistungsschätzwerte berechnen
    predM <- data.frame(dfr.i[1,paste0(maineff,".i")])
    names(predM) <- maineff
    
    pred.ii <- predict( mod.ii ,  predM )
    dfr1$fitted_np <- pred.ii  
    dfr1$h.min_sumwgt <- sum( dfr.i0$wgt.h )
    dfr1$h_sumwgt  <- sumw
    
    # Leistungsschätzwerte zum Datensatz hinzufügen
    dat$expTWLE.np[match(dfr1$idschool.i[1],dat$idschool)] <- pred.ii
    dfr1.list[[i]] <- dfr1
  }
  
  ###
  # Ergebnisse im Stratum st zusammenfassen
  dfr2 <- NULL

  for(i in 1:length(dfr1.list)){
    dat.ff <- dfr1.list[[i]]
    dfr2.ff <- dat.ff[1,c("idschool.i","h.min","fitted_np","h.min_sumwgt",
                          "CVhmin","eff_gain")]
    dfr2.ff$CVlinreg <- dat.ff[V1,"CVh"]
    names(dfr2.ff) <- c("idschool","h.min","fitted_np","h.min_sumwgt",
                        "CVhmin","eff_gain","CVlinreg")
    dfr2 <- rbind(dfr2, dfr2.ff)
    print(i)
  }
  
  #---------------------------------------------------##
  # R2 berechnen
  varY <- var(dat$TWLE[pos])
  varY.np <- var(dat$expTWLE.np[pos])
  dfr2$R2.np <- varY.np/varY
  
  #---------------------------------------------------##
  # Zur Gesamtergebnistabelle
  dfr3 <- rbind(dfr3,cbind("Stratum"=st,dfr2))
  
}

# Effizienz der NP-Regression gegenüber OLS-Regression
summary(dfr3$eff_gain)
table(dfr3$eff_gain > 5)
table(dfr3$eff_gain > 10)
table(dfr3$eff_gain > 20)

# Regressionsresiduen
dat$resTWLE.np <- dat$TWLE - dat$expTWLE.np

## -------------------------------------------------------------
## Abschnitt 9.2.6, Ergebnisse im Vergleich, Ergänzung zum Buch
## -------------------------------------------------------------

# Output-Variablen
out <- grep("expTWLE",names(dat),value=T)
lt <- length(out)

# Korrelationsmatrix
tab <- tab1 <- as.matrix(round2(cor(dat[,out]),3))

# Varianzmatrix
tab2 <- as.matrix(round2(sqrt(var(dat[,out])),1))

tab3 <- matrix(NA,lt,lt)
# Differenzmatrix
for(ii in 1:(lt-1))
  for(jj in (ii+1):lt) tab3[ii,jj] <- round2(mean(abs(dat[,out[jj]]-dat[,out[ii]])),1)

tab4 <- matrix(NA,lt,lt)
# Differenzmatrix
for(ii in 1:(lt-1))
  for(jj in (ii+1):lt) tab4[ii,jj] <- round2(sd(abs(dat[,out[jj]]-dat[,out[ii]])),1)

# Ergebnistabelle
diag(tab) <- diag(tab2)
tab[upper.tri(tab)] <- tab3[upper.tri(tab3)]

# R2 Gesamt
varY <- var(dat$TWLE)
varexp.OLS1 <- var(dat$expTWLE.OLS1); R2.OLS1 <- varexp.OLS1/varY
varexp.OLS2 <- var(dat$expTWLE.OLS2); R2.OLS2 <- varexp.OLS2/varY
varexp.Lasso1 <- var(dat$expTWLE.Lasso1); R2.Lasso1 <- varexp.Lasso1/varY
varexp.Lasso2 <- var(dat$expTWLE.Lasso2); R2.Lasso2 <- varexp.Lasso2/varY
varexp.np <- var(dat$expTWLE.np); R2.np <- varexp.np/varY
R2 <- c(R2.OLS1,R2.OLS2,R2.Lasso1,R2.Lasso2,R2.np)
tab <- cbind(tab,R2)

# R2 pro Stratum
dat0 <- dat
for(st in 1:4){
  # st <- 1
  dat <- dat0[which(dat0$stratum == st),]
  varY <- var(dat$TWLE)
  varexp.OLS1 <- var(dat$expTWLE.OLS1); R2.OLS1 <- varexp.OLS1/varY
  varexp.OLS2 <- var(dat$expTWLE.OLS2); R2.OLS2 <- varexp.OLS2/varY
  varexp.Lasso1 <- var(dat$expTWLE.Lasso1); R2.Lasso1 <- varexp.Lasso1/varY
  varexp.Lasso2 <- var(dat$expTWLE.Lasso2); R2.Lasso2 <- varexp.Lasso2/varY
  varexp.np <- var(dat$expTWLE.np); R2.np <- varexp.np/varY
  R2 <- c(R2.OLS1,R2.OLS2,R2.Lasso1,R2.Lasso2,R2.np)
  tab <- cbind(tab,R2)
}

colnames(tab)[7:10] <- paste0("R2_stratum",1:4)

## -------------------------------------------------------------
## Abschnitt 9.2.7, Berücksichtigung der Schätzfehler
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 9.2.7, Listing 17: Bestimmung des Erwartungsbereichs
#

vv <- "expTWLE.OLS1" # Variablenname
mm <- "OLS1" # Kurzname des Modells
dfr <- NULL # Ergebnistabelle
# Schleife über alle möglichen Breite von 10 bis 60
for(w in 10:60){
  # Variablen für Ergebnisse pro w
  var <- paste0(mm,".pos.eb",w) # Position der Schule
  var.low <- paste0(mm,".eblow",w) # Untere Grenze des EBs
  var.upp <- paste0(mm,".ebupp",w) # Obere Grenze des EBs
  # Berechnen
  dat[,var.low] <- dat[,vv]-w/2 # Untere Grenze des EBs
  dat[,var.upp] <- dat[,vv]+w/2 # Obere Grenze des EBs 
  # Position: -1=unterhalb, 0=innerhalb, 1=oberhalb des EBs 
  dat[,var] <- -1*(dat$TWLE < dat[,var.low]) + 1*(dat$TWLE > dat[,var.upp])
  # Verteilung der Schulpositionen
  tmp <- data.frame(t(matrix(prop.table(table(dat[,var])))))
  names(tmp) <- c("unterhalb","innerhalb","oberhalb")
  tmp <- data.frame("ModellxBereich"=var,tmp); dfr <- rbind(dfr,tmp) }

# Abweichung zur Wunschverteilung 25-50-25 
dfr1 <- dfr 
dfr1[,c(2,4)] <- (dfr1[,c(2,4)] - .25)^2 
dfr1[,3] <- (dfr1[,3] - .5)^2 
dfr1$sumquare <- rowSums(dfr1[,-1]) 
# Auswahl markieren 
dfr$Auswahl <- 1*(dfr1$sumquare == min(dfr1$sumquare) )

# -------------------------------------------------------------
# Abschnitt 9.2.7, Bestimmung des Erwartungsbereichs
# Ergänzung zum Buch

dat <- dat0
# Liste der Erwartungswerte-Variablen
exp.vars <- grep("expTWLE",names(dat),value=T)
# Modellnamen
m.vars <- gsub("expTWLE\\.","",exp.vars)

# Liste der Ergebnistabelle
list0 <- list()

# Ergebnisse
tab.erg <- NULL

# Schleife über alle Erwartungswerte aller Modelle
for(ii in 1:length(exp.vars)){
  # ii <- 1
  vv <- exp.vars[ii]
  mm <- m.vars[ii]
  
  # Ergebnistabelle
  dfr <- NULL
  
  # Schleife über alle möglichen Breite von 10 bis 60
  for(w in 10:60){
    # eb <- 10
    var <- paste0(mm,".pos.eb",w) # Position der Schule
    var.low <- paste0(mm,".eblow",w) # Untere Grenze des EBs
    var.upp <- paste0(mm,".ebupp",w) # Obere Grenze des EBs
    # Untere Grenze des EBs = Erwartungswert - w/2
    dat[,var.low] <- dat[,vv]-w/2
    # Obere Grenze des EBs = Erwartungswert + w/2
    dat[,var.upp] <- dat[,vv]+w/2
    # Position der Schule bestimmen
    # -1 = unterhalb, 0 = innterhalb, 1 = oberhalb des EBs
    dat[,var] <- -1*(dat$TWLE < dat[,var.low]) + 1*(dat$TWLE > dat[,var.upp])
    # Verteilung der Positionen
    tmp <- data.frame(t(matrix(prop.table(table(dat[,var])))))
    names(tmp) <- c("unterhalb","innerhalb","oberhalb")
    tmp <- data.frame("ModellxBereich"=var,tmp)
    dfr <- rbind(dfr,tmp)
  }
  
  # Vergleich mit Wunschverteilung 25-50-25
  dfr1 <- dfr
  dfr1[,c(2,4)] <- (dfr1[,c(2,4)] - .25)^2
  dfr1[,3] <- (dfr1[,3] - .5)^2
  dfr1$sumquare <- rowSums(dfr1[,-1])
  # Auswahl markieren
  dfr$Auswahl <- 1*(dfr1$sumquare == min(dfr1$sumquare) )
  
  # Zum Liste hinzufügen
  list0[[ii]] <- dfr
  print(dfr[which(dfr$Auswahl == 1),])
  tab.erg <- rbind(tab.erg, dfr[which(dfr$Auswahl == 1),])
  
}

# Nur gewählte Ergebnisse im Datensatz beibehalten
all.vars <- grep("eb",names(dat),value=T)
# Untere und Obere Grenze mit speichern
eb.vars <- tab.erg[,1]
low.vars <- gsub("pos.eb","eblow",eb.vars)
upp.vars <- gsub("pos.eb","ebupp",eb.vars)
del.vars <- setdiff(all.vars, c(eb.vars,low.vars,upp.vars))
dat <- dat[,-match(del.vars,names(dat))]


## -------------------------------------------------------------
## Appendix: Abbildungen
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abbildung 9.4
#

# Koeffizienten bei der ersten 50 lambdas ausdrucken
# Stratum 4

lambda <- lasso.mod2$lambda[1:50]
a <- round2(lambda,2)
a1 <- a[order(a)]
L <- length(a)

dfr <- NULL

for(ll in 1:L){
  dfr.ll <- as.matrix(coef(lasso.mod2,newx = Z,s=a[ll] ))
  colnames(dfr.ll) <- paste0("a_",ll)
  dfr.ll <- data.frame("coef"=rownames(dfr.ll),dfr.ll)
  rownames(dfr.ll) <- NULL
  if(ll == 1) dfr <- dfr.ll else dfr <- merge(dfr, dfr.ll)
}

# Ohne Intercept
dfr <- dfr[-1,]
rownames(dfr) <- 1:nrow(dfr)

cl <- colors()
cl <- grep("grey",cl,value=T)

# Umgekehrte Reihenfolge
dfr1 <- dfr
for(x in 2:(L+1)) {dfr1[,x] <- dfr[,(L+3)-x]; 
names(dfr1)[x] <- names(dfr)[(L+3)-x]}

###
plot(x = log(a), y = rep(0,L), xlim = rev(range(log(a))), ylim=c(-20,22), type = "l", 
     xaxt ="n",
     xlab = expression(paste(lambda)), ylab="Geschätzte Regressionskoeffizienten")
axis(1, at=log(a), labels=a,cex=1)

tmp <- nrow(dfr)
for(ll in 1:tmp){
  # ll <- 1
  lines(x=log(a),y=dfr[ll,2:(L+1)],type="l",pch=15-ll,col=cl[15-ll])
  points(x=log(a),y=dfr[ll,2:(L+1)],type="p",pch=15-ll)
  legend(x=2.8-0.7*(ll>tmp/2),y=25-2*(ifelse(ll>7,ll-7,ll)),
         legend =dfr$coef[ll],pch=15-ll,bty="n",cex=0.9)
}

# Kennzeichung der gewählten lambda
v <- log(lasso.mod2$lambda.min)
lab2 <- expression(paste("ausgewähltes ",lambda," = .43"))
text(x=v+0.6,y=-8,labels=lab2)

abline(v = v,lty=2,cex=1.2)

# -------------------------------------------------------------
# Abbildung 9.5
# Auswahl Lambda anhand min(cvm)
#

xlab <- expression(paste(lambda))
plot(lasso.mod2, xlim = rev(range(log(lambda))), 
     ylim=c(550,1300),xlab=xlab,xaxt ="n",
     ylab = "Mittleres Fehlerquadrat der Kreuzvalidierung (cvm)",
     font.main=1,cex.main=1)
axis(1, at=log(a), labels=a,cex=1)

lab1 <- expression(paste(lambda," bei min(cvm)"))
text(x=log(lasso.mod2$lambda.min)+0.5,y=max(lasso.mod2$cvm)-50,
     labels=lab1,cex=1)

lab2 <- expression(paste("(ausgewähltes ",lambda," = .43)"))
text(x=log(lasso.mod2$lambda.min)+0.6,y=max(lasso.mod2$cvm)-100,
     labels=lab2,cex=1)

abline(v = log(lasso.mod2$lambda.min),lty=2)

text(x=log(lasso.mod2$lambda.min)-0.3,y = min(lasso.mod2$cvm)-30,
     labels="min(cvm)",cex=1 )
abline(h = min(lasso.mod2$cvm),lty=2)

text <- expression(paste("Anzahl der Nicht-null-Koeffizienten (",lambda," entsprechend)"))
mtext(text=text,side=3,line=3)


# -------------------------------------------------------------
# Abbildung 9.6
# Rohwert-Schätzwert Schule 1196 & 1217 im Vergleich
#

id <- c(1196, 1217)
par(mai=c(1.2,3,1,.5))
plot(x=rep(NA,2),y=c(1:2),xlim=c(470,610),yaxt ="n",type="l",
     xlab="Erwartungswerte je nach Modell und Schulleistung",ylab="")
legend <- c("Schulleistung (TWLE)",paste0("",
                                          c("OLS1","OLS2","Lasso1","Lasso2","NP"),
                                          "-Modell"))
axis(2, at=c(seq(1,1.4,0.08),seq(1.6,2,0.08)), las=1,cex=0.7,
     labels=rep(legend,2))
text <- paste0("Schule ",id)
mtext(text=text,side=2,at = c(1.2,1.8),line = 10)

exp.vars <- c("TWLE", 
              paste0("expTWLE.", c("OLS1","OLS2","Lasso1","Lasso2","np")))

pch = c(19, 0,3,2,4,5)
ii <- 1
col = c("grey", rep("lightgrey",5))
for(vv in exp.vars){
  # vv <- "TWLE"
  x <- dat0[which(dat0$idschool \%in\% id),vv]
  abline(h = c(0.92+ii*0.08,1.52+ii*0.08), lty=1+1*(ii>1),col=col[ii])
  points(x=x,y=c(0.92+ii*0.08,1.52+ii*0.08),type="p",pch=pch[ii])
  ii <- ii + 1
}
}%end dontrun
}