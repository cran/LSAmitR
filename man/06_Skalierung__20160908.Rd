%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel  6}
\alias{Kapitel 6}
\encoding{UTF-8}
\title{Kapitel 6: Skalierung und Linking}

\description{
Das ist die Nutzerseite zum Kapitel 6, \emph{Skalierung und Linking}, im 
Herausgeberband Large-Scale Assessment mit \R: Methodische 
Grundlagen der österreichischen Bildungsstandardüberprüfung. 
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. 
Im Abschnitt \strong{Examples} werden die \R-Syntaxen des Kapitels vollständig 
wiedergegeben und gegebenfalls erweitert.
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Trendtel, M., Pham, G. & Yanagida, T. (2016). Skalierung und Linking.
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 185--224). Wien: facultas.
}

% \note{
% % wenn es Hinweise gibt
% }

\author{
  Matthias Trendtel, Giang Pham, Takuya Yanagida
}

\seealso{
%Verweis zu den Daten im Kapitel
Zu \code{\link{datenKapitel06}}, den im Kapitel verwendeten Daten.\cr
%Verweis zum nächsten und zum vorigen Kapitel
Zurück zu \code{\link{Kapitel 5}}, Testdesign.\cr
Zu \code{\link{Kapitel 7}}, Statistische Analysen produktiver Kompetenzen.\cr
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
library(TAM)
library(sirt)
library(WrightMap)
library(miceadds)
library(plyr)
set.seed(20150528)

dat <- data(datenKapitel06)
# Hauptstudie
dat <- datenKapitel06$dat
ue <- datenKapitel06$itembank
items <- grep("I", colnames(dat), value=TRUE)

# Nur TH1
datTH1 <- datenKapitel06$datTH1
ueTH1 <- datenKapitel06$itembankTH1
rownames(ueTH1) <- ueTH1$Item
itemsTH1 <- grep("I", colnames(datTH1), value=TRUE)
respTH1 <- datTH1[, -(1:4)]; wTH1 <- datTH1$wgtstud

# Normierungsstudie
normdat <- datenKapitel06$normdat

\dontrun{
## -------------------------------------------------------------
## Abschnitt 6.3.4 Das Partial Credit Model (PCM)
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.3.4, Listing 1: Leistungsdaten und Stich-
#                             probengewichte Objekten zuweisen
#

resp <- dat[, grep("I", colnames(dat))]; w <- dat$wgtstud

# -------------------------------------------------------------
# Abschnitt 6.3.4, Listing 2: Anpassen eines PCMs
#

mod.1PL <- tam.mml(resp = resp, irtmodel = "1PL", pweights = w)

# -------------------------------------------------------------
# Abschnitt 6.3.4, Listing 2a: Ergänzung zum Buch
# Runden zur besseren Darstellung im Buch
#

mod.1PL$item$M <- round(mod.1PL$item$M, 2)

# -------------------------------------------------------------
# Abschnitt 6.3.4, Listing 3: Darstellung des letzen Items
#

tail(mod.1PL$item, 1)

# -------------------------------------------------------------
# Abschnitt 6.3.4, Listing 4: Umparametrisierung
#

b_ih <- mod.1PL$item[, grep("AXsi_", colnames(mod.1PL$item))]
delta.tau <- pcm.conversion(b_ih)

# -------------------------------------------------------------
# Abschnitt 6.3.4, Listing 5: Berechnung der Thursonian 
#                             Threshods und Lokations Indizes
#

thurst.thres <- IRT.threshold(mod.1PL)
LI <- IRT.threshold(mod.1PL, type="item")


## -------------------------------------------------------------
## Abschnitt 6.3.5 Itemtrennschärfen polytomer Items und
##                 Rateparameter
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.3.5, Listing 1: Anpassen eines Generalized
#                             Partial Credit Models
# 

mod.GPCM <- tam.mml.2pl(resp, irtmodel = "GPCM", pweights = w)

# -------------------------------------------------------------
# Abschnitt 6.3.5, Listing 2: Anpassen eines 
#                             Nominal Item Response Models
# 

mod.NIRM <- tam.mml.2pl(resp, irtmodel="2PL", pweights = w)

# -------------------------------------------------------------
# Abschnitt 6.3.5, Listing 3: Anpassen eines Generalized 
#                             Partial Credit Models mit festen 
#                             Itemgewichten (Trennschärfen)
# 

tammodel <- "
  LAVAAN MODEL:
  F =~ a1__a50*I1__I50;
  # Trait-Varianz auf 1 fixieren
  F ~~ 1*F
  MODEL CONSTRAINT:
  # Gewichtung für die Items festlegen
  a1__a40 == 1*a # dichotome Items
  a41__a44 == .3333*a # T/F Items mit max. Score von 3
  a45__a50 == .25*a # M56 Items mit max. Score von 4
  " 
mod.GPCMr <- tamaan(tammodel, resp, pweights = w)

# -------------------------------------------------------------
# Abschnitt 6.3.5, Listing 4: Itemtrennschärfevergleich
# 

## Itemparameter im Vergleich
rbind(GPCM = mod.GPCM$item[50, 9:12], 
      NIRM = mod.NIRM$item[50, 9:12],
      GPCMr = mod.GPCMr$item[50, 10:13]) / rep(c(1:4), each=3)

# -------------------------------------------------------------
# Abschnitt 6.3.5, Listing 5: Itemtrennschärfen eines 
#                             dichotomen und eines polytomen 
#                             Items

rbind(I40 = mod.GPCMr$item[40, 10:13],
      I50 = mod.GPCMr$item[50, 10:13])

# -------------------------------------------------------------
# Abschnitt 6.3.5, Listing 6: Anpassen eines 1PL-G Modells
#


## Das 1PL-G Modell
tammodel <- "
  LAVAAN MODEL:
  F =~ 1*I1__I50
  F ~~ F
  # Rateparameter für MC4 Items
  I1__I10 ?= gMC4*g1
  # Rateparameter für MC3 Items
  I11__I20 + I31__I40 ?= gMC3*g1
  "
mod.1PL_G <- tamaan(tammodel, resp, pweights = w, 
                    control = list(Msteps = 15))

# -------------------------------------------------------------
# Abschnitt 6.3.5, Listing 7: Ausgabe geschätzter Rateparameter
#                             für MC3 und MC4 Items
#

mod.1PL_G$item[c(10,11), c(1,4,5)]


## -------------------------------------------------------------
## Abschnitt 6.3.6 Bookleteffekte
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.3.6, Listing 1: Anpassen eines Bookletmodells
# 

mod.1PL_Book <- tam.mml.mfr(resp, facets = cbind(th = dat$th), 
                 formulaA= ~ item + item:step + th, pweights = w)

# -------------------------------------------------------------
# Abschnitt 6.3.6, Listing 2: Ausgabe der Bookleteffekte der einzelnen
#                             Testhefte
# 

rbind((tmp <- mod.1PL_Book$xsi[paste0("thER0", 1:5),]), 
      thER06 = - c(sum(tmp[,1]), NA))


## -------------------------------------------------------------
## Abschnitt 6.3.7 Personenfähigkeitsschätzer
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.3.7, Listing 1: WLEs
# 

WLE.1PL <- as.data.frame(tam.wle(mod.1PL))
round(head(WLE.1PL, 2), 4)

# -------------------------------------------------------------
# Abschnitt 6.3.7, Listing 2: WLE Reliabilität
# 

WLErel(WLE.1PL$theta, WLE.1PL$error, w)

# -------------------------------------------------------------
# Abschnitt 6.3.7, Listing 3: EAPs
# 

round(head(mod.1PL$person, 2), 4)

# -------------------------------------------------------------
# Abschnitt 6.3.7, Listing 4: EAP Reliabilität
# 

EAPrel(mod.1PL$person$EAP, mod.1PL$person$SD.EAP, w)

# -------------------------------------------------------------
# Abschnitt 6.3.7, Listing 4a: Ergänzung zum Buch
# Alternative Berechnung der EAP-Reliabilität
#

1 - weighted.mean(mod.1PL$person$SD.EAP^2, w)/mod.1PL$variance


# -------------------------------------------------------------
# Abschnitt 6.3.7, Listing 5: PVs
# 

PV.1PL <- tam.pv(mod.1PL)$pv
round(head(PV.1PL, 2), 4)

# -------------------------------------------------------------
# Abschnitt 6.3.7, Listing 6: Statistische Kennwerte der einzelnen
#                             Personenfähigkeitsschätzer
# 

cbind(WLEs = c(M = weighted.mean(WLE.1PL$theta, w),
               SD = weighted_sd(WLE.1PL$theta, w)),
      EAPs = c(M = weighted.mean(mod.1PL$person$EAP, w),
               SD = weighted_sd(mod.1PL$person$EAP, w)),
      PVs = c(M = mean(apply(PV.1PL[, -1], 2, weighted.mean, w)),
              SD=mean(apply(PV.1PL[, -1], 2, weighted_sd, w))))


## -------------------------------------------------------------
## Abschnitt 6.3.8 Mehrdimensionale Modelle
## -------------------------------------------------------------

# Achtung: Algorithmen benötigen einige Zeit
# Zur schnelleren Konvergenz werden nur Daten aus Testheft 1 verwendet

# -------------------------------------------------------------
# Abschnitt 6.3.8, Listing 1: Verteilung der Items auf Foki 
# 

table(paste("Fokus", ue$focus[ue$Item \%in\% colnames(datTH1)]))
table(paste("Fokus", ueTH1$focus))

# -------------------------------------------------------------
# Abschnitt 6.3.8, Listing 2: Spezifizierung der Q-Matrix und 
#                             Anpassung des Modells
#                             Achtung: Schätzung benötigt > 300 Iterationen
# 

Q <- array(0, c(25, 5), list(items[items \%in\% colnames(datTH1)]))
for(i in 1:25) Q[i, ueTH1$focus[i] + 1] <- 1
mod.1PL_multi <- tam(resp = respTH1, pweights = wTH1,
                     Q = Q, control = list(snodes = 1500))

# -------------------------------------------------------------
# Abschnitt 6.3.8, Listing 3: Anpassen eines Bifaktormodells
#                             Achtung: Schätzung benötigt > 350 Iterationen
# 

mod.1PL_bi <- tam.fa(respTH1, irtmodel = "bifactor1", 
                dims = ueTH1$format, pweights = wTH1, 
                control = list(snodes = 1500))

# -------------------------------------------------------------
# Abschnitt 6.3.8, Listing 4: Darstellung der Varianzen des 
#                             Hauptfaktors und der Störfaktoren
# 

nams <- c("I26", "I45", "I12", "I1", "I41")
dfr <- data.frame(mod.1PL_bi$B.stand[nams,],
                  row.names=ueTH1[nams, "format"])
dfr

# -------------------------------------------------------------
# Abschnitt 6.3.8, Listing 5: Darstellung der Reliabilitätsschätzer
# 

mod.1PL_bi$meas


## -------------------------------------------------------------
## Abschnitt 6.3.9 Modellpassung
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.3.9, Listing 1: Berechnung und Darstellungen von 
#                             Itemfitstatistiken
# 

itemfit <- tam.fit(mod.1PL)
summary(itemfit)

# -------------------------------------------------------------
# Abschnitt 6.3.9, Listing 2: Berechnung und Darstellungen von 
#                             Modellfitstatistiken
# 

modfit <- tam.modelfit(mod.1PL)
modfit$fitstat

# -------------------------------------------------------------
# Abschnitt 6.3.9, Listing 3: LRT für Modelltestung
# 

anova(mod.1PL, mod.GPCM)


## -------------------------------------------------------------
## Abschnitt 6.4.1 Simultane Kalibrierung
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.4.1, Listing 1: Daten vorbereiten
#

vars <- c("idstud", "wgtstud", "th")
# Daten der Hauptstudie
tmp1 <- cbind("Hauptstudie" = 1, dat[,c(vars, items)])
# Daten der Normierungsstudie
n.items <- grep("I|J",names(normdat),value=T)
tmp2 <- cbind("Hauptstudie" = 0, normdat[, c(vars, n.items)])
# Schülergewichte der Normierungsstudie sind konstant 1
# Datensätze zusammenfügen
dat.g <- rbind.fill(tmp1,tmp2)
all.items <- grep("I|J",names(dat.g),value=T)

# -------------------------------------------------------------
# Abschnitt 6.4.1, Listing 2: Simultane Kalibrierung
#                             Achtung: Schätzung benötigt > 450 Iterationen
#

# 2-Gruppenmodell
linkmod1 <-  tam.mml(resp=dat.g[, all.items], pid=dat.g[, 2], 
              group = dat.g$Hauptstudie, pweights=dat.g$wgtstud)
summary(linkmod1)

# -------------------------------------------------------------
# Abschnitt 6.4.1, Listing 2a: Ergänzung zum Buch
# Berechnung von Verteilungsparametern
#

set.seed(20160828)

# PVs
PV_linkmod1 <- tam.pv(linkmod1, nplausible = 20)

# Personendatensatz
dfr_linkmod1 <- linkmod1$person
dfr_linkmod1 <- merge( x = dfr_linkmod1, y = PV_linkmod1$pv, by = "pid" , all=T)
dfr_linkmod1 <- dfr_linkmod1[ order(dfr_linkmod1$case) , ]

# Leistungsskala transformieren
vars.pv <- grep("PV",names(dfr_linkmod1),value=T)
# Mittlere Fähigkeit der Normierungsgruppe
p0 <- which(dat.g$Hauptstudie == 0)
M_PV <- mean(apply(dfr_linkmod1[p0,vars.pv],2,Hmisc::wtd.mean,
                   weights = dfr_linkmod1[p0,"pweight"]))
SD_PV <- mean(sqrt(apply(dfr_linkmod1[p0,vars.pv],2,Hmisc::wtd.var,
                         weights = dfr_linkmod1[p0,"pweight"])))
# Tranformationsparameter
a <- 100/SD_PV; b <- 500 - a*M_PV

# Verteilungsparameter der Hauptstudie
p1 <- which(dat.g$Hauptstudie == 1)
M1_PV <- mean(apply(dfr_linkmod1[p1,vars.pv],2,Hmisc::wtd.mean,
                    weights = dfr_linkmod1[p1,"pweight"]))
SD1_PV <- mean(sqrt(apply(dfr_linkmod1[p1,vars.pv],2,Hmisc::wtd.var,
                          weights = dfr_linkmod1[p1,"pweight"])))
TM_PV <- M1_PV*a + b; TSD_PV <- SD1_PV*a

# Ergebnisse
trafo_linkmod1 <- data.frame(M_Norm = 500, SD_Norm = 100, a = a, b = b,
                             M = TM_PV, SD = TSD_PV)


## -------------------------------------------------------------
## Abschnitt 6.4.2 Separate Kalibrierung mit fixiertem 
##                 Itemparameter
## -------------------------------------------------------------


# Vorgehensweise 1: 
# Daten der Normierungsstudie frei kalibrieren und skalieren
# Skalierung der Hauptstudie-Daten mit fixiertem Itemparameter

# -------------------------------------------------------------
# Abschnitt 6.4.2, Listing 1: Daten der Normierungsstudie frei 
#                             kalibrieren und skalieren
#

normmod <- tam.mml(resp = normdat[, n.items], 
                   pid = normdat[, "idstud"])

# -------------------------------------------------------------
# Abschnitt 6.4.2, Listing 1a: Ergänzung zum Buch
# Berechnung von Verteilungsparametern
#

summary(normmod)

set.seed(20160828)

# Personenfähigkeitsschätzer
PV_normmod <- tam.pv(normmod, nplausible = 20)
# In Personendatensatz kombinieren
dfr_normmod <- normmod$person
dfr_normmod <- merge( x = dfr_normmod, y = PV_normmod$pv, by = "pid" , all=T)
dfr_normmod <- dfr_normmod[ order(dfr_normmod$case) , ]

M_norm <- mean(apply(dfr_normmod[,vars.pv],2,Hmisc::wtd.mean,
                     weights = dfr_normmod[,"pweight"]))
SD_norm <- mean(sqrt(apply(dfr_normmod[,vars.pv],2,Hmisc::wtd.var,
                           weights = dfr_normmod[,"pweight"])))
# Tranformationsparameter
a_norm <- 100/SD_norm; b_norm <- 500 - a_norm*M_norm

TM_norm <- M_norm * a_norm + b_norm
TSD_norm <- SD_norm * a_norm

# -------------------------------------------------------------
# Abschnitt 6.4.2, Listing 2: Parameter aus Normierungsstudie
#                             für die Skalierung der Haupt-
#                             studie bei deren Skalierung 
#                             fixieren
#

# Itemschwierigkeit aus der Normierungsstudie
norm.xsi <- normmod$xsi.fixed.estimated
# Hauptstudie: xsi-Matrix aus mod.1PL
xsi.fixed <- mod.1PL$xsi.fixed.estimated
# nur Parameter von Items in Hauptstudie
norm.xsi <- norm.xsi[ 
  rownames(norm.xsi) \%in\% rownames(xsi.fixed), ]
# Setzen der Parameter in richtiger Reihenfolge
xsi.fixed <- cbind(match(rownames(norm.xsi), 
                         rownames(xsi.fixed)), norm.xsi[, 2])
# Skalierung der Hauptstudie-Daten mit fixierten Itemparameter
mainmod.fixed <- tam.mml(resp = resp, xsi.fixed = xsi.fixed,
                         pid = dat$MB_idstud, pweights = w)

# -------------------------------------------------------------
# Abschnitt 6.4.2, Listing 2a: Ergänzung zum Buch
# Berechnung von Verteilungsparametern
#

summary(mainmod.fixed)

set.seed(20160828)

# Personenfähigkeitsschätzer
WLE_mainmod.fixed <- tam.wle(mainmod.fixed)
PV_mainmod.fixed <- tam.pv(mainmod.fixed, nplausible = 20)
# In Personendatensatz kombinieren
dfr_mainmod.fixed <- mainmod.fixed$person
dfr_mainmod.fixed <- merge( x = dfr_mainmod.fixed, y = WLE_mainmod.fixed, by = "pid" , all=T)
dfr_mainmod.fixed <- merge( x = dfr_mainmod.fixed, y = PV_mainmod.fixed$pv, by = "pid" , all=T)
dfr_mainmod.fixed <- dfr_mainmod.fixed[ order(dfr_mainmod.fixed$case) , ]

M_main <- mean(apply(dfr_mainmod.fixed[,vars.pv],2,Hmisc::wtd.mean,
                     weights = dfr_mainmod.fixed[,"pweight"]))
SD_main <- mean(sqrt(apply(dfr_mainmod.fixed[,vars.pv],2,Hmisc::wtd.var,
                           weights = dfr_mainmod.fixed[,"pweight"])))

TM_main <- M_main * a_norm + b_norm
TSD_main <- SD_main * a_norm

trafo.fixed1 <- data.frame(M_norm = M_norm, SD_norm = SD_norm,
                           a = a_norm, b = b_norm,
                           TM_norm = TM_norm, TSD_norm = TSD_norm,
                           M_PV = M_main, SD_PV = SD_main,
                           M_TPV = TM_main, SD_TPV = TSD_main)

# Vorgehensweise 2: 
# Daten der Hauptstudie frei kalibrieren und skalieren
# Skalierung der Hauptstudie-Daten mit fixierten Itemparameter

# -------------------------------------------------------------
# Abschnitt 6.4.2, Listing 2b: Ergänzung zum Buch
# Analoges Vorgehen mit fixierten Parametern aus der 
# Hauptstudie für die Skalierung der Normierungsstudie
#

# Daten der Hauptstudie kalibrieren und skalieren
mainmod <- tam.mml(resp=dat[, items], irtmodel="1PL", 
                   pid=dat$MB_idstud, pweights=dat[,"wgtstud"])
summary(mainmod)

set.seed(20160828)

# Personenfähigkeitsschätzer
WLE_mainmod <- tam.wle(mainmod)
PV_mainmod <- tam.pv(mainmod, nplausible = 20)
# In Personendatensatz kombinieren
dfr_mainmod <- mainmod$person
dfr_mainmod <- merge( x = dfr_mainmod, y = WLE_mainmod, by = "pid" , all=T)
dfr_mainmod <- merge( x = dfr_mainmod, y = PV_mainmod$pv, by = "pid" , all=T)
dfr_mainmod <- dfr_mainmod[order(dfr_mainmod$case),]

M_main <- mean(apply(dfr_mainmod[,vars.pv],2,Hmisc::wtd.mean,
                     weights = dfr_mainmod[,"pweight"]))
SD_main <- mean(sqrt(apply(dfr_mainmod[,vars.pv],2,Hmisc::wtd.var,
                           weights = dfr_mainmod[,"pweight"])))


# Itemschwierigkeit aus der Hauptstudie
main.xsi <- mod.1PL$xsi.fixed.estimated
# Hauptstudie: xsi-Matrix aus normmod
xsi.fixed <- normmod$xsi.fixed.estimated
# nur Parameter von Items in Hauptstudie
main.xsi <- main.xsi[ 
  rownames(main.xsi) \%in\% rownames(xsi.fixed), ]
# Setzen der Parameter in richtiger Reihenfolge
xsi.fixed <- cbind(match(rownames(main.xsi), 
                         rownames(xsi.fixed)), main.xsi[, 2])

# Skalierung der Hauptstudie-Daten mit fixiertem Itemparameter
normmod.fixed <- tam.mml(resp=normdat[, n.items], irtmodel="1PL", 
                         xsi.fixed = xsi.fixed,
                         pid=normdat$MB_idstud, pweights=normdat[,"wgtstud"])
summary(normmod.fixed)

set.seed(20160828)

# Personenfähigkeitsschätzer
PV_normmod.fixed <- tam.pv(normmod.fixed, nplausible = 20)
dfr_normmod.fixed <- normmod.fixed$person
dfr_normmod.fixed <- merge( x = dfr_normmod.fixed, y = PV_normmod.fixed$pv, by = "pid" , all=T)
dfr_normmod.fixed <- dfr_normmod.fixed[ order(dfr_normmod.fixed$case) , ]

M_norm <- mean(apply(dfr_normmod.fixed[,vars.pv],2,Hmisc::wtd.mean,
                     weights = dfr_normmod.fixed[,"pweight"]))
SD_norm <- mean(sqrt(apply(dfr_normmod.fixed[,vars.pv],2,Hmisc::wtd.var,
                           weights = dfr_normmod.fixed[,"pweight"])))

# Tranformationsparameter
a_norm <- 100/SD_norm; b_norm <- 500 - a_norm*M_norm

TM_norm <- M_norm * a_norm + b_norm
TSD_norm <- SD_norm * a_norm

TM_main <- M_main * a_norm + b_norm
TSD_main <- SD_main * a_norm

trafo.fixed2 <- data.frame(M_PV = M_main, SD_PV = SD_main,
                           M_Norm.fixed = M_norm, SD_Norm.fixed = SD_norm,
                           a = a_norm, b = b_norm,
                           TM_norm = TM_norm, TSD_norm = TSD_norm,
                           M_TPV = TM_main, SD_TPV = TSD_main)


## -------------------------------------------------------------
## Abschnitt 6.4.3 Separate Kalibrierung mit Linking durch 
##                 Transformationsfunktion
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.4.3, Listing 1: equating.rasch()
#

# Freigeschätzte Itemparameter der Normierung- und Hauptstudie
norm.pars <- normmod$item[,c("item","xsi.item")]
main.pars <- mainmod$item[,c("item","xsi.item")]
# Linking mit equating.rasch
mod.equate <- equating.rasch(x = norm.pars, y = main.pars)
mod.equate$B.est
#   Mean.Mean    Haebara Stocking.Lord
#  -0.1798861 -0.1788159    -0.1771145
head(mod.equate$anchor,2)

# -------------------------------------------------------------
# Abschnitt 6.4.3, Listing 1a: Ergänzung zum Buch
# Berechnung Linkingfehler                             
#
linkitems <- intersect(n.items, items)

head(mod.equate$transf.par,2)
mod.equate$descriptives

# Linkingfehler: Jackknife unit ist Item
pars <- data.frame(unit = linkitems,
                   study1 = normmod$item$xsi.item[match(linkitems, normmod$item$item)],
                   study2 = mainmod$item$xsi.item[match(linkitems, mainmod$item$item)],
                   item = linkitems)
# pars <- as.matrix(pars)
mod.equate.jk <- equating.rasch.jackknife(pars,se.linkerror = T)
mod.equate.jk$descriptives

# -------------------------------------------------------------
# Abschnitt 6.4.3, Listing 2: Linking nach Haberman
#

# Itemparameter der Normierungsstudie
M1 <- mean( apply(dfr_normmod[,vars.pv], 2, mean ) )
SD1 <- mean( apply(dfr_normmod[,vars.pv], 2, sd ) )
a1 <- 1/SD1; b1 <- 0-a1*M1
A <- normmod$item$B.Cat1.Dim1/a1
B <- (normmod$item$xsi.item + b1/a1)
# Itemparameter der Normierungsstudie fuer haberman.linking
tab.norm <- data.frame(Studie = "1_Normierung",
                       item = normmod$item$item,
                       a = A, b = B/A)
# Itemparameter der Hauptstudie
A <- mainmod$item$B.Cat1.Dim1
B <- mainmod$item$xsi.item
tab.main <- data.frame(Studie = "2_Hauptstudie",
                       item = mainmod$item$item,
                       a = A, b = B/A)
# Itemparameter aller Studien
itempars <- rbind(tab.norm, tab.main)
# Personenparameter
personpars <- list(PV_normmod$pv*a1+b1, PV_mainmod$pv)
# Linking nach Habermans Methode
linkhab <- linking.haberman(itempars = itempars, 
                            personpars = personpars)

# -------------------------------------------------------------
# Abschnitt 6.4.3, Listing 2a: Ergänzung zum Buch
# Ergebnisdarstellung, Transformation und Berechnung
# von Verteilungsparametern
#

# Ergebnisse
# Transformationsparameter der Itemparameter
linkhab$transf.itempars
# Transformationsparameter der Personenparameter
linkhab$transf.personpars

# Itemparameter
dfr.items <- data.frame(linkhab$joint.itempars,
                        linkhab$b.orig, linkhab$b.trans)
names(dfr.items)[-1] <- c("joint_a","joint_b",
                          "orig_b_norm","orig_b_main",
                          "trans_b_norm","trans_b_main")
head(round2(dfr.items[,-1],2),2)

# Transformierte Personenparameter der Hauptstudie
dfr_main_transpv <- linkhab$personpars[[2]]
names(dfr_main_transpv)[-1] <- paste0("linkhab_",vars.pv)
dfr_main_transpv <- cbind(dfr_mainmod,dfr_main_transpv[,-1])
round2(head(dfr_main_transpv[,c("PV1.Dim1","linkhab_PV1.Dim1","PV2.Dim1","linkhab_PV2.Dim1")],2),2)

# Aufgeklärte und Fehlvarianz des Linkings
linkhab$es.invariance

# Transformationsparameter der Normierungsstudie auf Skala 500,100
# trafo.fixed1
a <- 100/mean( apply(dfr_normmod[,vars.pv]*a1+b1, 2, sd ) )
b <- 500 - a*mean( apply(dfr_normmod[,vars.pv]*a1+b1, 2, mean ) )

# trafo.fixed2
M_PV <- mean( apply(linkhab$personpars[[2]][vars.pv], 2, 
                    Hmisc::wtd.mean, weights = dfr_mainmod$pweight ) )
SD_PV <- mean( sqrt(apply(linkhab$personpars[[2]][vars.pv], 2, 
                          Hmisc::wtd.var, weights = dfr_mainmod$pweight )) )
M_TPV <- M_PV*a + b
SD_TPV <- SD_PV * a

trafo.linkhab <- data.frame(trafo.fixed1[,1:2],
                            a1 = a1, b1 = b1,
                            M_norm_trans = 0,
                            SD_norm_trans = 1,
                            a = 100, b = 500,
                            trafo.fixed2[,1:2],
                            linkhab_M_PV = M_PV, 
                            linkhab_SD_PV = SD_PV,
                            linkhab_M_TPV = M_TPV,
                            linkhab_SD_TPV = SD_TPV)


## -------------------------------------------------------------
## Abschnitt 6.4.4 Ergebnisse im Vergleich und Standardfehler
##                 des Linkings
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 6.4.4, Listing 3a: Ergänzung zum Buch
# Berechnung von Standardfehlern Ergebnisvergleiche
#

# Gemeinsame Skalierung mit fixiertem Itemparameter aus Hauptstudie
# Standardfehler bzgl. Itemstichprobenfehler

# Matrix für fixerte Itemparameter vorbereiten
xsi.fixed <- normmod.fixed$xsi.fixed.estimated
npar <- length(xsi.fixed[,"xsi"])
mat.xsi.fixed <- cbind(index=1:npar,par = dimnames(xsi.fixed)[[1]])
sequence <- match(mat.xsi.fixed[,"par"],dimnames(main.xsi)[[1]])
mat.xsi.fixed <- cbind(index=as.numeric(mat.xsi.fixed[,1]), 
                       par = mat.xsi.fixed[,2],
                       xsi.fixed = as.numeric(main.xsi[sequence,"xsi"]))
# Nicht fixierte Itemparameter löschen
del <- which(is.na(mat.xsi.fixed[,"xsi.fixed"]))
mat.xsi.fixed <- mat.xsi.fixed[-del,]
head(mat.xsi.fixed,3)

dfr <- data.frame(elim = "none",growth=trafo.fixed2$M_TPV-500)
# Jedes Mal ein Ankeritem weniger
# Schleife über alle Ankeritems

set.seed(20160828)

for(ii in linkitems){
  # ii <- linkitems[1]
  del <- grep(paste0(ii,"_"), mat.xsi.fixed[,2])
  tmp <- mat.xsi.fixed[-del,c(1,3)]
  tmp <- data.frame(index = as.numeric(tmp[,1]),xsi.fixed = as.numeric(tmp[,2]))
  
  # Skalierung der Hauptstudie-Daten mit fixiertem Itemparameter
  normmod.tmp <- tam.mml(resp=normdat[, n.items], irtmodel="1PL", 
                         xsi.fixed = tmp,
                         pid=normdat$MB_idstud, pweights=normdat[,"wgtstud"])
  
  # Personenfähigkeitsschätzer
  # WLE_normmod.tmp <- tam.wle(normmod.tmp)
  PV_normmod.tmp <- tam.pv(normmod.tmp, nplausible = 20)
  # In Personendatensatz kombinieren
  
  M_norm.tmp <- mean(apply(PV_normmod.tmp$pv[,vars.pv],2,mean))
  SD_norm.tmp <- mean(apply(PV_normmod.tmp$pv[,vars.pv],2,sd))
  
  # Tranformationsparameter
  a_norm.tmp <- 100/SD_norm.tmp 
  b_norm.tmp <- 500 - a_norm.tmp*M_norm.tmp
  
  TM_main.tmp <- M_main * a_norm.tmp + b_norm.tmp
  dfr.tmp <- data.frame(elim = ii,growth=TM_main.tmp-500)
  dfr <- rbind(dfr,dfr.tmp)
  
}

dfr$diff2 <- (dfr$growth-dfr$growth[1])^2
sum <- sum(dfr$diff2)
Var <- sum*28/29
SE <- sqrt(Var)

quant <- 1.96 
low <- trafo.fixed2$M_TPV - quant*SE
upp <- trafo.fixed2$M_TPV + quant*SE

dfr$SE <- SE; dfr$quant <- quant
dfr$low <- low; dfr$upp <- upp

}%end dontrun
}