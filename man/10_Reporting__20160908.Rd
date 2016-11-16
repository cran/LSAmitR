%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel 10}
\alias{Kapitel 10}
\encoding{UTF-8}
\title{Kapitel 10: Reporting und Analysen}

\description{
Das ist die Nutzerseite zum Kapitel 10, \emph{Reporting und Analysen}, im 
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
  Bruneforth, M., Oberwimmer, K. & Robitzsch, A. (2016). Reporting und Analysen. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 333--362). Wien: facultas.
}

% \note{
% % wenn es Hinweise gibt
% }

\author{
  Michael Bruneforth, Konrad Oberwimmer, Alexander Robitzsch
}

\seealso{
%Verweis zu den Daten im Kapitel
Zu \code{\link{datenKapitel10}}, den im Kapitel verwendeten Daten.\cr
Zurück zu \code{\link{Kapitel 9}}, Fairer Vergleich in der Rückmeldung.\cr
Zu \code{\link{Kapitel 11}}, Aspekte der Validierung.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach 
%Übersicht einfügen)
%Zur Hilfeseite von \code{\link[TAM:TAM-package]{TAM}}
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
library(BIFIEsurvey)
library(matrixStats)

data(datenKapitel10)
dat <- datenKapitel10$dat
dat.roh <- datenKapitel10$dat.roh
dat.schule <- datenKapitel10$dat.schule
dat.schule.roh <- datenKapitel10$dat.schule.roh

\dontrun{
## -------------------------------------------------------------
## Abschnitt 10.4.1: Datenbasis
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.4.1 a, Ergänzung zum Buch
# Herunterladen, entpacken und setzen des Arbeitsspeichers
# 

# setwd(dir = ".../DatenKapitel10")

# -------------------------------------------------------------
# Abschnitt 10.4.1, Listing 1: Einlesen der Schülerdaten
#

# Anlegen eines leeren Listenobjektes für Schülerdaten
dat <- list()

# Vektor mit Liste der Dateinamen für Schülerdaten
dateinamen <- paste0("e8pv__schueler_imp_",1:10,".csv")
# Schleife zum Einlesen der Daten, die in die Listenobjekte 
# abgelegt werden
for (ii in 1:10) {
  schueler_dfr<-read.csv2(file = dateinamen[[ii]])
  dat[[ii]] <- schueler_dfr
}
# Überprüfen des Listenobjektes und der eingelesenen Daten
str(dat)

# Rohdaten als Datenmatrix einlesen
dat.roh <- read.csv2(file = "e8pv__schueler_raw.csv")

# -------------------------------------------------------------
# Abschnitt 10.4.1, Listing 1a: Ergänzung zum Buch
# Einlesen der Schulendaten
#

# Anlegen eines leeren Listenobjektes für Schuldaten
dat.schule <- list()

# Vektor mit Liste der Dateinamen für Schuldaten
dateinamen <- paste0("e8pv__schule_imp_",1:10,".csv")
# Schleife zum Einlesen der Daten, die in die Listenobjekte 
# abgelegt werden
for (ii in 1:10) {
  schule_dfr<-read.csv2(file = dateinamen[[ii]])
  dat.schule[[ii]] <- schule_dfr
}
# Überprüfen des Listenobjektes und der eingelesenen Daten
str(dat.schule)

#Rohdaten als Datenmatrix einlesen
dat.schule.roh <- read.csv2(file = "e8pv__schule_raw.csv")

## -------------------------------------------------------------
## Abschnitt 10.4.2: Merging verschiedener Ebenen
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.4.2, Listing 1
#

for (i in 1:10) {
   dat[[i]] <- merge(dat[[i]],dat.schule[[i]],
                      by = "idschool",all.x = TRUE)
}

# -------------------------------------------------------------
# Abschnitt 10.4.2, Listing 2

for (i in 1:10) {
   dat.agg <- aggregate(dat[[i]][,c("HISEI","E8RPV")],
                        by = list(idschool = dat[[i]]$idschool),
                        FUN = mean,na.rm = TRUE)
   dat.schule[[i]] <- merge(dat.schule[[i]],dat.agg,
                            by="idschool",all.x = TRUE)
}

## -------------------------------------------------------------
## Abschnitt 10.4.3: Erzeugen von BIFIEdata-Objekten
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.4.3, a: Ergänzung zum Buch
# Einlesen der Replikationsgewichte
#

# Zwischenspeichern des Schülerdatensatzes
dat.tmp <- dat

# Daten aus Large-Scale Assessments können mit replicate weights 
#  abgespeichert werden (z.B. PISA) oder mit Informationen zu den 
#  Jackknifezonen und -gewichten (z.B. PIRLS). In diesem Beispiel 
#  werden beide Methoden vorgestellt, daher wird die Gewichtungs-
#  information in beiden Formen eingelesen: mit replicate weights
#  im Datensatz dat1; mit Replikationsdesign im Datensatz dat2.

# replicate weights für Schüler/innen als Datenmatrix einlesen 
dat.repwgts <- read.csv2(file = "e8__schueler_repwgts.csv")
# replicate weights an Schülerdaten mergen
for (ii in 1:10) {
  dat[[ii]]<-merge(x = dat[[ii]],y = dat.repwgts,
                    by = c("idschool","idstud"))
}

# Jackknifezonen und -gewichte für Schulen als Datenmatrix einlesen 
dat2 <- list()
dat.schule.jk <- read.csv2(file = "e8__schule_jkzones.csv")
# Jackknifezonen und -gewichte an schülerdaten mergen
for (ii in 1:10) {
  dat2[[ii]]<-merge(x = dat.tmp[[ii]],y = dat.schule.jk,
                    by = "idschool")
}

# -------------------------------------------------------------
# Abschnitt 10.4.3, b: Ergänzung zum Buch
# Kontrolle der Sortierung
#

# Die Observationen in den 10 Imputationen muessen gleich sortiert 
# sein. Dies wir zur Sicherheit getestet. 
for (i in 2:10) {
  if (sum(dat[[1]]$idstud!=dat[[i]]$idstud )>0) 
      stop("Imputationsdatensätze nicht gleich sortiert!")  
}
  
# -------------------------------------------------------------
# Abschnitt 10.4.3, c: Ergänzung zum Buch
# Verwendung des R-Datenobjekts
#

dat <- datenKapitel10$dat
  
# -------------------------------------------------------------
# Abschnitt 10.4.3, Listing 1: Übernahme der Gewichte aus 
# Datenmatrix
#

wgtstud <- dat[[1]]$wgtstud
repwgtsvar <- grep("^w_fstr",colnames(dat[[1]]))
repwgts <- dat[[1]][,repwgtsvar]
dat <- BIFIE.data(data.list = dat,wgt = wgtstud,
                  wgtrep = repwgts,fayfac = 1,
                  cdata = TRUE)
summary(dat)

# -------------------------------------------------------------
# Abschnitt 10.4.3, Listing 2: Erzeugung der Gewichte aus 
# Replikationsdesign
#         

dat2 <- BIFIE.data.jack(data = dat2,wgt = "wgtstud",
                        jktype = "JK_GROUP",
                        jkzone = "jkzone",
                        jkrep = "jkrep",
                        fayfac = 1)
summary(dat2)


# -------------------------------------------------------------
# Abschnitt 10.4.3, Listing 3: Univariate Statistik Reading
#

res.univar <- BIFIE.univar(BIFIEobj = dat,
                          vars = c("E8RPV"),
                          group = "Strata")
summary(res.univar)
res2.univar <- BIFIE.univar(BIFIEobj = dat2,
                          vars = c("E8RPV"),
                          group = "Strata")
summary(res2.univar)

## -------------------------------------------------------------
## Abschnitt 10.4.4: Rekodierung und Transformation von 
##                   Variablen
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.4.4, Listing 1: Neue Variable GERSER mit 
# BIFIE.data.transform
#

transform.formula <- as.formula(
   "~ 0 + I(cut(E8RPV,breaks = c(0,406,575,1000),labels = FALSE))"
   )
dat <- BIFIE.data.transform(dat,transform.formula,
                            varnames.new = "GERSER")
res.freq <- BIFIE.freq(BIFIEobj = dat,vars = c("GERSER"))
summary(res.freq)

# -------------------------------------------------------------
# Abschnitt 10.4.4, Listing 2: Zwei neue Variablen PVERfit und 
# PVERres mit BIFIE.data.transform
#

transform.formula <- as.formula(
   "~ 0 + I(fitted(lm(E8RPV ~ HISEI + female))) +
          I(residuals(lm(E8RPV ~ HISEI + female)))"
   )
dat <- BIFIE.data.transform(dat,transform.formula,
                            varnames.new = c("PVERfit","PVERres"))
res.univar <- BIFIE.univar(BIFIEobj = dat,
                          vars = c("PVERfit","PVERres"))
summary(res.univar)

## -------------------------------------------------------------
## Abschnitt 10.4.5: Berechnung von Kenngroessen und deren 
##                   Standardfehlern
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.4.5, Listing 1: Anwenderfunktion
#
library(matrixStats)

anwenderfct.weightedMad <- function(X,w)
{
  # Die Funktion weightedMad wird auf jede Spalte der 
  # übergebenen Matrix X angewendet.
  Wmad<-apply(X = X, MARGIN = 2,FUN = matrixStats::weightedMad, 
              w = w, na.rm = T)
}

wgt.Mad <- BIFIE.by(BIFIEobj = dat,
                     vars =  c("HISEI", "E8RPV"),
                     userfct = anwenderfct.weightedMad,
                     userparnames = c("wMadHISEI", "wMadE8RPV"))
summary(wgt.Mad)

## -------------------------------------------------------------
## Abschnitt 10.6.1: Datenexploration
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.6.1, Listing 1: Ungewichtete univariate 
# Statistiken
#

# Ungewichtete univariate Statistiken
# Häufigkeitstabelle zu 'eltausb' und 'migrant' (Kreuztabelle)
tab1 <- table(dat.roh[,c("eltausb","migrant")],useNA = "always")
# Ausgabe der Tabelle, ergänzt um Randsummen
addmargins(tab1, FUN = list(Total = sum), quiet = TRUE)

# Ausgabe der Tabelle als Prozentverteilungen 
# (in Prozent, gerundet)
round(addmargins(prop.table(x = tab1), FUN = list(Total = sum), 
  quiet = TRUE)*100,2)

# Ausgabe mit Prozentverteilungen der Spalten bzw. Zeilen 
# (in Prozent, gerundet)
round(prop.table(x = tab1,margin = 2)*100,2)
round(prop.table(x = tab1,margin = 1)*100,2)
# Ausgabe nicht wiedergegeben

# -------------------------------------------------------------
# Abschnitt 10.6.1, Listing 2: Gewichtete univariate 
# Statistiken an imputierten Daten


# Gewichtete univariate Statistiken an imputierten Daten
# Häufigkeitstabelle zu 'eltausb' und 'migrant'
res1 <- BIFIE.freq(BIFIEobj = dat,vars = c("eltausb","migrant"))
summary(res1)
# Häufigkeitstabelle zu 'eltausb' gruppiert nach 'migrant'
res2 <- BIFIE.freq(BIFIEobj = dat,vars = "eltausb",
                   group = "migrant")
summary(res2)
# Kreuztabelle mit zwei Variablen
res3 <- BIFIE.crosstab(BIFIEobj = dat,vars1 = "eltausb",
                      vars2 = "migrant")
summary(res3)

# -------------------------------------------------------------
# Abschnitt 10.6.1, Listing 3: Export der Tabelle
#

res_export <- res1$stat[,c("var","varval","Ncases","Nweight", 
  "perc","perc_SE")]
colnames(res_export) <- c("Variable","Wert","N (ungewichtet)",
 "N gewichtet)","Prozent","Standardfehler")
write.table(x = res_export,file = "res_export.dat", sep = ";",
            dec = ",", row.names = FALSE)

## -------------------------------------------------------------
## Abschnitt 10.6.2: Analyse fehlender Werte
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.6.2, Listing 1: Fehlende Werte
#

res1 <- BIFIE.mva(dat, missvars = c("eltausb","migrant"), 
                  se = TRUE)
summary(res1)

# -------------------------------------------------------------
# Abschnitt 10.6.2, Listing 2: Fehlende Werte unter Kovariaten
#

res2 <- BIFIE.mva(dat,missvars = c("eltausb","migrant"), 
  covariates = c("E8RTWLE","eltausb", "migrant"), se = TRUE)   
summary(res2)

## -------------------------------------------------------------
## Abschnitt 10.6.3: Mittelwerte, Perzentilbaender und Quantile
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.6.3, Listing 1: Hilfsvariable
#

# Hilfsvariable zur Gruppierung anlegen
transform.formula <- as.formula("~ 0 + I(migrant*10+female)")
dat <- BIFIE.data.transform(dat,transform.formula,
                  varnames.new="migrant_female")

# -------------------------------------------------------------
# Abschnitt 10.6.3, Listing 2: Statistiken an Hilfsvariablen
#

# Univariate Statistiken mit Mittelwerten und Standardfehlern
res1 <- BIFIE.univar(BIFIEobj = dat,vars = "E8RPV",
                    group = "migrant_female")
# summary(res1)
mittelwerte<-res1$stat[,c("groupval","M","M_SE")]

# Berechne Quantile
probs<-c(.05,.25,.75,.95)
res2 <- BIFIE.ecdf(BIFIEobj = dat,breaks = probs,
                   quanttype = 1, vars = "E8RPV", 
                   group = "migrant_female")
# summary(res2)
quantile<-data.frame(t(matrix(res2$output$ecdf,nrow = 4)))
colnames(quantile)<-probs
# Führe Ergebnisse zusammen
res3<-cbind(mittelwerte,quantile)
print(res3)

# -------------------------------------------------------------
# Abschnitt 10.6.3, Listing 3: IQA
#

# Berechne Interquartilabstand (IQA)
res3$IQA<-res3$"0.75"-res3$"0.25"
# Berechne Grenzen des Vertrauensintervals
res3$VIunten<-res3$M-2*res3$M_SE
res3$VIoben<-res3$M+2*res3$M_SE
round(res3,1)

## -------------------------------------------------------------
## Abschnitt 10.6.4: Gruppenvergleiche mit Regressionen
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 10.6.4, Listing 1: Gruppenvergleich Geschlecht
#

# Gruppenvergleich Geschlecht, gesamte Population
res1 <- BIFIE.linreg(BIFIEobj = dat, formula = E8RPV ~ female)
# Alternativer Aufruf mit identischem Resultat
res1 <- BIFIE.linreg(BIFIEobj = dat,dep = "E8RPV", 
                     pre = c("one","female"))
                     
# Vollständige Ausgabe
summary(res1)

# Reduzierte Ausgabe der Ergebnisse
res1_short <- res1$stat[res1$stat$parameter == "b" &
            res1$stat$var == "female",c("est","SE")]
colnames(res1_short) <- c("Geschlechterunterschied","SE")
res1_short

# Gruppenvergleich Geschlecht getrennt nach 'migrant'
res2 <- BIFIE.linreg(BIFIEobj = dat,
                    formula = E8RPV ~ female,
                    group = "migrant")
# Vollständige Ausgabe
summary(res2)

# Reduzierte Ausgabe der Ergebnisse
res2_short <- res2$stat[res2$stat$parameter == "b" &
                        res2$stat$var == "female",
                      c("groupval","est","SE")]
colnames(res2_short) <- c("Migrant","Geschlechterunterschied",
                          "SE")
res2_short

# -------------------------------------------------------------
# Abschnitt 10.6.4, Listing 2: Wald-Test
#

res3 <- BIFIE.univar(vars = "E8RPV",BIFIEobj = dat, 
                     group = c("migrant","female"))
res3_wald <- BIFIE.univar.test(BIFIE.method = res3)

# summary(res3_wald)
res3_wald$stat.dstat[,c("group","groupval1","groupval2",
                        "M1","M2","d","d_SE","d_t","d_p")]

# -------------------------------------------------------------
# Abschnitt 10.6.4, Listing 3: Kontrolle um soziale Herkunft
#

# Gruppenvergleich ohne Berücksichtigung der sozialen Herkunft
res1 <- BIFIE.linreg(BIFIEobj = dat, formula = E8RPV ~ migrant)
# summary(res1)
res1$stat[res1$stat$parameter == "b" & res1$stat$var == "migrant",
         c("groupval","est","SE")]

# Gruppenvergleich mit Berücksichtigung der sozialen Herkunft
res2 <- BIFIE.linreg(BIFIEobj = dat,
                    formula = E8RPV ~ migrant+HISEI+eltausb+buch)
# summary(res2)
res2$stat[res2$stat$parameter == "b" & res2$stat$var == "migrant",
         c("groupval","est","SE")]

}%end dontrun
}