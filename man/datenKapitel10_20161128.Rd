\name{datenKapitel10}
\alias{datenKapitel10}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 10, Reporting und Analysen
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 10, 
\emph{Reporting und Analysen}, im Herausgeberband 
Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel10)
}

\references{
  Bruneforth, M., Oberwimmer, K. & Robitzsch, A. (2016). Reporting und Analysen. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 333--362). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 10}}.
}	

\format{
\code{datenKapitel10} ist eine Liste mit den vier Elementen, 
\code{dat}, \code{dat.roh}, \code{dat.schule} und \code{dat.schule.roh}.\cr
Die  Elemente \code{dat} und \code{dat.schule} enthalten jeweils zehn imputierte 
Datensätze (vgl. Kapitel 8, \emph{Fehlende Daten und Plausible Values}, im 
Band). Zum Vergleich stehen denen die Rohdatensätze \code{dat.roh} bzw. 
\code{dat.schule.roh} gegenüber.

%%
\itemize{
\item \strong{dat} und \strong{dat.roh}: Roh-Datensatz bzw. Liste mit zehn 
imputierten Datensätzen für 9885 Schülerinnen und Schüler.

\ifelse{latex}{%start latex
\itemize{
\item \code{idschool}: Schulenidentifikator.
\item \code{idstud}: Schüleridentifikator.
\item \code{idclass}: Klassenidentifikator.
\item \code{wgtstud}: Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band).
\item \code{female}: Geschlecht (\code{1} = weiblich, \code{0} = männlich).
\item \code{migrant}: Migrationsstatus (\code{1} = mit Migrationshintergrund, 
\code{0} = ohne Migrationshintergrund).
\item \code{HISEI}: Sozialstatus (vgl. Kapitel 10, \emph{Reporting und Analysen}, 
im Band).
\item \code{eltausb}: Ausbildung der Eltern.
\item \code{buch}: Anzahl der Bücher zu Hause.
\item \code{SK}: Fragebogenskala "Selbstkonzept".
\item \code{LF}: Fragebogenskala "Lernfreude".
\item \code{E8RTWLE}: WLE der Lesekompetenz (vgl. Kapitel 1, 
\emph{Testkonstruktion}, und Kapitel 6, \emph{Skalierung und Linking}, 
im Band).
\item \code{E8RPV}: Plausible Values für die Leistung in Englisch Lesen (vgl. 
Kapitel 8 im Band).
\item \code{jkzone}: Jackknife-Zone im Jackknife-Repeated-Replication-Design (vgl. 
Kapitel 2).
\item \code{jkrep}: Jackknife-Replikationsfaktor im 
Jackknife-Repeated-Replication-Design (vgl. Kapitel 2).
\item \code{w_fstr*}: Jackknife-Replikationsgewichte (vgl. Kapitel 2).
}}{\tabular{ll}{
\code{idschool} \tab Schulenidentifikator. \cr
\code{idstud} \tab Schüleridentifikator.\cr
\code{idclass} \tab Klassenidentifikator.\cr
\code{wgtstud} \tab Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band). \cr
\code{female} \tab Geschlecht (\code{1} = weiblich, \code{0} = männlich).\cr
\code{migrant} \tab Migrationsstatus (\code{1} = mit Migrationshintergrund, 
\code{0} = ohne Migrationshintergrund).\cr
\code{HISEI} \tab Sozialstatus (vgl. Kapitel 10, \emph{Reporting und Analysen}, 
im Band).\cr
\code{eltausb} \tab Ausbildung der Eltern.\cr
\code{buch} \tab Anzahl der Bücher zu Hause.\cr
\code{SK} \tab Fragebogenskala "Selbstkonzept".\cr
\code{LF} \tab Fragebogenskala "Lernfreude".\cr
\code{E8RTWLE} \tab WLE der Lesekompetenz (vgl. Kapitel 1, 
\emph{Testkonstruktion}, und Kapitel 6, \emph{Skalierung und Linking}, 
im Band).\cr
\code{E8RPV} \tab Plausible Values für die Leistung in Englisch Lesen (vgl. 
Kapitel 8 im Band).\cr
\code{jkzone} \tab Jackknife-Zone im Jackknife-Repeated-Replication-Design (vgl. 
Kapitel 2).\cr
\code{jkrep} \tab Jackknife-Replikationsfaktor im 
Jackknife-Repeated-Replication-Design (vgl. Kapitel 2).\cr
\code{w_fstr*} \tab Jackknife-Replikationsgewichte (vgl. Kapitel 2).
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{List of 10}\cr
\code{ $ :'data.frame':	9885 obs. of  151 variables:}\cr
\code{  ..$ idschool    : int [1:9885] 1001 1001 1001 1001 1001 1001 1001 ...}\cr
\code{  ..$ idstud      : int [1:9885] 10010101 10010102 10010103 10010105 ...}\cr
\code{  ..$ idclass     : int [1:9885] 100101 100101 100101 100101 100101 ...}\cr
\code{  ..$ wgtstud     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...}\cr
\code{  ..$ female      : int [1:9885] 0 0 0 0 1 0 1 1 1 1 ...}\cr
\code{  ..$ migrant     : int [1:9885] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{  ..$ HISEI       : int [1:9885] 31 28 25 27 27 76 23 57 52 58 ...}\cr
\code{  ..$ eltausb     : int [1:9885] 2 1 2 2 2 2 2 1 2 1 ...}\cr
\code{  ..$ buch        : int [1:9885] 1 1 1 1 3 3 4 2 5 4 ...}\cr
\code{  ..$ SK          : num [1:9885] 2.25 2.25 3 3 2.5 3.25 2.5 3.25 3.5 2.5 ...}\cr
\code{  ..$ LF          : num [1:9885] 1.25 1.5 1 1 4 3 2 3.5 3.75 2.25 ...}\cr
\code{  ..$ E8RTWLE     : num [1:9885] 350 438 383 613 526 ...}\cr
\code{  ..$ E8RPV       : num [1:9885] 390 473 380 599 509 ...}\cr
\code{  ..$ jkzone      : int [1:9885] 37 37 37 37 37 37 37 37 37 37 ...}\cr
\code{  ..$ jkrep       : int [1:9885] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{  ..$ w_fstr1     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...}\cr
\code{  ..$ w_fstr2     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...}\cr
\code{  ..$ w_fstr3     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...}\cr
\code{    [...]}\cr
\code{  ..$ w_fstr83    : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...}\cr
\code{  ..$ w_fstr84    : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...}\cr
\code{ $ :'data.frame':	9885 obs. of  151 variables:}\cr
\code{ [...]}\cr
}{\verb{List of 10
 $ :'data.frame':	9885 obs. of  151 variables:
  ..$ idschool    : int [1:9885] 1001 1001 1001 1001 1001 1001 1001 ...
  ..$ idstud      : int [1:9885] 10010101 10010102 10010103 10010105 ...
  ..$ idclass     : int [1:9885] 100101 100101 100101 100101 100101 ...
  ..$ wgtstud     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...
  ..$ female      : int [1:9885] 0 0 0 0 1 0 1 1 1 1 ...
  ..$ migrant     : int [1:9885] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ HISEI       : int [1:9885] 31 28 25 27 27 76 23 57 52 58 ...
  ..$ eltausb     : int [1:9885] 2 1 2 2 2 2 2 1 2 1 ...
  ..$ buch        : int [1:9885] 1 1 1 1 3 3 4 2 5 4 ...
  ..$ SK          : num [1:9885] 2.25 2.25 3 3 2.5 3.25 2.5 3.25 3.5 2.5 ...
  ..$ LF          : num [1:9885] 1.25 1.5 1 1 4 3 2 3.5 3.75 2.25 ...
  ..$ E8RTWLE     : num [1:9885] 350 438 383 613 526 ...
  ..$ E8RPV       : num [1:9885] 390 473 380 599 509 ...
  ..$ jkzone      : int [1:9885] 37 37 37 37 37 37 37 37 37 37 ...
  ..$ jkrep       : int [1:9885] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ w_fstr1     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...
  ..$ w_fstr2     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...
  ..$ w_fstr3     : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...
    [...]
  ..$ w_fstr83    : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...
  ..$ w_fstr84    : num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...
 $ :'data.frame':	9885 obs. of  151 variables:
 [...]
}%end verb
}%end else / end ifelse latex


%%
\item \strong{dat.schule} und \strong{dat.schule.roh}: Roh-Datensatz bzw. Liste 
mit zehn imputierten Datensätzen als Liste für 244 Schulen. Es handelt sich 
hierbei -- wie bei allen Datensätzen im Band -- um fiktive (höchstens 
partiell-synthetische) Daten!

\ifelse{latex}{%start latex
\itemize{
\item \code{idschool}: Schulenidentifikator.
\item \code{Schultyp}: Schultyp (\code{AHS} = allgemeinbildende höhere Schule, 
bzw. \code{APS} = allgemeinbildende Pflichtschule).
\item \code{Strata}: Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4, 
für eine genauere Beschreibung der Strata; siehe Kapitel 
2 im Band).
\item \code{Strata.label}: Bezeichnung des Stratums.
\item \code{NSchueler}: Anzahl Schüler/innen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).
\item \code{NKlassen}: Anzahl Klassen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).
\item \code{gemgroesse}: Gemeindegröße.
\item \code{SCFRA04x02}: Fragebogenvariable aus Schulleiterfragebogen zur 
Schulgröße (vgl.\cr https://www.bifie.at/node/2119).
\item \code{SCFO05a*}: Fragebogenvariable aus Schulleiterfragebogen zu 
"Schwerpunktschule für ..." (\code{*a01} = Informatik, \code{*a02} = 
Mathematik, \code{*a03} = Musik, \code{*a04} = Naturwissenschaften, 
\code{*a05} = Sport, \code{*a06} = Sprachen, \code{*a07} = Technik, 
\code{*a081} = Anderes; vgl.\cr https://www.bifie.at/node/2119).
\item \code{HISEI}: Auf Schulenebene aggregierte HISEI.
\item \code{E8RPV}: Auf Schulenebene aggregierte Plausible Values für die Leistung 
in Englisch Lesen.
}}{
\tabular{ll}{
\code{idschool} \tab Schulenidentifikator. \cr
\code{Schultyp} \tab Schultyp (\code{AHS} = allgemeinbildende höhere Schule, bzw. 
\code{APS} = allgemeinbildende Pflichtschule).\cr
\code{Strata} \tab Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4, 
für eine genauere Beschreibung der Strata; siehe Kapitel 
2 im Band).\cr
\code{Strata.label} \tab Bezeichnung des Stratums.\cr
\code{NSchueler} \tab Anzahl Schüler/innen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).\cr
\code{NKlassen} \tab Anzahl Klassen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).\cr
\code{gemgroesse} \tab Gemeindegröße. \cr
\code{SCFRA04x02} \tab Fragebogenvariable aus Schulleiterfragebogen zur 
Schulgröße (vgl. https://www.bifie.at/node/2119).\cr
\code{SCFO05a*} \tab Fragebogenvariable aus Schulleiterfragebogen zur 
"Schwerpunktschule für ..." (\code{*a01} = Informatik, \code{*a02} = 
Mathematik, \code{*a03} = Musik, \code{*a04} = Naturwissenschaften, 
\code{*a05} = Sport, \code{*a06} = Sprachen, \code{*a07} = Technik, 
\code{*a081} = Anderes; vgl. https://www.bifie.at/node/2119). Es handelt sich 
hierbei um rein fiktive Daten!\cr
\code{HISEI} \tab Auf Schulenebene aggregierte HISEI.\cr
\code{E8RPV} \tab Auf Schulenebene aggregierte Plausible Values für die Leistung 
in Englisch Lesen.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{List of 10}\cr
\code{ $ :'data.frame':	244 obs. of  18 variables:}\cr
\code{  ..$ idschool    : int [1:244] 1001 1002 1003 1004 1005 1006 1007 1010 ...}\cr
\code{  ..$ Schultyp    : chr [1:244] "HS" "HS" "HS" "HS" ...}\cr
\code{  ..$ Strata      : int [1:244] 1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{  ..$ Strata.label: chr [1:244] "HS/Land" "HS/Land" "HS/Land" "HS/Land" ...}\cr
\code{  ..$ NSchueler   : int [1:244] 12 14 15 17 18 19 20 20 21 22 ...}\cr
\code{  ..$ NKlassen    : int [1:244] 1 1 1 1 2 1 2 1 2 2 ...}\cr
\code{  ..$ gemgroesse  : int [1:244] 5 4 4 5 3 4 5 4 4 5 ...}\cr
\code{  ..$ SCFRA04x02  : int [1:244] 45 63 47 81 95 80 66 86 104 126 ...}\cr
\code{  ..$ SCFO05a01   : int [1:244] 1 0 0 0 0 0 0 1 1 0 ...}\cr
\code{  ..$ SCFO05a02   : int [1:244] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{  ..$ SCFO05a03   : int [1:244] 1 1 0 0 0 0 0 0 0 0 ...}\cr
\code{  ..$ SCFO05a04   : int [1:244] 1 0 0 0 0 1 0 0 0 0 ...}\cr
\code{  ..$ SCFO05a05   : int [1:244] 0 0 0 0 1 0 1 0 0 0 ...}\cr
\code{  ..$ SCFO05a06   : int [1:244] 0 1 1 0 0 1 0 0 1 0 ...}\cr
\code{  ..$ SCFO05a07   : int [1:244] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{  ..$ SCFO05a081  : int [1:244] 0 0 1 0 0 1 1 0 0 0 ...}\cr
\code{  ..$ HISEI       : num [1:244] 33.5 48.6 41.1 43.5 46.9 ...}\cr
\code{  ..$ E8RPV       : num [1:244] 471 463 513 494 525 ...}\cr
\code{ $ :'data.frame':	244 obs. of  18 variables:}\cr
\code{ [...]}\cr
}{\verb{List of 10
 $ :'data.frame':	244 obs. of  18 variables:
  ..$ idschool    : int [1:244] 1001 1002 1003 1004 1005 1006 1007 1010 ...
  ..$ Schultyp    : chr [1:244] "HS" "HS" "HS" "HS" ...
  ..$ Strata      : int [1:244] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ Strata.label: chr [1:244] "HS/Land" "HS/Land" "HS/Land" "HS/Land" ...
  ..$ NSchueler   : int [1:244] 12 14 15 17 18 19 20 20 21 22 ...
  ..$ NKlassen    : int [1:244] 1 1 1 1 2 1 2 1 2 2 ...
  ..$ gemgroesse  : int [1:244] 5 4 4 5 3 4 5 4 4 5 ...
  ..$ SCFRA04x02  : int [1:244] 45 63 47 81 95 80 66 86 104 126 ...
  ..$ SCFO05a01   : int [1:244] 1 0 0 0 0 0 0 1 1 0 ...
  ..$ SCFO05a02   : int [1:244] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ SCFO05a03   : int [1:244] 1 1 0 0 0 0 0 0 0 0 ...
  ..$ SCFO05a04   : int [1:244] 1 0 0 0 0 1 0 0 0 0 ...
  ..$ SCFO05a05   : int [1:244] 0 0 0 0 1 0 1 0 0 0 ...
  ..$ SCFO05a06   : int [1:244] 0 1 1 0 0 1 0 0 1 0 ...
  ..$ SCFO05a07   : int [1:244] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ SCFO05a081  : int [1:244] 0 0 1 0 0 1 1 0 0 0 ...
  ..$ HISEI       : num [1:244] 33.5 48.6 41.1 43.5 46.9 ...
  ..$ E8RPV       : num [1:244] 471 463 513 494 525 ...
 [...]
}%end verb
}%end else / end ifelse latex

}%end itemize
}%end format

\keyword{datasets}
