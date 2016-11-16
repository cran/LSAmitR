\name{datenKapitel08}
\alias{datenKapitel08}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 8, Fehlende Daten und Plausible Values
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 8, 
\emph{Fehlende Daten und Plausible Values}, im Herausgeberband 
Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel08)
}

\references{
  Robitzsch, A., Pham, G. & Yanagida, T. (2016). Fehlende Daten und Plausible 
  Values.
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 259--293). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 8}}.
}	

\format{
\code{datenKapitel08} ist eine Liste mit den vier Elementen \code{data08H}, 
\code{data08I}, \code{data08J} und \code{data08K}, die Kontextinformationen mit 
fehlenden Daten zur Imputation sowie Instrumentendaten im Multiple-Matrix-Design
für die Plausible-Value-Ziehung enthalten.

%%
\itemize{
\item \strong{data08H}: Roh-Datensatz mit Leistungsschätzern und 
Kontextinformationen für 2507 Schüler/innen in 74 Schulen.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{idschool}: Schulenidentifikator.
\item \code{wgtstud}: Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band).
\item \code{wgtstud}: Stichprobengewicht der Schule (vgl. Kapitel 
2 im Band). 
\item \code{Stratum}: Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Kapitel 
2 im Band).
\item \code{female}: Geschlecht (\code{1} = weiblich, \code{0} = männlich).
\item \code{migrant}: Migrationsstatus (\code{1} = mit Migrationshintergrund, 
\code{0} = ohne Migrationshintergrund).
\item \code{HISEI}: Sozialstatus (vgl. Kapitel 10, \emph{Reporting und Analysen}, 
im Band).
\item \code{eltausb}: Ausbildung der Eltern.
\item \code{buch}: Anzahl der Bücher zu Hause.
\item \code{SK}: Fragebogenskala "Selbstkonzept".
\item \code{LF}: Fragebogenskala "Lernfreude".
\item \code{NSchueler}: Anzahl Schüler/innen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).
\item \code{NKlassen}: Anzahl Klassen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).
\item \code{SES_Schule}: Auf Schulebene erfasster Sozialstatus (siehe Buchkapitel).
\item \code{E8WWLE}: WLE der Schreibkompetenz (vgl. Kapitel 7, \emph{Statistische 
Analysen produktiver Kompetenzen}, im Band).
\item \code{E8LWLE}: WLE der Hörverstehenskompetenz (vgl. Kapitel 6, 
\emph{Skalierung und Linking}, im Band).
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{idschool} \tab Schulenidentifikator. \cr
\code{wgtstud} \tab Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band). \cr
\code{wgtstud} \tab Stichprobengewicht der Schule (vgl. Kapitel 
2 im Band). \cr
\code{Stratum} \tab Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Kapitel 
2 im Band).\cr
\code{female} \tab Geschlecht (\code{1} = weiblich, \code{0} = männlich).\cr
\code{migrant} \tab Migrationsstatus (\code{1} = mit Migrationshintergrund, 
\code{0} = ohne Migrationshintergrund).\cr
\code{HISEI} \tab Sozialstatus (vgl. Kapitel 10, \emph{Reporting und Analysen}, 
im Band).\cr
\code{eltausb} \tab Ausbildung der Eltern.\cr
\code{buch} \tab Anzahl der Bücher zu Hause.\cr
\code{SK} \tab Fragebogenskala "Selbstkonzept".\cr
\code{LF} \tab Fragebogenskala "Lernfreude".\cr
\code{NSchueler} \tab Anzahl Schüler/innen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).\cr
\code{NKlassen} \tab Anzahl Klassen in der 4. Schulstufe (vgl. Kapitel 
2 im Band).\cr
\code{SES_Schule} \tab Auf Schulebene erfasster Sozialstatus (siehe Buchkapitel).\cr
\code{E8WWLE} \tab WLE der Schreibkompetenz (vgl. Kapitel 7, \emph{Statistische 
Analysen produktiver Kompetenzen}, im Band).\cr
\code{E8LWLE} \tab WLE der Hörverstehenskompetenz (vgl. Kapitel 6, 
\emph{Skalierung und Linking}, im Band).
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{
\code{'data.frame':	2507 obs. of  17 variables: }\cr
\code{ $ idstud    : int [1:2507] 10010101 10010102 10010103 10010105 10010106 10010107 ... }\cr
\code{ $ idschool  : int [1:2507] 1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ... }\cr
\code{ $ wgtstud   : num [1:2507] 34.5 34.5 34.5 34.5 34.5 ... }\cr
\code{ $ wgtschool : num [1:2507] 31.2 31.2 31.2 31.2 31.2 ... }\cr
\code{ $ stratum   : int [1:2507] 1 1 1 1 1 1 1 1 1 1 ... }\cr
\code{ $ female    : int [1:2507] 0 0 0 0 1 0 1 1 1 1 ... }\cr
\code{ $ migrant   : int [1:2507] 0 0 0 0 0 NA 0 0 0 0 ... }\cr
\code{ $ HISEI     : int [1:2507] 31 NA 25 27 27 NA NA 57 52 58 ... }\cr
\code{ $ eltausb   : int [1:2507] 2 NA 2 2 2 NA 2 1 2 1 ... }\cr
\code{ $ buch      : int [1:2507] 1 1 1 1 3 NA 4 2 5 4 ... }\cr
\code{ $ SK        : num [1:2507] 2.25 2.25 3 3 2.5 NA 2.5 3.25 3.5 2.5 ... }\cr
\code{ $ LF        : num [1:2507] 1.25 1.5 1 1 4 NA 2 3.5 3.75 2.25 ... }\cr
\code{ $ NSchueler : int [1:2507] 69 69 69 69 69 69 69 69 69 69 ... }\cr
\code{ $ NKlassen  : int [1:2507] 1 1 1 1 1 1 1 1 1 1 ... }\cr
\code{ $ SES_Schule: num [1:2507] 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 ... }\cr
\code{ $ E8WWLE    : num [1:2507] -3.311 -0.75 -3.311 0.769 1.006 ... }\cr
\code{ $ E8LWLE    : num [1:2507] -1.175 -1.731 -1.311 0.284 0.336 ...}\cr
}{\verb{'data.frame':	2507 obs. of  17 variables:
 $ idstud    : int [1:2507] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ idschool  : int [1:2507] 1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...
 $ wgtstud   : num [1:2507] 34.5 34.5 34.5 34.5 34.5 ...
 $ wgtschool : num [1:2507] 31.2 31.2 31.2 31.2 31.2 ...
 $ stratum   : int [1:2507] 1 1 1 1 1 1 1 1 1 1 ...
 $ female    : int [1:2507] 0 0 0 0 1 0 1 1 1 1 ...
 $ migrant   : int [1:2507] 0 0 0 0 0 NA 0 0 0 0 ...
 $ HISEI     : int [1:2507] 31 NA 25 27 27 NA NA 57 52 58 ...
 $ eltausb   : int [1:2507] 2 NA 2 2 2 NA 2 1 2 1 ...
 $ buch      : int [1:2507] 1 1 1 1 3 NA 4 2 5 4 ...
 $ SK        : num [1:2507] 2.25 2.25 3 3 2.5 NA 2.5 3.25 3.5 2.5 ...
 $ LF        : num [1:2507] 1.25 1.5 1 1 4 NA 2 3.5 3.75 2.25 ...
 $ NSchueler : int [1:2507] 69 69 69 69 69 69 69 69 69 69 ...
 $ NKlassen  : int [1:2507] 1 1 1 1 1 1 1 1 1 1 ...
 $ SES_Schule: num [1:2507] 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 ...
 $ E8WWLE    : num [1:2507] -3.311 -0.75 -3.311 0.769 1.006 ...
 $ E8LWLE    : num [1:2507] -1.175 -1.731 -1.311 0.284 0.336 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{data08I}: Datensatz zur Illustration der Bedeutung einer 
geeigneten Behandlung fehlender Werte und von Messfehlern.

\ifelse{latex}{%start latex
\itemize{
\item \code{index}: Laufindex.
\item \code{x}: Vollständig beobachteter Sozialstatus.
\item \code{theta}: Kompetenzwert.
\item \code{WLE}: WLE-Personenschätzer (vgl. Kapitel 6 im Band).
\item \code{SEWLE}: Messfehlers ("standard error") des WLE-Personenschätzers.
\item \code{X}: Sozialstatus mit teilweise fehlenden Werten. 
}}{\tabular{ll}{
\code{index} \tab Laufindex. \cr
\code{x} \tab Vollständig beobachteter Sozialstatus.\cr
\code{theta} \tab Kompetenzwert. \cr
\code{WLE} \tab WLE-Personenschätzer (vgl. Kapitel 6 im Band). \cr
\code{SEWLE} \tab Messfehlers ("standard error") des WLE-Personenschätzers. \cr
\code{X} \tab Sozialstatus mit teilweise fehlenden Werten. 
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	1500 obs. of  6 variables: }\cr
\code{ $ index: int [1:1500] 1 2 3 4 5 6 7 8 9 10 ... }\cr
\code{ $ x    : num [1:1500] 0.69 0.15 -0.13 -0.02 0.02 0.02 -0.56 0.14 -0.06 -1.41 ... }\cr
\code{ $ theta: num [1:1500] 2.08 -1.56 -0.65 -0.62 0.76 -1 1.12 0.08 0 -0.6 ... }\cr
\code{ $ WLE  : num [1:1500] 1.22 -2.9 -2.02 0.03 0.8 0.93 0.28 -0.77 -0.31 -1.76 ... }\cr
\code{ $ SEWLE: num [1:1500] 0.83 0.82 0.8 0.8 0.8 0.81 0.81 0.8 0.8 0.8 ... }\cr
\code{ $ X    : num [1:1500] 0.69 0.15 NA NA 0.02 0.02 -0.56 NA -0.06 -1.41 ...}\cr
}{\verb{'data.frame':	1500 obs. of  6 variables:
 $ index: int [1:1500] 1 2 3 4 5 6 7 8 9 10 ...
 $ x    : num [1:1500] 0.69 0.15 -0.13 -0.02 0.02 0.02 -0.56 0.14 -0.06 -1.41 ...
 $ theta: num [1:1500] 2.08 -1.56 -0.65 -0.62 0.76 -1 1.12 0.08 0 -0.6 ...
 $ WLE  : num [1:1500] 1.22 -2.9 -2.02 0.03 0.8 0.93 0.28 -0.77 -0.31 -1.76 ...
 $ SEWLE: num [1:1500] 0.83 0.82 0.8 0.8 0.8 0.81 0.81 0.8 0.8 0.8 ...
 $ X    : num [1:1500] 0.69 0.15 NA NA 0.02 0.02 -0.56 NA -0.06 -1.41 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{data08J}: Datensatz data08H nach Imputation der fehlenden Werte. 
Für die Beschreibung der Variablen, siehe \code{data08H}.

\ifelse{latex}{%start latex
\code{'data.frame':	2507 obs. of  14 variables: }\cr
\code{ $ idstud    : int [1:2507] 10010101 10010102 10010103 10010105 10010106 10010107 ... }\cr
\code{ $ wgtstud   : num [1:2507] 34.5 34.5 34.5 34.5 34.5 ... }\cr
\code{ $ female    : int [1:2507] 0 0 0 0 1 0 1 1 1 1 ... }\cr
\code{ $ migrant   : num [1:2507] 0 0 0 0 0 ... }\cr
\code{ $ HISEI     : num [1:2507] 31 56.8 25 27 27 ... }\cr
\code{ $ eltausb   : num [1:2507] 2 1.04 2 2 2 ... }\cr
\code{ $ buch      : num [1:2507] 1 1 1 1 3 ... }\cr
\code{ $ SK        : num [1:2507] 2.25 2.25 3 3 2.5 ... }\cr
\code{ $ LF        : num [1:2507] 1.25 1.5 1 1 4 ... }\cr
\code{ $ E8LWLE    : num [1:2507] -1.175 -1.731 -1.311 0.284 0.336 ... }\cr
\code{ $ E8WWLE    : num [1:2507] -3.311 -0.75 -3.311 0.769 1.006 ... }\cr
\code{ $ NSchueler : num [1:2507] 69 69 69 69 69 69 69 69 69 69 ... }\cr
\code{ $ NKlassen  : int [1:2507] 1 1 1 1 1 1 1 1 1 1 ... }\cr
\code{ $ SES_Schule: num [1:2507] 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 ...}\cr
}{\verb{'data.frame':	2507 obs. of  14 variables:
 $ idstud    : int [1:2507] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ wgtstud   : num [1:2507] 34.5 34.5 34.5 34.5 34.5 ...
 $ female    : int [1:2507] 0 0 0 0 1 0 1 1 1 1 ...
 $ migrant   : num [1:2507] 0 0 0 0 0 ...
 $ HISEI     : num [1:2507] 31 56.8 25 27 27 ...
 $ eltausb   : num [1:2507] 2 1.04 2 2 2 ...
 $ buch      : num [1:2507] 1 1 1 1 3 ...
 $ SK        : num [1:2507] 2.25 2.25 3 3 2.5 ...
 $ LF        : num [1:2507] 1.25 1.5 1 1 4 ...
 $ E8LWLE    : num [1:2507] -1.175 -1.731 -1.311 0.284 0.336 ...
 $ E8WWLE    : num [1:2507] -3.311 -0.75 -3.311 0.769 1.006 ...
 $ NSchueler : num [1:2507] 69 69 69 69 69 69 69 69 69 69 ...
 $ NKlassen  : int [1:2507] 1 1 1 1 1 1 1 1 1 1 ...
 $ SES_Schule: num [1:2507] 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 0.57 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{data08K}: Datensatz mit Itemantworten der Schüler/innen zu den 
Testinstrumenten zu Hörverstehen und Schreiben.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{wgtstud}: Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2 im Band).
\item \code{E8LS*}: Itemantworten für Hörverstehen (vgl. Kapitel 6). 
\item \code{E8W*}: Itemantworten für Schreiben (vgl. Kapitel 7).
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{wgtstud} \tab Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2 im Band). \cr
\code{E8LS*} \tab Itemantworten für Hörverstehen (vgl. Kapitel 6). \cr
\code{E8W*} \tab Itemantworten für Schreiben (vgl. Kapitel 7).
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	2507 obs. of  99 variables: }\cr
\code{ $ idstud   : int [1:2507] 10010101 10010102 10010103 10010105 10010106 10010107 ... }\cr
\code{ $ wgtstud  : num [1:2507] 34.5 34.5 34.5 34.5 34.5 ... }\cr
\code{ $ E8LS0158 : int [1:2507] NA NA NA NA NA NA 0 0 NA NA ... }\cr
\code{ $ E8LS0165 : int [1:2507] 0 1 1 0 1 0 NA NA 1 0 ... }\cr
\code{ $ E8LS0166 : int [1:2507] 0 0 1 1 0 1 NA NA 1 1 ... }\cr
\code{ [...] }\cr
\code{ $ E8W014CC : int [1:2507] 0 0 0 3 5 2 NA NA NA NA ... }\cr
\code{ $ E8W014GR : int [1:2507] 0 0 0 3 5 3 NA NA NA NA ... }\cr
\code{ $ E8W014VOC: int [1:2507] 0 0 0 3 5 2 NA NA NA NA ...}\cr
}{\verb{'data.frame':	2507 obs. of  99 variables:
 $ idstud   : int [1:2507] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ wgtstud  : num [1:2507] 34.5 34.5 34.5 34.5 34.5 ...
 $ E8LS0158 : int [1:2507] NA NA NA NA NA NA 0 0 NA NA ...
 $ E8LS0165 : int [1:2507] 0 1 1 0 1 0 NA NA 1 0 ...
 $ E8LS0166 : int [1:2507] 0 0 1 1 0 1 NA NA 1 1 ...
 [...]
 $ E8W014CC : int [1:2507] 0 0 0 3 5 2 NA NA NA NA ...
 $ E8W014GR : int [1:2507] 0 0 0 3 5 3 NA NA NA NA ...
 $ E8W014VOC: int [1:2507] 0 0 0 3 5 2 NA NA NA NA ...
}%end verb
}%end else / end ifelse latex

}%end itemize
}%end format

\keyword{datasets}
