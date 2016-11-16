\name{datenKapitel02}
\alias{datenKapitel02}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 2, Stichprobenziehung
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 2, 
\emph{Stichprobenziehung}, im Herausgeberband Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel02)
}

\references{
  George, A. C., Oberwimmer, K. & Itzlinger-Bruneforth, U. (2016). 
  Stichprobenziehung. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 51--81). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 2}}.
}	


\format{
\code{datenKapitel02} ist eine Liste mit den zwei Elementen \code{schueler} und 
\code{schule}, die auf Schulen- und Schülerebene alle für eine 
Stichprobenziehung und die Berechnung von Stichprobengewichten relevanten 
Informationen beinhalten.

Diese 51644 Schülerinnen und Schüler in 1327 Schulen -- verteilt über vier 
Strata -- stellen die Zielpopulation der im Band durchgeführten Analysen dar.

%%
\itemize{
\item \strong{schueler}: Schülerdatensatz.

\ifelse{latex}{%start latex
\itemize{
\item \code{SKZ}: Schulenidentifikator ("Schulkennzahl"). 
\item \code{klnr}: Nummer der Klasse innerhalb der Schule. 
\item \code{idclass}: Klassenidentifikator.
\item \code{idstud}: Schüleridentifikator.
\item \code{female}: Geschlecht (\code{1} = weiblich, \code{0} = männlich).
\item \code{Stratum}: Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Buchkapitel).
\item \code{teilnahme}: Information über die Teilnahme der Schülerin/ des Schülers 
an der Erhebung (\code{1} = nimmt teil, \code{0} = nimmt nicht teil). 
Information ist erst zum Zeitpunkt der Erhebung vorhanden (nicht schon bei 
der Stichprobenziehung) und wird zur Berechnung der Stichprobengewichte mit 
Ausfalladjustierung herangezogen (siehe Buchkapitel, Unterabschnitt 2.4.4).
}}{\tabular{ll}{
\code{SKZ} \tab Schulenidentifikator ("Schulkennzahl"). \cr
\code{klnr} \tab Nummer der Klasse innerhalb der Schule. \cr
\code{idclass} \tab Klassenidentifikator.\cr
\code{idstud} \tab Schüleridentifikator.\cr
\code{female} \tab Geschlecht (\code{1} = weiblich, \code{0} = männlich).\cr
\code{Stratum} \tab Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Buchkapitel).\cr
\code{teilnahme} \tab Information über die Teilnahme der Schülerin/ des Schülers 
an der Erhebung (\code{1} = nimmt teil, \code{0} = nimmt nicht teil). 
Information ist erst zum Zeitpunkt der Erhebung vorhanden (nicht schon bei 
der Stichprobenziehung) und wird zur Berechnung der Stichprobengewichte mit 
Ausfalladjustierung herangezogen (siehe 
Buchkapitel, Unterabschnitt 2.4.4).
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	51644 obs. of  7 variables:}\cr
\code{ $ SKZ      : int [1:51644] 10001 10001 10001 10001 10001 10001 10001 10001 10001 10001 ...}\cr
\code{ $ klnr     : int [1:51644] 1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{ $ idclass  : int [1:51644] 1000101 1000101 1000101 1000101 1000101 1000101 1000101 1000101 ...}\cr
\code{ $ idstud   : int [1:51644] 100010101 100010102 100010103 100010104 100010105 100010106 100010107 ...}\cr
\code{ $ female   : int [1:51644] 1 0 0 0 0 1 0 1 0 1 ...}\cr
\code{ $ Stratum  : int [1:51644] 1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{ $ teilnahme: int [1:51644] 1 1 1 1 0 1 1 1 1 1 ...}\cr
}{\verb{'data.frame':	51644 obs. of  7 variables:
 $ SKZ      : int [1:51644] 10001 10001 10001 10001 10001 10001 10001 10001 10001 10001 ...
 $ klnr     : int [1:51644] 1 1 1 1 1 1 1 1 1 1 ...
 $ idclass  : int [1:51644] 1000101 1000101 1000101 1000101 1000101 1000101 1000101 1000101 ...
 $ idstud   : int [1:51644] 100010101 100010102 100010103 100010104 100010105 100010106 100010107 ...
 $ female   : int [1:51644] 1 0 0 0 0 1 0 1 0 1 ...
 $ Stratum  : int [1:51644] 1 1 1 1 1 1 1 1 1 1 ...
 $ teilnahme: int [1:51644] 1 1 1 1 0 1 1 1 1 1 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{schule}: Schulendatensatz.

\ifelse{latex}{%start latex
\itemize{
\item \code{index}: Laufparameter.
\item \code{SKZ}: Schulenidentifikator ("Schulkennzahl").
\item \code{stratum}: Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Buchkapitel).
\item \code{NSchueler}: Anzahl Schüler/innen in der 4. Schulstufe der Schule.
\item \code{NKlassen}: Anzahl Klassen in der 4. Schulstufe der Schule.
}}{\tabular{ll}{
\code{index} \tab Laufparameter.\cr
\code{SKZ} \tab Schulenidentifikator ("Schulkennzahl"). \cr
\code{stratum} \tab Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Buchkapitel).\cr
\code{NSchueler} \tab Anzahl Schüler/innen in der 4. Schulstufe der Schule.\cr
\code{NKlassen} \tab Anzahl Klassen in der 4. Schulstufe der Schule.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	1327 obs. of  5 variables:}\cr
\code{ $ index    : int [1:1327] 1 2 3 4 5 6 7 8 9 10 ...}\cr
\code{ $ SKZ      : int [1:1327] 10204 10215 10422 11017 10257 10544 10548 10846 11127 10126 ...}\cr
\code{ $ stratum  : int [1:1327] 1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{ $ NSchueler: int [1:1327] 8 9 9 9 10 10 10 10 10 11 ...}\cr
\code{ $ NKlassen : int [1:1327] 1 1 1 1 1 1 1 2 1 1 ...}\cr
}{\verb{'data.frame':	1327 obs. of  5 variables:
 $ index    : int [1:1327] 1 2 3 4 5 6 7 8 9 10 ...
 $ SKZ      : int [1:1327] 10204 10215 10422 11017 10257 10544 10548 10846 11127 10126 ...
 $ stratum  : int [1:1327] 1 1 1 1 1 1 1 1 1 1 ...
 $ NSchueler: int [1:1327] 8 9 9 9 10 10 10 10 10 11 ...
 $ NKlassen : int [1:1327] 1 1 1 1 1 1 1 2 1 1 ...
}%end verb
}%end else / end ifelse latex

}%end itemize
}%end format


\keyword{datasets}
