\name{datenKapitel09}
\alias{datenKapitel09}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 9, Fairer Vergleich in der Rueckmeldung
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 9, 
\emph{Fairer Vergleich in der Rückmeldung}, im Herausgeberband 
Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel09)
}

\references{
  Pham, G., Robitzsch, A., George, A. C. & Freunberger, R. (2016).
  Fairer Vergleich in der Rückmeldung. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 295--332). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 9}}.
}	

\format{
\code{datenKapitel09} ist ein singulärer vollständiger Datensatz. 

\itemize{\item \strong{{datenKapitel09}}: Datensatz mit sieben 
Kontextinformationen und 43 im Fairen Vergleich daraus abgeleiteten und 
berechneten Kenngrößen zu 244 Schulen (\code{\link{Kapitel 9}}).

\ifelse{latex}{%start latex
\itemize{
\item \code{idschool}: Schulenidentifikator.
\item \code{Stratum}: Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Kapitel 
2, \emph{Stichprobenziehung}, im Band).
\item \code{groesse}: Logarithmierte Schulgröße.
\item \code{TWLE}: Aggregierte Leistungsschätzer der Schüler in der Schule 
(abhängige Variable im Fairen Verlgeich).
\item \code{female}: Anteil an Mädchen in der Schule.
\item \code{mig}: Anteil an Schülerinnen und Schülern mit Migrationshintergrund.
\item \code{sozstat}: Mittlerer sozioökonomischer Status (SES).
\item \code{zgroesse...zsozzsoz}: z-Standardisierte Werte der entsprechenden 
Variablen und Interaktionen.
\item \code{expTWLE.*}: Nach den jeweiligen Modellen erwartete (aggregierte) 
Leistungswerte der Schulen unter Berücksichtigung des Schulkontexts.
\item \code{*.eb*}: Untere und obere Grenzen der Erwartungsbereiche (EB) der 
Schulen und Indikator der Lage der Schule zum Bereich (\code{-1} = unter dem EB,
\code{0} = im EB, \code{1} = über dem EB).
}}{
\tabular{ll}{
\code{idschool} \tab Schulenidentifikator. \cr
\code{Stratum} \tab Stratum der Schule. (\code{1:4} = Stratum 1 bis Stratum 4; 
für eine genauere Beschreibung der Strata, siehe Kapitel 
2, \emph{Stichprobenziehung}, im Band).\cr
\code{groesse} \tab Logarithmierte Schulgröße.\cr
\code{TWLE} \tab Aggregierte Leistungsschätzer der Schüler in der Schule 
(abhängige Variable im Fairen Verlgeich).\cr
\code{female} \tab Anteil an Mädchen in der Schule.\cr
\code{mig} \tab Anteil an Schülerinnen und Schülern mit Migrationshintergrund.\cr
\code{sozstat} \tab Mittlerer sozioökonomischer Status (SES).\cr
\code{zgroesse...zsozzsoz} \tab z-Standardisierte Werte der entsprechenden 
Variablen und Interaktionen.\cr
\code{expTWLE.*} \tab Nach den jeweiligen Modellen erwartete (aggregierte) 
Leistungswerte der Schulen unter Berücksichtigung des Schulkontexts.\cr
\code{*.eb*} \tab Untere und obere Grenzen der Erwartungsbereiche (EB) der 
Schulen und Indikator der Lage der Schule zum Bereich (\code{-1} = unter dem EB,
\code{0} = im EB, \code{1} = über dem EB).
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	244 obs. of  50 variables:}\cr
\code{ $ idschool       : int  1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 ...}\cr
\code{ $ stratum        : int  1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{ $ groesse        : num  2.48 2.64 2.71 2.83 2.89 ...}\cr
\code{ $ TWLE           : num  449 447 495 482 514 ...}\cr
\code{ $ female         : num  0.545 0.462 0.571 0.529 0.389 ...}\cr
\code{ $ mig            : num  0.0168 0.0769 0 0 0 ...}\cr
\code{ $ sozstat        : num  -1.034 -0.298 -0.413 -0.259 -0.197 ...}\cr
\code{ $ zgroesse       : num  -2.86 -2.54 -2.4 -2.14 -2.02 ...}\cr
\code{ [...]}\cr
\code{ $ expTWLE.OLS1   : num  431 475 481 489 485 ...}\cr
\code{ $ expTWLE.OLS2   : num  439 463 483 490 471 ...}\cr
\code{ $ expTWLE.Lasso1 : num  430 472 475 484 482 ...}\cr
\code{ $ expTWLE.Lasso2 : num  434 470 481 486 476 ...}\cr
\code{ [...]}\cr
\code{ $ expTWLE.np     : num  422 478 479 490 465 ...}\cr
\code{ [...]}\cr
\code{ $ OLS1.eblow31   : num  415 460 465 474 470 ...}\cr
\code{ $ OLS1.ebupp31   : num  446 491 496 505 501 ...}\cr
\code{ $ OLS1.pos.eb31  : int  1 -1 0 0 1 -1 -1 -1 0 0 ...}\cr
\code{ [...]}\cr
}{\verb{'data.frame':	244 obs. of  50 variables:
 $ idschool       : int  1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 ...
 $ stratum        : int  1 1 1 1 1 1 1 1 1 1 ...
 $ groesse        : num  2.48 2.64 2.71 2.83 2.89 ...
 $ TWLE           : num  449 447 495 482 514 ...
 $ female         : num  0.545 0.462 0.571 0.529 0.389 ...
 $ mig            : num  0.0168 0.0769 0 0 0 ...
 $ sozstat        : num  -1.034 -0.298 -0.413 -0.259 -0.197 ...
 $ zgroesse       : num  -2.86 -2.54 -2.4 -2.14 -2.02 ...
 [...]
 $ expTWLE.OLS1   : num  431 475 481 489 485 ...
 $ expTWLE.OLS2   : num  439 463 483 490 471 ...
 $ expTWLE.Lasso1 : num  430 472 475 484 482 ...
 $ expTWLE.Lasso2 : num  434 470 481 486 476 ...
 [...]
 $ expTWLE.np     : num  422 478 479 490 465 ...
 [...]
 $ OLS1.eblow31   : num  415 460 465 474 470 ...
 $ OLS1.ebupp31   : num  446 491 496 505 501 ...
 $ OLS1.pos.eb31  : int  1 -1 0 0 1 -1 -1 -1 0 0 ...
 [...]
}%end verb
}%end else / end ifelse latex

}%end itemize
}%end format

\keyword{datasets}
