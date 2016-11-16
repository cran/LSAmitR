\name{datenKapitel03}
\alias{datenKapitel03}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 3, Standard-Setting
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 3, 
\emph{Standard-Setting}, im Herausgeberband Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel03)
}

\references{
  Luger-Bazinger, C., Freunberger, R. & Itzlinger-Bruneforth, U. (2016). 
  Standard-Setting. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 83--110). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 3}}.
}	


\format{
\code{datenKapitel03} ist eine Liste mit den vier Elementen \code{ratings}, 
\code{bookmarks}, \code{sdat} und \code{productive}, die Daten zu verschiedenen 
Methoden eines Standard-Settings beinhalten.

Normierte Personen- und Itemparameter entstammen einer Vorgängerstudie, in der 
die Parameter für das jeweils zu betrachtende Testinstrumente auf die 
Berichtsmetrik transformiert wurden (vgl. Kapitel 5, \emph{Testdesign}, und 
Kapitel 6, \emph{Skalierung und Linking}, im Band).

%%
\itemize{
\item \strong{ratings}: Daten aus der IDM-Methode (siehe Buchkapitel, 
Unterabschnitt 3.2.2).

\ifelse{latex}{%start latex
\itemize{
\item \code{task}: Itemnummer.
\item \code{Norm_rp23}: Itemparameter auf der Berichtsmetrik.
\item \code{Seite_OIB}: Seitenzahl im OIB.
\item \code{R01...R12}: Von der jeweiligen Expertin/dem jeweiligen Experten 
(Rater/in) zugeordnete Kompetenzstufe des Items.
}}{\tabular{ll}{
\code{task} \tab Itemnummer. \cr
\code{Norm_rp23} \tab Itemparameter auf der Berichtsmetrik. \cr
\code{Seite_OIB} \tab Seitenzahl im OIB.\cr
\code{R01...R12} \tab Von der jeweiligen Expertin/dem jeweiligen Experten 
(Rater/in) zugeordnete Kompetenzstufe des Items.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	60 obs. of  15 variables:}\cr
\code{ $ task     : chr [1:60] "E8RS89991" "E8RS14021" "E8RS16031" "E8RS14171" ...}\cr
\code{ $ Norm_rp23: num [1:60] 376 396 396 413 420 ...}\cr
\code{ $ Seite_OIB: int [1:60] 1 2 3 4 5 6 7 8 9 10 ...}\cr
\code{ $ R01      : int [1:60] 1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{ $ R02      : int [1:60] 1 1 1 2 1 2 2 1 2 2 ...}\cr
\code{ $ R03      : int [1:60] 1 2 1 2 1 2 2 1 1 2 ...}\cr
\code{ $ R04      : int [1:60] 1 1 1 1 2 1 1 1 2 1 ...}\cr
\code{ $ R05      : int [1:60] 2 2 1 2 1 1 2 1 2 2 ...}\cr
\code{ $ R06      : int [1:60] 1 1 1 1 2 1 2 1 2 2 ...}\cr
\code{ $ R07      : int [1:60] 1 1 1 1 1 1 1 1 1 2 ...}\cr
\code{ $ R08      : int [1:60] 2 2 1 2 2 2 2 1 2 2 ...}\cr
\code{ $ R09      : int [1:60] 2 1 1 1 1 1 2 1 2 2 ...}\cr
\code{ $ R10      : int [1:60] 1 2 1 1 1 1 1 1 2 1 ...}\cr
\code{ $ R11      : int [1:60] 2 2 1 1 2 2 2 1 2 1 ...}\cr
\code{ $ R12      : int [1:60] 1 2 1 2 3 2 2 1 1 2 ...}\cr
}{\verb{'data.frame':	60 obs. of  15 variables:
 $ task     : chr [1:60] "E8RS89991" "E8RS14021" "E8RS16031" "E8RS14171" ...
 $ Norm_rp23: num [1:60] 376 396 396 413 420 ...
 $ Seite_OIB: int [1:60] 1 2 3 4 5 6 7 8 9 10 ...
 $ R01      : int [1:60] 1 1 1 1 1 1 1 1 1 1 ...
 $ R02      : int [1:60] 1 1 1 2 1 2 2 1 2 2 ...
 $ R03      : int [1:60] 1 2 1 2 1 2 2 1 1 2 ...
 $ R04      : int [1:60] 1 1 1 1 2 1 1 1 2 1 ...
 $ R05      : int [1:60] 2 2 1 2 1 1 2 1 2 2 ...
 $ R06      : int [1:60] 1 1 1 1 2 1 2 1 2 2 ...
 $ R07      : int [1:60] 1 1 1 1 1 1 1 1 1 2 ...
 $ R08      : int [1:60] 2 2 1 2 2 2 2 1 2 2 ...
 $ R09      : int [1:60] 2 1 1 1 1 1 2 1 2 2 ...
 $ R10      : int [1:60] 1 2 1 1 1 1 1 1 2 1 ...
 $ R11      : int [1:60] 2 2 1 1 2 2 2 1 2 1 ...
 $ R12      : int [1:60] 1 2 1 2 3 2 2 1 1 2 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{bookmarks}: Daten aus der Bookmark-Methode (siehe Buchkapitel, 
Unterabschnitt 3.2.3).

\ifelse{latex}{%start latex
\itemize{
\item \code{Rater}: Rateridentifikator der Expertin/des Experten im Panel.
\item \code{Cut1}: Bookmark der Expertin/des Experten in Form einer Seite im OIB, 
wo ein Schüler an der Grenze zwischen der ersten und zweiten Stufe das Item 
nicht mehr sicher lösen könnte (für eine genauere Beschreibung der Stufen, siehe 
Buchkapitel).
\item \code{Cut2}: Entsprechender Bookmark für die Grenze zwischen zweiter und
dritter Stufe.
}}{\tabular{ll}{
\code{Rater} \tab Rateridentifikator der Expertin/des Experten im Panel. \cr
\code{Cut1} \tab Bookmark der Expertin/des Experten in Form einer Seite im OIB, 
wo ein Schüler an der Grenze zwischen der ersten und zweiten Stufe das Item 
nicht mehr sicher lösen könnte (für eine genauere Beschreibung der Stufen, siehe 
Buchkapitel).\cr
\code{Cut2} \tab Entsprechender Bookmark für die Grenze zwischen zweiter und
dritter Stufe.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	12 obs. of  3 variables:}\cr
\code{ $ Rater: chr [1:12] "R01" "R02" "R03" "R04" ...}\cr
\code{ $ Cut1 : int [1:12] 6 4 6 2 4 4 4 4 3 6 ...}\cr
\code{ $ Cut2 : int [1:12] 45 39 39 45 39 30 39 39 44 45 ...}\cr
}{\verb{'data.frame':	12 obs. of  3 variables:
 $ Rater: chr [1:12] "R01" "R02" "R03" "R04" ...
 $ Cut1 : int [1:12] 6 4 6 2 4 4 4 4 3 6 ...
 $ Cut2 : int [1:12] 45 39 39 45 39 30 39 39 44 45 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{sdat}: Plausible Values zum Berichten von Impact Data (siehe 
Buchkapitel, Unterabschnitt 3.2.4).

\ifelse{latex}{%start latex
\itemize{
\item \code{pid}: Schüleridentifikator.
\item \code{studwgt}: Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band).
\item \code{TPV1...TPV10}: Plausible Values der Schülerin/des Schülers auf der 
Berichtsmetrik (vgl. Kapitel 8, \emph{Fehlende Daten und Plausible Values}, im 
Band).
}}{\tabular{ll}{
\code{pid} \tab Schüleridentifikator. \cr
\code{studwgt} \tab Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band). \cr
\code{TPV1...TPV10} \tab Plausible Values der Schülerin/des Schülers auf der 
Berichtsmetrik (vgl. Kapitel 8, \emph{Fehlende Daten und Plausible Values}, im Band).
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	3500 obs. of  12 variables:}\cr
\code{ $ pid    : int [1:3500] 1 2 3 4 5 6 7 8 9 10 ...}\cr
\code{ $ studwgt: num [1:3500] 0.978 0.978 0.978 0.978 0.978 ...}\cr
\code{ $ TPV1   : num [1:3500] 635 562 413 475 427 ...}\cr
\code{ $ TPV2   : num [1:3500] 601 558 409 452 462 ...}\cr
\code{ $ TPV3   : num [1:3500] 512 555 383 444 473 ...}\cr
\code{ $ TPV4   : num [1:3500] 675 553 375 473 454 ...}\cr
\code{ $ TPV5   : num [1:3500] 595 553 384 471 457 ...}\cr
\code{ $ TPV6   : num [1:3500] 593 557 362 490 501 ...}\cr
\code{ $ TPV7   : num [1:3500] 638 518 292 460 490 ...}\cr
\code{ $ TPV8   : num [1:3500] 581 493 306 467 477 ...}\cr
\code{ $ TPV9   : num [1:3500] 609 621 333 448 462 ...}\cr
\code{ $ TPV10  : num [1:3500] 573 634 406 537 453 ...}\cr
}{\verb{'data.frame':	3500 obs. of  12 variables:
 $ pid    : int [1:3500] 1 2 3 4 5 6 7 8 9 10 ...
 $ studwgt: num [1:3500] 0.978 0.978 0.978 0.978 0.978 ...
 $ TPV1   : num [1:3500] 635 562 413 475 427 ...
 $ TPV2   : num [1:3500] 601 558 409 452 462 ...
 $ TPV3   : num [1:3500] 512 555 383 444 473 ...
 $ TPV4   : num [1:3500] 675 553 375 473 454 ...
 $ TPV5   : num [1:3500] 595 553 384 471 457 ...
 $ TPV6   : num [1:3500] 593 557 362 490 501 ...
 $ TPV7   : num [1:3500] 638 518 292 460 490 ...
 $ TPV8   : num [1:3500] 581 493 306 467 477 ...
 $ TPV9   : num [1:3500] 609 621 333 448 462 ...
 $ TPV10  : num [1:3500] 573 634 406 537 453 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{productive}: Daten aus der Contrasting-Groups-Methode (siehe 
Buchkapitel, Unterabschnitt 3.3.2).

\ifelse{latex}{%start latex
\itemize{
\item \code{Script}: Nummer des Schülertexts.
\item \code{Performance}: Personenparameter der Schülerin/des Schülers auf der 
Berichtsmetrik.
\item \code{R01...R10}: Von der jeweiligen Expertin/dem jeweiligen Experten 
(Rater/in) zugeordnete Kompetenzstufe der Performanz (\code{0} = untere Stufe, 
\code{1} = obere Stufe; für eine genauere Beschreibung der Stufen, siehe 
Buchkapitel).
}}{\tabular{ll}{
\code{Script} \tab Nummer des Schülertexts. \cr
\code{Performance} \tab Personenparameter der Schülerin/des Schülers auf der 
Berichtsmetrik. \cr
\code{R01...R10} \tab Von der jeweiligen Expertin/dem jeweiligen Experten 
(Rater/in) zugeordnete Kompetenzstufe der Performanz (\code{0} = untere Stufe, 
\code{1} = obere Stufe; für eine genauere Beschreibung der Stufen, siehe 
Buchkapitel).
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	45 obs. of  12 variables:}\cr
\code{ $ Script     : int [1:45] 1 2 3 4 5 6 7 8 9 10 ...}\cr
\code{ $ Performance: num [1:45] 211 260 269 308 321 ...}\cr
\code{ $ R01        : int [1:45] 1 0 0 1 0 0 0 0 0 0 ...}\cr
\code{ $ R02        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ R03        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ R04        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ R05        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ R06        : int [1:45] 1 0 0 0 0 0 1 0 0 0 ...}\cr
\code{ $ R07        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ R08        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ R09        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ R10        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...}\cr
}{\verb{'data.frame':	45 obs. of  12 variables:
 $ Script     : int [1:45] 1 2 3 4 5 6 7 8 9 10 ...
 $ Performance: num [1:45] 211 260 269 308 321 ...
 $ R01        : int [1:45] 1 0 0 1 0 0 0 0 0 0 ...
 $ R02        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
 $ R03        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
 $ R04        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
 $ R05        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
 $ R06        : int [1:45] 1 0 0 0 0 0 1 0 0 0 ...
 $ R07        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
 $ R08        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
 $ R09        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
 $ R10        : int [1:45] 0 0 0 0 0 0 0 0 0 0 ...
}%end verb
}%end else / end ifelse latex

}%end itemize
}%end format

\keyword{datasets}
