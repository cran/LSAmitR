\name{datenKapitel06}
\alias{datenKapitel06}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 6, Skalierung und Linking
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 6, 
\emph{Skalierung und Linking}, im Herausgeberband 
Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel06)
}

\references{
  Trendtel, M., Pham, G. & Yanagida, T. (2016). Skalierung und Linking.
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 185--224). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 6}}.
}	


\format{
\code{datenKapitel06} ist eine Liste mit den fünf Elementen \code{dat}, 
\code{itembank}, \code{datTH1}, \code{itembankTH1} und \code{normdat}.

%%
\itemize{
\item \strong{dat}: Dichotome und polytome Itemantworten von 9885 Schülerinnen 
und Schülern im Multiple-Matrix-Design mit Stichprobengewichten und 
Testheftinformation.

\ifelse{latex}{%start latex
\itemize{
\item \code{index}: Laufindex.
\item \code{idstud}: Schüleridentifikator.
\item \code{wgtstud}: Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band).
\item \code{th}: Bearbeitetes Testheft.
\item \code{I1...I50}: Itemantworten.
}}{\tabular{ll}{
\code{index} \tab Laufindex. \cr
\code{idstud} \tab Schüleridentifikator.\cr
\code{wgtstud} \tab Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band). \cr
\code{th} \tab Bearbeitetes Testheft.\cr
\code{I1...I50} \tab Itemantworten.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	9885 obs. of  54 variables: }\cr
\code{ $ index  : int [1:9885] 1 2 3 4 5 6 7 8 9 10 ... }\cr
\code{ $ idstud : int [1:9885] 10010101 10010102 10010103 10010105 10010106 10010107 10010108 ... }\cr
\code{ $ wgtstud: num [1:9885] 34.5 34.5 34.5 34.5 34.5 ... }\cr
\code{ $ th     : chr [1:9885] "ER04" "ER03" "ER05" "ER02" ... }\cr
\code{ $ I1     : int [1:9885] 0 NA 1 NA 0 0 NA 1 NA 0 ... }\cr
\code{ $ I2     : int [1:9885] 0 NA 0 NA 0 0 NA 0 NA 0 ... }\cr
\code{ $ I3     : int [1:9885] NA 1 NA 1 NA NA 1 NA 1 NA ... }\cr
\code{ [...] }\cr
\code{ $ I49    : int [1:9885] 0 NA NA 4 NA NA 3 NA 3 NA ... }\cr
\code{ $ I50    : int [1:9885] NA 0 0 NA 1 2 NA 0 NA 2 ...}\cr
}{\verb{'data.frame':	9885 obs. of  54 variables:
 $ index  : int [1:9885] 1 2 3 4 5 6 7 8 9 10 ...
 $ idstud : int [1:9885] 10010101 10010102 10010103 10010105 10010106 10010107 10010108 ...
 $ wgtstud: num [1:9885] 34.5 34.5 34.5 34.5 34.5 ...
 $ th     : chr [1:9885] "ER04" "ER03" "ER05" "ER02" ...
 $ I1     : int [1:9885] 0 NA 1 NA 0 0 NA 1 NA 0 ...
 $ I2     : int [1:9885] 0 NA 0 NA 0 0 NA 0 NA 0 ...
 $ I3     : int [1:9885] NA 1 NA 1 NA NA 1 NA 1 NA ...
 [...]
 $ I49    : int [1:9885] 0 NA NA 4 NA NA 3 NA 3 NA ...
 $ I50    : int [1:9885] NA 0 0 NA 1 2 NA 0 NA 2 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{itembank}: Den Instrumentendaten zugrundeliegende Itembank mit 
klassifizierenden Item-Informationen (vgl. Kapitel 1, Testkonstruktion, im Band).

\ifelse{latex}{%start latex
\itemize{
%\code{index} \tab Laufindex. \cr
\item \code{Item}: Itemname.
\item \code{format}: Antwortformat des Items.
\item \code{focus}: Fokuskategorie des Items.%\code{focus.txt} \tab Beschreibung des Fokus. \cr%\code{focus.label} \tab Fokusname. \cr%\code{testlet} \tab Testletname des Items (gleichbedeutend mit zugewiesenem %Stimulus). \cr
\item \code{itemnr}: Itemidentifikator.
\item \code{N.subI}: Anzahl Subitems.
}}{\tabular{ll}{
%\code{index} \tab Laufindex. \cr
\code{Item} \tab Itemname. \cr
\code{format} \tab Antwortformat des Items. \cr
\code{focus} \tab Fokuskategorie des Items. \cr
%\code{focus.txt} \tab Beschreibung des Fokus. \cr
%\code{focus.label} \tab Fokusname. \cr
%\code{testlet} \tab Testletname des Items (gleichbedeutend mit zugewiesenem 
%Stimulus). \cr
\code{itemnr} \tab Itemidentifikator.\cr
\code{N.subI} \tab Anzahl Subitems.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	50 obs. of  5 variables: }\cr%\code{ $ index      : int [1:50] 1 2 3 4 5 6 7 8 9 10 ... }\cr
\code{ $ Item       : chr [1:50] "I1" "I2" "I3" "I4" ... }\cr
\code{ $ format     : chr [1:50] "MC4" "MC4" "MC4" "MC4" ... }\cr
\code{ $ focus      : int [1:50] 0 0 0 0 0 0 0 0 0 0 ... }\cr%\code{ $ focus.txt  : chr [1:50] "" "" "" "" ... }\cr%\code{ $ focus.label: chr [1:50] "RFocus0" "RFocus0" "RFocus0" "RFocus0" ... }\cr%\code{ $ testlet    : chr [1:50] "E8RS0007" "E8RS0011" "E8RS0012" "E8RS0023" ... }\cr
\code{ $ itemnr     : int [1:50] 1661 2011 2201 2231 2251 2421 2461 2891 2931 3131 ... }\cr
\code{ $ N.subI     : int [1:50] 1 1 1 1 1 1 1 1 1 1 ...}\cr
}{\verb{'data.frame':	50 obs. of  5 variables:
 $ index      : int [1:50] 1 2 3 4 5 6 7 8 9 10 ...
 $ Item       : chr [1:50] "I1" "I2" "I3" "I4" ...
 $ format     : chr [1:50] "MC4" "MC4" "MC4" "MC4" ...
 $ focus      : int [1:50] 0 0 0 0 0 0 0 0 0 0 ...% $ focus.txt  : chr [1:50] "" "" "" "" ...% $ focus.label: chr [1:50] "RFocus0" "RFocus0" "RFocus0" "RFocus0" ...% $ testlet    : chr [1:50] "E8RS0007" "E8RS0011" "E8RS0012" "E8RS0023" ...
 $ itemnr     : int [1:50] 1661 2011 2201 2231 2251 2421 2461 2891 2931 3131 ...
 $ N.subI     : int [1:50] 1 1 1 1 1 1 1 1 1 1 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{datTH1}: Teildatensatz mit Itemantworten der Subgruppe von 1637 
Schülerinnen und Schülern, die das erste Testheft bearbeitet haben.

\ifelse{latex}{%start latex
\itemize{
\item \code{index}: Laufindex.
\item \code{idstud}: Schüleridentifikator.
\item \code{wgtstud}: Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band).
\item \code{th}: Bearbeitetes Testheft.
\item \code{I1...I50}: Itemantworten.
}}{\tabular{ll}{
\code{index} \tab Laufindex. \cr
\code{idstud} \tab Schüleridentifikator.\cr
\code{wgtstud} \tab Stichprobengewicht der Schülerin/des Schülers (vgl. Kapitel 
2, \emph{Stichprobenziehung}, im Band). \cr
\code{th} \tab Bearbeitetes Testheft.\cr
\code{I1...I50} \tab Itemantworten.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	1637 obs. of  29 variables: }\cr
\code{ $ index  : int [1:1637] 8 10 12 23 24 34 41 46 54 57 ... }\cr
\code{ $ idstud : int [1:1637] 10010109 10010111 10020101 10020113 10020114 10030110 10040103 ... }\cr
\code{ $ wgtstud: num [1:1637] 34.5 34.5 29.2 29.2 29.2 ... }\cr
\code{ $ th     : chr [1:1637] "ER01" "ER01" "ER01" "ER01" ... }\cr
\code{ $ I1     : int [1:1637] 1 0 0 1 0 1 0 0 0 0 ... }\cr
\code{ $ I2     : int [1:1637] 0 0 0 1 0 0 1 0 0 1 ... }\cr
\code{ $ I6     : int [1:1637] 0 0 0 0 0 1 0 0 0 1 ... }\cr
\code{ [...] }\cr
\code{ $ I47    : int [1:1637] 0 2 0 2 0 0 2 1 0 1 ... }\cr
\code{ $ I50    : int [1:1637] 0 2 0 2 0 0 1 1 0 1 ...}\cr
}{\verb{'data.frame':	1637 obs. of  29 variables:
 $ index  : int [1:1637] 8 10 12 23 24 34 41 46 54 57 ...
 $ idstud : int [1:1637] 10010109 10010111 10020101 10020113 10020114 10030110 10040103 ...
 $ wgtstud: num [1:1637] 34.5 34.5 29.2 29.2 29.2 ...
 $ th     : chr [1:1637] "ER01" "ER01" "ER01" "ER01" ...
 $ I1     : int [1:1637] 1 0 0 1 0 1 0 0 0 0 ...
 $ I2     : int [1:1637] 0 0 0 1 0 0 1 0 0 1 ...
 $ I6     : int [1:1637] 0 0 0 0 0 1 0 0 0 1 ...
 [...]
 $ I47    : int [1:1637] 0 2 0 2 0 0 2 1 0 1 ...
 $ I50    : int [1:1637] 0 2 0 2 0 0 1 1 0 1 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{itembankTH1}: Itembank zum Testheft 1.

\ifelse{latex}{%start latex
\itemize{%\code{index} \tab Laufindex. \cr
\item \code{Item}: Itemname.
\item \code{format}: Antwortformat des Items.
\item \code{focus}: Fokuskategorie des Items.%\code{focus.txt} \tab Beschreibung des Fokus. \cr%\code{focus.label} \tab Fokusname. \cr%\code{testlet} \tab Testletname des Items (gleichbedeutend mit zugewiesenem %Stimulus). \cr
\item \code{itemnr}: Itemidentifikator.
\item \code{N.subI}: Anzahl Subitems.
}}{\tabular{ll}{
%\code{index} \tab Laufindex. \cr
\code{Item} \tab Itemname. \cr
\code{format} \tab Antwortformat des Items. \cr
\code{focus} \tab Fokuskategorie des Items. \cr
%\code{focus.txt} \tab Beschreibung des Fokus. \cr
%\code{focus.label} \tab Fokusname. \cr
%\code{testlet} \tab Testletname des Items (gleichbedeutend mit zugewiesenem 
%Stimulus). \cr
\code{itemnr} \tab Itemidentifikator.\cr
\code{N.subI} \tab Anzahl Subitems.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	25 obs. of  5 variables: }\cr
%\code{ $ index      : int [1:25] 1 2 6 9 10 12 14 16 18 20 ... }\cr
\code{ $ Item       : chr [1:25] "I1" "I2" "I6" "I9" ... }\cr
\code{ $ format     : chr [1:25] "MC4" "MC4" "MC4" "MC4" ... }\cr
\code{ $ focus      : int [1:25] 0 0 0 0 0 1 1 1 1 1 ... }\cr
%\code{ $ focus.txt  : chr [1:25] "" "" "" "" ... }\cr
%\code{ $ focus.label: chr [1:25] "RFocus0" "RFocus0" "RFocus0" "RFocus0" ... }\cr
%\code{ $ testlet    : chr [1:25] "E8RS0007" "E8RS0011" "E8RS0052" "E8RS0074" ... }\cr
\code{ $ itemnr     : int [1:25] 1661 2011 2421 2931 3131 3641 4491 4681 5621 5761 ... }\cr
\code{ $ N.subI     : int [1:25] 1 1 1 1 1 1 1 1 1 1 ...}\cr
}{\verb{'data.frame':	25 obs. of  5 variables:% $ index      : int [1:25] 1 2 6 9 10 12 14 16 18 20 ...
 $ Item       : chr [1:25] "I1" "I2" "I6" "I9" ...
 $ format     : chr [1:25] "MC4" "MC4" "MC4" "MC4" ...
 $ focus      : int [1:25] 0 0 0 0 0 1 1 1 1 1 ...% $ focus.txt  : chr [1:25] "" "" "" "" ...% $ focus.label: chr [1:25] "RFocus0" "RFocus0" "RFocus0" "RFocus0" ...% $ testlet    : chr [1:25] "E8RS0007" "E8RS0011" "E8RS0052" "E8RS0074" ...
 $ itemnr     : int [1:25] 1661 2011 2421 2931 3131 3641 4491 4681 5621 5761 ...
 $ N.subI     : int [1:25] 1 1 1 1 1 1 1 1 1 1 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{normdat}: Instrumentendaten einer Normierungsstudie (vgl. 
Kapitel 3, \emph{Standard-Setting}, und  Kapitel 5, \emph{Testdesign}, im Band) 
mit Ankeritems für die Illustration von Linkingmethoden.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{wgtstud}: Stichprobengewicht der Schülerin/des Schülers in der 
Normierungsstudie (es wird von einer vollständig randomisierten Stichprobe
ausgegangen, weshalb die Gewichte konstant 1 sind).
\item \code{th}: Testheft.
\item \code{I*}: Itemantworten zu Items, die in der zu linkenden Studie auch 
eingesetzt werden.
\item \code{J*}: Itemantworten zu Items, die in der zu linkenden Studie nicht 
verwendet werden.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator.\cr
\code{wgtstud} \tab Stichprobengewicht der Schülerin/des Schülers in der 
Normierungsstudie (es wird von einer vollständig randomisierten Stichprobe
ausgegangen, weshalb die Gewichte konstant 1 sind). \cr
\code{th} \tab Testheft.\cr
\code{I*} \tab Itemantworten zu Items, die in der zu linkenden Studie auch 
eingesetzt werden. \cr
\code{J*} \tab Itemantworten zu Items, die in der zu linkenden Studie nicht 
verwendet werden.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	3000 obs. of  327 variables: }\cr
\code{ $ idstud : int [1:3000] 1000 1005 1011 1014 1021 1024 1025 1026 1027 1028 ... }\cr
\code{ $ wgtstud: int [1:3000] 1 1 1 1 1 1 1 1 1 1 ... }\cr
\code{ $ th     : chr [1:3000] "E8R01" "E8R02" "E8R03" "E8R04" ... }\cr
\code{ $ J1     : int [1:3000] NA NA NA NA NA NA NA NA 0 NA ... }\cr
\code{ $ J2     : int [1:3000] NA NA 0 NA NA NA NA NA NA NA ... }\cr
\code{ $ J3     : int [1:3000] NA NA NA NA NA NA NA NA 0 NA ... }\cr
\code{ [...] }\cr
\code{ $ I39    : int [1:3000] NA NA NA NA NA NA NA NA NA NA ... }\cr
\code{ $ I40    : int [1:3000] NA NA NA NA NA NA NA NA 0 NA ...}\cr
}{\verb{'data.frame':	3000 obs. of  327 variables:
 $ idstud : int [1:3000] 1000 1005 1011 1014 1021 1024 1025 1026 1027 1028 ...
 $ wgtstud: int [1:3000] 1 1 1 1 1 1 1 1 1 1 ...
 $ th     : chr [1:3000] "E8R01" "E8R02" "E8R03" "E8R04" ...
 $ J1     : int [1:3000] NA NA NA NA NA NA NA NA 0 NA ...
 $ J2     : int [1:3000] NA NA 0 NA NA NA NA NA NA NA ...
 $ J3     : int [1:3000] NA NA NA NA NA NA NA NA 0 NA ...
 [...]
 $ I39    : int [1:3000] NA NA NA NA NA NA NA NA NA NA ...
 $ I40    : int [1:3000] NA NA NA NA NA NA NA NA 0 NA ...
}%end verb
}%end else / end ifelse latex

}%end subsection

}%end format

\keyword{datasets}
