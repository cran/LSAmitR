\name{datenKapitel04}
\alias{datenKapitel04}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 4, Differenzielles Itemfunktionieren in Subgruppen
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 4, 
\emph{Differenzielles Itemfunktionieren in Subgruppen}, im Herausgeberband 
Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel04)
}

\references{
  Trendtel, M., Schwabe, F. & Fellinger, R. (2016). Differenzielles 
  Itemfunktionieren in Subgruppen. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 111--147). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 4}}.
}	


\format{
\code{datenKapitel04} ist eine Liste mit den drei Elementen \code{dat}, 
\code{dat.th1} und \code{ibank}.

%%
\itemize{
\item \strong{dat}: Dichotome Itemantworten von 9884 Schülerinnen und Schülern 
im Multiple-Matrix-Design mit Gruppierungsmerkmal.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator. 
\item \code{AHS}: Besuch einer allgemeinbildenden höheren Schulen (\code{AHS} = 1), bzw. 
allgemeinbildenden Pflichtschule (\code{AHS} = 0).
\item \code{E8RS*}: dichotom bewertete Itemantworten zu Items 
\code{E8RS01661} bis \code{E8RS79931}.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{AHS} \tab Besuch einer allgemeinbildenden höheren Schulen (\code{AHS} = 1), bzw. 
allgemeinbildenden Pflichtschule (\code{AHS} = 0).\cr
\code{E8RS*} \tab dichotom bewertete Itemantworten zu Items 
\code{E8RS01661} bis \code{E8RS79931}.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	9884 obs. of  52 variables:}\cr
\code{ $ idstud   : int [1:9884] 10010101 10010102 10010103 10010105 10010106 10010107 ...}\cr
\code{ $ AHS      : int [1:9884] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ E8RS01661: int [1:9884] 0 NA 1 NA 0 0 NA 1 NA 0 ...}\cr
\code{ $ E8RS02011: int [1:9884] 0 NA 0 NA 0 0 NA 0 NA 0 ...}\cr
\code{ $ E8RS02201: int [1:9884] NA 1 NA 1 NA NA 1 NA 1 NA ...}\cr
\code{ [...]}\cr
\code{ $ E8RS79641: int [1:9884] NA 0 0 0 0 0 NA NA 0 NA ...}\cr
\code{ $ E8RS79931: int [1:9884] 0 NA NA NA NA NA 0 1 NA 0 ...}\cr
}{\verb{'data.frame':	9884 obs. of  52 variables:
 $ idstud   : int [1:9884] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ AHS      : int [1:9884] 0 0 0 0 0 0 0 0 0 0 ...
 $ E8RS01661: int [1:9884] 0 NA 1 NA 0 0 NA 1 NA 0 ...
 $ E8RS02011: int [1:9884] 0 NA 0 NA 0 0 NA 0 NA 0 ...
 $ E8RS02201: int [1:9884] NA 1 NA 1 NA NA 1 NA 1 NA ...
 [...]
 $ E8RS79641: int [1:9884] NA 0 0 0 0 0 NA NA 0 NA ...
 $ E8RS79931: int [1:9884] 0 NA NA NA NA NA 0 1 NA 0 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{dat.th1}: Teildatensatz mit Itemantworten der Subgruppe von 1636 
Schülerinnen und Schülern, die das erste Testheft (vgl. \code{ibank}) bearbeitet 
haben.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{AHS}: Besuch einer allgemeinbildenden höheren Schulen (\code{AHS} = 1), bzw. 
allgemeinbildenden Pflichtschule (\code{AHS} = 0).
\item \code{E8RS*}: dichotom bewertete Itemantworten zu Items 
\code{E8RS01661} bis \code{E8RS79931}.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{AHS} \tab Besuch einer allgemeinbildenden höheren Schulen (\code{AHS} = 1), bzw. 
allgemeinbildenden Pflichtschule (\code{AHS} = 0).\cr
\code{E8RS*} \tab dichotom bewertete Itemantworten zu Items 
\code{E8RS01661} bis \code{E8RS79931}.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	1636 obs. of  27 variables:}\cr
\code{ $ idstud   : int [1:1636] 10010109 10010111 10020101 10020113 10020114 10030110 ...}\cr
\code{ $ AHS      : int [1:1636] 0 0 0 0 0 0 0 0 0 0 ...}\cr
\code{ $ E8RS01661: int [1:1636] 1 0 0 1 0 1 0 0 0 0 ...}\cr
\code{ $ E8RS02011: int [1:1636] 0 0 0 1 0 0 1 0 0 1 ...}\cr
\code{ $ E8RS02421: int [1:1636] 0 0 0 0 0 1 0 0 0 1 ...}\cr
\code{ [...]}\cr
\code{ $ E8RS28551: int [1:1636] 1 0 1 0 0 0 1 1 0 0 ...}\cr
\code{ $ E8RS79931: int [1:1636] 1 0 0 0 0 0 0 0 0 1 ...}\cr
}{\verb{'data.frame':	1636 obs. of  27 variables:
 $ idstud   : int [1:1636] 10010109 10010111 10020101 10020113 10020114 10030110 ...
 $ AHS      : int [1:1636] 0 0 0 0 0 0 0 0 0 0 ...
 $ E8RS01661: int [1:1636] 1 0 0 1 0 1 0 0 0 0 ...
 $ E8RS02011: int [1:1636] 0 0 0 1 0 0 1 0 0 1 ...
 $ E8RS02421: int [1:1636] 0 0 0 0 0 1 0 0 0 1 ...
 [...]
 $ E8RS28551: int [1:1636] 1 0 1 0 0 0 1 1 0 0 ...
 $ E8RS79931: int [1:1636] 1 0 0 0 0 0 0 0 0 1 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{ibank}: Beispielhafte Itembank mit klassifizierenden 
Item-Informationen (vgl. Kapitel 1, \emph{Testkonstruktion}, im Band).

\ifelse{latex}{%start latex
\itemize{
\item \code{task}: Itemname.
\item \code{format}: Antwortformat des Items.
\item \code{focus}: Fokuskategorie des Items.
%\code{focus.txt} \tab Beschreibung des Fokus. \cr
%\code{focus.label} \tab Fokusname. \cr
%\code{testlet} \tab Bezeichnung des Testlet.\cr
\item \code{itemnr}: Itemidentifikator.
}}{\tabular{ll}{
\code{task} \tab Itemname. \cr
\code{format} \tab Antwortformat des Items. \cr
\code{focus} \tab Fokuskategorie des Items. \cr
%\code{focus.txt} \tab Beschreibung des Fokus. \cr
%\code{focus.label} \tab Fokusname. \cr
%\code{testlet} \tab Bezeichnung des Testlet.\cr
\code{itemnr} \tab Itemidentifikator.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	50 obs. of  4 variables:}\cr
\code{ $ task       : chr  "E8RS01661" "E8RS02011" "E8RS02201" "E8RS02231" ...}\cr
\code{ $ format     : chr  "MC4" "MC4" "MC4" "MC4" ...}\cr
\code{ $ focus      : int  0 0 0 0 0 0 0 0 0 0 ...}\cr%\code{ $ focus.txt  : chr  "" "" "" "" ...}\cr %\code{ $ focus.label: chr  "RFocus0" "RFocus0" "RFocus0" "RFocus0" ...}\cr
\code{ $ itemnr     : int  1661 2011 2201 2231 2251 2421 2461 2891 2931 3131 ...}\cr
}{\verb{'data.frame':	50 obs. of  4 variables:
 $ task       : chr  "E8RS01661" "E8RS02011" "E8RS02201" "E8RS02231" ...
 $ format     : chr  "MC4" "MC4" "MC4" "MC4" ...
 $ focus      : int  0 0 0 0 0 0 0 0 0 0 ...% $ focus.txt  : chr  "" "" "" "" ...% $ focus.label: chr  "RFocus0" "RFocus0" "RFocus0" "RFocus0" ...
 $ itemnr     : int  1661 2011 2201 2231 2251 2421 2461 2891 2931 3131 ...
}%end verb
}%end else / end ifelse latex


}%end itemize
}%end format


\keyword{datasets}
