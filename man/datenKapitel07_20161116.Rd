\name{datenKapitel07}
\alias{datenKapitel07}
\alias{datenKapitel07Ex}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 7, Statistische Analysen produktiver Kompetenzen
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 7, 
\emph{Statistische Analysen produktiver Kompetenzen}, im Herausgeberband 
Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel07)
}

\references{
  Freunberger, R., Robitzsch, A. & Luger-Bazinger, C. (2016). Statistische 
  Analysen produktiver Kompetenzen. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 225--258). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 7}}.
}	

\format{
\code{datenKapitel07} ist eine Liste mit den fünf Elementen \code{prodRat}, 
\code{prodPRat}, \code{prodRatL}, \code{prodRatEx} und \code{prodRatLEx}, 
zu unterschiedlichen Darstellungen von Ratings zu Schreib-Performanzen für 
das Kapitel wie auch die darin gestellten Übungsaufgaben.

%%
\itemize{
\item \strong{prodRat}: Bewertung der Texte von 9736 Schülerinnen und Schülern 
zu einer von 3 "long prompts" durch einen (oder mehrere) der 41 Raters.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{aufgabe}: 3 lange Schreibaufgaben.
\item \code{rater}: 41 Raters.
\item \code{TA}: Bewertung des Schülertexts auf der Dimension \emph{Task 
Achievement} anhand einer 8-stufigen Ratingskala.
\item \code{CC}: Bewertung des Schülertexts auf der Dimension \emph{Coherence and
Cohesion} anhand einer 8-stufigen Ratingskala.
\item \code{GR}: Bewertung des Schülertexts auf der Dimension \emph{Grammar} 
anhand einer 8-stufigen Ratingskala.
\item \code{VO}: Bewertung des Schülertexts auf der Dimension \emph{Vocabulary}
anhand einer 8-stufigen Ratingskala.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{aufgabe} \tab 3 lange Schreibaufgaben.\cr
\code{rater} \tab 41 Raters. \cr
\code{TA} \tab Bewertung des Schülertexts auf der Dimension \emph{Task 
Achievement} anhand einer 8-stufigen Ratingskala.\cr
\code{CC} \tab Bewertung des Schülertexts auf der Dimension \emph{Coherence and
Cohesion} anhand einer 8-stufigen Ratingskala.\cr
\code{GR} \tab Bewertung des Schülertexts auf der Dimension \emph{Grammar} 
anhand einer 8-stufigen Ratingskala.\cr
\code{VO} \tab Bewertung des Schülertexts auf der Dimension \emph{Vocabulary}
anhand einer 8-stufigen Ratingskala.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	10755 obs. of  7 variables:}\cr
\code{$ idstud : int [1:10755] 10010101 10010102 10010103 10010105 10010106 10010107 ...}\cr
\code{$ aufgabe: chr [1:10755] "E8W014" "E8W014" "E8W014" "E8W014" ...}\cr
\code{$ rater  : chr [1:10755] "R141" "R143" "R191" "R191" ...}\cr
\code{$ TA     : int [1:10755] 0 0 0 3 4 4 0 0 2 4 ...}\cr
\code{$ CC     : int [1:10755] 0 0 0 3 5 2 0 0 1 3 ...}\cr
\code{$ GR     : int [1:10755] 0 0 0 3 5 3 0 0 1 4 ...}\cr
\code{$ VO     : int [1:10755] 0 0 0 3 5 2 0 0 1 3 ...}\cr
}{\verb{'data.frame':	10755 obs. of  7 variables:
 $ idstud : int [1:10755] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ aufgabe: chr [1:10755] "E8W014" "E8W014" "E8W014" "E8W014" ...
 $ rater  : chr [1:10755] "R141" "R143" "R191" "R191" ...
 $ TA     : int [1:10755] 0 0 0 3 4 4 0 0 2 4 ...
 $ CC     : int [1:10755] 0 0 0 3 5 2 0 0 1 3 ...
 $ GR     : int [1:10755] 0 0 0 3 5 3 0 0 1 4 ...
 $ VO     : int [1:10755] 0 0 0 3 5 2 0 0 1 3 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{prodPRat}: Bewertung der Schülertexte von 841 Schülerinnen und 
Schülern durch \emph{Pseudoraters}.\cr 
Die Mehrfachkodierungen der Schülertexte werden auf zwei zufällige Raters 
reduziert (siehe Unterabschnitt 7.1 für eine Erläuterung).

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{aufgabe}: 3 lange Schreibaufgaben.
\item \code{TA_R1...VO_R1}: Bewertung des Schülertexts auf den Dimension 
\emph{Task Achievement} (\code{TA_*}), \emph{Coherence and Cohesion} 
(\code{CC_*}), \emph{Grammar} (\code{GR_*}) und \emph{Vocabulary} (\code{VO_*}) 
anhand einer 8-stufigen Ratingskala durch Pseudorater/in 1.
\item \code{TA_R2...VO_R2}: Bewertung des Schülertexts auf den Dimension 
\emph{Task Achievement} (\code{TA_*}), \emph{Coherence and Cohesion} 
(\code{CC_*}), \emph{Grammar} (\code{GR_*}) und \emph{Vocabulary} (\code{VO_*}) 
anhand einer 8-stufigen Ratingskala durch Pseudorater/in 2.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{aufgabe} \tab 3 lange Schreibaufgaben.\cr
\code{TA_R1...VO_R1} \tab Bewertung des Schülertexts auf den Dimension 
\emph{Task Achievement} (\code{TA_*}), \emph{Coherence and Cohesion} 
(\code{CC_*}), \emph{Grammar} (\code{GR_*}) und \emph{Vocabulary} (\code{VO_*}) 
anhand einer 8-stufigen Ratingskala durch Pseudorater/in 1.\cr
\code{TA_R2...VO_R2} \tab Bewertung des Schülertexts auf den Dimension 
\emph{Task Achievement} (\code{TA_*}), \emph{Coherence and Cohesion} 
(\code{CC_*}), \emph{Grammar} (\code{GR_*}) und \emph{Vocabulary} (\code{VO_*}) 
anhand einer 8-stufigen Ratingskala durch Pseudorater/in 2.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	841 obs. of  10 variables:}\cr
\code{$ idstud : int [1:841] 10010108 10010112 10030106 10030110 10030112 10050105 ...}\cr
\code{$ aufgabe: chr [1:841] "E8W006" "E8W006" "E8W010" "E8W006" ...}\cr
\code{$ TA_R1  : int [1:841] 0 1 5 2 4 6 2 4 0 5 ...}\cr
\code{$ CC_R1  : int [1:841] 0 1 5 2 6 5 2 6 0 3 ...}\cr
\code{$ GR_R1  : int [1:841] 0 0 5 1 5 5 2 6 0 1 ...}\cr
\code{$ VO_R1  : int [1:841] 0 2 4 1 5 5 3 6 0 2 ...}\cr
\code{$ TA_R2  : int [1:841] 0 0 3 4 4 6 5 2 0 5 ...}\cr
\code{$ CC_R2  : int [1:841] 0 0 2 2 4 5 2 3 0 2 ...}\cr
\code{$ GR_R2  : int [1:841] 0 0 2 1 5 5 3 4 0 2 ...}\cr
\code{$ VO_R2  : int [1:841] 0 0 3 2 5 6 4 3 0 2 ...}\cr
}{\verb{'data.frame':	841 obs. of  10 variables:
 $ idstud : int [1:841] 10010108 10010112 10030106 10030110 10030112 10050105 ...
 $ aufgabe: chr [1:841] "E8W006" "E8W006" "E8W010" "E8W006" ...
 $ TA_R1  : int [1:841] 0 1 5 2 4 6 2 4 0 5 ...
 $ CC_R1  : int [1:841] 0 1 5 2 6 5 2 6 0 3 ...
 $ GR_R1  : int [1:841] 0 0 5 1 5 5 2 6 0 1 ...
 $ VO_R1  : int [1:841] 0 2 4 1 5 5 3 6 0 2 ...
 $ TA_R2  : int [1:841] 0 0 3 4 4 6 5 2 0 5 ...
 $ CC_R2  : int [1:841] 0 0 2 2 4 5 2 3 0 2 ...
 $ GR_R2  : int [1:841] 0 0 2 1 5 5 3 4 0 2 ...
 $ VO_R2  : int [1:841] 0 0 3 2 5 6 4 3 0 2 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{prodRatL}: Bewertung der Schülertexte im \emph{Long Format}.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{aufgabe}: 3 lange Schreibaufgaben.
\item \code{rater}: 41 Raters.
\item \code{item}: Dimension.
\item \code{response}: Rating zur Aufgabe in jeweiliger Dimension.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{aufgabe} \tab 3 lange Schreibaufgaben.\cr
\code{rater} \tab 41 Raters. \cr
\code{item} \tab Dimension. \cr
\code{response} \tab Rating zur Aufgabe in jeweiliger Dimension.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	43020 obs. of  5 variables:}\cr
\code{$ idstud  : int [1:43020] 10010101 10010102 10010103 10010105 10010106 10010107 ...}\cr
\code{$ aufgabe : chr [1:43020] "E8W014" "E8W014" "E8W014" "E8W014" ...}\cr
\code{$ rater   : chr [1:43020] "R141" "R143" "R191" "R191" ...}\cr
\code{$ item    : Factor w/ 4 levels "TA","CC","GR",..: 1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{$ response: int [1:43020] 0 0 0 3 4 4 0 0 2 4 ...}\cr
}{\verb{'data.frame':	43020 obs. of  5 variables:
 $ idstud  : int [1:43020] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ aufgabe : chr [1:43020] "E8W014" "E8W014" "E8W014" "E8W014" ...
 $ rater   : chr [1:43020] "R141" "R143" "R191" "R191" ...
 $ item    : Factor w/ 4 levels "TA","CC","GR",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ response: int [1:43020] 0 0 0 3 4 4 0 0 2 4 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{prodRatEx}: Übungsdatensatz: Bewertung der Texte von 9748 
Schülerinnen und Schülern zu einer von 3 "short prompts" durch einen (oder 
mehrere) der 41 Raters.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{aufgabe}: 3 Schreibaufgaben.
\item \code{rater}: 41 Raters.
\item \code{TA}: Bewertung des Schülertexts auf der Dimension \emph{Task 
Achievement} anhand einer 8-stufigen Ratingskala.
\item \code{CC}: Bewertung des Schülertexts auf der Dimension \emph{Coherence and
Cohesion} anhand einer 8-stufigen Ratingskala:
\item \code{GR}: Bewertung des Schülertexts auf der Dimension \emph{Grammar} 
anhand einer 8-stufigen Ratingskala.
\item \code{VO}: Bewertung des Schülertexts auf der Dimension \emph{Vocabulary}
anhand einer 8-stufigen Ratingskala.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{aufgabe} \tab 3 Schreibaufgaben.\cr
\code{rater} \tab 41 Raters. \cr
\code{TA} \tab Bewertung des Schülertexts auf der Dimension \emph{Task 
Achievement} anhand einer 8-stufigen Ratingskala.\cr
\code{CC} \tab Bewertung des Schülertexts auf der Dimension \emph{Coherence and
Cohesion} anhand einer 8-stufigen Ratingskala.\cr
\code{GR} \tab Bewertung des Schülertexts auf der Dimension \emph{Grammar} 
anhand einer 8-stufigen Ratingskala.\cr
\code{VO} \tab Bewertung des Schülertexts auf der Dimension \emph{Vocabulary}
anhand einer 8-stufigen Ratingskala.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	10643 obs. of  7 variables:}\cr
\code{$ idstud : int [1:10643] 10010101 10010102 10010103 10010105 10010106 10010107 ...}\cr
\code{$ aufgabe: chr [1:10643] "E8W001" "E8W011" "E8W001" "E8W011" ...}\cr
\code{$ rater  : chr [1:10643] "R123" "R132" "R132" "R113" ...}\cr
\code{$ TA     : int [1:10643] 0 3 0 4 3 2 0 1 1 5 ...}\cr
\code{$ CC     : int [1:10643] 0 3 0 4 2 2 0 1 2 3 ...}\cr
\code{$ GR     : int [1:10643] 0 3 0 4 3 1 0 1 3 1 ...}\cr
\code{$ VO     : int [1:10643] 0 3 0 4 3 2 0 1 3 1 ...}\cr
}{\verb{'data.frame':	10643 obs. of  7 variables:
 $ idstud : int [1:10643] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ aufgabe: chr [1:10643] "E8W001" "E8W011" "E8W001" "E8W011" ...
 $ rater  : chr [1:10643] "R123" "R132" "R132" "R113" ...
 $ TA     : int [1:10643] 0 3 0 4 3 2 0 1 1 5 ...
 $ CC     : int [1:10643] 0 3 0 4 2 2 0 1 2 3 ...
 $ GR     : int [1:10643] 0 3 0 4 3 1 0 1 3 1 ...
 $ VO     : int [1:10643] 0 3 0 4 3 2 0 1 3 1 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{prodRatLEx}: Übungsdatensatz: Bewertung der Schülertexte im 
\emph{Long Format}.

\ifelse{latex}{%start latex
\itemize{
\item \code{idstud}: Schüleridentifikator.
\item \code{aufgabe}: 3 kurze Schreibaufgaben.
\item \code{rater}: 41 Raters.
\item \code{item}: Dimension.
\item \code{response}: Rating zur Aufgabe in jeweiliger Dimension.
}}{\tabular{ll}{
\code{idstud} \tab Schüleridentifikator. \cr
\code{aufgabe} \tab 3 kurze Schreibaufgaben.\cr
\code{rater} \tab 41 Raters. \cr
\code{item} \tab Dimension. \cr
\code{response} \tab Rating zur Aufgabe in jeweiliger Dimension.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	42572 obs. of  5 variables:}\cr
\code{$ idstud  : int [1:42572] 10010101 10010102 10010103 10010105 10010106 10010107 ...}\cr
\code{$ aufgabe : chr [1:42572] "E8W001" "E8W011" "E8W001" "E8W011" ...}\cr
\code{$ rater   : chr [1:42572] "R123" "R132" "R132" "R113" ...}\cr
\code{$ item    : Factor w/ 4 levels "TA","CC","GR",..: 1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{$ response: int [1:42572] 0 3 0 4 3 2 0 1 1 5 ...}\cr
}{\verb{'data.frame':	42572 obs. of  5 variables:
 $ idstud  : int [1:42572] 10010101 10010102 10010103 10010105 10010106 10010107 ...
 $ aufgabe : chr [1:42572] "E8W001" "E8W011" "E8W001" "E8W011" ...
 $ rater   : chr [1:42572] "R123" "R132" "R132" "R113" ...
 $ item    : Factor w/ 4 levels "TA","CC","GR",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ response: int [1:42572] 0 3 0 4 3 2 0 1 1 5 ...
}%end verb
}%end else / end ifelse latex

}%end itemize
}%end format

\keyword{datasets}
