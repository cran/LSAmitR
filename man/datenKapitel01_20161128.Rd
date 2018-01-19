\name{datenKapitel01}
\alias{datenKapitel01}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 1, Testkonstruktion
}


\description{
Hier befindet sich die Dokumentation der in Kapitel 1, 
\emph{Testkonstruktion}, im Herausgeberband Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}


\usage{
data(datenKapitel01)
}

\references{
  Itzlinger-Bruneforth, U., Kuhn, J.-T. & Kiefer, T. (2016). Testkonstruktion. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 21--50). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 1}}.
}	

\format{

\code{datenKapitel01} ist eine Liste mit den vier Elementen \code{pilotScored}, 
\code{pilotItems}, \code{pilotRoh} und \code{pilotMM}, die einer fiktiven
Pilotierung entstammen.


\itemize{
\item \strong{pilotScored}: Rekodierte Instrumentendaten der Pilotierung (vgl. 
\code{pilotItems}).

\ifelse{latex}{%start latex
\itemize{
\item \code{sidstud}: Schüleridentifikator.
\item \code{female}: Geschlecht (\code{"w"} = weiblich, \code{"m"} = männlich).
\item \code{form}:  das von der Schülerin/dem Schüler bearbeitete Testheft.
\item \code{E8RS*}: dichotom und polytom bewertete Itemantworten auf Items 
\code{E8RS13151} bis \code{E8RS7993} (\code{0:4} = Score der Itemantwort, 
\code{8} = Itemantwort "nicht bewertbar", \code{9} = "omitted response").
}}{\tabular{ll}{
\code{sidstud} \tab Schüleridentifikator.\cr
\code{female} \tab Geschlecht (\code{"w"} = weiblich, \code{"m"} = männlich).\cr
\code{form} \tab das von der Schülerin/dem Schüler bearbeitete Testheft.\cr
\code{E8RS*} \tab dichotom und polytom bewertete Itemantworten auf Items 
\code{E8RS13151} bis \code{E8RS7993} (\code{0:4} = Score der Itemantwort, 
\code{8} = Itemantwort "nicht bewertbar", \code{9} = "omitted response").
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	2504 obs. of  163 variables:}\cr
\code{ $ sidstud  : int  1052 1057 1058 1064 1068 1073 1074 1076 1078 1080 ...}\cr
\code{ $ female   : chr  "w" "w" "w" "w" ...}\cr
\code{ $ form     : chr  "PR019" "PR020" "PR021" "PR022" ...}\cr
\code{ $ E8RS13151: int  NA NA NA NA NA NA NA NA NA NA ...}\cr
\code{ $ E8RS13171: int  NA NA 1 NA NA NA NA 1 NA NA ...}\cr
\code{ $ E8RS13491: int  NA NA NA NA 0 NA NA NA NA NA ...}\cr
\code{ $ E8RS13641: int  0 NA NA NA NA NA 0 NA NA NA ...}\cr
\code{ [...] }\cr
\code{ $ E8RS7929: int  NA NA 0 NA NA NA NA NA NA NA ...}\cr
\code{ $ E8RS7940: int  NA NA NA NA NA 0 NA NA NA NA ...}\cr
\code{ $ E8RS7955: int  NA 0 NA NA NA NA NA NA 2 NA ...}\cr
\code{ $ E8RS7993: int  NA NA NA 0 NA NA 2 NA NA NA ...}\cr
}{\verb{'data.frame':	2504 obs. of  163 variables:
 $ sidstud  : int  1052 1057 1058 1064 1068 1073 1074 1076 1078 1080 ...
 $ female   : chr  "w" "w" "w" "w" ...
 $ form     : chr  "PR019" "PR020" "PR021" "PR022" ...
 $ E8RS13151: int  NA NA NA NA NA NA NA NA NA NA ...
 $ E8RS13171: int  NA NA 1 NA NA NA NA 1 NA NA ...
 $ E8RS13491: int  NA NA NA NA 0 NA NA NA NA NA ...
 $ E8RS13641: int  0 NA NA NA NA NA 0 NA NA NA ...
 [...] 
 $ E8RS7929: int  NA NA 0 NA NA NA NA NA NA NA ...
 $ E8RS7940: int  NA NA NA NA NA 0 NA NA NA NA ...
 $ E8RS7955: int  NA 0 NA NA NA NA NA NA 2 NA ...
 $ E8RS7993: int  NA NA NA 0 NA NA 2 NA NA NA ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{pilotItems}: Itembank der Pilotierung.

\ifelse{latex}{%start latex
\itemize{
\item \code{testlet}: Testletname des Items (gleichbedeutend mit zugewiesenem 
Stimulus).
\item \code{item}: Itemname. 
\item \code{format}: Antwortformat. 
\item \code{focus}: Fokus des Testitems. 
\item \code{focusLabel}: Bezeichnung des Fokus des Testitems. 
\item \code{topic}: Themengebiet des Stimulus. 
\item \code{no.Words}: Anzahl Wörter im Stimulus. 
\item \code{key}: Indikator der richtigen Antwort (\code{1:3} = korrekte 
Antwortoption bei Multiple-Choice Items, \code{A:F} = korrekt zuzuordnende 
Antwortoption bei Matching-Items, \code{""} = korrekte Antworten für Items im 
Antwortformat "open gap-fill" werden in Form von Coding-Guides ausgebildeten 
Kodiererinnen/Kodierern vorgelegt; vgl. Buchkapite). 
\item \code{maxScore}: Maximal zu erreichende Punkte. 
\item \code{PR*}: Positionen der Items in den Testheften \code{PR001} bis 
\code{PR056}.
}}{\tabular{ll}{
\code{testlet} \tab Testletname des Items (gleichbedeutend mit zugewiesenem 
Stimulus). \cr
\code{item} \tab Itemname. \cr
\code{format} \tab Antwortformt. \cr
\code{focus} \tab Fokus des Testitems. \cr
\code{focusLabel} \tab Bezeichnung des Fokus des Testitems. \cr
\code{topic} \tab Thema. \cr
\code{no.Words} \tab Anzahl Wörter im Stimulus. \cr
\code{key} \tab Indikator der richtigen Antwort (\code{1:3} = korrekte 
Antwortoption bei Multiple-Choice Items, \code{A:F} = korrekt zuzuordnende 
Antwortoption bei Matching-Items, \code{""}= korrekte Antworten für Items im 
Antwortformat "open gap-fill" werden in Form von Coding-Guides ausgebildeten 
Kodiererinnen/Kodierern vorgelegt). \cr
\code{maxScore} \tab Maximal zu erreichende Punkte. \cr
\code{PR*} \tab Positionen der Items in den Testheften \code{PR001} bis 
\code{PR056}.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	320 obs. of  65 variables:}\cr
\code{ $ testlet   : chr  "E8RS1315" "E8RS1317" "E8RS1340" "E8RS1349" ...}\cr
\code{ $ item      : chr  "E8RS13151" "E8RS13171" "E8RS13401" "E8RS13491" ...}\cr
\code{ $ format    : chr  "MC3" "MC3" "MC3" "MC3" ...}\cr
\code{ $ focus     : int  1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{ $ focusLabel: chr  "RFocus1" "RFocus1" "RFocus1" "RFocus1" ...}\cr
\code{ $ topic     : chr  "Interkulturelle und landeskundliche Aspekte" "Familie und Freunde" ...}\cr
\code{ $ no.Words  : int  24 24 29 32 10 33 22 41 10 37 ...}\cr
\code{ $ key       : chr  "1" "3" "2" "2" ...}\cr
\code{ $ maxScore  : int  1 1 1 1 1 1 1 1 1 1 ...}\cr
\code{ $ PR001     : int  NA NA NA 10 NA NA NA NA NA NA ...}\cr
\code{ $ PR002     : int  5 NA 6 NA 7 NA NA 8 NA NA ...}\cr
\code{ $ PR003     : int  NA NA NA 6 NA NA NA NA NA NA ...}\cr
\code{ $ PR004     : int  NA NA NA 10 NA NA NA NA NA NA ...}\cr
\code{ [...] }\cr
\code{ $ PR054     : int  NA NA NA NA NA NA NA NA NA NA ...}\cr
\code{ $ PR055     : int  NA 9 NA NA NA NA 10 NA NA 11 ...}\cr
\code{ $ PR056     : int  NA NA NA NA NA NA NA NA 6 NA ...}\cr
}{\verb{'data.frame':	320 obs. of  65 variables:
 $ testlet   : chr  "E8RS1315" "E8RS1317" "E8RS1340" "E8RS1349" ...
 $ item      : chr  "E8RS13151" "E8RS13171" "E8RS13401" "E8RS13491" ...
 $ format    : chr  "MC3" "MC3" "MC3" "MC3" ...
 $ focus     : int  1 1 1 1 1 1 1 1 1 1 ...
 $ focusLabel: chr  "RFocus1" "RFocus1" "RFocus1" "RFocus1" ...
 $ topic     : chr  "Interkulturelle und landeskundliche Aspekte" "Familie und Freunde" ...
 $ no.Words  : int  24 24 29 32 10 33 22 41 10 37 ...
 $ key       : chr  "1" "3" "2" "2" ...
 $ maxScore  : int  1 1 1 1 1 1 1 1 1 1 ...
 $ PR001     : int  NA NA NA 10 NA NA NA NA NA NA ...
 $ PR002     : int  5 NA 6 NA 7 NA NA 8 NA NA ...
 $ PR003     : int  NA NA NA 6 NA NA NA NA NA NA ...
 $ PR004     : int  NA NA NA 10 NA NA NA NA NA NA ...
 [...] 
 $ PR054     : int  NA NA NA NA NA NA NA NA NA NA ...
 $ PR055     : int  NA 9 NA NA NA NA 10 NA NA 11 ...
 $ PR056     : int  NA NA NA NA NA NA NA NA 6 NA ...
}%end verb
}%end else / end ifelse latex

%}%end subsection


%%
\item \strong{pilotRoh}: Instrumentendaten der Pilotierung mit Roh-Antworten 
(vgl. \code{pilotItems}).

\ifelse{latex}{%start latex
\itemize{
\item \code{sidstud}: Schüleridentifikator.
\item \code{female}: Geschlecht (\code{"w"} = weiblich, \code{"m"} = männlich).
\item \code{form}: das von der Schülerin/dem Schüler bearbeitete Testheft.
\item \code{E8RS*}: Roh-Antworten der Schülerin/des Schülers auf Items 
\code{E8RS13151} bis \code{E8RS37281} (\code{(8, 9)} = für alle Items, 
wie oben, nicht bewertbare bzw. ausgelassene Itemantwort, 
\code{1:3} = gewählte Antwortoption bei Multiple-Choice Items, 
\code{A:F} = zugeordnete Antwortoption bei Matching-Items, 
\code{0:1} = von Kodiererinnen/Kodierern 
bewertete Antworten für Items im Antwortformat "open gap-fill").
}}{
\tabular{ll}{
\code{sidstud} \tab eindeutiger Schüleridentifikator.\cr
\code{female} \tab Geschlecht (\code{"w"} = weiblich, \code{"m"} = männlich).\cr
\code{form} \tab das von der Schülerin/dem Schüler bearbeitete Testheft.\cr
\code{E8RS*} \tab Rohantworten der Schülerin/des Schülers auf Items 
\code{E8RS13151} bis \code{E8RS37281} (\code{(8, 9)} = für alle Items, wie oben, 
nicht bewertbare bzw. ausgelassene Itemantwort, 
\code{1:3} = gewählte Antwortoption bei Multiple-Choice Items, 
\code{A:F} = zugeordnete Antwortoption bei Matching-Items, 
\code{0:1} = von Kodiererinnen/Kodierern 
bewertete Antworten für Items im Antwortformat "open gap-fill").
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	2504 obs. of  323 variables:}\cr
\code{ $ sidstud  : int  1052 1057 1058 1064 1068 1073 1074 1076 1078 1080 ...}\cr
\code{ $ female   : chr  "w" "w" "w" "w" ...}\cr
\code{ $ form     : chr  "PR019" "PR020" "PR021" "PR022" ...}\cr
\code{ $ E8RS13151: int  NA NA NA NA NA NA NA NA NA NA ...}\cr
\code{ $ E8RS13171: int  NA NA 3 NA NA NA NA 3 NA NA ...}\cr
\code{ $ E8RS13491: int  NA NA NA NA 3 NA NA NA NA NA ...}\cr
\code{ $ E8RS13641: int  2 NA NA NA NA NA 2 NA NA NA ...}\cr
\code{ [...] }\cr
\code{ $ E8RS37163: chr  "" "" "" "" ...}\cr
\code{ $ E8RS37164: chr  "" "" "" "" ...}\cr
\code{ $ E8RS37165: chr  "" "" "" "" ...}\cr
\code{ $ E8RS37281: chr  "" "" "" "" ...}\cr
}{\verb{'data.frame':	2504 obs. of  323 variables:
 $ sidstud  : int  1052 1057 1058 1064 1068 1073 1074 1076 1078 1080 ...
 $ female   : chr  "w" "w" "w" "w" ...
 $ form     : chr  "PR019" "PR020" "PR021" "PR022" ...
 $ E8RS13151: int  NA NA NA NA NA NA NA NA NA NA ...
 $ E8RS13171: int  NA NA 3 NA NA NA NA 3 NA NA ...
 $ E8RS13491: int  NA NA NA NA 3 NA NA NA NA NA ...
 $ E8RS13641: int  2 NA NA NA NA NA 2 NA NA NA ...
 [...] 
 $ E8RS37163: chr  "" "" "" "" ...
 $ E8RS37164: chr  "" "" "" "" ...
 $ E8RS37165: chr  "" "" "" "" ...
 $ E8RS37281: chr  "" "" "" "" ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{pilotMM}: Multiple-Marking-Datensatz der Pilotierung mit 
gemeinsamen Bewertungen einer itemweisen Auswahl von Schülerantworten durch alle 
Kodiererinnen/Kodierer (\code{0} = falsch, \code{1} = richtig, \code{(8, 9)} = wie 
oben, nicht bewertbare bzw. ausgelassene Itemantwort).

\ifelse{latex}{%start latex
\itemize{
\item \code{sidstud}: Schüleridentifikator.
\item \code{item}: Itemnummer.
\item \code{Coder_1}: Bewertung der Schülerantwort von Kodiererin/Kodierer 1.
\item \code{Coder_2}: Bewertung der Schülerantwort von Kodiererin/Kodierer 2.
\item \code{Coder_3}: Bewertung der Schülerantwort von Kodiererin/Kodierer 3.
}}{
\tabular{ll}{
\code{sidstud} \tab Schüleridentifikator.\cr
\code{item} \tab Itemnummer. \cr
\code{Coder_1} \tab Bewertung der Schülerantwort von Kodiererin/Kodierer 1. \cr
\code{Coder_2} \tab Bewertung der Schülerantwort von Kodiererin/Kodierer 2. \cr
\code{Coder_3} \tab Bewertung der Schülerantwort von Kodiererin/Kodierer 3.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	1200 obs. of  5 variables:}\cr
\code{ $ sidstud: int  1185 1269 1311 1522 1658 1665 1854 1889 1921 2067 ...}\cr
\code{ $ item   : chr  "E8RS46051" "E8RS46051" "E8RS46051" "E8RS46051" ...}\cr
\code{ $ Coder_1: int  1 1 9 0 0 9 9 1 9 0 ...}\cr
\code{ $ Coder_2: int  1 1 9 0 0 9 9 1 9 0 ...}\cr
\code{ $ Coder_3: int  1 1 9 0 0 9 9 1 9 0 ...}\cr
}{\verb{'data.frame':	1200 obs. of  5 variables:
 $ sidstud: int  1185 1269 1311 1522 1658 1665 1854 1889 1921 2067 ...
 $ item   : chr  "E8RS46051" "E8RS46051" "E8RS46051" "E8RS46051" ...
 $ Coder_1: int  1 1 9 0 0 9 9 1 9 0 ...
 $ Coder_2: int  1 1 9 0 0 9 9 1 9 0 ...
 $ Coder_3: int  1 1 9 0 0 9 9 1 9 0 ...
}%end verb
}%end else / end ifelse latex

}%end itemize
}% end format

\keyword{datasets}
