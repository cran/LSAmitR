\name{datenKapitel05}
\alias{datenKapitel05}
\docType{data}
\encoding{UTF-8}

\title{
Illustrationsdaten zu Kapitel 5, Testdesign
}

\description{
Hier befindet sich die Dokumentation der in Kapitel 5, 
\emph{Testdesign}, im Herausgeberband 
Large-Scale Assessment mit \R: 
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung, 
verwendeten Daten. Die Komponenten der Datensätze werden knapp erläutert und 
deren Strukturen dargestellt. 
}

\usage{
data(datenKapitel05)
}

\references{
  Kiefer, T., Kuhn, J.-T. & Fellinger, R. (2016). Testdesign. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 149--184). Wien: facultas.
}

\seealso{
Für die Verwendung der Daten, siehe \code{\link{Kapitel 5}}.
}	


\format{
\code{datenKapitel05} ist eine Liste mit den sechs Elementen \code{tdItembank}, 
\code{tdBib2d}, \code{tdBibPaare}, \code{tdExItembank}, \code{tdExBib2d} und 
\code{tdExBibPaare}, die sowohl für die Umsetzung im Kapitel als auch für die 
Übungsaufgaben die relevanten Informationen auf Itemebene in Form einer 
Itembank und Zwischenergebnisse aus dem Blockdesign für die Weiterverarbeitung 
beinhalten.

%%
\itemize{
\item \strong{tdItembank}: Itembank für den Testdesignprozess, bestehend aus 
286 dichotomen und polytomen Items.

\ifelse{latex}{%start latex
\itemize{
\item \code{testlet}: Testletname des Items (gleichbedeutend mit zugewiesenem 
Stimulus). 
\item \code{itemnr}: Itemidentifikator.
\item \code{task}: Itemname.
\item \code{format}: Antwortformat.
\item \code{focus}: Fokuskategorie des Items.
\item \code{focus.label}: Bezeichnung des Fokus.
\item \code{topic}: Themengruppe des Inhalts des zum Item gehörenden Stimulus.
\item \code{audiolength}: Länge der Tonaufnahme in Sekunden.
\item \code{RelFreq}: Item-Schwierigkeit (genauer: aus Pilotierung gewonnener 
Erwartungswert gewichtet mit höchstem erreichbaren Punktewert bei dem Item; 
vgl. Kapitel 1, \emph{Testkonstruktion}, im Band).
\item \code{rpb.WLE}: Item-Trennschärfe (genauer: Punktbiseriale Korrelation der 
Itemantworten mit dem Weighted Likelihood Personenschätzer (WLE); vgl. Kapitel 1
und Kapitel 6, \emph{Skalierung und Linking}, im Band).
\item \code{uniformDIF}: Uniformes Differenzielles Itemfunktionieren (vgl. Kapitel 
4, \emph{Differenzielles Itemfunktionieren in Subgruppen}, im Band).
\item \code{DIF.ETS}: Klassifikation des uniform DIF nach ETS (vgl. Kapitel 
4 im Band).
\item \code{IIF_380}: Wert der Fisher-Iteminformationsfunktionen am Skalenwert 380 
(vgl. Kapitel 6 im Band). 
\item \code{IIF_580}: Wert der Fisher-Iteminformationsfunktionen am Skalenwert 
580.
}}{
\tabular{ll}{
\code{testlet} \tab Testletname des Items (gleichbedeutend mit zugewiesenem 
Stimulus). \cr
\code{itemnr} \tab Itemidentifikator \cr
\code{task} \tab Itemname.\cr
\code{format} \tab Antwortformat.\cr
\code{focus} \tab Fokuskategorie des Items. \cr
\code{focus.label} \tab Bezeichnung des Fokus. \cr
\code{topic} \tab Themengruppe des Inhalts des zum Item gehörenden Stimulus.\cr
\code{audiolength} \tab Länge der Tonaufnahme in Sekunden.\cr
\code{RelFreq} \tab Item-Schwierigkeit (genauer: aus Pilotierung gewonnener 
Erwartungswert gewichtet mit höchstem erreichbaren Punktewert bei dem Item; 
vgl. Kapitel 1, \emph{Testkonstruktion}, im Band).\cr
\code{rpb.WLE} \tab Item-Trennschärfe (genauer: Punktbiseriale Korrelation der 
Itemantworten mit dem Weighted Likelihood Personenschätzer (WLE); vgl. Kapitel 1
und Kapitel 6, \emph{Skalierung und Linking}, im Band).\cr
\code{uniformDIF} \tab Uniformes Differenzielles Itemfunktionieren (vgl. Kapitel 
4, \emph{Differenzielles Itemfunktionieren in Subgruppen}, im Band). \cr
\code{DIF.ETS} \tab Klassifikation des uniform DIF nach ETS (vgl. Kapitel 
4 im Band). \cr
\code{IIF_380} \tab Wert der Fisher-Iteminformationsfunktionen am Skalenwert 380 
(vgl. Kapitel 6 im Band). \cr 
\code{IIF_580} \tab Wert der Fisher-Iteminformationsfunktionen am Skalenwert 
580.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	286 obs. of  14 variables:}\cr
\code{ $ testlet    : chr [1:286] "E8LS0127" "E8LS0128" "E8LS0132" "E8LS0135" ...}\cr
\code{ $ itemnr     : int [1:286] 127 128 132 135 139 141 142 144 145 147 ...}\cr
\code{ $ task       : chr [1:286] "E8LS0127" "E8LS0128" "E8LS0132" "E8LS0135" ...}\cr
\code{ $ format     : chr [1:286] "MC4" "MC4" "MC4" "MC4" ...}\cr
\code{ $ focus      : int [1:286] 0 2 2 5 2 5 2 4 2 5 ...}\cr
\code{ $ focus.label: chr [1:286] "LFocus0" "LFocus2" "LFocus2" "LFocus5" ...}\cr
\code{ $ topic      : chr [1:286] "Körper und Gesundheit" "Gedanken, Empfindungen und Gefühle" ...}\cr
\code{ $ audiolength: int [1:286] 47 46 39 62 51 30 44 28 50 23 ...}\cr
\code{ $ RelFreq    : num [1:286] 0.71 0.314 0.253 0.847 0.244 ...}\cr
\code{ $ rpb.WLE    : num [1:286] 0.516 0.469 0.285 0.54 0.352 ...}\cr
\code{ $ uniformDIF : num [1:286] 0.115726 0.474025 0.11837 0.083657 -0.000051 ...}\cr
\code{ $ DIF.ETS    : chr [1:286] "A+" "B+" "A+" "A+" ...}\cr
\code{ $ IIF_380    : num [1:286] 0.4073 0.1542 0.0708 0.4969 0.0611 ...}\cr
\code{ $ IIF_580    : num [1:286] 0.157 0.508 0.277 0.26 0.148 ...}\cr
}{\verb{'data.frame':	286 obs. of  14 variables:
 $ testlet    : chr [1:286] "E8LS0127" "E8LS0128" "E8LS0132" "E8LS0135" ...
 $ itemnr     : int [1:286] 127 128 132 135 139 141 142 144 145 147 ...
 $ task       : chr [1:286] "E8LS0127" "E8LS0128" "E8LS0132" "E8LS0135" ...
 $ format     : chr [1:286] "MC4" "MC4" "MC4" "MC4" ...
 $ focus      : int [1:286] 0 2 2 5 2 5 2 4 2 5 ...
 $ focus.label: chr [1:286] "LFocus0" "LFocus2" "LFocus2" "LFocus5" ...
 $ topic      : chr [1:286] "Körper und Gesundheit" "Gedanken, Empfindungen und Gefühle" ...
 $ audiolength: int [1:286] 47 46 39 62 51 30 44 28 50 23 ...
 $ RelFreq    : num [1:286] 0.71 0.314 0.253 0.847 0.244 ...
 $ rpb.WLE    : num [1:286] 0.516 0.469 0.285 0.54 0.352 ...
 $ uniformDIF : num [1:286] 0.115726 0.474025 0.11837 0.083657 -0.000051 ...
 $ DIF.ETS    : chr [1:286] "A+" "B+" "A+" "A+" ...
 $ IIF_380    : num [1:286] 0.4073 0.1542 0.0708 0.4969 0.0611 ...
 $ IIF_580    : num [1:286] 0.157 0.508 0.277 0.26 0.148 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{tdBib2d}: Vollständiges durch den BIB-Design-Algorithmus erzeugtes
Itemblock-Design (vgl. Tabelle 5.3) in tabellarischer Aufstellung mit 30 
Testheften (Zeilen), 6 Positionen (Spalten) und 30 Itemblöcken (Zelleneinträge).

\ifelse{latex}{%start latex
\code{'data.frame':	30 obs. of  6 variables:}\cr
\code{ $ V1: int [1:30] 12 5 6 7 3 1 17 4 18 13 ...}\cr
\code{ $ V2: int [1:30] 2 11 9 4 10 8 15 17 7 26 ...}\cr
\code{ $ V3: int [1:30] 7 6 10 12 1 5 20 15 17 8 ...}\cr
\code{ $ V4: int [1:30] 11 9 3 2 8 4 13 5 22 7 ...}\cr
\code{ $ V5: int [1:30] 10 7 2 9 4 12 6 18 13 1 ...}\cr
\code{ $ V6: int [1:30] 3 8 1 5 6 11 16 27 14 24 ...}\cr
}{\verb{'data.frame':	30 obs. of  6 variables:
 $ V1: int [1:30] 12 5 6 7 3 1 17 4 18 13 ...
 $ V2: int [1:30] 2 11 9 4 10 8 15 17 7 26 ...
 $ V3: int [1:30] 7 6 10 12 1 5 20 15 17 8 ...
 $ V4: int [1:30] 11 9 3 2 8 4 13 5 22 7 ...
 $ V5: int [1:30] 10 7 2 9 4 12 6 18 13 1 ...
 $ V6: int [1:30] 3 8 1 5 6 11 16 27 14 24 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{tdBibPaare}: Ergebnis des BIB-Design-Algorithmus als Blockpaare, 
wobei die Zelleneinträge die paarweisen Auftretenshäufigkeiten des Zeilenblocks
mit dem Spaltenblock im Design anzeigen.

\ifelse{latex}{%start latex
\code{'data.frame':	30 obs. of  30 variables:}\cr
\code{ $ V1 : int [1:30] 6 1 2 2 1 2 1 3 1 2 ...}\cr
\code{ $ V2 : int [1:30] 1 6 2 2 1 1 3 0 3 2 ...}\cr
\code{ $ V3 : int [1:30] 2 2 6 1 0 3 1 2 2 5 ...}\cr
\code{ [...]}\cr
\code{ $ V29: int [1:30] 0 0 1 1 0 1 0 0 1 2 ...}\cr
\code{ $ V30: int [1:30] 1 1 1 0 0 0 0 1 0 1 ...}\cr
}{\verb{'data.frame':	30 obs. of  30 variables:
 $ V1 : int [1:30] 6 1 2 2 1 2 1 3 1 2 ...
 $ V2 : int [1:30] 1 6 2 2 1 1 3 0 3 2 ...
 $ V3 : int [1:30] 2 2 6 1 0 3 1 2 2 5 ...
 [...]
 $ V29: int [1:30] 0 0 1 1 0 1 0 0 1 2 ...
 $ V30: int [1:30] 1 1 1 0 0 0 0 1 0 1 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{tdExItembank}: Beispiel-Itembank für den Testdesignprozess in den 
Übungsaufgaben zum Kapitel.

\ifelse{latex}{%start latex
\itemize{
\item \code{task}: Itemname. 
\item \code{format}: Antwortformat.
\item \code{focus}: Fokuskategorie des Items.
\item \code{p}: Item-Leichtigkeit (genauer: in der Pilotierung beobachtete 
relative Lösungshäufigkeit für dichotome Items).
\item \code{p_cat}: Dreistufige Kategorisierung der Schwierigkeit.
\item \code{itemdiff}: Rasch-kalibrierte Itemparameter.
\item \code{bearbeitungszeit}: Geschätzte mittlere Bearbeitungszeit des Items.
}}{\tabular{ll}{
\code{task} \tab Itemname. \cr
\code{format} \tab Antwortformat.\cr
\code{focus} \tab Fokuskategorie des Items. \cr
\code{p} \tab Item-Leichtigkeit (genauer: in der Pilotierung beobachtete 
relative Lösungshäufigkeit für dichotome Items). \cr
\code{p_cat} \tab Dreistufige Kategorisierung der Schwierigkeit. \cr
\code{itemdiff} \tab Rasch-kalibrierte Itemparameter. \cr
\code{bearbeitungszeit} \tab Geschätzte mittlere Bearbeitungszeit des Items.
}%end tabular
}%end else / end ifelse latex

\ifelse{latex}{%start latex
\code{'data.frame':	250 obs. of  7 variables:}\cr
\code{ $ task            : chr [1:250] "M80003" "M80004" "M80006" "M80007" ...}\cr
\code{ $ format          : chr [1:250] "ho" "MC4" "MC4" "ho" ...}\cr
\code{ $ focus           : int [1:250] 1 4 4 2 3 4 1 2 3 3 ...}\cr
\code{ $ p               : num [1:250] 0.84 0.56 0.34 0.45 0.2 0.42 0.77 0.42 0.34 0.71 ...}\cr
\code{ $ p_cat           : chr [1:250] "leicht" "mittel" "mittel" "mittel" ...}\cr
\code{ $ itemdiff        : int [1:250] 404 570 676 622 761 636 457 636 676 494 ...}\cr
\code{ $ bearbeitungszeit: int [1:250] 90 60 90 120 90 150 90 30 120 90 ...}\cr
}{\verb{'data.frame':	250 obs. of  7 variables:
 $ task            : chr [1:250] "M80003" "M80004" "M80006" "M80007" ...
 $ format          : chr [1:250] "ho" "MC4" "MC4" "ho" ...
 $ focus           : int [1:250] 1 4 4 2 3 4 1 2 3 3 ...
 $ p               : num [1:250] 0.84 0.56 0.34 0.45 0.2 0.42 0.77 0.42 0.34 0.71 ...
 $ p_cat           : chr [1:250] "leicht" "mittel" "mittel" "mittel" ...
 $ itemdiff        : int [1:250] 404 570 676 622 761 636 457 636 676 494 ...
 $ bearbeitungszeit: int [1:250] 90 60 90 120 90 150 90 30 120 90 ...
}%end verb
}%end else / end ifelse latex


%%
\item \strong{tdExBib2d}: Vollständiges Itemblock-Design zur Weiterverarbeitung 
in den Übungsaufgaben zum Kapitel in tabellarischer Aufstellung mit 10 
Testheften (Zeilen), 4 Positionen (Spalten) und 10 Itemblöcken (Zelleneinträge). 

\ifelse{latex}{%start latex
\code{'data.frame':	10 obs. of  4 variables:}\cr
\code{ $ V1: int [1:10] 1 9 8 2 10 4 7 3 5 6}\cr
\code{ $ V2: int [1:10] 10 6 7 8 4 1 9 5 3 2}\cr
\code{ $ V3: int [1:10] 6 10 9 1 5 2 3 8 4 7}\cr
\code{ $ V4: int [1:10] 7 8 4 3 9 6 1 10 2 5}\cr
}{\verb{'data.frame':	10 obs. of  4 variables:
 $ V1: int [1:10] 1 9 8 2 10 4 7 3 5 6
 $ V2: int [1:10] 10 6 7 8 4 1 9 5 3 2
 $ V3: int [1:10] 6 10 9 1 5 2 3 8 4 7
 $ V4: int [1:10] 7 8 4 3 9 6 1 10 2 5
}%end verb
}%end else / end ifelse latex


%%
\item \strong{tdExBibPaare}: Itemblock-Design zur Weiterverarbeitung in den 
Übungsaufgaben in der Darstellung als Blockpaare, wobei die Zelleneinträge 
die paarweisen Auftretenshäufigkeiten des Zeilenblocks mit dem Spaltenblock 
im Design anzeigen.

\ifelse{latex}{%start latex
\code{'data.frame':	10 obs. of  10 variables:}\cr
\code{ $ V1 : int [1:10] 4 2 2 1 0 2 2 1 1 1}\cr
\code{ $ V2 : int [1:10] 2 4 2 2 2 2 1 1 0 0}\cr
\code{ $ V3 : int [1:10] 2 2 4 1 2 0 1 2 1 1}\cr
\code{ $ V4 : int [1:10] 1 2 1 4 2 1 1 1 2 1}\cr
\code{ $ V5 : int [1:10] 0 2 2 2 4 1 1 1 1 2}\cr
\code{ $ V6 : int [1:10] 2 2 0 1 1 4 2 1 1 2}\cr
\code{ $ V7 : int [1:10] 2 1 1 1 1 2 4 1 2 1}\cr
\code{ $ V8 : int [1:10] 1 1 2 1 1 1 1 4 2 2}\cr
\code{ $ V9 : int [1:10] 1 0 1 2 1 1 2 2 4 2}\cr
\code{ $ V10: int [1:10] 1 0 1 1 2 2 1 2 2 4}\cr
}{\verb{'data.frame':	10 obs. of  10 variables:
 $ V1 : int [1:10] 4 2 2 1 0 2 2 1 1 1
 $ V2 : int [1:10] 2 4 2 2 2 2 1 1 0 0
 $ V3 : int [1:10] 2 2 4 1 2 0 1 2 1 1
 $ V4 : int [1:10] 1 2 1 4 2 1 1 1 2 1
 $ V5 : int [1:10] 0 2 2 2 4 1 1 1 1 2
 $ V6 : int [1:10] 2 2 0 1 1 4 2 1 1 2
 $ V7 : int [1:10] 2 1 1 1 1 2 4 1 2 1
 $ V8 : int [1:10] 1 1 2 1 1 1 1 4 2 2
 $ V9 : int [1:10] 1 0 1 2 1 1 2 2 4 2
 $ V10: int [1:10] 1 0 1 1 2 2 1 2 2 4
}%end verb
}%end else / end ifelse latex

}%end itemize
}%end format

\keyword{datasets}
