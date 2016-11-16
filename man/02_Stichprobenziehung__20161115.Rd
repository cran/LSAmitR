%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel  2}
\alias{Kapitel 2}
\encoding{UTF-8}
\title{Kapitel 2: Stichprobenziehung}

\description{
Das ist die Nutzerseite zum Kapitel 2, \emph{Stichprobenziehung}, im 
Herausgeberband Large-Scale Assessment mit \R: Methodische Grundlagen der 
österreichischen Bildungsstandardüberprüfung. 
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. 
Im Abschnitt \strong{Examples} werden die \R-Syntaxen des Kapitels vollständig 
wiedergegeben und gegebenfalls erweitert.
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  George, A. C., Oberwimmer, K. & Itzlinger-Bruneforth, U. (2016). 
  Stichprobenziehung. 
  In S. Breit & C. Schreiner (Hrsg.), \emph{Large-Scale Assessment mit \R:  
  Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung} 
  (pp. 51--81). Wien: facultas.
}

\author{
  Ann Cathrice George, Konrad Oberwimmer, Ursula Itzlinger-Bruneforth
}

\seealso{
%Verweis zu den Daten im Kapitel
Zu \code{\link{datenKapitel02}}, den im Kapitel verwendeten Daten.\cr
%Verweis zum nächsten und zum vorigen Kapitel
Zurück zu \code{\link{Kapitel 1}}, Testkonstruktion.\cr
Zu \code{\link{Kapitel 3}}, Standard-Setting.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach Übersicht einfügen)
%Zur Hilfeseite von fit.r \code{\link{fit.r}}
}

%% ----------------------------------------------------------------------------
%% Details
\details{
\subsection{Vorbereitungen}{
Zunächst werden die Datensätze \code{schule} mit den 1.327 Schulen der 
Population und \code{schueler} mit den 51.644 Schüler/innen dieser Schulen 
geladen.
Durch das Setzen eines festen Startwerts für den Zufallszahlengenerator 
(\code{set.seed(20150506)}) wird erreicht, dass wiederholte Programmdurchläufe 
immer wieder zur selben Stichprobe führen.
} % end Vorbereitungen

\subsection{Abschnitt 4.1: Stratifizierung - Schichtung einer Stichprobe}{
Die für die explizite Stratifizierung notwendige Information der Anzahl der 
Schüler/innen pro Stratum wird durch Aggregierung (Summe) aus dem Schuldatensatz 
in das Objekt \code{strata} extrahiert. Die entsprechende Spalte wird aus 
Gründen der Eindeutigkeit noch in \code{NSchuelerStratum} umbenannt.

\ifelse{latex}{%start latex
\code{}\cr
\code{> strata <- aggregate(schule[,"NSchueler", drop = FALSE],}\cr
\code{+                     by=schule[,"stratum", drop = FALSE], sum)}\cr
\code{> colnames(strata)[2] <- "NSchuelerStratum" #Ergänzung zum Buch}\cr
}{\verb{strata <- aggregate(schule[,"NSchueler", drop = FALSE],
                  by=schule[,"stratum", drop = FALSE], sum)
colnames(strata)[2] <- "NSchuelerStratum" #Ergänzung zum Buch
}
}%end else / end ifelse latex

}% end Abschnitt 4.1

\subsection{Abschnitt 4.2: Schulenziehung, Listing 1}{
Im Schuldatensatz wird eine Dummyvariable \code{Klassenziehung} angelegt, die 
indiziert, in welchen Schulen mehr als drei Klassen sind, aus denen in Folge 
gezogen werden muss.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schule$Klassenziehung <- 0}\cr
\code{> schule[which(schule$NKlassen>3), "Klassenziehung"] <- 1}\cr
}{\verb{schule$Klassenziehung <- 0
schule[which(schule$NKlassen>3), "Klassenziehung"] <- 1
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 1

\subsection{Abschnitt 4.2: Schulenziehung, Listing 2}{
Dann wird der unter Beachtung der Klassenziehung erwartete Beitrag der Schulen 
(d. h. die Anzahl ihrer Schülerinnen bzw. Schüler) zur Stichprobe in der Spalte 
\code{NSchueler.erw} errechnet.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schule$NSchueler.erw <- schule$NSchueler}\cr
\code{> ind <- which(schule$Klassenziehung == 1)}\cr
\code{> schule[ind, "NSchueler.erw"] <- }\cr
\code{+   schule[ind, "NSchueler"]/schule[ind, "NKlassen"]*3}\cr
}{\verb{schule$NSchueler.erw <- schule$NSchueler
ind <- which(schule$Klassenziehung == 1)
schule[ind, "NSchueler.erw"] <- 
 schule[ind, "NSchueler"]/schule[ind, "NKlassen"]*3
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 2

\subsection{Abschnitt 4.2: Schulenziehung, Listing 3}{
Berechnet man aus der erwarteten Anzahl von Lernenden pro Schule ihren relativen 
Anteil (Spalte \code{AnteilSchueler}) an der Gesamtschülerzahl im Stratum, so 
kann per Mittelwertbildung die mittlere Anzahl (Spalte 
\code{NSchueler/Schule.erw}) von Lernenden einer Schule pro Stratum bestimmt 
werden. 
Die mittlere Anzahl der Schulen im Stratum wird zusätzlich mit den einfachen 
Ziehungsgewichten der Schulen gewichtet, da große Schulen mit höherer 
Wahrscheinlichkeit für die Stichprobe gezogen werden.

\ifelse{latex}{%start latex
\code{}\cr
\code{> temp <- merge(schule[, c("SKZ","stratum","NSchueler")], }\cr
\code{+               strata[, c("stratum","NSchuelerStratum")])}\cr
\code{> schule$AnteilSchueler <- }\cr
\code{+   temp$NSchueler/temp$NSchuelerStratum}\cr
\code{> strata$"NSchueler/Schule.erw" <- }\cr
\code{+   rowsum(apply(schule, 1, function(x)}\cr
\code{+     x["NSchueler.erw"]*x["AnteilSchueler"]), schule$stratum)}\cr
}{\verb{temp <- merge(schule[, c("SKZ","stratum","NSchueler")], 
             strata[, c("stratum","NSchuelerStratum")])
schule$AnteilSchueler <- 
 temp$NSchueler/temp$NSchuelerStratum
strata$"NSchueler/Schule.erw" <- 
 rowsum(apply(schule, 1, function(x)
   x["NSchueler.erw"]*x["AnteilSchueler"]), schule$stratum)
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 3

\subsection{Abschnitt 4.2: Schulenziehung, Listing 4}{
Schließlich erfolgt die Berechnung der Anzahl an Schulen 
(\code{Schulen.zu.ziehen}), die in jedem Stratum gezogen werden müssen, um einen 
Stichprobenumfang von 2500 Schülerinnen bzw. Schülern in etwa einzuhalten.

\ifelse{latex}{%start latex
\code{}\cr
\code{> strata$Schulen.zu.ziehen <- }\cr
\code{+   round(2500/strata[,"NSchueler/Schule.erw"])}\cr
}{\verb{strata$Schulen.zu.ziehen <- 
  round(2500/strata[,"NSchueler/Schule.erw"])
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 4

\subsection{Abschnitt 4.2: Schulenziehung, Listing 5}{
Die Schulenliste wird vorab nach expliziten und impliziten Strata sortiert.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schule <- schule[order(schule$stratum, schule$NSchueler),]}\cr
}{\verb{schule <- schule[order(schule$stratum, schule$NSchueler),]
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 5
 
\subsection{Abschnitt 4.2: Schulenziehung, Listing 6}{
Das Sampling-Intervall pro Stratum wird bestimmt (\code{Samp.Int}).

\ifelse{latex}{%start latex
\code{}\cr
\code{> strata$Samp.Int <- }\cr
\code{+   strata$NSchuelerStratum/strata$Schulen.zu.ziehen}\cr
}{\verb{strata$Samp.Int <- 
  strata$NSchuelerStratum/strata$Schulen.zu.ziehen
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 6

\subsection{Abschnitt 4.2: Schulenziehung, Listing 7}{
Ein zufälliger Startwert aus dem Bereich 1 bis \code{Samp.Int} wird für jedes 
Stratum bestimmt (\code{Startwert}). Zur Festlegung eines festen Ausgangswertes 
des Zufallszahlengenerators siehe oben unter "Vorbereitungen".

\ifelse{latex}{%start latex
\code{}\cr
\code{> set.seed(20150506)}\cr
\code{> strata$Startwert <- }\cr
\code{+   sapply(ceiling(strata$Samp.Int), sample, size = 1)}\cr
}{\verb{set.seed(20150506)
strata$Startwert <- 
  sapply(ceiling(strata$Samp.Int), sample, size = 1)
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 7

\subsection{Abschnitt 4.2: Schulenziehung, Listing 8}{
Die Listenpositionen der Lernenden, deren Schulen gezogen werden, werden vom 
Startwert ausgehend im Sampling-Intervall (pro Stratum) ermittelt. 
Die Positionen werden im Objekt \code{tickets} abgelegt.

\ifelse{latex}{%start latex
\code{}\cr
\code{> tickets <- sapply(1:4, function(x)}\cr
\code{+   trunc(0:(strata[strata$stratum==x,"Schulen.zu.ziehen"]-1)}\cr
\code{+     * strata[strata$stratum==x, "Samp.Int"] +}\cr
\code{+     strata$Startwert[x]))}\cr
}{\verb{tickets <- sapply(1:4, function(x)
  trunc(0:(strata[strata$stratum==x,"Schulen.zu.ziehen"]-1)
    * strata[strata$stratum==x, "Samp.Int"] +
    strata$Startwert[x]))
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 8
  
\subsection{Abschnitt 4.2: Schulenziehung, Listing 9}{
Um die Auswahl der Schulen (entsprechend den Tickets der Lernenden) direkt auf 
der Schulliste durchführen zu können wird in \code{NSchuelerKum} die kumulierte 
Anzahl an Schülerinnen und Schülern nach Sortierung (siehe oben Abschnit 4.2, 
Listing 5) berechnet.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schule$NSchuelerKum <- }\cr
\code{+   unlist(sapply(1:4, function(x)}\cr
\code{+     cumsum(schule[schule$stratum==x, "NSchueler"])))}\cr
}{\verb{schule$NSchuelerKum <- 
  unlist(sapply(1:4, function(x)
    cumsum(schule[schule$stratum==x, "NSchueler"])))
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 9

\subsection{Abschnitt 4.2: Schulenziehung, Listing 10}{
Durch die Dummy-Variable \code{SInSamp} werden nun jene Schulen als zugehörig 
zur Stichprobe markiert, von denen wenigstens eine Schülerin oder ein Schüler 
in Listing 8 dieses Abschnitts ein Ticket erhalten hat.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schule$SInSamp <- 0 }\cr
\code{> for(s in 1:4) \{}\cr
\code{+   NSchuelerKumStrat <- }\cr
\code{+     schule[schule$stratum==s, "NSchuelerKum"]}\cr
\code{+   inds <- sapply(tickets[[s]], function(x)}\cr
\code{+     setdiff(which(NSchuelerKumStrat <= x),}\cr
\code{+             which(NSchuelerKumStrat[-1] <= x)))}\cr
\code{+   schule[schule$stratum==s, "SInSamp"][inds] <- 1 \}}\cr
}{\verb{schule$SInSamp <- 0 
for(s in 1:4) {
  NSchuelerKumStrat <- 
    schule[schule$stratum==s, "NSchuelerKum"]
  inds <- sapply(tickets[[s]], function(x)
    setdiff(which(NSchuelerKumStrat <= x),
            which(NSchuelerKumStrat[-1] <= x)))
  schule[schule$stratum==s, "SInSamp"][inds] <- 1 }
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 10

\subsection{Abschnitt 4.2: Schulenziehung, Listing 11}{
Die Ziehungswahrscheinlichkeiten der Schulen (\code{Z.Wsk.Schule}) werden für 
die später folgende Gewichtung berechnet.

\ifelse{latex}{%start latex
\code{}\cr
\code{> temp <- merge(schule[, c("stratum", "AnteilSchueler")], }\cr
\code{+               strata[, c("stratum", "Schulen.zu.ziehen")])}\cr
\code{> schule$Z.Wsk.Schule <- }\cr
\code{+   temp$AnteilSchueler*temp$Schulen.zu.ziehen}\cr
}{\verb{temp <- merge(schule[, c("stratum", "AnteilSchueler")],
              strata[, c("stratum", "Schulen.zu.ziehen")])
schule$Z.Wsk.Schule <- 
  temp$AnteilSchueler*temp$Schulen.zu.ziehen
}
}%end else / end ifelse latex

} %% end Abschnitt 4.2, Listing 11

\subsection{Abschnitt 4.3: Klassenziehung, Listing 1}{
Im Objekt \code{schukla} werden zunächst notwendige Informationen für die 
Klassenziehung zusammengetragen. Die Dummy-Variable \code{KlInSamp} darin 
indiziert schließlich gezogene Klassen (aus bereits gezogenen Schulen), wobei 
aus Schulen mit drei oder weniger Klassen alle Klassen gezogen werden. 
Daher wird der Aufruf von \code{sample.int} mit \code{min(3, length(temp))} 
parametrisiert.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schukla <- unique(merge(}\cr
\code{+   schule[, c("SKZ","NKlassen", "Klassenziehung", }\cr
\code{+     "Z.Wsk.Schule", "SInSamp")],}\cr
\code{+     schueler[, c("SKZ", "idclass")], by="SKZ"))}\cr
\code{> schukla$KlInSamp <- 0}\cr
\code{> for(skz in unique(schukla[schukla$SInSamp==1,"SKZ"])) \{}\cr
\code{+   temp <- schukla[schukla$SKZ==skz, "idclass"]}\cr
\code{+   schukla[schukla$idclass \%in\% temp[sample.int}\cr
\code{+     (min(3, length(temp)))], "KlInSamp"] <- 1 \}}\cr
}{\verb{schukla <- unique(merge(
  schule[, c("SKZ","NKlassen", "Klassenziehung", 
    "Z.Wsk.Schule", "SInSamp")],
    schueler[, c("SKZ", "idclass")], by="SKZ"))
schukla$KlInSamp <- 0
for(skz in unique(schukla[schukla$SInSamp==1,"SKZ"])) {
  temp <- schukla[schukla$SKZ==skz, "idclass"]
  schukla[schukla$idclass \%in\% temp[sample.int
    (min(3, length(temp)))], "KlInSamp"] <- 1 }
}
}%end else / end ifelse latex

} %% end Abschnitt 4.3, Listing 1

\subsection{Abschnitt 4.3: Klassenziehung, Listing 2}{
Die Ziehungswahrscheinlichkeit einer Klasse (\code{Z.Wsk.Klasse}) kann 
entsprechend der Dummy-Variable \code{Klassenziehung} (siehe Abschnitt 4.2, 
Listing 1) berechnet werden. Man beachte, dass entweder der erste oder der 
zweite Term der Addition Null ergeben muss, sodass die Fallunterscheidung direkt 
ausgedrückt werden kann.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schukla$Z.Wsk.Klasse <- ((1 - schukla$Klassenziehung) * 1 +}\cr
\code{+     schukla$Klassenziehung * 3 / schukla$NKlassen) }\cr
}{\verb{schukla$Z.Wsk.Klasse <- ((1 - schukla$Klassenziehung) * 1 +
    schukla$Klassenziehung * 3 / schukla$NKlassen) 
}
}%end else / end ifelse latex

} %% end Abschnitt 4.3, Listing 2

\subsection{Abschnitt 4.4: Gewichtung, Listing 1}{
Nachdem das Objekt \code{schueler} um die Informationen zur Klassenziehung sowie
den Ziehungswahrscheinlichkeiten von Schule und Klasse ergänzt wird, kann die 
Ziehungswahrscheinlichkeit einer Schülerin bzw. eines Schülers 
(\code{Z.Wsk.Schueler}) berechnet werden.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schueler <- merge(schueler, schukla[, c("idclass", "KlInSamp", "Z.Wsk.Schule",}\cr 
\code{+                                         "Z.Wsk.Klasse")],}\cr
\code{+                   by="idclass", all.x=T)}\cr
\code{> schueler$Z.Wsk.Schueler <- }\cr
\code{+   schueler$Z.Wsk.Schule * schueler$Z.Wsk.Klasse}\cr
}{\verb{schueler <- merge(schueler, schukla[, c("idclass", "KlInSamp", "Z.Wsk.Schule", 
                                        "Z.Wsk.Klasse")],
                  by="idclass", all.x=T)
schueler$Z.Wsk.Schueler <- 
  schueler$Z.Wsk.Schule * schueler$Z.Wsk.Klasse
}
}%end else / end ifelse latex

} %% end Abschnitt 4.4, Listing 1

\subsection{Abschnitt 4.4: Gewichtung, Listing 2}{
Nach Reduktion des Objekts \code{schueler} auf die gezogenen Lernenden, werden 
in \code{temp} die nonresponse-Raten (Variable \code{x}) bestimmt.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schueler <- schueler[schueler$KlInSamp==1,]}\cr
\code{> temp <- merge(schueler[, c("idclass", }\cr
\code{+                            "Z.Wsk.Schueler")],}\cr
\code{+               aggregate(schueler$teilnahme, }\cr
\code{+                         by=list(schueler$idclass),}\cr
\code{+                         function(x) sum(x)/length(x)), }\cr
\code{+               by.x="idclass", by.y="Group.1")}\cr
}{\verb{schueler <- schueler[schueler$KlInSamp==1,]
temp <- merge(schueler[, c("idclass", 
                           "Z.Wsk.Schueler")],
              aggregate(schueler$teilnahme, 
                        by=list(schueler$idclass),
                        function(x) sum(x)/length(x)), 
              by.x="idclass", by.y="Group.1")
}
}%end else / end ifelse latex

} %% end Abschnitt 4.4, Listing 2

\subsection{Abschnitt 4.4: Gewichtung, Listing 3}{
Mittels der Ziehungswahrscheinlichkeiten der Schülerinnen und Schüler sowie der 
nonresponse-Raten (siehe vorangegangenes Listing) werden die (nicht normierten) 
Schülergewichte (\code{studwgt}) bestimmt.

\ifelse{latex}{%start latex
\code{}\cr
\code{> schueler$studwgt <- 1/temp$x/temp$Z.Wsk.Schueler}\cr
}{\verb{schueler$studwgt <- 1/temp$x/temp$Z.Wsk.Schueler
}
}%end else / end ifelse latex

} %% end Abschnitt 4.4, Listing 3

\subsection{Abschnitt 4.4: Gewichtung, Listing 4}{
Schließlich werden die Schülergewichte in Bezug auf die Anzahl an Schülerinnen 
und Schülern im jeweiligen Stratum normiert (\code{NormStudwgt}), sodass sie in 
Summe dieser Anzahl entsprechen.

\ifelse{latex}{%start latex
\code{}\cr
\code{> Normierung <- strata$NSchuelerStratum / }\cr
\code{+   rowsum(schueler$studwgt * schueler$teilnahme,}\cr
\code{+          group = schueler$Stratum)}\cr
\code{> schueler$NormStudwgt <- }\cr
\code{+   schueler$studwgt * Normierung[schueler$Stratum]}\cr
}{\verb{Normierung <- strata$NSchuelerStratum / 
  rowsum(schueler$studwgt * schueler$teilnahme,
         group = schueler$Stratum)
schueler$NormStudwgt <- 
  schueler$studwgt * Normierung[schueler$Stratum]
}
}%end else / end ifelse latex

} %% end Abschnitt 4.4, Listing 3

\subsection{Abschnitt 5.3: Anwendung per Jackknife-Repeated-Replication, 
Listing 1}{

Die im Folgenden genutzte Hilfsfunktion zones.within.stratum erzeugt ab einem 
Offset einen Vektor mit jeweils doppelt vorkommenden IDs zur Bildung der 
Jackknife-Zonen.
Nachdem die Schulliste zunächst auf die gezogenen Schulen und nach expliziten 
und impliziten Strata* sortiert wurde, werden die Strata in Pseudo-Strata mit 
zwei (oder bei ungerader Anzahl drei) Schulen unterteilt. 
Dies führt zur Variable \code{jkzone}.
Basierend auf \code{jkzone} wird für jeweils eine der Schulen im Pseudo-Stratum 
der Indikator \code{jkrep} auf Null gesetzt, um diese in der jeweiligen 
Replikation von der Berechnung auszuschließen. 
Ergänzend zum Buch wird hier eine Fallunterscheidung getroffen, ob in einem 
Pseudo-Stratum zwei oder drei Schulen sind (s.o): Bei drei Schulen wird zufällig 
ausgewählt, ob bei ein oder zwei Schulen \code{jkrep=0} gesetzt wird.

* Die Sortierung nach dem impliziten Strata Schulgröße erfolgt hier absteigend, 
nachzulesen im Buch-Kapitel.

\ifelse{latex}{%start latex
\code{}\cr
\code{> ### Ergänzung zum Buch: Hilfsfunktion zones.within.stratum}\cr
\code{> zones.within.stratum <- function(offset,n.str) \{}\cr
\code{+   maxzone <- offset-1+floor(n.str/2)}\cr
\code{+   zones <- sort(rep(offset:maxzone,2))}\cr
\code{+   if (n.str \%\% 2 == 1) zones <- c(zones,maxzone)}\cr
\code{+   return(zones) \}}\cr
\code{> ### Ende der Ergänzung}\cr
\code{> }\cr
\code{> # Sortieren der Schulliste (explizite und implizite Strata)}\cr
\code{> schule <- schule[schule$SInSamp==1,]}\cr
\code{> schule <- schule[order(schule$stratum,-schule$NSchueler),]}\cr
\code{> }\cr
\code{> # Unterteilung in Pseudostrata}\cr
\code{> cnt.strata <- length(unique(schule$stratum))}\cr
\code{> offset <- 1}\cr
\code{> jkzones.vect <- integer()}\cr
\code{> for (i in 1:cnt.strata) \{}\cr
\code{+   n.str <- table(schule$stratum)[i]}\cr
\code{+   jkzones.vect <-}\cr 
\code{+     c(jkzones.vect,zones.within.stratum(offset,n.str))}\cr
\code{+   offset <- max(jkzones.vect)+1 \}}\cr
\code{> schule$jkzone <- jkzones.vect}\cr
\code{> }\cr
\code{> # Zufällige Auswahl von Schulen mit Gewicht 0}\cr
\code{> schule$jkrep <- 1}\cr
\code{> cnt.zones <- max(schule$jkzone)}\cr
\code{> jkrep.rows.null <- integer()}\cr
\code{> for (i in 1:cnt.zones) \{}\cr
\code{+   rows.zone <- which(schule$jkzone==i)}\cr
\code{+ ### Ergänzung zum Buch: Fallunterscheidung je nach Anzahl Schulen in der Zone}\cr
\code{+   if (length(rows.zone)==2) jkrep.rows.null <- }\cr
\code{+     c(jkrep.rows.null,sample(rows.zone,size=1))}\cr
\code{+   else \{}\cr
\code{+       num.null <- sample(1:2,size=1)}\cr
\code{+       jkrep.rows.null <- }\cr
\code{+         c(jkrep.rows.null,sample(rows.zone,size=num.null)) }\cr
\code{+     \} \}}\cr
\code{> schule[jkrep.rows.null,]$jkrep <- 0}\cr
}{\verb{### Ergänzung zum Buch: Hilfsfunktion zones.within.stratum
zones.within.stratum <- function(offset,n.str) {
  maxzone <- offset-1+floor(n.str/2)
  zones <- sort(rep(offset:maxzone,2))
  if (n.str \%\% 2 == 1) zones <- c(zones,maxzone)
  return(zones) }
### Ende der Ergänzung

# Sortieren der Schulliste (explizite und implizite Strata)
schule <- schule[schule$SInSamp==1,]
schule <- schule[order(schule$stratum,-schule$NSchueler),]

# Unterteilung in Pseudostrata 
cnt.strata <- length(unique(schule$stratum))
offset <- 1
jkzones.vect <- integer()
for (i in 1:cnt.strata) {
  n.str <- table(schule$stratum)[i]
  jkzones.vect <- 
    c(jkzones.vect,zones.within.stratum(offset,n.str))
  offset <- max(jkzones.vect)+1 }
schule$jkzone <- jkzones.vect

# Zufällige Auswahl von Schulen mit Gewicht 0
schule$jkrep <- 1
cnt.zones <- max(schule$jkzone)
jkrep.rows.null <- integer()
for (i in 1:cnt.zones) {
  rows.zone <- which(schule$jkzone==i)
### Ergänzung zum Buch: Fallunterscheidung je nach Anzahl Schulen in der Zone
  if (length(rows.zone)==2) jkrep.rows.null <- 
    c(jkrep.rows.null,sample(rows.zone,size=1))
  else {
      num.null <- sample(1:2,size=1)
      jkrep.rows.null <- 
        c(jkrep.rows.null,sample(rows.zone,size=num.null)) 
    } }
schule[jkrep.rows.null,]$jkrep <- 0
}
}%end else / end ifelse latex

} %% end Abschnitt 5.3, Listing 1

\subsection{Abschnitt 5.3: Anwendung per Jackknife-Repeated-Replication, 
Listing 2}{
Die Anwendung von Jackknife-Repeated-Replication zur Abschätzung der 
Stichprobenvarianz wird im folgenden am Schülerdatensatz demonstriert, weswegen 
\code{jkzone} und \code{jkrep} zunächst auf diese Aggregatsebene übertragen 
werden.
In einer Schleife werden replicate weights mittels \code{jkzone} und 
\code{jkrep} generiert. 
Diese beziehen sich auf das normierte Schülergewicht \code{NormStudwgt}.
Man beachte: Es gilt entweder \code{in.zone==0} oder \code{(in.zone-1)==0}, 
sodass Formel 5 aus dem Buch-Kapitel direkt in einer Addition ausgedrückt werden 
kann. 
Es entstehen so viele replicate weights (\code{w_fstr1} usw.) wie 
Jackknife-Zonen existieren.

\ifelse{latex}{%start latex
\code{}\cr
\code{> # Übertragung auf Schülerebene}\cr
\code{> schueler <- }\cr
\code{+   merge(schueler,schule[,c("SKZ","jkzone","jkrep")],all.x=TRUE)}\cr
\code{> # Schleife zur Generierung von Replicate Weights}\cr
\code{> for (i in 1:cnt.zones) \{}\cr
\code{+   in.zone <- as.numeric(schueler$jkzone==i)}\cr
\code{+   schueler[paste0("w_fstr",i)] <-   # vgl. Formel 5}\cr
\code{+     in.zone * schueler$jkrep * schueler$NormStudwgt * 2 +}\cr
\code{+     (1-in.zone) * schueler$NormStudwgt \}}\cr
}{\verb{# Übertragung auf Schülerebene
schueler <- 
  merge(schueler,schule[,c("SKZ","jkzone","jkrep")],all.x=TRUE)
# Schleife zur Generierung von Replicate Weights
for (i in 1:cnt.zones) {
  in.zone <- as.numeric(schueler$jkzone==i)
  schueler[paste0("w_fstr",i)] <-   # vgl. Formel 5
    in.zone * schueler$jkrep * schueler$NormStudwgt * 2 +
    (1-in.zone) * schueler$NormStudwgt }
}
}%end else / end ifelse latex

} %% end Abschnitt 5.3, Listing 2

\subsection{Abschnitt 5.3: Anwendung per Jackknife-Repeated-Replication, 
Listing 3}{
Als einfaches Beispiel wird der Anteil Mädchen (\code{perc.female}) in der 
Population aus der Stichprobe heraus geschätzt. Die Schätzung selbst erfolgt als 
Punktschätzung über das normierte Schülergewicht.
Zur Bestimmung der Stichprobenvarianz \code{var.jrr} wird der Anteil wiederholt
mit allen replicate weights berechnet und die quadrierte Differenz zur 
Punktschätzung einfach aufsummiert (Formel 6 aus dem Buch-Kapitel).

\ifelse{latex}{%start latex
\code{}\cr
\code{> # Schätzung mittels Gesamtgewicht}\cr
\code{> n.female <- sum(schueler[schueler$female==1,]$NormStudwgt)}\cr
\code{> perc.female <- n.female / sum(schueler$NormStudwgt)}\cr
\code{> # wiederholte Berechnung und Varianz}\cr
\code{> var.jrr = 0}\cr
\code{> for (i in 1:cnt.zones) \{}\cr
\code{+   n.female.rep <- }\cr
\code{+     sum(schueler[schueler$female==1,paste0("w_fstr",i)])}\cr
\code{+   perc.female.rep <- }\cr
\code{+     n.female.rep / sum(schueler[paste0("w_fstr",i)])}\cr
\code{+   var.jrr <-   # vgl. Formel 6}\cr
\code{+     var.jrr + (perc.female.rep - perc.female) ^ 2.0 \} }\cr
}{\verb{# Schätzung mittels Gesamtgewicht
n.female <- sum(schueler[schueler$female==1,]$NormStudwgt)
perc.female <- n.female / sum(schueler$NormStudwgt)
# wiederholte Berechnung und Varianz
var.jrr = 0
for (i in 1:cnt.zones) {
  n.female.rep <- 
    sum(schueler[schueler$female==1,paste0("w_fstr",i)])
  perc.female.rep <- 
    n.female.rep / sum(schueler[paste0("w_fstr",i)])
  var.jrr <-   # vgl. Formel 6
    var.jrr + (perc.female.rep - perc.female) ^ 2.0 }
}
}%end else / end ifelse latex

}%% end Abschnitt 5.3, Listing 3
  
} % end details

%% ----------------------------------------------------------------------------
%% Examples
\examples{
data(datenKapitel02)
schueler <- datenKapitel02$schueler
schule <- datenKapitel02$schule
set.seed(20150506)

\dontrun{
## -------------------------------------------------------------
## Abschnitt 4.1: Stratifizierung
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 4.1, Listing 1

# Information in Strata
strata <- aggregate(schule[,"NSchueler", drop = FALSE],
                    by=schule[,"stratum", drop = FALSE], sum)
colnames(strata)[2] <- "NSchuelerStratum"

## -------------------------------------------------------------
## Abschnitt 4.2: Schulenziehung
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 1

# Dummyvariable Klassenziehung
schule$Klassenziehung <- 0
schule[which(schule$NKlassen>3), "Klassenziehung"] <- 1

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 2

# erwarteter Beitrag zur Stichprobe pro Schule 
schule$NSchueler.erw <- schule$NSchueler
ind <- which(schule$Klassenziehung == 1)
schule[ind, "NSchueler.erw"] <- 
  schule[ind, "NSchueler"]/schule[ind, "NKlassen"]*3

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 3

# relativer Anteil Schüler pro Schule
temp <- merge(schule[, c("SKZ","stratum","NSchueler")], 
              strata[, c("stratum","NSchuelerStratum")])
schule$AnteilSchueler <- 
  temp$NSchueler/temp$NSchuelerStratum
# mittlere Anzahl von Schülern pro Schule
strata$"NSchueler/Schule.erw" <- 
  rowsum(apply(schule, 1, function(x)
    x["NSchueler.erw"]*x["AnteilSchueler"]), schule$stratum)

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 4

# Bestimmung Anzahl zu ziehender Schulen
strata$Schulen.zu.ziehen <- 
  round(2500/strata[,"NSchueler/Schule.erw"])

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 5

# Schulenliste nach Stratum und Groesse ordnen
schule <- 
  schule[order(schule$stratum, schule$NSchueler),]

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 6

# Berechnung Sampling-Intervall
strata$Samp.Int <- 
  strata$NSchuelerStratum/strata$Schulen.zu.ziehen

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 7

# Startwerte bestimmen
strata$Startwert <- 
  sapply(ceiling(strata$Samp.Int), sample, size = 1)

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 8

# Schüler-Tickets
tickets <- sapply(1:4, function(x)
  trunc(0:(strata[strata$stratum==x,"Schulen.zu.ziehen"]-1)
  * strata[strata$stratum==x, "Samp.Int"] +
    strata$Startwert[x]))

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 9

# kummulierte Schüleranzahl pro Stratum berechnen
schule$NSchuelerKum <- 
  unlist(sapply(1:4, function(x)
    cumsum(schule[schule$stratum==x, "NSchueler"])))

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 10

# Schulen ziehen
schule$SInSamp <- 0 
for(s in 1:4) {
  NSchuelerKumStrat <- 
    schule[schule$stratum==s, "NSchuelerKum"]
  inds <- sapply(tickets[[s]], function(x)
    setdiff(which(NSchuelerKumStrat <= x),
            which(NSchuelerKumStrat[-1] <= x)))
  schule[schule$stratum==s, "SInSamp"][inds] <- 1 }

# -------------------------------------------------------------
# Abschnitt 4.2, Listing 11

# Berechnung Ziehungswahrscheinlichkeit Schule
temp <- merge(schule[, c("stratum", "AnteilSchueler")],
  strata[, c("stratum", "Schulen.zu.ziehen")])
schule$Z.Wsk.Schule <- 
  temp$AnteilSchueler*temp$Schulen.zu.ziehen

## -------------------------------------------------------------
## Abschnitt 4.3: Klassenziehung
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 4.3, Listing 1

### Klassenziehung (Alternative 2)
schukla <- unique(merge(
  schule[, c("SKZ","NKlassen", "Klassenziehung", 
    "Z.Wsk.Schule", "SInSamp")],
    schueler[, c("SKZ", "idclass")], by="SKZ"))
schukla$KlInSamp <- 0
for(skz in unique(schukla[schukla$SInSamp==1,"SKZ"])) {
  temp <- schukla[schukla$SKZ==skz, "idclass"]
  schukla[schukla$idclass\%in\%temp[sample.int(
    min(3, length(temp)))], "KlInSamp"] <- 1 }

# -------------------------------------------------------------
# Abschnitt 4.3, Listing 2

# Ziehungswahrscheinlichkeit Klasse 
schukla$Z.Wsk.Klasse <- ((1 - schukla$Klassenziehung) * 1 + 
     schukla$Klassenziehung * 3 / schukla$NKlassen) 

## -------------------------------------------------------------
## Abschnitt 4.4: Gewichtung
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 4.4, Listing 1

### Gewichte
schueler <- merge(schueler, schukla[, c("idclass", "KlInSamp", "Z.Wsk.Schule", 
                                        "Z.Wsk.Klasse")],
                  by="idclass", all.x=T)
# Ziehungswahrscheinlichkeiten Schueler 
schueler$Z.Wsk.Schueler <- 
  schueler$Z.Wsk.Schule * schueler$Z.Wsk.Klasse

# -------------------------------------------------------------
# Abschnitt 4.4, Listing 2

schueler <- schueler[schueler$KlInSamp==1,]
# Nonresponse Adjustment 
temp <- merge(schueler[, c("idclass", "Z.Wsk.Schueler")],
  aggregate(schueler$teilnahme, 
    by=list(schueler$idclass),
    function(x) sum(x)/length(x)), 
  by.x="idclass", by.y="Group.1")

# -------------------------------------------------------------
# Abschnitt 4.4, Listing 3

# Schülergewichte
schueler$studwgt <- 1/temp$x/temp$Z.Wsk.Schueler

# -------------------------------------------------------------
# Abschnitt 4.4, Listing 4

# Normierung
Normierung <- strata$NSchuelerStratum / 
  rowsum(schueler$studwgt * schueler$teilnahme,
         group = schueler$Stratum)
schueler$NormStudwgt <- 
  schueler$studwgt * Normierung[schueler$Stratum]

## -------------------------------------------------------------
## Abschnitt 5.3: Jackknife-Repeated-Replication
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 5.3, Listing 1

### Ergänzung zum Buch: Hilfsfunktion zones.within.stratum
zones.within.stratum <- function(offset,n.str) {
  maxzone <- offset-1+floor(n.str/2)
  zones <- sort(rep(offset:maxzone,2))
  if (n.str \%\% 2 == 1) zones <- c(zones,maxzone)
  return(zones) }
### Ende der Ergänzung

# Sortieren der Schulliste (explizite und implizite Strata)
schule <- schule[schule$SInSamp==1,]
schule <- schule[order(schule$stratum,-schule$NSchueler),]

# Unterteilung in Pseudostrata 
cnt.strata <- length(unique(schule$stratum))
offset <- 1
jkzones.vect <- integer()
for (i in 1:cnt.strata) {
  n.str <- table(schule$stratum)[i]
  jkzones.vect <- 
    c(jkzones.vect,zones.within.stratum(offset,n.str))
  offset <- max(jkzones.vect)+1 }
schule$jkzone <- jkzones.vect

# Zufällige Auswahl von Schulen mit Gewicht 0
schule$jkrep <- 1
cnt.zones <- max(schule$jkzone)
jkrep.rows.null <- integer()
for (i in 1:cnt.zones) {
  rows.zone <- which(schule$jkzone==i)
### Ergänzung zum Buch: Fallunterscheidung je nach Anzahl Schulen in der Zone
  if (length(rows.zone)==2) jkrep.rows.null <- 
    c(jkrep.rows.null,sample(rows.zone,size=1))
  else {
      num.null <- sample(1:2,size=1)
      jkrep.rows.null <- 
        c(jkrep.rows.null,sample(rows.zone,size=num.null)) 
    } }
schule[jkrep.rows.null,]$jkrep <- 0

# -------------------------------------------------------------
# Abschnitt 5.3, Listing 2

# Übertragung auf Schülerebene
schueler <- 
  merge(schueler,schule[,c("SKZ","jkzone","jkrep")],all.x=TRUE)
# Schleife zur Generierung von Replicate Weights
for (i in 1:cnt.zones) {
  in.zone <- as.numeric(schueler$jkzone==i)
  schueler[paste0("w_fstr",i)] <-   # vgl. Formel 5
    in.zone * schueler$jkrep * schueler$NormStudwgt * 2 +
    (1-in.zone) * schueler$NormStudwgt }

# -------------------------------------------------------------
# Abschnitt 5.3, Listing 3

# Schätzung mittels Gesamtgewicht
n.female <- sum(schueler[schueler$female==1,]$NormStudwgt)
perc.female <- n.female / sum(schueler$NormStudwgt)
# wiederholte Berechnung und Varianz
var.jrr = 0
for (i in 1:cnt.zones) {
  n.female.rep <- 
    sum(schueler[schueler$female==1,paste0("w_fstr",i)])
  perc.female.rep <- 
    n.female.rep / sum(schueler[paste0("w_fstr",i)])
  var.jrr <-   # vgl. Formel 6
    var.jrr + (perc.female.rep - perc.female) ^ 2.0 }
} % end dontrun
} % end Examples
