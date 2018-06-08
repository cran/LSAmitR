\name{LSAmitR-package}
\alias{LSAmitR-package}
\alias{LSAmitR}
\docType{package}
\encoding{UTF-8}

\title{
\packageTitle{LSAmitR}
}

\description{
Das Bundesinstitut für Bildungsforschung, Innovation und Entwicklung des 
österreichischen Schulwesens (BIFIE) führt die Überprüfung der Bildungsstandards 
(BIST-Ü) in Österreich durch. "Large-Scale Assessment mit \R" ist ein Handbuch der 
grundlegenden Methodik, die bei diesen Überprüfungen zum Einsatz kommt. 
Angefangen bei der Testkonstruktion bis zu Aspekten der Rückmeldung werden die 
dabei eingesetzten methodischen Verfahren dargestellt und diskutiert sowie deren 
Anwendung in \R anhand von Beispieldatensätzen, die in diesem \R-Paket zur 
Verfügung gestellt werden, illustriert.\cr

Beispiele, die sich durch den Band ziehen, lehnen sich an die BIST-Ü in Englisch 
im Jahr 2013 an. Die Daten, die den Ausführungen zugrunde liegen, sind jedoch
keine Echtdaten und erlauben daher auch keine Rekonstruktion der in den
Ergebnisberichten publizierten Kennwerte. Es handelt sich (mindestens) um 
partiell-synthetische Daten, die reale Kovarianzstrukturen zwischen Kovariaten
und den Leistungsdaten abbilden sowie eine Mehrebenenstruktur simulieren, die in 
den LSA-Erhebungen typischerweise auftreten. Die Datenmuster
können weder als Einzelstücke noch als Ganzes auf tatsächliche Testpersonen, auf 
Klassen oder Schulen zurückgeführt werden. Ebenso führen
Ergebnisse, die in den Ausführungen der einzelnen Kapitel erzielt werden,
nicht zu den Datensätzen, die in späteren Kapiteln verwendet werden (z. B.
entspricht die Stichprobe, die in Kapitel 2 gezogen wird, nicht jener, deren
Testwerte in Kapitel 6 oder Kapitel 7 untersucht werden).
}

\author{
\packageAuthor{LSAmitR}

Maintainer: \packageMaintainer{LSAmitR}
}
\references{
Breit, S. & Schreiner, C. [HG.] (2016). \emph{Large-Scale Assessment mit \R:
Methodische Grundlagen der österreichischen Bildungsstandardüberprüfung}. 
Wien: facultas.

\url{https://www.bifie.at/node/3770}
}

\seealso{
Zu \code{\link{Kapitel 0}}, Konzeption der Überprüfung der Bildungsstandards in Österreich.\cr
Zu \code{\link{Kapitel 1}}, Testkonstruktion.\cr
Zu \code{\link{Kapitel 2}}, Stichprobenziehung.\cr
Zu \code{\link{Kapitel 3}}, Standard-Setting.\cr
Zu \code{\link{Kapitel 4}}, Differenzielles Itemfunktionieren in Subgruppen.\cr
Zu \code{\link{Kapitel 5}}, Testdesign.\cr
Zu \code{\link{Kapitel 6}}, Skalierung und Linking.\cr
Zu \code{\link{Kapitel 7}}, Statistische Analysen produktiver Kompetenzen.\cr
Zu \code{\link{Kapitel 8}}, Fehlende Daten und Plausible Values.\cr
Zu \code{\link{Kapitel 9}}, Fairer Vergleich in der Rückmeldung.\cr
Zu \code{\link{Kapitel 10}}, Reporting und Analysen.\cr
Zu \code{\link{Kapitel 11}}, Aspekte der Validierung.
}

\examples{
\dontrun{
install.packages("LSAmitR", dependencies = TRUE)
library(LSAmitR)
package?LSAmitR
?"Kapitel 7"

data(datenKapitel07)
names(datenKapitel07)
dat <- datenKapitel07$prodRat
}}

\keyword{package}
