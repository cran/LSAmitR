\name{LSAmitR-package}
\alias{LSAmitR-package}
\alias{LSAmitR}
\docType{package}
\encoding{UTF-8}

\title{
Large-Scale Assessment mit R
}
\description{
    ...
}
\details{
\tabular{ll}{
Package: \tab \pkg{LSAmitR}\cr
Type: \tab Package\cr
Version: \tab 0.02 \cr
Publication Year: \tab 2016\cr
License: \tab GPL (>=2.0)\cr
URL: \tab \url{http://www.bifie.at}\cr
}
}
\author{
Thomas Kiefer [aut, cre], Alexander Robitzsch [aut], Matthias Trendtel [aut], Robert Fellinger [aut]

Maintainer: Thomas Kiefer <t.kiefer@bifie.at>
}
\references{
Schreiner, C. & Breit, S. (2016) \emph{Large-Scale Assessment mit R:
Methodische Grundlagen der österreichischen Bildungsstandards-Überprüfung}. 
Wien: facultas WUV.
}

\seealso{
Zu \code{\link{Kapitel 0}}, Testkonzeption: das Konzept der 
österreichischen Bildungsstandards-Überprüfung.\cr
Zu \code{\link{Kapitel 1}}, Testkonstruktion.\cr
Zu \code{\link{Kapitel 2}}, Stichprobenziehung.\cr
Zu \code{\link{Kapitel 3}}, Standard-Setting.\cr
Zu \code{\link{Kapitel 4}}, Differenzielles Itemfunktionieren in Subgruppen.\cr
Zu \code{\link{Kapitel 5}}, Testdesign.\cr
Zu \code{\link{Kapitel 6}}, Skalierung und Linking.\cr
Zu \code{\link{Kapitel 7}}, Statistische Analysen produktiver Kompetenzen.\cr
Zu \code{\link{Kapitel 8}}, Behandlung fehlender Daten und PV-Ziehung.\cr
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
