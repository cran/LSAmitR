%% ----------------------------------------------------------------------------
%% Header
\name{Kapitel 5}
\alias{Kapitel 5}
\encoding{UTF-8}
\title{Kapitel 5: Testdesign}

\description{
Das ist die Nutzerseite zum Kapitel 5, Testdesign, im Herausgeberband 
\emph{Large-Scale Assessment mit \R}:  Methodische Grundlagen der österreichischen 
Bildungsstandard-Überprüfung.\cr
Im Abschnitt \strong{Details} werden die im Kapitel verwendeten \R-Syntaxen zur 
Unterstützung für Leser/innen kommentiert und dokumentiert. Im Abschnitt
\strong{Examples} werden die \R-Syntaxen des Kapitels vollständig widergegeben und 
gegebenefalls erweitert. 
}

%% ----------------------------------------------------------------------------
%% Intermediate Deklarationes
\references{
  Kiefer, T., Kuhn, J.-T. & Fellinger, R. (2016). Testdesign. In C. Schreiner & 
  S. Breit (Hrsg.): \emph{Large-Scale Assessment mit \R}: Methodische Grundlagen 
  der österreichischen Bildungsstandard-Überprüfung (pp. xx-yy). Wien: facultas 
  wuv.
}

% \note{
% %wenn es Hinweise gibt
% }

\author{
  Thomas Kiefer, Jörg-Tobias Kuhn, Robert Fellinger
}

\seealso{
%Verweis zum nächsten und zum vorigen Kapitel
Zurück zu \code{\link{Kapitel 4}}, Differenzielles Itemfunktionieren in 
Subgruppen.\cr
Zu \code{\link{Kapitel 6}}, Skalierung und Linking.\cr
Zur \code{\link[LSAmitR:LSAmitR-package]{Übersicht}}.
%Verweis zu Hilfeseiten eventuell zusätzlicher R-Funktionen. (\cr nach Übersicht einfügen)
%Zur Hilfeseite von fit.r \code{\link{fit.r}}
}

%% ----------------------------------------------------------------------------
%% Details
\details{
\subsection{Vorbereitungen}{
..
\verb{
..
}
}%end Subsection Vorbereitungen


\subsection{Abschnitt x.1: KTT}{
\subsection{Listing 1: Skalierung der Rekodierten Daten}{
..
\verb{
..
}
}%end Subsection Listing 1: Skalierung der Rekodierten Daten
}%end Subsection Abschnitt x.1: KTT
}%end Details

%% ----------------------------------------------------------------------------
%% Examples
\examples{
library(tensor)
set.seed(1337)

data(datenKapitel05)
dat.ib <- datenKapitel05$tdItembank
dat.bib <- datenKapitel05$tdBib2d
dat.bibPaare <- datenKapitel05$tdBibPaare

\dontrun{
## -------------------------------------------------------------
## Abschnitt 5.3.2: ATA Methode für das Blockdesign
## -------------------------------------------------------------

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 1: Initialisierung
#

library(tensor)

nTh <- 30
nPos <- 6
nBl <- 30
inc <- array(0, dim = c(nTh, nPos, nBl))

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 2: Startdesign
#

for(tt in 1:nTh){
  inc[tt, , sample(1:nBl, nPos)] <- diag(1, nPos)
}

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 3: Zielfunktion
#
des <- inc
desAllePos <- tensor(des, rep(1, nPos), 2, 1)

blockPaarInd <- upper.tri(diag(nrow = nBl))
blockPaar <- crossprod(desAllePos)[blockPaarInd]

err.bb <- blockPaar
err.bb[blockPaar >= 2] <- blockPaar[blockPaar >= 2] - 2
err.bb[blockPaar <= 1] <- 1 - blockPaar[blockPaar <= 1]

objective <- sum(err.bb) / length(err.bb)
objWgt <- 2^0

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 4: Studienzuweisung
#

blMatching <- seq(6, nBl, 6)

nbStatus <- list(
  (desAllePos[1:6, -(1:12)] > 0) / (6 * 18),      # 1
  (desAllePos[25:30, -(19:30)] > 0) / (6 * 18),   # 2
  (rowSums(desAllePos[, blMatching]) != 1) / nTh  # 3
)
nbStatus <- unlist(lapply(nbStatus, sum))

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 5: Erweiterung Positionsbalancierung
#

# 4
nbPos <- sum((colSums(des) != 1) / (nPos * nBl))
# 5
nbPos.pLSA <- list(
  (colSums(des[1:6, 1:2, 1:12], dims = 2) != 1) / 12,
  (colSums(des[1:6, 3:4, 1:12], dims = 2) != 1) / 12,
  (colSums(des[1:6, 5:6, 1:12], dims = 2) != 1) / 12
)
nbPos.pLSA <- sum(unlist(lapply(nbPos.pLSA, sum)) / 3)
# 6
nbPos.link <- list(
  (colSums(des[25:30, 1:2, 19:30], dims = 2) != 1) / 12,
  (colSums(des[25:30, 3:4, 19:30], dims = 2) != 1) / 12,
  (colSums(des[25:30, 5:6, 19:30], dims = 2) != 1) / 12
)
nbPos.link <- sum(unlist(lapply(nbPos.link, sum)) / 3)

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 6: Zusammenfügen
#

nb <- c(nbStatus, nbPos, nbPos.pLSA, nbPos.link)
nbWgt <- c(
  rep(2^5, length(nbStatus)),
  rep(2^6, length(nbPos)),
  rep(2^4, length(nbPos.pLSA)),
  rep(2^3, length(nbPos.link))
)

nbWgt.norm <- nbWgt / (sum(nbWgt) + objWgt)
objWgt.norm <- objWgt / (sum(nbWgt) + objWgt)
oDes <- objWgt.norm \%*\% objective + nbWgt.norm \%*\% nb

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 6a: Ergänzung zum Buch
# 
#

fit <- function(des){
  desAllePos <- tensor(des, rep(1, nPos), 2, 1)
  
  #
  blockPaarInd <- upper.tri(diag(nrow = nBl))
  blockPaar <- crossprod(desAllePos)[blockPaarInd]
  
  err.bb <- blockPaar
  err.bb[blockPaar >= 2] <- blockPaar[blockPaar >= 2] - 2
  err.bb[blockPaar <= 1] <- 1 - blockPaar[blockPaar <= 1]
  
  objective <- sum(err.bb) / length(err.bb)
  objWgt <- 2^0
  
  #
  nbStatus <- list(
    (desAllePos[1:6, -(1:12)] > 0) / (6 * 18),      # 1
    (desAllePos[25:30, -(19:30)] > 0) / (6 * 18),   # 2
    (rowSums(desAllePos[, blMatching]) != 1) / nTh  # 3
  )
  nbStatus <- unlist(lapply(nbStatus, sum))
  
  # 4
  nbPos <- sum((colSums(des) != 1) / (nPos * nBl))
  # 5
  nbPos.pLSA <- list(
    (colSums(des[1:6, 1:2, 1:12], dims = 2) != 1) / 12,
    (colSums(des[1:6, 3:4, 1:12], dims = 2) != 1) / 12,
    (colSums(des[1:6, 5:6, 1:12], dims = 2) != 1) / 12
  )
  nbPos.pLSA <- sum(unlist(lapply(nbPos.pLSA, sum)) / 3)
  # 6
  nbPos.link <- list(
    (colSums(des[25:30, 1:2, 19:30], dims = 2) != 1) / 12,
    (colSums(des[25:30, 3:4, 19:30], dims = 2) != 1) / 12,
    (colSums(des[25:30, 5:6, 19:30], dims = 2) != 1) / 12
  )
  nbPos.link <- sum(unlist(lapply(nbPos.link, sum)) / 3)
  
  #
  nb <- c(nbStatus, nbPos, nbPos.pLSA, nbPos.link)
  nbWgt <- c(
    rep(2^5, length(nbStatus)),
    rep(2^6, length(nbPos)),
    rep(2^4, length(nbPos.pLSA)),
    rep(2^3, length(nbPos.link))
  )
  nbWgt.norm <- nbWgt / (sum(nbWgt) + objWgt)
  objWgt.norm <- objWgt / (sum(nbWgt) + objWgt)
  oDes <- objWgt.norm \%*\% objective + nbWgt.norm \%*\% nb
  
  return(oDes)
}

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 7: Initialisierung des Algorithmus
#

# t <- 1; t.min <- 1e-5; c <- 0.7; L <- 10000; l <- 1
t <- 1; tMin <- 1e-5; c <- 0.9; L <- 100000; l <- 1

fitInc <- fit(inc)

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 8: Störung
#

thisTh <- (l - 1) \%\% nTh + 1
child <- inc

bloeckeTh <- which(colSums(child[thisTh, , ]) == 1)
raus <- sample(bloeckeTh, 1)
rein <- sample(setdiff(1:nBl, bloeckeTh), 1)

child[thisTh, , rein] <- child[thisTh, , raus]
child[thisTh, , raus] <- 0


# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 9: Survival
#

fitChild <- fit(child)

behalte <- fitChild < fitInc
if(!behalte){
  pt <- exp(-(fitChild - fitInc) / t)
  behalte <- runif(1) <= pt
}

if(behalte){
  inc <- child
  fitInc <- fitChild
}

# -------------------------------------------------------------
# Abschnitt 5.3.2, Listing 9a: Ergänzung zum Buch
# 
#

start <- Sys.time()
best <- list(inc, fitInc)
while(t > tMin){
  while(l < L){
    thisTh <- (l - 1) \%\% nTh + 1
    child <- inc
    
    # Perturbation 
    bloeckeTh <- which(colSums(child[thisTh, , ]) == 1)
    raus <- sample(bloeckeTh, 1)
    rein <- sample(setdiff(1:nBl, bloeckeTh), 1)
    
    child[thisTh, , rein] <- child[thisTh, , raus]
    child[thisTh, , raus] <- 0
    
    # Fit und Survival
    fitChild <- fit(child)
    
    behalte <- fitChild < fitInc
    if(!behalte){
      pt <- exp(-(fitChild - fitInc) / t)
      behalte <- runif(1) <= pt
    }
    
    if(behalte){
      inc <- child
      fitInc <- fitChild
    }
    
    # Kontroll-Ausgaben
    if(fitInc < best[[2]]){
      best <- list(inc, fitInc)
    }    
    
    if (l \%\% 500 == 0) {
      cat("\r")
      cat(paste("l=", l), 
          paste("t=", as.integer(log(t) / log(c) + 1)),
          paste("fit=", round(fitInc, 4)), 
          paste("pt=", round(pt, 5)),        
          sep=";   ")
      cat("                     ")
      flush.console()
    }
    l <- l + 1
  }
  l <- 1
  t <- t * c
}
end <- Sys.time()

tdBib2d <- apply(inc, 1, function(bb){
  this <- which(colSums(bb) > 0)
  this[order((1:nrow(bb) \%*\% bb)[this])] 
})

## -------------------------------------------------------------
## Abschnitt 5.3.3: ATA Methode für die Item-zu-Block-Zuordnung
## -------------------------------------------------------------

set.seed(1338)

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 1: Initialisierung
#

nTh <- nrow(dat.bib)
nPos <- ncol(dat.bib)
nBl <- length(unique(unlist(dat.bib)))
blMatching <- seq(6, nBl, 6)

nI <- nrow(dat.ib)
itemsMatching <- which(dat.ib$format == "Matching")
itemsSonst <- which(dat.ib$format != "Matching")

# -------------------------------------------------------------
# Abschnitt 3.3, Listing 2: Startdesign
#

inc <- array(0, dim = c(nI, nBl))
for(bb in blMatching){
  inc[sample(itemsMatching, 2), bb] <- 1
}
for(bb in setdiff(1:nBl, blMatching)){
  inc[sample(itemsSonst, 7), bb] <- 1
}

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 3: Testheftebene
#

des <- inc
desTh <- des[, dat.bib[, 1]] + des[, dat.bib[, 2]] + 
  des[, dat.bib[, 3]] + des[, dat.bib[, 4]] + 
  des[, dat.bib[, 5]] + des[, dat.bib[, 6]]

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 4: IIF
#

theta <- c(380, 580)
InfoItem <- dat.ib[,grep("IIF", colnames(dat.ib))]
TIF <- (t(InfoItem) \%*\% desTh) / 37

objective <- - sum(TIF) / prod(dim(TIF))
objWgt <- 2^0

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 5: KEY
#

nbKey <- list(
  (colSums(desTh > 1) > 0) / nTh,              # 7
  ((rowSums(desTh[, 1:6]) > 0) +               # 8
     (rowSums(desTh[, 25:30]) > 0) > 1) / nI  
)
nbKey <- unlist(lapply(nbKey, sum))
nbWgt <- 2^c(7, 6)

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 6: Kategorial
#

# 9
zFocus.block <- c(0, 1, 1, 1, 1, 2, 0)
gFocus.block <- rowsum(des[, -blMatching], dat.ib$focus) - 
  zFocus.block
# 10
zFocus.form <- c(2, 6, 6, 6, 6, 13, 1)
gFocus.form <- rowsum(desTh, dat.ib$focus) - zFocus.form
# 11
gTopic.form <- rowsum(desTh, dat.ib$topic) - 4

nbKonstrukt <- list(
  colSums(gFocus.block < 0) / prod(dim(gFocus.block)), 
  colSums(gFocus.form > 0) / prod(dim(gFocus.form)), 
  colSums(gTopic.form > 0) / 30
)
nbKonstrukt <- unlist(lapply(nbKonstrukt, sum))
nbWgt <- c(nbWgt, 2^c(4, 4, 3))

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 7: Stetig
#

length.form <- ((dat.ib$audiolength + 13) \%*\% desTh) / 60
nbStetig <- list(
  (length.form > 32) / length(length.form),
  (length.form < 28) / length(length.form)
)
nbStetig <- unlist(lapply(nbStetig, sum))
nbWgt <- c(nbWgt, 2^c(3, 2))

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 8: Perturbation
#

thisBl <- 1
child <- inc

items.raus <- which(child[, thisBl] == 1)
raus <- sample(items.raus, 1)

bibPaar.bl <- dat.bibPaare[thisBl, ] != 0
items.bibPaare <- rowSums(child[, bibPaar.bl]) > 0
rein <- which(!items.bibPaare)

if(thisBl \%in\% blMatching){
  rein <- sample(intersect(rein, itemsMatching), 1)
}else{
  rein <- sample(intersect(rein, itemsSonst), 1)
}  

child[c(raus, rein), thisBl] <- c(0, 1)

# -------------------------------------------------------------
# Abschnitt 5.3.3, Listing 8a: Ergänzung zum Buch
# 
#

fit <- function(des, dat.ib, dat.bib){
  desTh <- des[, dat.bib[, 1]] + des[, dat.bib[, 2]] + 
    des[, dat.bib[, 3]] + des[, dat.bib[, 4]] + 
    des[, dat.bib[, 5]] + des[, dat.bib[, 6]]
  
  #
  TIF <- (t(InfoItem) \%*\% desTh) / 37
  
  objective <- - sum(TIF) / prod(dim(TIF))
  objWgt <- 2^0
  
  #
  nbKey <- list(
    (colSums(desTh > 1) > 0) / nTh,              # 7
    ((rowSums(desTh[, 1:6]) > 0) +               # 8
       (rowSums(desTh[, 25:30]) > 0) > 1) / nI  
  )
  nbKey <- unlist(lapply(nbKey, sum))
  nbWgt <- 2^c(7, 6)
  
  # 9
  zFocus.block <- c(0, 1, 1, 1, 1, 2, 0)
  gFocus.block <- rowsum(des[, -blMatching], dat.ib$focus) - 
    zFocus.block
  # 10
  zFocus.form <- c(2, 6, 6, 6, 6, 13, 1)
  gFocus.form <- rowsum(desTh, dat.ib$focus) - zFocus.form
  # 11
  gTopic.form <- rowsum(desTh, dat.ib$topic) - 4
  
  nbKonstrukt <- list(
    colSums(gFocus.block < 0) / prod(dim(gFocus.block)), 
    colSums(gFocus.form > 0) / prod(dim(gFocus.form)), 
    colSums(gTopic.form > 0) / 30
  )
  nbKonstrukt <- unlist(lapply(nbKonstrukt, sum))
  nbWgt <- c(nbWgt, 2^c(4, 4, 3))
  
  #
  length.form <- ((dat.ib$audiolength + 13) \%*\% desTh) / 60
  nbStetig <- list(
    (length.form > 32) / length(length.form),
    (length.form < 28) / length(length.form)
  )
  nbStetig <- unlist(lapply(nbStetig, sum))
  nbWgt <- c(nbWgt, 2^c(3, 2))
  
  #
  nb <- c(nbKey, nbKonstrukt, nbStetig)
  
  nbWgt.norm <- nbWgt / (sum(nbWgt) + objWgt)
  objWgt.norm <- objWgt / (sum(nbWgt) + objWgt)
  oDes <- objWgt.norm \%*\% objective + nbWgt.norm \%*\% nb
  
  return(oDes)
}

#
# t <- 1; tMin <- 1e-5; c <- 0.7; L <- 10000; l <- 1
# t <- 1; tMin <- 1e-5; c <- 0.8; L <- 25000; l <- 1
# t <- 1; tMin <- 1e-5; c <- 0.9; L <- 50000; l <- 1
t <- 1; tMin <- 1e-7; c <- 0.9; L <- 100000; l <- 1

#
fitInc <- fit(inc, dat.ib, dat.bib)
best <- list(inc, fitInc)
vers <- versBest <- 1
#
start <- Sys.time()
while(t > tMin){
  while(l < L){
    thisBl <- (l - 1) \%\% nBl + 1
    
    # Perturbation 
    child <- inc
    
    items.raus <- which(child[, thisBl] == 1)
    raus <- sample(items.raus, 1)
    
    bibPaar.bl <- dat.bibPaare[thisBl, ] != 0
    items.bibPaare <- rowSums(child[, bibPaar.bl]) > 0
    rein <- which(!items.bibPaare)
    
    if(thisBl \%in\% blMatching){
      rein <- sample(intersect(rein, itemsMatching), 1)
    }else{
      rein <- sample(intersect(rein, itemsSonst), 1)
    }  
    
    child[c(raus, rein), thisBl] <- c(0, 1)
    
    # Fit und Survival
    fitChild <- fit(child, dat.ib, dat.bib)
    
    behalte <- fitChild < fitInc
    if(!behalte){
      pt <- exp((fitInc - fitChild) / t)
      behalte <- runif(1) <= pt
    }
    
    if(behalte){
      inc <- child
      fitInc <- fitChild
    }
    
    if(fitInc < best[[2]]){
      best <- list(inc, fitInc)
      versBest <- versBest + 1
    }    
    
    # Kontroll-Ausgaben; ggf. löschen
    if (identical(inc, child)) vers <- vers + 1
    if (l \%\% 500 == 0) {
      cat("\r")
      cat(paste("l=", l), 
          paste("t=", as.integer(log(t) / log(c) + 1)),
          paste("versionen=", vers), 
          paste("versionenBest=", versBest), 
          paste("fit=", round(fitInc, 4)), 
          paste("fitBest=", round(best[[2]], 4)), 
          paste("pt=", round(pt, 5)),        
          sep=";   ")
      cat("                     ")
      flush.console()
    }
    l <- l + 1
  }
  l <- 1
  t <- t * c
}
end <- Sys.time()
}%end dontrun
}