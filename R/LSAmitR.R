# ** Stichprobenziehung ----
zones.within.stratum <- function(offset,n.str) {
  maxzone <- offset-1+floor(n.str/2)
  zones <- sort(rep(offset:maxzone,2))
  if (n.str %% 2 == 1) zones <- c(zones,maxzone)
  return(zones) }

# # ** Statistische Analysen produktiver Kompetenzen ----
# summary.VarComp <- function(mod){ 
#   var.c <- VarCorr(mod)
#   var.c <- c(unlist(var.c) , attr(var.c , "sc")^2)
#   names(var.c)[length(var.c)] <- "Residual"
#   dfr1 <- data.frame(var.c)
#   colnames(dfr1) <- "Varianz"
#   dfr1 <- rbind(dfr1, colSums(dfr1))
#   rownames(dfr1)[nrow(dfr1)] <- "Total"
#   dfr1$prop.Varianz <- 100 * (dfr1$Varianz / dfr1$Varianz[nrow(dfr1)])
#   dfr1 <- round(dfr1,2)
#   return(dfr1)
# }

# ** Fairer Vergleich in der Rückmeldung ----
covainteraction <- function(dat,covas,nchar){
  for(ii in 1:(length(covas))){
    vv1 <- covas[ii]
    # Interaktion von vv1 mit sich selbst
    subname1 <- substr(vv1,1,nchar)
    intvar <- paste0(subname1, subname1)
    if(vv1 == covas[1]){
      dat.int <- dat[,vv1]*dat[,vv1];
      newvars <- intvar } else {
        dat.int <- cbind(dat.int,dat[,vv1]*dat[,vv1]); 
        newvars <- c(newvars,intvar) 
      }
    # Interaktion von vv1 mit restlichen Variablen
    if(ii < length(covas)){
      for(jj in ((ii+1):length(covas))){
        vv2 <- covas[jj]
        subname2 <- substr(vv2,1,nchar)
        intvar <- paste0(subname1, subname2)
        newvars <- c(newvars, intvar)
        dat.int <- cbind(dat.int,dat[,vv1]*dat[,vv2])
      }
    }
    
  }
  dat.int <- data.frame(dat.int)
  names(dat.int) <- newvars
  return(dat.int)
}

# ** Reporting und Analysen ----
quintfunct <- function(X,w){
  quint<-wtd.quantile(X[,1],weights=w,
                      probs=c(0.2,0.4,0.6,0.8),
                      na.rm=TRUE)
  return(unname(quint))
}
