
resolveFileConnection<- function(filepath=NA,askfile=F) {
  con<-'' #file content
  if(askfile){
    filepath <- file.choose() # select file
    if (!exists("filepath")) {
      warning("select file canceled")
      return(NA)
    }
  } else if (is.na(filepath)) {
    warning("filepath parameter is NA")
    return(NA)
  }
  return ( file(filepath,"r") )
}

#get bundle from cpcht
getBundleName<- function(con) {
  bundle_line <- readLines(con,n=1)
  bundlename  <- trimws(gsub(pattern = '--- (.+) ---', replace = '\\1', x = bundle_line))
  return(bundlename)
}

getFormattedCPCHT<- function(con,saverawpath='data/fcpcht.txt') {
  tempcpcht  <- readLines(con)
  tempcpcht  <- gsub(pattern = '([[:digit:]]{4}\\.[[:digit:]]{3})', replace = '\\1\t', x = tempcpcht)
  tempcpcht  <- gsub(pattern = '--------    --------    --------', replace = '0.0000\t       0.0000\t    ', x = tempcpcht)
  tempcpcht  <- gsub(pattern = '\\s\\s\\[', replace = '\t\\[', x = tempcpcht)
  if (length(saverawpath)) writeLines(tempcpcht,saverawpath)
  return (tempcpcht)
}


getCPCHT <- function(filepath=NA,askfile=T,saverawpath=NULL) {
  con<-resolveFileConnection(filepath,askfile)
  bundlename<-getBundleName(con)
  fcpcht<-getFormattedCPCHT(con,saverawpath)
  cpcht<-read.delim(text =fcpcht,sep = '\t',skip=11,header=F, col.names=c('FAD','FAD2','LAD','LAD2','EVENT','PARAMS'))
  cpcht$EVENT<-trimws(cpcht$EVENT)
  return(cpcht)
}

cpcht<-getCPCHT(filepath=NA,askfile=T)
