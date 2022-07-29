library(zoo)
read.gsim.ts <- function(file){
  ### read  time series 
  ###
  ### file : file name   
  nm <- tail(strsplit(file,"/")[[1]],1)
  nm <- strsplit(nm,"\\.")[[1]][1]
  dd <- read.table(file,
                   header=TRUE,
                   comment.char="#",
                   sep=",",
                   strip.white=TRUE,
                   stringsAsFactors=FALSE)        
  #dd.date <- as.Date(dd[,"date"],format="%Y-%m-%d")
  #dd.zoo <- zoo(dd[,!names(dd)=="date",drop=FALSE],order.by=dd.date)
  return(dd)
}

read.gsim <- function(file){
  ### read ETH-ERDB file
  ### meta data and time series
  meta <- read.gsim.meta(file)
  tseries <- read.gsim.ts(file)
  op <- list(meta.data=meta,time.series=tseries)
  class(op) <- "gsim"
  return(op)
}