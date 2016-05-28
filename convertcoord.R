library(plyr)

latitudes <- c("40º2'30''","40º3'45''")
#-------------------------------------------------Mi fórmula anidada-----------------------------------------------
convert <- function(x){
  result <- lapply(lapply(strsplit(gsub("'","",gsub("([0-9]*).([0-9]*).([0-9]*).","\\1-\\2-\\3-",latitudes)),split="-"),as.numeric),function(x) x[1]+x[2]/60+x[3]/3600)
  result
}
convert(latitudes)
#-----------------------------------------------------------------------------------------------------------
convert <- function(x){
  a <- gsub("([0-9]*).([0-9]*).([0-9]*).","\\1-\\2-\\3-",latitudes)
  b <- gsub("'","",a)
  c <- strsplit(b,split="-")
  d <- lapply(c,as.numeric)
  result <- lapply(d,function(x) x[1]+x[2]/60+x[3]/3600)
  result
}
#-------------------------------------------------------------------------------
##Solucion alternativa

tmp <- gregexpr("[0-9]+",latitudes)
tmp <- regmatches(latitudes,tmp)
tmp <- lapply(tmp,as.numeric)
res <- lapply(tmp,function(x) x[1]+x[2]/60+x[3]/3600)
res