rowMaxs <- function(df, na.rm=TRUE) {
  
  # Returns the max value in each column of a data.frame or matrix

  # NOTE: max() and min() & this function will handle character elements by coercing all others to character (see the help for Comparison http://127.0.0.1:45798/help/library/base/help/Comparison)
  # which can be confusing -- e.g., note that min(c(8,10,'txt')) returns '10' not '8' and max returns 'txt'
  
  # Note than max() and min() default to na.rm=FALSE, but this function defaults to na.rm=TRUE because that just seems more frequently useful
  
  if (is.matrix(df)) {df <- data.frame(df, stringsAsFactors=FALSE)}
  
  valid.cols <- sapply(df, function(x) { is.numeric(x) || is.logical(x) || is.character(x)})
  stopifnot(any(valid.cols)) 
  # or could just return NA?:    
  # if (!any(valid.cols)) {return(NA)}
  if (any(!valid.cols) ) {warning('using only numeric (double or integer) or logical or character columns -- ignoring other columns ')}
  
  result <- do.call(pmax, c(df[ , valid.cols], na.rm=na.rm))
  
  # to mimic how min() and max() behave, return Inf or -Inf if no non-missing arguments to min or max respectively?
  # to call this and suppress that warning, use suppressWarnings( f(x) )
  # to avoid that and get NA rather than Inf or -Inf, do this:
  #   z=colMaxs(x, na.rm=na.rm)
  #   z[is.infinite(z)] <- NA
  
  result[nononmissing <- rowSums(!is.na(df[ , valid.cols]))==0] <- -Inf
  if (any(nononmissing)) {warning('where no non-missing arguments, returning -Inf')}
  return(result)

  # df = data.frame of numeric values, i.e. a list of vectors passed to pmax
  # Value returned is vector, each element is max of a row of df  
  
  # Example: 
  if (1==0) {
    blah <- rbind(NA, data.frame(a=c(0, 0:8), b=c(0.1+(0:9)), c=c(1:10), d=c(rep(NA, 10)), e=TRUE, f=factor('factor'), g='words', stringsAsFactors=FALSE) )
    cbind(blah, min=rowMins(blah), max=rowMaxs(blah))
    rbind(blah, min=colMins(blah), max=colMaxs(blah))
    blah <- blah[ , sapply(blah, function(x) is.numeric(x) || is.logical(x)) ]
    cbind(blah, min=rowMins(blah), max=rowMaxs(blah), mean=rowMeans(blah, na.rm=TRUE), sum=rowSums(blah, na.rm=TRUE))
    rbind(blah, min=colMins(blah), max=colMaxs(blah), mean=colMeans(blah, na.rm=TRUE), sum=colSums(blah, na.rm=TRUE))
  }
}

# had been doing this:
# mymin <- do.call(pmin, c(bg[,names.ej.pctile], na.rm=TRUE)) 
# mymax <- do.call(pmax, c(bg[,names.ej.pctile], na.rm=TRUE)) 
# now can do this:
# mymin <- rowMins(bg[,names.ej.pctile])
# mymax <- rowMaxs(bg[,names.ej.pctile])
