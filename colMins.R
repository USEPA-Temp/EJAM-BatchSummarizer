colMins <- function(df, na.rm=TRUE) {
  
  # Returns the minimum value in each column of a data.frame or matrix
  
  # ** Note than max() and min() default to na.rm=FALSE, but this function defaults to na.rm=TRUE because that just seems more frequently useful
  
  # Note if it were just as max() and min(), cols that are factors would make this fail, even if as.character() of the factor col would return a valid numeric vector
  # To fix that, did this:
  # if (is.factor(x)) {x<-as.numeric(as.character(x))}
  
  # based on how min() and max() behave, return Inf or -Inf if no non-missing arguments to min or max respectively?
  # to call this and suppress that warning, use suppressWarnings( f(x) )
  
  # NOTE: max() and min() & this function will handle character elements by coercing all others to character (see the help for Comparison http://127.0.0.1:45798/help/library/base/help/Comparison)
  # which can be confusing -- e.g., note that min(c(8,10,'txt')) returns '10' not '8' and max returns 'txt'
  
  if (is.matrix(df)) {
    return( apply(df, 2, function(x) {
      if (is.factor(x)) {x<-as.numeric(as.character(x))} # finds min of numbers or characters representing numbers, but stored as factors
      min(x, na.rm=na.rm)
    } ) )
  } else {
    return( sapply(df,   function(x) {
      if (is.factor(x)) {x<-as.numeric(as.character(x))} # finds min of numbers or characters representing numbers, but stored as factors
      min(x, na.rm=na.rm)
    } ) )  
  }
  
  # There is surely a faster way to do this, but this is certainly fast enough for what I need, and is simple
  # and isn't all that slow if there are few columns and many rows.
  # or  on mac, mclapply() would do it in parallel
  # or  tried  rowMins(as.data.frame(t(df))) but that is MUCH slower than this
  
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

