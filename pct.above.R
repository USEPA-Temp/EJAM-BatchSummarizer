pct.above <- function(df, benchmarks='mean', benchnames='cutoff', na.rm=FALSE, or.tied=FALSE, below=FALSE, wts=1, of.what='all') {
  
  # These could be renamed  rows.above.count(), rows.above.pct(), rows.above.which()
  # to follow convention of cols.above.count(), cols.above.pct(), cols.above.which()
  # and same using below too, like rows.below.pct() etc.
  # and *** should make param names consistent, like x not df, cutoff(s) not benchmarks?, or.tied not gte
  # but *** cols versions and all should have wts, na.rm, benchmarks as vector not just 1 number, benchnames, params
  # and ** should have a "below" version for each variant
  
  
  # NA handling: This by default has na.rm=FALSE, so it returns the percent as a fraction of ALL rows (assuming wts is not NA!), not just the valid rows, 
  # so the rows with NA values in df are not in the numerator but are in the denominator.
  # That may be a problem for rows where there is a valid number in df but wts is NA for that row! Therefore, if is.na(wts) then that row of df is ignored, regardless of the na.rm setting.
  # If na.rm=TRUE, the function excludes rows for which df is NA, removing them from the denominator, so it is a percentage of valid rows, not necessarily all rows. 
  # (but note the default benchmark is the wtd.mean always using na.rm, regardless of what is passed to pct.above as a na.rm parameter)
  
  #################################################################################
  # FUNCTIONS TO REPORT THE NUMBER OR PERCENT OF ROWS (FOR EACH COLUMN OF A DATA.FRAME) 
  # WHERE THE VALUE EXCEEDS SOME SPECIFIED CUTOFF(S)
  #
  # If wts is population counts, for example, this gives the PERCENT of people (not rows) for whom value in df[,x] exceeds benchmark for each column x
  #
  # or.tied=FALSE by default, reporting on those > cutoff. But, if or.tied=TRUE, this reports on those >= cutoff. 
  # below=FALSE by default, reports on those above (or tied with, if or.tied) cutoff. But if below=TRUE, this reports on those below  (or tied with, if or.tied) cutoff.
  #
  # If df (passed to the function) is a data.frame or matrix, the function returns a vector of length= length(df) or # of cols in matrix.
  # If df is just a vector, it is treated like a 1-column data.frame, so the function returns a single value.
  #
  # If benchmarks (passed to the function) is a data.frame matching df in dimensions, each value is used as the cutoff for the corresponding cell in df.
  # If benchmarks is a vector of length= length(df), each value in benchmarks is the cutoff for the corresponding column in df.
  # If benchmarks is a shorter vector, it is recycled. (e.g., a vector of length 2 would use the first benchmark as the cutoff for all odd columns of df, the second for all even columns of df).
  # If benchmarks is a single numeric value, it is used as the cutoff value in every comparison for all of df.
  # If benchmarks is omitted, the default behavior is to use the arithmetic mean value a column of df as the cutoff for that column of df.
  #
  # If benchnames is omitted, the word "cutoff" is used by default (unless benchmarks is also omitted).
  # If benchnames is specified but benchmarks is not, the benchmarks default to the column means, so benchnames is ignored and "mean" is used instead.
  #
  # If wts is omitted the default is 1 which means no weighting. Just row counts.
  # If wts is a vector of length= length(df[,1]) then each row of df uses the corresponding weight and count is sum of wts not count of rows.
  # If wts is shorter than that, it is recycled but # of rows in df must be an integer multiple of length(wts).
  #
  # NA values in df are not counted and are not in the numerator of pct.above() but the denominator of pct.above() is a count of all rows of df, not just the non-NA ones.
  #
  #################################################################################

  if (is.matrix(df)) {df <- as.data.frame(df) } #warning("df is a matrix... converting to data.frame")
  if (is.vector(df)) {df <- as.data.frame(df) } #warning("df is a vector... converting to data.frame")
  if (length(df[,1])==0) {stop("Error - zero rows in df, so percents would be undefined.")}
  
  if (length(benchmarks)==1 && benchmarks=='mean') {
    # use the simple mean value as the benchmark, & set benchnames <- "mean" 
    # but note that may replace user-defined names if they set benchnames but not benchmarks
    # Use wtd.mean as benchmark if (wts!=1):
    if (any(wts!=1)) {
      benchmarks <- sapply(df, FUN=function(x) weighted.mean(x, wts, na.rm=TRUE) ) # note this part ignores the na.rm settings passed to here
    } else {
      benchmarks <- colMeans(df, na.rm=TRUE)
    }
    benchnames <- "mean"
  }
  
  if (length(benchnames) > length(df)) {
    warning("length of benchnames must be less than or equal to # of cols in df. Renaming benchnames to 'cutoff' ")
    benchnames <- 'cutoff' 
  }
  
  counts.above <- count.above(df, benchmarks, benchnames, or.tied=or.tied, below=below, wts=wts)
  
  if (length(counts.above)==1 && is.na(counts.above)) {stop("Error in counts.above"); return(NA)}
  
  recycles <- length(df[ , 1]) / length(wts) # ensure sum of wts is right if it was recycled

  # THIS HAS NOT BEEN TESTED:
  denominator <- ifelse(na.rm,    sum( rep(wts, recycles)[!is.na(counts.above)] , na.rm=TRUE)  ,  sum( rep(wts, recycles), na.rm=TRUE))
  
  results <- counts.above / denominator

  if (or.tied) {mytext <- 'above.or.tied.with.'} else {mytext <- 'above.'}
  mytext <- paste('pct.of', of.what, mytext, sep='.')
  if (below) {mytext <- gsub('above', 'below', mytext)}

  names( results) <- paste(mytext, benchnames, ".for.", names(df), sep="")
  return( results)
}

###########################################################################

# examples/ test cases:

if (FALSE==TRUE) {
  
  x <- data.frame(a=1:20, b=10, c=c(1:9,100:110))
  mywts <- c(rep(1,10), rep(2,10))
  mybench <- c(3,100,10)
  mynames <- c("HI","USavg","HealthStandard")
  
  count.above(x, 0, wts=mywts)
  count.above(x, 100, wts=mywts)
  count.above(x, 10, wts=mywts)
  count.above(x, mybench, wts=mywts)
  cbind(count= count.above(x, mybench, mynames, wts=mywts))
  cbind(pct= pct.above(x, benchmarks=mybench, benchnames=mynames, wts=mywts) )
  cbind(
    count= count.above(x, mybench, mynames, wts=mywts),
    pct= pct.above(x, benchmarks=mybench, benchnames=mynames, wts=mywts) )
  cbind(stat= pct.above(as.matrix(x), mybench, mynames, wts=mywts) )
  cbind(stat= pct.above(1:100, 98 , wts=mywts))
  #cbind( pct.above(1:100, wts=mywts) )  # does not recycle weights in this situation of a single vector argument
  #count.above(data.frame(a=c(1:10, NA)), 2, wts=mywts)   # does not work if NA values 
  #cbind( pct.above(data.frame(a=c(1:10, NA)), 0 , wts=mywts))
  #pct.above(data.frame(a=c(NA, NA, NA)), 3, wts=mywts)
  count.above(x, c(3,1), wts=mywts) # 3,1 is recycled as 3,1,3 since x has 3 cols
  pct.above(x, benchnames=mynames, wts=mywts)  # ignores names since default benchmarks are column means
  
}
