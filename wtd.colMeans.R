wtd.colMeans <- function(x, wts, by, na.rm = TRUE, dims = 1) {
  #  version of this function for batch.summarizer originally had these defaults:
  #   wts=1, na.rm=FALSE 
  # and is just this function:
  return( colMeans(x * t(wts), na.rm = na.rm) * colSums(!is.na(x)) / sum(wts, na.rm = na.rm) )
  # which is the same as analyze.stuff::wtd.colMeans() I think 
  
  # and doesn't need data.table package which makes it a bit easier for a hosted app.
  # suppressWarnings( ) might be useful.
  
#   require(data.table)
#   if (!missing(wts)) {
#     if (any(is.na(wts)) ) {warning('For wts with NA values, not fully tested -- I think mean uses total number of rows (or sum of non-NA weights) as denominator, not just rows where the actual value is non-NA!')}
#   }
#   if (any(is.na(x)) ) {warning('For cols with NA values, not fully tested -- I think mean uses total number of rows (or sum of non-NA weights) as denominator, not just rows where the actual value is non-NA!')}
#   myna.rm <- na.rm
#   # if just a vector (single col from data.frame with drop=TRUE) then cannot use setDT() -- setDT saves RAM by not making a copy
#   if (NCOL(x) == 1) {
#     x <- data.table(x)
#   } else {
#     x <- data.table(x)
#     #setDT(x)
#   }
#   if (missing(wts)) {
#     result <- x[ , lapply(.SD, mean, na.rm = myna.rm  ), by = by]
#   } else {
#     result <- x[ , lapply(.SD, weighted.mean, wts, na.rm = myna.rm  ), by = by]
#   }
#   setDF(result)
#   return(result)
}
