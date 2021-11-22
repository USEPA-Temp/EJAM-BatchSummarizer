cols.above.count <- function(x, cutoff, or.tied=FALSE, na.rm=TRUE, below=FALSE) {
  if (is.null(dim(x))) {numcols <- 1; stop('expected data.frame as x but has only 1 dimension')} else {numcols <- dim(x)[2]}
  if (missing(cutoff)) {cutoff <- rowMeans(x)}
  if (below) {
    if  (or.tied) { count.per.row <- rowSums( x <= cutoff, na.rm=na.rm) }
    if (!or.tied) { count.per.row <- rowSums( x <  cutoff, na.rm=na.rm) }
  } else {
    if  (or.tied) { count.per.row <- rowSums( x >= cutoff, na.rm=na.rm) }
    if (!or.tied) { count.per.row <- rowSums( x >  cutoff, na.rm=na.rm) }
  }
  return(count.per.row)

  # These could be renamed  rows.above.count(), rows.above.pct(), rows.above.which()
  # to follow convention of cols.above.count(), cols.above.pct(), cols.above.which()
  # and same using below too, like rows.below.pct() etc.
  # and *** should make param names consistent, like x not df, cutoff(s) not benchmarks?, or.tied not gte
  # but *** cols versions and all should have wts, na.rm, benchmarks as vector not just 1 number, benchnames, params
  # and ** should have a "below" version for each variant
  
}
