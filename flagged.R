######################################################
#	CREATE 'FLAGGED' FIELD
######################################################

flagged <- function(df, cutoff=0.80, or.tied=TRUE, na.rm=TRUE) {

	# CREATE A LOGICAL VECTOR THAT IS TRUE FOR EACH ROW OF DATA FRAME WHERE AT LEAST ONE VALUE IN ROW IS >= CUTOFF (or just >CUTOFF if above.only=TRUE)

  # *** Be careful to check if percentiles are 0-100 or 0-1 !!! ***
  if (cutoff <= 1 && any(df > 1) ) {warning('Cutoff is <=1 so it might be a percentage as fraction, but some of data are >1 so may percentages as 0-100 not as fraction')}
  
	if (or.tied) {
    flag <- do.call(pmax, c(df, na.rm=na.rm)) >= cutoff
	} else {
    flag <- do.call(pmax, c(df, na.rm=na.rm)) > cutoff
	}
	return(flag)
}

# Note: If na.rm=TRUE it means it will always ignore NA values in a given place and take the max of the valid (non-NA) values instead of returning NA when there is an NA in that row
# You would almost never want na.rm=FALSE
# flagged(places[ , pctilecols], 0.80)
# flagged(places[ , bincols], 9)
# flagged(places[ , bincols], 8, or.tied=FALSE)
# or if ej pctiles are in pctile.ej:
# x <- flagged(pctile.ej)
# t(head(cbind(x, pctile.ej)))
# or
# x <- flagged(bin.ej, 9)
# t(head(cbind(x, bin.ej)))

