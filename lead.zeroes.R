lead.zeroes <- function(fips, length.desired) {
  # Latest version of this is also now available in package called analyze.stuff
	########
	# DEFINE FUNCTION THAT CAN ADD LEADING ZEROES AS NEEDED (FOR TABLE OR SEQUENCE FILE NUMBER OR FIPS)
	########
	# Inputs:
	# fips is a character or numeric FIPS code, or vector of them (e.g., Census Bureau FIPS code for a block, block group, tract, county, or state)
	# length.desired is a numeric specifying how many characters should be in the corrected FIPS code, including any leading zeroes that are needed. This can be different for each corresponding fips - they are handled pairwise.
	# Returns a character string that is the corrected FIPS code, or vector of them, now including a leading zero whenever needed to bring a fips to the correct number of characters as specified by length.desired.

	fips <- as.character(fips)
	# might trim whitespace?  
	if ( (length(length.desired) > 1) & (length(fips) != length(length.desired))) {warning("numbers of inputs don't match")}
	if ( any(length.desired == 0 | length.desired >= 100) ) {stop("error: string lengths must be >0 & <100")}
	if ( any(nchar(fips) > length.desired) ) {stop("error: some are longer than desired length")}

	fips <- paste( paste( rep( rep("0", length(length.desired)), length.desired), collapse = ""), fips, sep = "") 
	# does that work vectorized?

	# or maybe this, but can't say length.desired[i] unless it has same length as fip & can't handle recycling also:
	# fips <- for (i in 1:length(fip)) { paste( paste( rep("0", length.desired[i]), collapse=""), fips[i], sep="") }

	fips <- substr(fips, nchar(fips) - length.desired + 1, nchar(fips))

	return(fips)
	
	# NOTE: # of digits in FIPS codes, assuming leading zeroes are there:
	#
	# state		2	(2 cumulative)
	# county	3	(5 cum)
	# tract		6	(11 cum) (11 digits is ambiguous if not sure leading zero is there)
	# block group 	1	(12 cum) (12 digits is ambiguous if not sure leading zero is there)
	# block 	1	(13 cum)
}
