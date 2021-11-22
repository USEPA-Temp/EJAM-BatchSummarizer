#' @title Core function that calculates summary stats across all sites and nearby people for EJSCREEN batch analysis
#' @description This is the function that takes the full tables of batch buffer results and calculates summary statistics 
#'   like maximum at any site, median across all sites, maximum percentile for all specified indicators at a given site, etc.
#'   It can be expanded to provide other summary stats by adding those other formulas to this code. 
#' @param sitestats A matrix or data.frame to summarize, one row per site, one column per variable. 
#'   Must have correct stats for all people near a given site.
#' @param popstats  A matrix or data.frame to summarize, one row per site, one column per variable. 
#'   Must have reduced counts that count only once each unique person near one or more of the sites. 
#'   Used to sum population and get stats of distribution of each indicator across all unique individuals.
#' @param cols NOT USED YET. Specifies which colums of x should be summarized or used during summarization. A single string value 'all' as default to specify all, or a vector of colnames.
#' @param probs Vector of numeric values, fractions, to use as probabilities used in finding quantiles. Default is c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)
#' @param na.rm Logical TRUE by default, specifying if na.rm should be used for sum(), mean(), and other functions.
#' @param wts Obsolete. Was the actual weights vector for wtd.mean or weighted.quantile etc.
#' @param wtscolname Name of the column that contains the relevant weights to be used (e.g., "pop")
#' @param threshold list of vectors each with 1+ thresholds (cutoff numbers) used to count find sites where 1+ of given set of indicators are at/above the threshold & how many of the indicators are.
#'  If an element of the list is a single number, that is used for the whole group (all the threshnames in that nth list element). 
#'  Otherwise/in general, each vector is recycled over the threshnames in corresponding list element, 
#'  so each threshname can have its own threshold like some field-specific benchmark, or they can all use the same threshold like 50.
#' @param threshnames list of vectors of character colnames defining fields in x that get compared to threshold, or to thresholds
#' @param threshgroup list of 1+ character strings naming the elements of threshnames list, such as "EJ US pctiles"
#' @param rowfun.picked logical vector specifying which of the pre-defined functions (like at/above threshold) are needed and will be applied
#' @param colfun.picked  logical vector specifying which of the pre-defined functions (like colSums) are needed and will be applied
#' @param testing optional, default is FALSE. prints some debugging info if TRUE.
#' @return output is a list with two named elements, rows and cols, where each is a matrix of summary stats. 
#'   cols: Each element in a summary col summarizes 1 row (site) across all the RELEVANT cols of batch data (e.g., all US EJ Index percentiles)
#'   rows: Each element in a summary row summarizes 1 column (field) across all the rows of batch data.
batch.summarize <- function(sitestats, popstats, cols = 'all', wtscolname = 'pop', wts = popstats[ , wtscolname], probs = c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1), threshold=list(80), threshnames=list(names(which(sapply(sitestats,  class) != 'character'))), threshgroup = list('variables'), na.rm = TRUE, rowfun.picked = 'all', colfun.picked = 'all', testing = FALSE) {
  if (missing(sitestats) | missing(popstats)) {stop('sitestats and popstats are outputs of batch processing in EJSCREEN and must each be a matrix or data.frame to be analyzed, where each row has stats on a buffer around some point called a site')}
  if (cols[1] == 'all') {cols <- colnames(sitestats)}
  if (!all.equal(dim(sitestats), dim(popstats))) {stop('sitestats and popstats must be identical in number of rows and in number of columns')}
  if (!all.equal(colnames(sitestats), colnames(popstats))) {stop('sitestats and popstats must be identical in colnames')}
  if (any(!(cols %in% colnames(sitestats)))) {stop('invalid cols -- must be a vector of strings, all of which must be elements of names(sitestats)')}
  if (!is.vector(probs) | !is.numeric(probs) | any(probs > 1) | any(probs < 0)) {stop('probs must be a numeric vector of fractions for quantile function')}
  
  if (1 != length(unique( length(threshold), length(threshnames), length(threshgroup) ) )) {stop('lengths of threshold list, threshnames list, threshgroup list must be identical') }
  
  # numericols  <- names(which(sapply(sitestats,  class) != 'character'))
  
  # Specify the population weighting to use unique individuals, not the double counted total near each site. #####
  wts <- popstats[ , wtscolname]
  
  # Specify summary metrics to calculate.####
  # Provide a default set of summary metrics.
  # A later version could allow user to select from a list of functions, and eventually even specify custom functions perhaps, 
  # but offline work in R or a spreadsheet could provide that capability easily.
  
  #'###########################################
  # SUMMARY COLUMNS (a summary of each row): ####
  #'###########################################
  
  rowfuname <- vector()
  rowfun  <- list()
  rowargs <- list()
  i <- 0
  
  # create a set of comparisons to threshold(s), one set per element/vector in the list ####
  
  for (setnum in 1:length(threshgroup)) {
    
    # one set of thresholds, which are recycled as needed to match length of this threshnames[[threshnum]] vector
    
    # at site, what is max indicator? 
    # FOR EACH SITE WHAT IS THE MAX VALUE OF A FEW INDICATORS (PERCENTILES) AT THAT SITE?
    i = i + 1
    rowfuname[i] = paste('Max of', threshgroup[[setnum]])
    rowfun[[i]] = function(x, varnames, na.rm = na.rm) {
      rowMaxs.sofar <- suppressWarnings(rowMaxs( x[ , varnames ], na.rm = na.rm))
      # replace negative infinity with NA, to handle cases where entire row was NA values so rowMaxs returned -Inf
      rowMaxs.sofar[is.infinite(rowMaxs.sofar)] <- NA
      return(rowMaxs.sofar)
    }
    rowargs[[i]] <- list(x = NULL, varnames = threshnames[[setnum]], na.rm = na.rm)
    
    # NOT ESSENTIAL ONCE YOU HAVE THE NUMBER OF SITES AT/ABOVE:
    #     i=i+1
    #     rowfuname[i]=paste('Are any', threshgroup[[setnum]], 'at/above threshold of', paste(threshold[[setnum]], collapse='/' ) )
    #     rowfun[[i]]= function(x, varnames, cutoff, or.tied, na.rm) {
    #       0 < cols.above.count( x[ ,  varnames], cutoff=cutoff, or.tied=or.tied, na.rm=na.rm ) 
    #     }
    #     rowargs[[i]] <- list(x=NULL, varnames=threshnames[[setnum]], cutoff=threshold[[setnum]], or.tied=TRUE, na.r = a.rm)
    
    # at site, how many indicators are > x%ile? ####
    # FOR EACH SITE HOW MANY OF A FEW INDICATORS (PERCENTILES) ARE AT/ABOVE A SPECIFIED THRESHOLD?
    i = i + 1
    rowfuname[i] = paste('Number of', threshgroup[[setnum]], 'at/above threshold of', paste(threshold[[setnum]], collapse = '/' ) )
    rowfun[[i]] = function(x, varnames, cutoff, or.tied, na.rm) {
      cols.above.count( x[ , varnames], cutoff = cutoff, or.tied = or.tied, na.rm = na.rm )
    }
    rowargs[[i]] <- list(x = NULL, varnames = threshnames[[setnum]], cutoff = threshold[[setnum]], or.tied = TRUE, na.rm = na.rm)
  }
  
  
  #i = i+1
  #rowfuname[i] = ''
  #rowfun[[i]] = function(x, ...) {
  #  f( x, na.rm  =  na.rm)
  #}
  
  
  # RATIO TO AVERAGE IN ZONE, ####
  # RATHER THAN JUST ABOVE/BELOW COMPARISON TO A THRESHOLD:
  #
  # possibly add functions here that create multiple cols, one per indicator, showing for each indicator at each site, 
  # - is value above US avg (would have to extract that AVG first, a bit more work than just comparing to specified thresholds)
  # - what is ratio of value to US avg for that value, etc.:
  #ratios.to.pop.usavg (1 col per relevant variable)
  #ratios.to.pop.region.avg (1 col per relevant variable)
  #ratios.to.pop.state.avg (1 col per relevant variable)
  #ratios.to.pop.county.avg?? (1 col per relevant variable)
  
  #i = i+1
  # rowfunames[i] = 'Ratio of indicator X at average site to US pop avg'  
  # rowfun[i] = function(x, usavgvalues, na.rm = TRUE) { x / usavgvalues } # that is not how the math will work - just a placeholder
  # need to identify those usavgvalues columns either here or when that is called
  
  #i = i+1
  #rowfunames[i] = 'Ratio to State pop avg'
  #rowfun[i] = function(x, stateavgvalues, na.rm = TRUE) { x / stateavgvalues } # that is not how the math will work - just a placeholder
  
  #'###########################################
  # SUMMARY ROWS (a summary of each column): #####
  #'###########################################
  
  # THESE SUMMARY FUNCTIONS RETURN 1 ROW EACH:
  
  colfuname <- vector()
  colfun <- list()
  bywhat <- vector() # specify if this function gets applied across people or across sites
  # n specifies the order in which summary stat rows will appear
  n = 0
  
  # avg site ####
  n = n + 1
  colfuname[n] = 'Average site'
  colfun[[n]] = function(x, ...) {colMeans(x, na.rm = na.rm)}
  bywhat[n] <- 'site'
  
  # avg person ####
  n = n + 1
  colfuname[n] = 'Average person'
  # *** above specified wts as popstats[ , wtscolname] since wts is not passed to this function later
  colfun[[n]] = function(x, ...) {wtd.colMeans(x, wts = wts, na.rm = na.rm)} # function was defined in this package and in analyze.stuff
  bywhat[n] <- 'pop'
  
  # median site ####
  n = n + 1
  colfuname[n] = 'Median site'
  colfun[[n]] = function(x, ...) {
    sapply(x, FUN = function(y) {median(y, na.rm = na.rm)})
  }
  bywhat[n] <- 'site'
  
  # median person ####
  n = n + 1
  colfuname[n] = 'Median person' 
  colfun[[n]] = function(x, ...) {
    # *** above specified wts as popstats[ , wtscolname] since wts is not passed to this function later
    sapply(x, FUN = function(y) {Hmisc::wtd.quantile(y, probs = 0.50, weights = wts, na.rm = na.rm)})
  }
  bywhat[n] <- 'pop'
  
  # min ####
  n = n + 1
  colfuname[n] = 'Min'
  colfun[[n]] = function(x, ...) {
    # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
    z = suppressWarnings( colMins(x, na.rm = na.rm) ) 
    z[is.infinite(z)] <- NA
    return(z)
  }
  bywhat[n] <- 'site'
  
  # max ####
  n = n + 1
  colfuname[n] = 'Max'
  colfun[[n]] = function(x, ...) {
    # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
    z = suppressWarnings( colMaxs(x, na.rm = na.rm) )
    z[is.infinite(z)] <- NA
    return(z)
  }
  bywhat[n] <- 'site'
  
  # sum ####
     n = n + 1
    colfuname[n] = 'Sum'
     colfun[[n]] = function(x, ...) {colSums(x, na.rm = na.rm)}
     bywhat[n] <- 'pop'

  #   n = n + 1
  #   colfuname[n]='Count of sites'
  #   colfun[[n]]=function(x, ...) {apply(x, 2, FUN=function(y) length(y))}
  #   bywhat[n] <- 'site'
  
  # manually removed this stat because colfun.picked is hard to use as currently written
  #n = n + 1
  #   colfuname[n]='Number of unique values'
  #   colfun[[n]]=function(x, ...) {apply(x, 2, FUN=function(y) length(unique(y)))}
  # bywhat[n] <- 'site'
  
  #n = n + 1
  #  colfuname[n]='Standard Deviation'
  #  colfun[[n]]=function(x, ...) {apply(x, 2, FUN=function(y) {sd(y, na.rm=na.rm)}) }
  # bywhat[n] <- 'site'
  
  #'###########################################
  # THESE SPECIAL SUMMARY FUNCTIONS RETURN MULTIPLE ROWS EACH:####
  # THAT REQUIRES A DIFFERENT APPROACH TO POPULATING THE RESULTS VARIABLE
  # 1) could append the group of quantiles to summarycols (use probs) outside the loop over functions, using rbind instead of [i] <-  
  # 2) could write each quantile as a single function - time consuming to write out & hard to accomodate probs. 
  # 3) could have each function also have a value that specifies how many rows or cols it will output & account for that (seems complicated)
  
  # quantiles ####
  n = n + 1
  # while not yet working, use other approach via rbind, later in this code:
  if (1 == 0) {
    just.rbind.quantiles <- FALSE
    if (length(probs) > 0) {
      myfunlist <- list()
      for (z in 1:length(probs)) {
        myfunlist[[z]] <- function(x, ...) {  }
        f <- (parse( " function (x) ", as.symbol("{"), paste('quantile(x,probs = probs[',z,'], na.rm = na.rm)'), '}' )) 
        body(myfunlist[[z]]) <- f
      }
      colfuname[n:(n - 1 + length(probs))]    <- paste('Percentile of sites', 100 * probs)
      colfun[[  n:(n - 1 + length(probs))  ]] <- myfunlist
      
      myfunlist <- list()
      for (z in 1:length(probs)) {
        myfunlist[[z]] <- function(x, ...) {  }
        
        # *** may need to specify wts here as popstats[ , wtscolname]
        
        f <- (parse( " function (x) ", as.symbol("{"), paste('wtd.quantile(x, weights = wts, probs = probs[',z,'], na.rm = na.rm)'), '}' )) 
        body(myfunlist[[z]]) <- f
      }
      
      colfuname[(n + length(probs)):((n - 1) + 2 * length(probs))]  <- paste('Percentile of people', 100 * probs)
      colfun[[  (n + length(probs)):((n - 1) + 2 * length(probs))]] <- myfunlist
      
      nextcol <- 1 + ((n - 1) + 2 * length(probs)) 
      # *** nextcol is defined but not used anywhere in this draft code
    } 
  } else {
    just.rbind.quantiles <- TRUE  
    # while not working
  }
  
  # colfuname[ nextcol ]=' '
  # colfun[[ nextcol  ]]=function(x, na.rm=TRUE) {  }
  
  #'###########################################
  
  #'###########################################
  # Use those functions to get summary stats #####
  #'###########################################
  
  colfuns.count.all <- length(colfun)
  rowfuns.count.all <- length(rowfun)
  
  # for now, just pick all of them by default. later allow user to select perhaps.
  if (colfun.picked == 'all') {
    colfun.picked = rep(TRUE, colfuns.count.all)  
  }
  if (rowfun.picked == 'all') {
    rowfun.picked = rep(TRUE, rowfuns.count.all) 
  }
  
  colfuns.count.picked <- sum(colfun.picked)
  rowfuns.count.picked <- sum(rowfun.picked)
  
  # preallocate space to store summary stats on only those picked
  summary.rows <- matrix(NA, nrow = colfuns.count.picked, ncol = ncol(sitestats)) # rows with summary stats summarizing all the columns. This will hold 1 row per summary stat, and same # cols as original table of batch results
  summary.cols <- matrix(NA, nrow = nrow(sitestats), ncol = rowfuns.count.picked ) # columns with summary stats summarizing all the rows
  
  summary.rows.names <- vector()
  summary.cols.names <- vector()
  
  # don't summarize character columns like name of site
  charcol <- sapply(sitestats, class) == 'character'
  
  #'###########################################
  # Create summary rows ######
  # where each element in a summary row summarizes 1 column (field) across all the rows of batch data
  #'###########################################
  
  for (i in 1:colfuns.count.picked) {
    
    fnum <- which(colfun.picked)[i]
    
    if (bywhat[fnum] == 'site') {
      # right now wts are not passed from here to function- they are specified when function is specified
      # don't pass parameters since they are variables available in this memory space. like wts, na.rm, threshold, probs, etc.
      # weight by the total pop count at each site?? why?
      # wts <- sitestats[ , wtscolname]
      summary.rows[i, ][  !charcol] <- as.vector( colfun[[fnum]](sitestats[ , !charcol]) )  
    } else {
      # wts <- popstats[ , wtscolname] # weight by the unique individuals assigned to a given site
      summary.rows[i, ][  !charcol] <- as.vector( colfun[[fnum]](popstats[ , !charcol]) )  
    }
    
    if (testing) {cat('now summary.rows[i, ] all cols is \n'); print(summary.rows[i, ])}
    
    summary.rows.names[i] <- colfuname[fnum]
    
    # could this have the wrong # of elements if na.rm=TRUE and it somehow doesn't return NA for one of the columns??
  }
  
  #'###########################################
  # Create summary cols ######
  # where each element in a summary col summarizes 1 row (site) across all the RELEVANT cols of batch data (e.g., all US EJ Index percentiles)
  #'###########################################
  
  for (i in 1:rowfuns.count.picked) {
    fnum <- which(rowfun.picked)[i]
    myargs <- rowargs[[fnum]]
    
    if (bywhat[fnum] == 'site') {
      myargs$x <- sitestats
    } else {
      myargs$x <- popstats
    }
    
    summary.cols[ , i] <- round(as.vector(
      do.call(rowfun[[fnum]], args = myargs)
    ), 0)
    
    summary.cols.names[ i] <- rowfuname[fnum]
  }
  
  #'###########################################
  # create useful rownames and colnames for the outputs ####
  #'###########################################
  
  rownames(summary.rows) <- summary.rows.names
  colnames(summary.rows) <- cols # colnames(sitestats)  # Not sure this works if only some cols selected to summarize
  
  colnames(summary.cols) <- summary.cols.names
  rownames(summary.cols) <- sitestats[ , 1] # assumes you want the first column to be used as the rownames, which is OBJECTID as of 2/2015
  # rownames(colsout) <- rownames(sitestats)  # is another option
  
  #'###########################################
  # less elegant WAY TO APPEND QUANTILE SUMMARY ROWS: ######
  #'###########################################
  
  if (just.rbind.quantiles) {
    quantile.rows     <- matrix(NA, nrow = length(probs), ncol = NCOL(sitestats)); rownames(quantile.rows)     <- paste('Percentile of sites', 100 * probs)
    wtd.quantile.rows <- matrix(NA, nrow = length(probs), ncol = NCOL(popstats)); rownames(wtd.quantile.rows) <- paste('Percentile of people', 100 * probs)
    colnames(quantile.rows) <- colnames(sitestats) # should be same as cols variable
    colnames(wtd.quantile.rows) <- colnames(sitestats) # ditto
    quantile.rows[     , !charcol] <- sapply(sitestats[ , !charcol], function(y) quantile(y,     probs = probs, na.rm = na.rm) )
    wtd.quantile.rows[ , !charcol] <- sapply( popstats[ , !charcol], function(y) Hmisc::wtd.quantile(y, probs = probs, na.rm = na.rm,  weights = wts) )
    summary.rows <- rbind(summary.rows, quantile.rows, wtd.quantile.rows)
  }
  # summary.cols ###########################################
  summary.cols <- as.data.frame(summary.cols, stringsAsFactors = FALSE)
  # THIS FIXES BUG IN SORTING/ FORMATTING cols RESULTS AS NUMERIC VS CHARACTER
  # x$cols <- as.data.frame(x$cols, stringsAsFactors = FALSE)
  # but can't do this here WHILE STRINGS ARE IN SOME CELLS: 
  # x$rows <- as.data.frame(x$rows, stringsAsFactors = FALSE)
  
  return( list(rows = summary.rows, cols = summary.cols) )
}

#'###########################################  ############################################
# NOTES
#
# str(fulltable)
# 'data.frame':  42 obs. of  179 variables:
# $ OBJECTID                                     : int  1 2 3 4 5 6 7 8 9 10 ...
# $ id                                        : chr  "999" ...
# $ name                                         : chr  "xyz facil" ...
# $ lat                                          : num  32.3 ...
# $ lon                                          : num  -94.5 ...
# $ pop                                         *: chr  "7,946" "1,487" "14,003" "106,245" ...
# $ radius.miles                                *: chr  "3 miles" "3 miles" "3 miles" "3 miles" ...
# $ ST                                           : chr  "TX" "AR" "AR" "IL" ...
# $ statename                                    : chr  "Texas" "Arkansas" "Arkansas" "Illinois" ...
# $ REGION                                       : int  6 6 6 5 5 5 5 5 5 7 ...
# $ state.pctile.proximity.tsdf                 * : chr  "97" "98" "99" "96" ...
# $ region.pctile.EJ.DISPARITY.traffic.score.eo * : chr  "74" "30" "76" "93" ...
# $ state.pctile.pm                             * : chr  "44" "29" "15" "85" ...
# $ region.pctile.EJ.DISPARITY.cancer.eo        * : chr  "75" "35" "69" "94" ...
# $ state.pctile.EJ.DISPARITY.dpm.eo            * : chr  "61" "49" "84" "88" ...
# $ us.avg.VSI.eo                               ** : chr  "35%" "35%" "35%" "35%" ...
# $ proximity.rmp                               * : chr  "1.2" "0.038" "1" "1.2" ...
# $ region.avg.pm                                : num  9.44 9.44 9.44 13.3 13.3 13.3 13.3 13.3 13.3 11.1 ...
# $ region.avg.pctlths                          ** : chr  "19%" "19%" "19%" "12%" ...
# $ region.avg.dpm                               : num  0.733 0.733 0.733 0.712 0.712 0.712 0.712 0.712 0.712 0.741 ...
# $ pctover64                                   ** : chr  "13%" "18%" "16%" "10%" ...
# $ us.avg.proximity.tsdf                        : num  0.054 0.054 0.054 0.054 0.054 0.054 0.054 0.054 0.054 0.054 ...
# $ region.pctile.pctpre1960                    * : chr  "74" "69" "85" "66" ...
# $ region.pctile.proximity.rmp                 * : chr  "92" "5" "89" "94" ...
# $ state.pctile.dpm                            * : chr  "37" "36" "67" "73" ...
# $ resp                                        * : chr  "1.5" "0.76" "1.8" "2.1" ...
# $ region.pctile.VSI.eo                        * : chr  "72" "33" "68" "92" ...
# $ pctlths                                     ** : chr  "23%" "14%" "27%" "16%" ...
# $ us.avg.traffic.score                         : int  110 110 110 110 110 110 110 110 110 110 ...
# $ state.pctile.neuro                          * : chr  "99" "13" "89" "94" ...
# $ pctile.EJ.DISPARITY.proximity.npl.eo        * : chr  "95" "51" "89" "92" ...
# $ state.avg.VSI.eo                            ** : chr  "47%" "34%" "34%" "34%" ...
# $ state.avg.pctmin                            ** : chr  "55%" "26%" "26%" "36%" ...
# $ state.avg.pctlths                           ** : chr  "20%" "17%" "17%" "14%" ...
# $ state.pctile.EJ.DISPARITY.resp.eo           * : chr  "66" "54" "87" "87" ...
# $ pctile.pm                                   * : chr  "27" "27" "24" "95" ...
# $ VSI.eo                                      ** : chr  "61%" "29%" "56%" "70%" ...
# $ pctile.neuro                                * : chr  "92" "4" "61" "96" ...
# $ pctunder5                                   ** : chr  "7%" "3%" "7%" "6%" ...
# $ pctpre1960                                   : chr  "0.27" "0.22" "0.45" "0.51" ...
# $ region.pctile.proximity.npl                  : chr  "97" "41" "95" "86" ...
# $ state.pctile.resp                            : chr  "54" "26" "92" "67" ...
# $ state.pctile.o3                              : chr  "51" "60" "1" "60" ...
# $ pctile.EJ.DISPARITY.pm.eo                    : chr  "77" "53" "72" "91" ...
# $ state.pctile.pctlths                         : chr  "64" "43" "82" "68" ...
# $ pctile.dpm                                   : chr  "49" "12" "31" "81" ...
# $ state.pctile.pctlowinc                       : chr  "70" "48" "73" "76" ...
# $ proximity.npl                                : chr  "0.29" "0.024" "0.22" "0.13" ...
# $ region.pctile.pctmin                         : chr  "69" "18" "61" "95" ...
# $ state.avg.traffic.score                      : int  91 64 64 69 69 24 24 24 24 38 ...
# $ region.pctile.EJ.DISPARITY.proximity.tsdf.eo : chr  "96" "7" "99" "97" ...
# $ proximity.tsdf                               : chr  "0.33" "0.33" "2.4" "0.13" ...
# $ pctile.EJ.DISPARITY.cancer.eo                : chr  "80" "52" "76" "86" ...
# $ neuro                                        : chr  "0.12" "0.023" "0.057" "0.16" ...
# $ state.avg.dpm                                : num  0.913 0.245 0.245 0.968 0.968 0.341 0.341 0.341 0.341 0.343 ...
# $ pctlowinc                                    : chr  "52%" "41%" "54%" "46%" ...
# $ pctile.EJ.DISPARITY.proximity.rmp.eo         : chr  "95" "56" "92" "96" ...
# $ pctile.o3                                    : chr  "34" "37" "14" "30" ...
# $ state.avg.o3                                 : num  42.9 44.5 44.5 42.8 42.8 47.2 47.2 47.2 47.2 45.2 ...
# $ region.avg.resp                              : num  1.4 1.4 1.4 1.5 1.5 1.5 1.5 1.5 1.5 1.4 ...
# $ state.avg.resp                               : num  1.5 1.1 1.1 1.8 1.8 1.1 1.1 1.1 1.1 0.99 ...
# $ us.avg.dpm                                   : num  0.824 0.824 0.824 0.824 0.824 0.824 0.824 0.824 0.824 0.824 ...
# $ pctile.VSI.eo                                : chr  "83" "50" "79" "89" ...
# $ pctile.proximity.rmp                         : chr  "95" "8" "93" "95" ...
# $ pctmin                                       : chr  "69%" "17%" "58%" "94%" ...
# $ pctile.cancer                                : chr  "72" "33" "81" "71" ...
# $ region.pctile.o3                             : chr  "47" "53" "19" "35" ...
# $ state.pctile.VSI.eo                          : chr  "68" "50" "84" "88" ...
# $ us.avg.proximity.rmp                         : num  0.31 0.31 0.31 0.31 0.31 0.31 0.31 0.31 0.31 0.31 ...
# $ region.pctile.EJ.DISPARITY.pctpre1960.eo     : chr  "84" "18" "81" "93" ...
# $ region.avg.neuro                             : num  0.043 0.043 0.043 0.067 0.067 0.067 0.067 0.067 0.067 0.052 ...
# $ us.avg.pctpre1960                            : num  0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 ...
# $ state.pctile.proximity.rmp                   : chr  "91" "17" "91" "92" ...
# $ region.avg.proximity.rmp                     : num  0.42 0.42 0.42 0.33 0.33 0.33 0.33 0.33 0.33 0.41 ...
# $ dpm                                          : chr  "0.478" "0.0875" "0.24" "1.35" ...
# $ region.pctile.pctlingiso                     : chr  "66" "36" "39" "67" ...
# $ region.avg.traffic.score                     : int  81 81 81 69 69 69 69 69 69 61 ...
# $ region.avg.pctpre1960                        : num  0.18 0.18 0.18 0.39 0.39 0.39 0.39 0.39 0.39 0.35 ...
# $ region.pctile.pctover64                      : chr  "63" "83" "77" "38" ...
# $ pctile.EJ.DISPARITY.neuro.eo                 : chr  "90" "55" "75" "96" ...
# $ region.pctile.cancer                         : chr  "87" "39" "95" "80" ...
# $ region.pctile.proximity.npdes                : chr  "73" "5" "96" "88" ...
# $ us.avg.cancer                                : int  49 49 49 49 49 49 49 49 49 49 ...
# $ pctile.pctmin                                : chr  "79" "38" "74" "93" ...
# $ state.avg.proximity.tsdf                     : num  0.073 0.046 0.046 0.037 0.037 0.042 0.042 0.042 0.042 0.063 ...
# $ state.avg.proximity.npl                      : num  0.067 0.033 0.033 0.069 0.069 0.1 0.1 0.1 0.1 0.063 ...
# $ region.avg.pctover64                         : chr  "11%" "11%" "11%" "13%" ...
# $ state.pctile.pctmin                          : chr  "62" "51" "86" "91" ...
# $ state.pctile.EJ.DISPARITY.proximity.tsdf.eo  : chr  "94" "4" "99" "98" ...
# $ state.pctile.EJ.DISPARITY.proximity.rmp.eo   : chr  "86" "56" "92" "96" ...
# $ us.avg.pm                                    : num  10.7 10.7 10.7 10.7 10.7 10.7 10.7 10.7 10.7 10.7 ...
# $ region.avg.proximity.tsdf                    : num  0.062 0.062 0.062 0.051 0.051 0.051 0.051 0.051 0.051 0.038 ...
# $ state.avg.proximity.rmp                      : num  0.47 0.33 0.33 0.43 0.43 0.35 0.35 0.35 0.35 0.43 ...
# $ pctlingiso                                   : chr  "6%" "0%" "1%" "1%" ...
# $ state.pctile.traffic.score                   : chr  "60" "44" "79" "77" ...
# $ state.pctile.EJ.DISPARITY.pm.eo              : chr  "61" "53" "80" "89" ...
# $ state.avg.pctpre1960                         : num  0.17 0.17 0.17 0.43 0.43 0.35 0.35 0.35 0.35 0.36 ...
# $ region.pctile.EJ.DISPARITY.neuro.eo          : chr  "92" "38" "68" "98" ...
# $ state.avg.pctlingiso                         : chr  "9%" "2%" "2%" "6%" ...
# [list output truncated]
