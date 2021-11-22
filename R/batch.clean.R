#' @title Function that cleans up raw output of EJSCREEN batch processor
#' @description Takes the raw output version of batch buffer results and cleans it up
#'   to make it ready for batch.summarize function
#'   Note this drops rows with no pop data - assumes those lack EJSCREEN batch results since they are in VI/Guam/etc.
#' @param x Required. The output of the batch processor that runs EJSCREEN buffer report once per site.
#' @param namesfile Must specify either namesfile, or both oldcolnames and newcolnames. A csv filename, of file that maps fieldnames from those found in raw output of batch processor 
#'   to more useful and friendly names that make more sense to me. Default had been included but now left out - used to use 'map_batch_to_friendly_fieldnames_2018.csv'
#'   If function is called with the special value namesfile='keepnames' then the names are unchanged from those in x.
#' @param oldcolnames Optional. The names to be found in x, ignored if namesfile specified.
#' @param newcolnames Optional. The corresponding names to change them to, ignored if namesfile specified.
#' @author ejanalyst info@ejanalysis.com
#' @return the output is the output
#' @export
batch.clean <- function(x, namesfile, oldcolnames, newcolnames) {
  # namesfile='map_batch_to_friendly_fieldnames_2020.csv' # was the default 
  # namesfile='keepnames' is one way to specify user wants to keep same names.
  if (missing(namesfile) & 1 == sum(missing(oldcolnames) + missing(newcolnames) )) {
    stop('must specify either namesfile, or both oldcolnames and newcolnames')
  }
  
  x[x == 'NA'] <- NA  # it also fixes 'N/A' later 

  if (colnames(x)[1] == '') {colnames(x)[1] <- "hadnocolnameinheader"} # blah
  
  # FIRST, FIX THE ISSUE WHERE ARCGIS IS EXPORTING TO TXT FILE WITH 
  # COLUMN NAMES THAT HAVE AN UNDERSCORE AS THE FIRST CHARACTER
  # and R reads those and adds X before the underscore!!
  # colnames(x) <- gsub(pattern = '^_', replacement = '', x = colnames(x))
  colnames(x) <- gsub(pattern = '^X_', replacement = '', x = colnames(x))
  oldcolnames <- gsub(pattern = '^_', replacement = '', x = oldcolnames)
  
  # Problems? If rownames in file, no colname for 1st col so gets called X
  # FACID appearing twice in file, so 2d col named that gets called FACID.1
  # NEURO related indicators may still be in file, but neuro indicators were dropped in 2016 or so.
  # c("X", "FACID.1",
  # "RAW_E_NEURO", "S_E_NEURO_PER", "R_E_NEURO_PER", "N_E_NEURO_PER", "S_E_NEURO", "R_E_NEURO", "N_E_NEURO")
  # Might want to drop those fields in case they got created for some reason?
  x <- x[ , names(x) != "FACID.1"]
  x <- x[ , names(x) != "X"]
  x <- x[ , !grepl('NEURO', names(x))] 

    
  if (missing(namesfile) & !missing(oldcolnames) & !missing(newcolnames) ) {
    # IMPROVE COLUMN ORDER 
    x <- x[ , change.fieldnames(names(x), oldnames = oldcolnames, sort = TRUE)] 
    # RENAME FIELDS TO FRIENDLIER NEW NAMES
    names(x) <- change.fieldnames(names(x), oldnames = oldcolnames, newnames = newcolnames)
  }
  
  if (!missing(namesfile)) {
    if (!missing(oldcolnames) | !missing(newcolnames)) {warning('ignoring oldcolnames and newcolnames because namesfile was specified')}
    if (namesfile == 'keepnames') {
      # that is how user can specify they want no changes made to the names
    } else {
      # IMPROVE COLUMN ORDER 
      x <- x[ , change.fieldnames(names(x), file = namesfile, sort = TRUE)] 
      # RENAME FIELDS TO FRIENDLIER NEW NAMES
      names(x) <- change.fieldnames(names(x), file = namesfile)
    }
  }
  
  if (missing(namesfile) & missing(oldcolnames) & missing(newcolnames) ) {
    # use default fieldname changes if nothing is specified for column names
    #namesfile <- 'map batchtool to gdb to R fieldnames.csv' #'map batch to friendly fieldnames v1.csv'
    # IMPROVE COLUMN ORDER
    x <- x[ , change.fieldnames(names(x), file = namesfile, sort = TRUE)] 
    # RENAME FIELDS TO FRIENDLIER NEW NAMES
    names(x) <- change.fieldnames(names(x), file = namesfile)
  }
  
  # try to convert fields from character to text by removing 
  # percent sign, comma, miles, and treat N/A as NA:
  makenum <- function(x) {as.data.frame( lapply(x, function(y) as.numeric(
    gsub('th', '', 
         gsub('<', '', 
              gsub(' miles', '', 
                   gsub('N/A','', 
                        gsub('%','', 
                             gsub(',','',y)) )))))),
    stringsAsFactors = FALSE)}
  
  charcol <- names(x) %in% c('OBJECTID', 'FACID', 'name', 'ST', 'statename', 'id', 'ID')
  x[ , !charcol] <- makenum(x[ , !charcol])
  if (!('OBJECTID' %in% names(x))) {x$OBJECTID <- 1:NROW(x)} # required by map and probably elsewhere
  
  # Remove rows where no results from ArcGIS since lat/lon in Guam, American Samoa, Mariana Islands, VI, etc. where EJSCREEN does not work.
  # It won't include those in any stats and also cannot map them since removed here, even though they had lat/lon data uploaded! 
  if (any(is.na(x$pop))) warning('Dropping the points where there were no EJSCREEN results at all, such as in Guam, American Samoa, Mariana Islands, VI')
  x <- x[!is.na(x$pop), ]  # it retains if pop = 0, but not if it is missing entirely, which shows up here as NA once cleaned above
  
  return(x)
}

