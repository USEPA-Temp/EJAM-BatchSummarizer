#' @title tab.parsed is OBSOLETE
#' @description WAS TO READ IN THE CSV SAVED FROM THE SINGLE BUFFER REPORT SHOWN AS TABULAR VIEW ON SCREEN
#'   AND IT IS NOT USED BY THE BATCH SUMMARIZER. IT WAS USED TO READ ONE AT A TIME WHEN THAT WAS USEFUL A WHILE AGO.
#' @param myfile name of file. a csv of saved tabular data from a buffer standard report in EJSCREEN 
#'   and it will be imported as a data.frame plus info from the 1st two rows, using read.csv(file, as.is=TRUE, skip=2) 
#' @param folder default is getwd()
#' @param skip default lines to skip is 2 
#' @param wordsbeforefips.bg default=2
#' @param wordsbeforestate.bg default=3
#' @param wordsafterstate.bg default=5
#' @param wordsafterregion.bg default=2 
#' @param wordsafterpop.bg default=0
#' @param wordsbeforemiles.ring =0
#' @param wordsbeforestate.ring =7
#' @param wordsafterstate.ring =5
#' @param wordsafterregion.ring =2
#' @param wordsafterpop.ring =0
#' @param wordsbeforelon.ring =6
#' @param wordsbeforelat.ring =5
#' @return output is not described here
tab.parsed <- function(myfile, folder=getwd(), skip=2, 
                       wordsbeforefips.bg=2,    wordsbeforestate.bg=3, wordsafterstate.bg=5, wordsafterregion.bg=2, wordsafterpop.bg=0, 
                       wordsbeforemiles.ring=0, wordsbeforestate.ring=7, wordsafterstate.ring=5, wordsafterregion.ring=2, wordsafterpop.ring=0,
                       wordsbeforelon.ring=6, wordsbeforelat.ring=5) {
  
  # This is based on old code in parsed.rows.R

  ##################### #
  # FORMAT OF EJSCREEN TABULAR REPORT SAVED AS CSV:
  ##################### #
  #
  # OLD example of format of first line in tabular report txt file saved for each location
  # Center: -75.662 39.656, Buffer: 3 mile(s), Delaware, Region 3 (Population: 83869)
  #
  # NEW format:
  # 
  # first row of csv saved from tabular view for a circular buffer:
  # "1 mile Ring Centered at 40.869024,-74.049613, NEW JERSEY, EPA Region 2 (Population: 19214)"
  #
  # first row of csv saved from tabular view for a block group:
  # "Block group 340030362001, NEW JERSEY, EPA Region 2 (Population: 1891)"
  #
  # remainder of csv:
  # "*The National-scale Air Toxics Assessment (NATA) environmental indicators and EJ indexes will be added into EJSCREEN during the first full public update after the soon-to-be-released 2011 dataset is made available."
  # ,Category,Selected Variables,Raw Data,State Avg.,%ile in State,EPA Region Avg.,%ile in EPA Region,USA Avg.,%ile in USA
  # "1","EJ Index","EJ Index for Particulate Matter (PM 2.5)","","","77","","72","","75"
  # "2","EJ Index","EJ Index for Ozone","","","76","","71","","73"
  # "3","EJ Index","EJ Index for NATA Diesel PM*","","","N/A","","N/A","","N/A"
  # ETC.
  
  mytable <- read.csv(file.path(folder, myfile), skip=skip, as.is=TRUE)  # reads all but first skip rows, AND GETS HEADERS FROM e.g., 3d ROW, WHERE THEY ARE; as character not turned into factor
  #   str(mytable)
  #   'data.frame':	31 obs. of  10 variables:
  #     $ X.                 : int  1 2 3 4 5 6 7 8 9 10 ...
  #   $ Category           : chr  "EJ Index" "EJ Index" "EJ Index" "EJ Index" ...
  #   $ Selected.Variables : chr  "EJ Index for Particulate Matter (PM 2.5)" "EJ Index for Ozone" "EJ Index for NATA Diesel PM*" "EJ Index for NATA Air Toxics Cancer Risk*" ...
  #   $ Raw.Data           : chr  "" "" "" "" ...
  #   $ State.Avg.         : chr  "" "" "" "" ...
  #   $ X.ile.in.State     : chr  "77" "76" "N/A" "N/A" ...
  #   $ EPA.Region.Avg.    : chr  "" "" "" "" ...
  #   $ X.ile.in.EPA.Region: chr  "72" "71" "N/A" "N/A" ...
  #   $ USA.Avg.           : chr  "" "" "" "" ...
  #   $ X.ile.in.USA       : chr  "75" "73" "N/A" "N/A" ...
  #   
  # reads only first row (basic info about the place analyzed)
  first.row   <- scan(myfile, what="character", nlines=1, quiet=TRUE)
  first.row <- gsub(",", " ", first.row)		# use spaces not commas
  first.row <- gsub(")", "", first.row)		# get rid of close parens
  first.row <- gsub("\\(", "", first.row)		# get rid of parens
  first.row <- gsub(":", "", first.row)		# get rid of :
  first.row <- strsplit(first.row, ' ')[[1]]
  first.row <- first.row[first.row!=''] # drop nulls
  first.row <- trimws(first.row)
  colcount <- length(first.row)

  validFormat <- FALSE
  
  if (first.row[1]=="Block") { 			
    # this is a block group, not buffer, report... it has FIPS but no lon/lat/milesS= 13 lines with ID but not fieldgroup added later
    # "Block group 340030362001, NEW JERSEY, EPA Region 2 (Population: 1891)"
    mytable <- cbind(
      FIPS=first.row[wordsbeforefips.bg + 1],
      State=paste(first.row[(wordsbeforestate.bg + 1):(colcount - wordsafterstate.bg)], collapse=" "),
      REGION=first.row[colcount - wordsafterregion.bg],
      pop=first.row[colcount - wordsafterpop.bg],
      mytable, stringsAsFactors = FALSE)
    validFormat <- TRUE
  }
  
  if (first.row[3]=='Ring') {
    # this is a buffer report, not a single block group report... it has lon/lat/miles, but no FIPS= 15 lines with ID but not fieldgroup added later
    # "1 mile Ring Centered at 40.869024,-74.049613, NEW JERSEY, EPA Region 2 (Population: 19214)"
    mytable <- cbind(
      lon=as.numeric(first.row[wordsbeforelon.ring + 1]),
      lat=as.numeric(first.row[wordsbeforelat.ring + 1]),
      miles=first.row[wordsbeforemiles.ring + 1],
      State=paste(first.row[(wordsbeforestate.ring + 1):(colcount - wordsafterstate.ring)], collapse=" "),
      REGION=first.row[colcount - wordsafterregion.ring],
      pop=as.numeric(first.row[colcount - wordsafterpop.ring]),
      mytable, stringsAsFactors = FALSE)
    validFormat <- TRUE
  }
  
  if (!validFormat) {stop('Cannot recognize format of csv file as report on circular buffer or blockgroup - other formats not supported')}

  mytable <- cbind(ID=myfile, mytable, stringsAsFactors = FALSE)
  return(mytable)
}

