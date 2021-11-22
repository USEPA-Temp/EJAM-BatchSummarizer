#' @title read tabular data files saved from a set of buffer reports - probably obsolete
#' @description This is a function to read a set of csv files saved from EJSCREEN tabular view
#'  and compile / format them and save them in format provided by output of batch processing tool
#'  so they can be uploaded by batch summarizer to be analyzed as if they had come from the batch processing tool.
#'  This allows users with access to EJSCREEN, but not the batch tool, to use the batch summarizer
#'  to get summary reports/ graphics on a group of buffer reports (i.e., standard reports on multiple points) (or block group reports if extended to that)
#' @param files character vector of filenames with full paths
#' @param folder default is working directory, searches for all files in this folder unless files param given
#' @param ... pass more to tabs.compile
#' @author ejanalyst info@ejanalysis.com
#' @return the output is the output
tab.to.batch <- function(files, folder=getwd(), ...) {
  
  # could recode to have this function write.csv() the finaltable
  
  if (missing(files)) { 
    # if files not specified, assume user wants all csv files in the folder (problem if some are not from tabular view saves)
    files <- list.files(path = folder, pattern = '.csv', ignore.case = TRUE)
  }
  
  # compile parsed tabular reports into a tall format
  fulltable  <- tabs.compile(files = files, folder = folder, ...)
  colcount <- dim(fulltable)[2] # how many columns- differs for circular buffer reports vs block group reports etc.
  
  # convert tall format into wide format, which is what batch.summarizer reads
  finaltable <- tabs.reformat(fulltable = fulltable, folder = folder, colcount = colcount)
  
  return(finaltable)

  # To view results so far:
  # t(head(finaltable,2))
  
  # example: 
  #   batchout = tab.to.batch(c('ejscreenbuffersavedfromtabular1.csv', 'ejscreenbuffersavedfromtabular1.csv'))
  #   write.csv(batchout, row.names=FALSE, file='batch_for_summarizer.csv')
}
