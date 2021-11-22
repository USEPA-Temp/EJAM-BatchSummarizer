#' @title compiles into one table the several tables from files saved from EJSCREEN buffer reports - probably obsolete
#' @description This is a function to read a set of csv files saved from EJSCREEN tabular view and compile / format them 
#' @param files character vector of filenames with full paths
#' @param folder default is working directory, searches for all files in this folder unless files param given
#' @param ... pass more to tabs.parsed
#' @author ejanalyst info@ejanalysis.com
#' @return the output is the output
tabs.compile <- function(files, folder=getwd(), ...) {
  
  # based on code from make.fulltable.R
  
  if (missing(files)) {
    files <- list.files(path = folder, pattern = '.csv')
  }
  
  #print(Sys.time())
  # pre-allocate enough memory for the fulltable, to speed up populating it with data:
  
  mytable <- tab.parsed(files[1], folder = folder, ...)
  colcount <- length(mytable)
  rowcount <- length(mytable[ , 1]) 
  
  fulltable <- mytable # give it the correct classes and dimensions
  fulltable[1:(rowcount * length(files)), ] <- NA  # MAKE IT THE FULL EVENTUAL LENGTH TO PREALLOCATE MEMORY
  #fulltable <- as.data.frame(matrix(rep(NA, rowcount * length(files) * colcount),  nrow=rowcount * length(files), ncol=colcount ))
  names(fulltable) <- names(mytable)
  nextrownum <- 1
  
  for (i in 1:length(files)) 
  {
    mytable <- tab.parsed(files[i], folder = folder, ...)
    # This avoids growing it with rbind which would be very slow:
    fulltable[nextrownum:(nextrownum + rowcount - 1), 1:colcount] <- mytable
    nextrownum <- nextrownum + rowcount
  }
  
  #print(Sys.time())  
  # Rewrote it 3/27/2014 or so, to use preallocated data frame and replacing subsets with <-, 
  # and took <10 seconds for 983 files on MacBookPro2012 or so?
  #logwrite(paste("Miles radius for buffering:",radius.miles)); logwrite("")
  
  ################################# #
  # NOW CLEAN UP AND ENHANCE THE fulltable DATAFRAME WHICH HAS ALL THE RESULTS IN ONE LONG TABLE
  ################################# #
  
  # Remove .txt from the filename stored in fulltable, leaving just the filename as the unique ID for the buffer/zone
  fulltable$ID <- gsub(".csv", "", fulltable$ID)

  # format now:
  #   [1,] "ID"                 
  #   [2,] "lon"                
  #   [3,] "lat"                
  #   [4,] "miles"              
  #   [5,] "State"              
  #   [6,] "REGION"             
  #   [7,] "pop"                
  #   [8,] "X."                 new
  #   [9,] "Category"           new
  #   [10,] "Selected.Variables"   varname content differs
  #   [11,] "Raw.Data"           
  #   [12,] "State.Avg."         
  #   [13,] "X.ile.in.State"     renamed
  #   [14,] "EPA.Region.Avg."    
  #   [15,] "X.ile.in.EPA.Region" renamed
  #   [16,] "USA.Avg."           
  #   [17,] "X.ile.in.USA"       renamed
  
  # OLD FORMAT:
  #
  # cbind(names(fulltable)) # shows the following at this point:
  #[1,] "ID"                
  #[2,] "lon"               
  #[3,] "lat"               
  #[4,] "miles"             
  #[5,] "State"             
  #[6,] "REGION"            
  #[7,] "pop"               
  # [8,] "Selected.Variables"
  # [9,] "Raw.Data"          
  #[10,] "State.Avg."        
  #[11,] "State..ile"        
  #[12,] "EPA.Region.Avg."   
  #[13,] "EPA.Region..ile"   
  #[14,] "USA.Avg."          
  #[15,] "USA..ile" 
  
  # OLD FORMAT:
  #
  # head(fulltable,33)# shows the following at this point:
  #
  # etc. (US %ile column is cutoff in this view here and not all lines of it shown)
  #
  #ID     lon    lat miles State REGION  pop               Selected.Variables Raw.Data State.Avg. State..ile EPA.Region.Avg. EPA.Region..ile USA.Avg.
  #1   1 -92.828 33.585     1     1      1  356           NATA Cancer Risk (Env)       58     51.000         79          53.000              65   61.000
  #2   1 -92.828 33.585     1     1      1  356                  Diesel PM (Env)    0.171      0.245         61           0.734              30    0.825
  #3   1 -92.828 33.585     1     1      1  356            NATA Cancer Risk (EJ)                  NA         89              NA              72       NA
 
  
  names(fulltable) <- gsub("Selected.Variables","fieldname",names(fulltable))
  names(fulltable) <- gsub("Raw.Data","raw",names(fulltable))
  names(fulltable) <- gsub("State.Avg.","stateavg",names(fulltable))
  names(fulltable) <- gsub("X.ile.in.State","statepctile",names(fulltable))
  names(fulltable) <- gsub("EPA.Region.Avg.","regionavg",names(fulltable))
  names(fulltable) <- gsub("X.ile.in.EPA.Region","regionpctile",names(fulltable))
  names(fulltable) <- gsub("USA.Avg.","usavg",names(fulltable))
  names(fulltable) <- gsub("X.ile.in.USA","uspctile",names(fulltable))
  names(fulltable) <- gsub("X.","rownum",names(fulltable)) # drop this actually... doesnt help and causes problem in tabs.reformat since unique() fails
  fulltable$rownum <- NULL
  names(fulltable) <- gsub("Category","category",names(fulltable))
  fulltable$category <- gsub('EJ Index', 'EJ', fulltable$category)
  fulltable$category <- gsub('Environmental', 'Env', fulltable$category)
  fulltable$category <- gsub('Demographic', 'Demog', fulltable$category)
  names(fulltable) <- gsub('category', 'fieldgroup', names(fulltable))

  ################################################### #
  # CHANGE THE ORDER OF THESE ROWS TO HAVE A MORE USEFUL ORGANIZATION TO THEM:
  # 1. Sort on ID first, keeping 31 (or varcount.typical) rows together for a point/buffer/place
  #  2. within a buffer/place, sort on nchar(fieldgroup) puts EJ first, env second, demog third
  #   3. within each cluster of fieldnames, sort on fieldname is alpha (that seems fine)
  ################################################### #
  
  fulltable <- fulltable[order(fulltable$ID, nchar(fulltable$fieldgroup), fulltable$fieldname), ]
  
  #   names(fulltable)
  #   [1] "ID"          
  #   [2] "lon"         
  #   [3] "lat"         
  #   [4] "miles"       
  #   [5] "State"       
  #   [6] "REGION"      
  #   [7] "pop"         
  #   [8] "rownum"      THIS WAS DROPPED NOW
  #   [9] "fieldgroup"  
  #   [10] "fieldname"   
  #   [11] "raw"         
  #   [12] "stateavg"    
  #   [13] "statepctile" 
  #   [14] "regionavg"   
  #   [15] "regionpctile"
  #   [16] "usavg"       
  #   [17] "uspctile"    
  
  return(fulltable)
  
  ################################################### #
  #  COULD SAVE JUST IN CASE A LONG FORMAT FILE COMPILATION
  ################################################### #
  
  # write.csv(fulltable, paste(as.numeric(fulltable$miles[1])," mile buffer EJSCREEN batch ", varcount.typical, " rows per ID.csv", sep=""), row.names=FALSE)
  #logwrite(" ")
  #logwrite("saved file:")
  #logwrite(paste(fulltable$miles[1]," mile buffer EJSCREEN batch ", varcount.typical, " rows per ID.csv", sep=""))
  #logwrite(" ")
  
}
