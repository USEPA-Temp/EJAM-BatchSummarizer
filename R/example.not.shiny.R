if (1 == 0) {
  
  # Example of using batch.summarize directly
  
  # *** These (newer versions) are also found in the packages called analyze.stuff and ejanalysis, (and ejscreen pkg provides popupunits)
  # available at http://ejanalysis.github.io
  #
  source('pct.above.R')         # returns percent of rows (or wtd people) that have value above specified cutoff (or mean as default), for each column of data.frame
  source('count.above.R')        # returns count of how many rows (or wtd people) have value above specified cutoff (or mean as default), for each column of data.frame
  source('cols.above.count.R')  # returns count of how many cols (e.g. how many variables or indicators) have value above specified cutoff (or mean as default), for each row of data.frame
  source('flagged.R')      # creates flag that is TRUE if at least one of 12 indicators is > cutoff (EJ index >50th or 80th or 95th%ile, for example), for each of many rows of a data.frame
  source('rowMaxs.R')     # returns Max of each row
  source('rowMins.R')     # returns Min of each row
  source('colMaxs.R')     # returns Max of each col
  source('colMins.R')     # returns Min of each col
  source('wtd.colMeans.R')     # returns wtd.mean of each col
  source('lead.zeroes.R')  # add leading zeroes as needed to fix FIPS that had been stored as numeric
  source('change.fieldnames.R')  # updated function that helps change or resort fieldnames using a map file mapping old to new names
  
  # source('wilcoxon.pvalues.r')  # for statistical significance testing - from air
  source('batch.read.R')
  source('batch.clean.R')
  source('batch.summarize.R')
  require(Hmisc) # various useful functions for data analysis
  #   library(plotrix) # for better weighted.hist than ggplot2 can provide.
  #   require(ggplot2) # for geom_histogram() that allows weights to be used. plotrix package also does wtd hist, but better.
  
  options(stringsAsFactors = FALSE)
  
  ######################### #
  # define the functions
  ######################### #

  readclean <- function(file, namesfile) {
    lookup    <- read.csv(namesfile)
    batch.clean(read.csv(file), oldcolnames = lookup$oldnames, newcolnames = lookup$newnames)
  }
  
  # get summary stats cols and rows 
  batch.all <- function(sitefile, popfile, namesfile, ...) {
    return(batch.summarize(
      readclean(sitefile, namesfile), 
      readclean(popfile,  namesfile)
    ))
    ## or 
    #     test.sitestats <- read.csv(sitefile) # or batch.read() is same thing
    #     test.popstats  <- read.csv(popfile)
    #     test.lookup    <- read.csv(namesfile)
    #     
    #     test.sitestats <- batch.clean(test.sitestats, oldcolnames = test.lookup$oldnames, newcolnames = test.lookup$newnames )
    #     test.popstats  <- batch.clean(test.popstats,  oldcolnames = test.lookup$oldnames, newcolnames = test.lookup$newnames )
    #     
    #     test.outlist <- batch.summarize(test.sitestats, test.popstats, ...) 
    #     return(test.outlist)
  }
  
  ######################### #
  # use the functions
  ######################### #
  sitedata <- readclean("R/sample_ejscreen_batch_output.csv", 'map_batch_to_friendly_fieldnames_2016.csv')   # or "map_batch_to_friendly_fieldnames_2018.csv"
  sitedata1 <- readclean("Export_Output_Example2.csv", 'map batch to friendly fieldnames v1.csv')               # or "map_batch_to_friendly_fieldnames_2018.csv"
  popdata  <-  readclean("R/sample_ejscreen_batch_output.csv", 'map_batch_to_friendly_fieldnames_2016.csv')  # or "map_batch_to_friendly_fieldnames_2018.csv"
  
  #names(sitedata)

    # test.outlist <- batch.summarize(sitedata, popdata)
  # or,
  
  # get summary stats cols and rows:
  test.outlist <- batch.all(
    sitefile = "Export_Output_Example2.csv",
    popfile  = "Export_Output_Example2.pop.csv",
    namesfile = 'map batch to friendly fieldnames v1.csv'
  )
  
  # test.outlist <- batch.all(
  #   sitefile = "R/sample_ejscreen_batch_output.csv",
  #   popfile  = "R/sample_ejscreen_batch_output.csv",
  #   namesfile = 'map_batch_to_friendly_fieldnames_2016.csv' # or "map_batch_to_friendly_fieldnames_2018.csv"
  # )
  # FAILS AS OF 8/27/16
  ######################### #
  # View results  
  ######################### #
  
  test.outlist$cols  # one row per site, one col per summary stat
  # cbind(sitedata, test.outlist$cols)[ 1:7, c(1:7, 178:181)]
  t(cbind(sitedata, test.outlist$cols)[ 1:2, c(1:22, 170:173)])

  #test.outlist$rows[1:4, 1:20 ]  # one row per summary stat before t(), one col per indicator before t()
  round( t(test.outlist$rows[1:4, 1:20 ]), 1) 
  
  ######################### #
  
  #   > test.fulltable$cols
  #   Max of variables Number of variables at/above threshold of 80
  #   1              7946                                           39
  #   2              1487                                            7
  #   3             14003                                           53
  #   4            106245                                           68
  
  #   round( t(head(test.fulltable$rows[ , 1:20 ] , 2)), 2)
  #   
  #   Average site Average person
  #   OBJECTID                    21.50          14.44
  #   FACID                     1020.50        1013.44
  #   name                           NA             NA
  #   lat                         37.25          40.42
  #   lon                        -88.67         -85.25
  #   pop                      29973.21      138436.48
  #   radius.miles                 3.00           3.00
  #   ST                             NA             NA
  #   statename                      NA             NA
  #   REGION                       5.10           4.46
  #   VSI.eo                      38.00          54.19
  #   us.avg.VSI.eo               35.00          35.00
  #   region.avg.VSI.eo           34.40          31.53
  #   state.avg.VSI.eo            33.24          33.13
  #   pctile.VSI.eo               56.85          73.79
  #   region.pctile.VSI.eo        57.80          75.68
  #   state.pctile.VSI.eo         60.44          75.64
  #   pctlowinc                   41.44          49.39
  #   us.avg.pctlowinc            34.00          34.00
  #   region.avg.pctlowinc        34.64          32.51
  #   > 
  
  #   > names(test.sitestats)
  #   [1] "OBJECTID"                                     
  #   [2] "FACID"                                        
  #   [3] "name"                                         
  #   [4] "lat"                                          
  #   [5] "lon"                                          
  #   [6] "pop"                                          
  #   [7] "radius.miles"                                 
  #   [8] "ST"                                           
  #   [9] "statename"                                    
  #   [10] "REGION"                                       
  #   [11] "VSI.eo"                                       
  #   [12] "us.avg.VSI.eo"                                
  #  etc.  
  
  # cbind(sapply(test.sitestats, class))
  #   name                                          "character"
  #   lat                                           "numeric"  
  #   lon                                           "numeric"  
  #   pop                                           "numeric"  
  #   radius.miles                                  "numeric"  
  #   ST                                            "character"
  #   statename                                     "character"
  #   
  #   List of 2
  #   $ rows: num [1:24, 1:179] 21.5 14.4 21.5 13 1 ...
  #   ..- attr(*, "dimnames")=List of 2
  #   .. ..$ : chr [1:24] "Average site" "Average person" "Median site" "Median person" ...
  #   .. ..$ : chr [1:179] "OBJECTID" "FACID" "name" "lat" ...
  #   $ cols: num [1:42, 1:2] 7946 1487 14003 106245 344991 ...
  #   ..- attr(*, "dimnames")=List of 2
  #   .. ..$ : chr [1:42] "1" "2" "3" "4" ...
  #   .. ..$ : chr [1:2] "Max of variables" "Number of variables at/above threshold of 80"
  
  
  
  ############## #
  # As script instead of function:
  # 
  #   test.mydemofile <- "Export_Output_Example2.csv"
  #   test.mydemofile.pop <- "Export_Output_Example2.pop.csv"
  #   test.mynamesfile.default <- "map batch to friendly fieldnames v1.csv"
  #   options(stringsAsFactors = FALSE)
  #   test.sitestats <- read.csv(test.mydemofile)
  #   test.popstats <- read.csv(test.mydemofile.pop)
  #   test.lookup <- read.csv(test.mynamesfile.default)
  #   test.sitestats <- batch.clean(test.sitestats,  oldcolnames = test.lookup$oldnames, newcolnames = test.lookup$newnames )
  #   test.popstats  <- batch.clean(test.popstats,  oldcolnames = test.lookup$oldnames, newcolnames = test.lookup$newnames )
  # test.fulltable <- batch.summarize(test.sitestats, test.popstats)
  # rm(test.lookup,test.mydemofile,test.mydemofile.pop,test.mynamesfile.default,test.sitestats,test.popstats)
  # rm(test.fulltable)
  ####################### #
  
  
  
  ###############  as used within server.R
  #   batch.summarize <- function(sitestats, popstats, cols = 'all', 
  #     wtscolname = 'pop', wts=popstats[ , wtscolname], 
  #     probs=c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1), 
  #     threshold=list(80), threshnames=list(colnames(sitestats)), threshgroup=list('variables'), 
  #     na.rm=TRUE, 
  #     rowfun.picked='all', colfun.picked='all') {
  #     
  #   outlist <- reactive({ 
  #     x <- batch.summarize(
  #       sitestats = fulltabler(), popstats = fulltabler.pop(),
  #       wtscolname = mywtsname, 
  #       #wts = fulltabler()[ , mywtsname],  
  #       cols = mycolnames(), 
  #       threshnames = mythreshnames(), threshold = mythresholds(), threshgroup = mythreshgroups(),
  #       colfun.picked = colfun.picked, rowfun.picked = rowfun.picked,
  #       probs = as.numeric( input$probs ), na.rm = na.rm
  #     )
  #     ... other steps to further clean up results here
  ###############  
  
  
}

#   test.mydemofile <- "Export_Output_Example2.csv"
# e.g.
# This shows it for the 2015 version:
#   > t(test.sitestats[1:2,])
#                   1                2               
#   OBJECTID        "1"              "2"             
#   FACID           "1000"           "1001"          
#   NAME            "BRASW Facility" "EHZST Facility"
#   LAT             "32.4"           "33.7"          
#   LON             "-94.7"          "-94.4"         
#   totpop          "7,946"          "1,487"         
#   buff            "3 miles"        "3 miles"       
#   stabbr          "TX"             "AR"            
#   statename       "Texas"          "Arkansas"      
#   region          "6"              "6"             
#   S_E_TSDF_PER    "97"             "98"            
#   R_P_TRAFFIC     "74"             "30"            
#   S_E_PM25_PER    "44"             "29"            
#   R_P_CANCER      "75"             "35"            
#   S_P_DIESEL      "61"             "49"            
#   N_D_INDEX       "35%"            "35%"           
#   RAW_E_RMP       "1.2"            "0.038"         
#   R_E_PM25        "9.44"           "9.44"          
#   R_D_LESSHS      "19%"            "19%"           
#   R_E_DIESEL      "0.733"          "0.733"         
#   RAW_D_OVER64    "13%"            "18%"           
#   N_E_TSDF        "0.054"          "0.054"         
#   R_E_LEAD_PER    "74"             "69"            
#   R_E_RMP_PER     "92"             "5"             
#   S_E_DIESEL_PER  "37"             "36"            
#   RAW_E_RESP      "1.5"            "0.76"          
#   R_D_INDEX_PER   "72"             "33"            
#   RAW_D_LESSHS    "23%"            "14%"           
#   N_E_TRAFFIC     "110"            "110"           
#   S_E_NEURO_PER   "99"             "13"            
#   N_P_NPL         "95"             "51"            
#   S_D_INDEX       "47%"            "34%"           
#   S_D_MINOR       "55%"            "26%"           
#   S_D_LESSHS      "20%"            "17%"           
#   S_P_RESP        "66"             "54"            
#   N_E_PM25_PER    "27"             "27"            
#   RAW_D_INDEX     "61%"            "29%"           
#   N_E_NEURO_PER   "92"             "4"             
#   RAW_D_UNDER5    "7%"             "3%"            
#   RAW_E_LEAD      "0.27"           "0.22"          
#   R_E_NPL_PER     "97"             "41"            
#   S_E_RESP_PER    "54"             "26"            
#   S_E_O3_PER      "51"             "60"            
#   N_P_PM25        "77"             "53"            
#   S_D_LESSHS_PER  "64"             "43"            
#   N_E_DIESEL_PER  "49"             "12"            
#   S_D_INCOME_PER  "70"             "48"            
#   RAW_E_NPL       "0.29"           "0.024"         
#   R_D_MINOR_PER   "69"             "18"            
#   S_E_TRAFFIC     "91"             "64"            
#   R_P_TSDF        "96"             "7"             
#   RAW_E_TSDF      "0.33"           "0.33"          
#   N_P_CANCER      "80"             "52"            
#   RAW_E_NEURO     "0.12"           "0.023"         
#   S_E_DIESEL      "0.913"          "0.245"         
#   RAW_D_INCOME    "52%"            "41%"           
#   N_P_RMP         "95"             "56"            
#   N_E_O3_PER      "34"             "37"            
#   S_E_O3          "42.9"           "44.5"          
#   R_E_RESP        "1.4"            "1.4"           
#   S_E_RESP        "1.5"            "1.1"           
#   N_E_DIESEL      "0.824"          "0.824"         
#   N_D_INDEX_PER   "83"             "50"            
#   N_E_RMP_PER     "95"             "8"             
#   RAW_D_MINOR     "69%"            "17%"           
#   N_E_CANCER_PER  "72"             "33"            
#   R_E_O3_PER      "47"             "53"            
#   S_D_INDEX_PER   "68"             "50"            
#   N_E_RMP         "0.31"           "0.31"          
#   R_P_LEAD        "84"             "18"            
#   R_E_NEURO       "0.043"          "0.043"         
#   N_E_LEAD        "0.3"            "0.3"           
#   S_E_RMP_PER     "91"             "17"            
#   R_E_RMP         "0.42"           "0.42"          
#   RAW_E_DIESEL    "0.478"          "0.0875"        
#   R_D_LING_PER    "66"             "36"            
#   R_E_TRAFFIC     "81"             "81"            
#   R_E_LEAD        "0.18"           "0.18"          
#   R_D_OVER64_PER  "63"             "83"            
#   N_P_NEURO       "90"             "55"            
#   R_E_CANCER_PER  "87"             "39"            
#   R_E_NPDES_PER   "73"             "5"             
#   N_E_CANCER      "49"             "49"            
#   N_D_MINOR_PER   "79"             "38"            
#   S_E_TSDF        "0.073"          "0.046"         
#   S_E_NPL         "0.067"          "0.033"         
#   R_D_OVER64      "11%"            "11%"           
#   S_D_MINOR_PER   "62"             "51"            
#   S_P_TSDF        "94"             "4"             
#   S_P_RMP         "86"             "56"            
#   N_E_PM25        "10.7"           "10.7"          
#   R_E_TSDF        "0.062"          "0.062"         
#   S_E_RMP         "0.47"           "0.33"          
#   RAW_D_LING      "6%"             "0%"            
#   S_E_TRAFFIC_PER "60"             "44"            
#   S_P_PM25        "61"             "53"            
#   S_E_LEAD        "0.17"           "0.17"          
#   R_P_NEURO       "92"             "38"            
#   S_D_LING        "9%"             "2%"            
#   N_E_NPL         "0.096"          "0.096"         
#   R_E_DIESEL_PER  "50"             "17"            
#   R_D_LESSHS_PER  "67"             "46"            
#   R_P_O3          "68"             "36"            
#   N_E_TRAFFIC_PER "60"             "35"            
#   RAW_E_NPDES     "0.32"           "0.031"         
#   N_E_NPDES       "0.25"           "0.25"          
#   N_E_NEURO       "0.063"          "0.063"         
#   R_P_DIESEL      "70"             "37"            
#   N_E_RESP_PER    "43"             "15"            
#   R_E_TSDF_PER    "97"             "97"            
#   RAW_E_TRAFFIC   "55"             "19"            
#   R_D_INDEX       "44%"            "44%"           
#   R_P_PM25        "68"             "35"            
#   N_D_UNDER5_PER  "62"             "20"            
#   N_D_LESSHS_PER  "78"             "58"            
#   R_E_NPDES       "0.35"           "0.35"          
#   N_D_LING        "5%"             "5%"            
#   S_E_PM25        "9.63"           "9.70"          
#   N_E_NPL_PER     "94"             "29"            
#   R_E_NEURO_PER   "99"             "6"             
#   R_D_MINOR       "49%"            "49%"           
#   N_P_TSDF        "97"             "7"             
#   S_D_LING_PER    "56"             "67"            
#   R_P_NPL         "95"             "34"            
#   S_P_NPDES       "74"             "58"            
#   S_E_NPDES_PER   "71"             "8"             
#   N_D_UNDER5      "7%"             "7%"            
#   S_E_NPL_PER     "97"             "68"            
#   S_E_CANCER_PER  "84"             "49"            
#   N_E_RESP        "2.3"            "2.3"           
#   N_D_LESSHS      "15%"            "15%"           
#   S_D_UNDER5      "8%"             "7%"            
#   N_P_LEAD        "84"             "39"            
#   RAW_E_CANCER    "56"             "38"            
#   S_P_TRAFFIC     "69"             "40"            
#   N_E_NPDES_PER   "81"             "4"             
#   R_E_TRAFFIC_PER "64"             "35"            
#   N_P_NPDES       "89"             "56"            
#   RAW_E_O3        "43.8"           "44.4"          
#   N_P_O3          "79"             "52"            
#   R_E_O3          "43.6"           "43.6"          
#   N_E_O3          "46.3"           "46.3"          
#   N_E_TSDF_PER    "98"             "98"            
#   R_E_RESP_PER    "63"             "22"            
#   S_D_OVER64      "10%"            "14%"           
#   N_D_INCOME      "34%"            "34%"           
#   R_E_NPL         "0.063"          "0.063"         
#   R_D_UNDER5_PER  "52"             "15"            
#   R_P_RESP        "73"             "37"            
#   R_P_NPDES       "79"             "40"            
#   S_P_O3          "62"             "53"            
#   N_P_DIESEL      "78"             "54"            
#   N_D_OVER64_PER  "55"             "78"            
#   R_P_RMP         "89"             "40"            
#   N_P_TRAFFIC     "80"             "45"            
#   N_E_LEAD_PER    "57"             "51"            
#   S_E_NPDES       "0.38"           "0.25"          
#   S_D_OVER64_PER  "69"             "74"            
#   S_P_NPL         "93"             "46"            
#   N_D_MINOR       "36%"            "36%"           
#   RAW_E_PM25      "9.44"           "9.45"          
#   N_D_LING_PER    "74"             "45"            
#   S_D_INCOME      "39%"            "42%"           
#   S_P_NEURO       "89"             "55"            
#   N_P_RESP        "76"             "54"            
#   N_D_OVER64      "13%"            "13%"           
#   S_D_UNDER5_PER  "49"             "19"            
#   R_D_LING        "7%"             "7%"            
#   R_E_CANCER      "42"             "42"            
#   S_E_CANCER      "44"             "40"            
#   S_P_CANCER      "68"             "53"            
#   N_D_INCOME_PER  "79"             "65"            
#   R_D_INCOME_PER  "71"             "55"            
#   S_E_NEURO       "0.044"          "0.038"         
#   R_D_INCOME      "39%"            "39%"           
#   R_E_PM25_PER    "47"             "47"            
#   R_D_UNDER5      "7%"             "7%"            
#   S_E_LEAD_PER    "76"             "71"            
#   S_P_LEAD        "82"             "33" 


# Assume same lookup.fieldnames will be used for both sitestats and popstats,
# since they should have identical formats, 
# and this shows it for the 2015 version:
# test.mynamesfile.default <- "map batch to friendly fieldnames v1.csv"
# e.g.
#   oldnames                                      newnames
#   1          OBJECTID                                      OBJECTID
#   2             FACID                                         FACID
#   3        Registry_I                                    registryID
#   4              NAME                                          name
#   5               LAT                                           lat
#   6               LON                                           lon
#   7            totpop                                           pop
#   8              buff                                  radius.miles
#   9            stabbr                                            ST
#   10        statename                                     statename
#   11           region                                        REGION
#   12      RAW_D_INDEX                                        VSI.eo
#   13        N_D_INDEX                                 us.avg.VSI.eo
#   14        R_D_INDEX                             region.avg.VSI.eo
#   15        S_D_INDEX                              state.avg.VSI.eo
#   16    N_D_INDEX_PER                                 pctile.VSI.eo
#   17    R_D_INDEX_PER                          region.pctile.VSI.eo
#   18    S_D_INDEX_PER                           state.pctile.VSI.eo
#   19     RAW_D_INCOME                                     pctlowinc
#   20       N_D_INCOME                              us.avg.pctlowinc
#   21       R_D_INCOME                          region.avg.pctlowinc
#   22       S_D_INCOME                           state.avg.pctlowinc
#   23   N_D_INCOME_PER                              pctile.pctlowinc
#   24   R_D_INCOME_PER                       region.pctile.pctlowinc
#   25   S_D_INCOME_PER                        state.pctile.pctlowinc
#   26      RAW_D_MINOR                                        pctmin
#   27
