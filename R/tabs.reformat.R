#' @title reformats tabular data saved from a single buffer report - probably obsolete
#' @description based on code from make.finaltable.R
#' @param fulltable input is fulltable, a set of tabular data from a buffer standard report in EJSCREEN
#' @param folder default is working directory
#' @param colcount colcount is 17 for circular buffer report, but differs for block group and maybe noncircular buffer reports
#' @author ejanalyst info@ejanalysis.com
#' @return the output is the output
tabs.reformat <- function(fulltable, folder=getwd(), colcount=17 ) {

warning(' # DOES NOT WORK YET -- IT IS CREATING PAIRS OF ROWS INSTEAD OF 2 COLS, ONE FOR EACH BUFFER REPORT I USED:') 

#     > t(batchout)
#   [,1]                                           [,2]                                                                 
#   ID                                                        "ejscreenbuffersavedfromtabular1"              "ejscreenbuffersavedfromtabular1"                                    
#   lon                                                       "-74.04961"                                    "-74.04961"                                                          
#   lat                                                       "40.86902"                                     "40.86902"                                                           
#   miles                                                     "1"                                            "1"                                                                  
#   State                                                     "NEW JERSEY"                                   "NEW JERSEY"                                                         
#   REGION                                                    "2"                                            "2"                                                                  
#   pop                                                       "19214"                                        "19214"                                                              
#   EJ Index for Lead Paint Indicator fieldname               "EJ Index for Lead Paint Indicator"            "NATA Neurological Hazard Index*"                                    
#   EJ Index for Lead Paint Indicator fieldname               "EJ Index for Lead Paint Indicator"            "NATA Respiratory Hazard Index*"                                     
#   EJ Index for NATA Air Toxics Cancer Risk* fieldname       "EJ Index for NATA Air Toxics Cancer Risk*"    "NATA Respiratory Hazard Index*"                                     
#   EJ Index for NATA Air Toxics Cancer Risk* fieldname       "EJ Index for NATA Air Toxics Cancer Risk*"    "NPL Proximity (site count/km distance)"                             
#   EJ Index for NATA Diesel PM* fieldname                    "EJ Index for NATA Diesel PM*"                 "NPL Proximity (site count/km distance)"                             
#   EJ Index for NATA Diesel PM* fieldname                    "EJ Index for NATA Diesel PM*"                 "Ozone (ppb)"                                                        
#   EJ Index for NATA Neurological Hazard Index* fieldname    "EJ Index for NATA Neurological Hazard Index*" "Ozone (ppb)"                                                        
#   EJ Index for NATA Neurological Hazard Index* fieldname    "EJ Index for NATA Neurological Hazard Index*" "Particulate Matter (PM 2.5 in ug/m3)"                               
#   EJ Index for NATA Respiratory Hazard Index* fieldname     "EJ Index for NATA Respiratory Hazard Index*"  "Particulate Matter (PM 2.5 in ug/m3)"                               
#   EJ Index for NATA Respiratory Hazard Index* fieldname     "EJ Index for NATA Respiratory Hazard Index*"  "RMP Proximity (facility count/km distance)"                         
#   EJ Index for NPL Proximity fieldname                      "EJ Index for NPL Proximity"                   "RMP Proximity (facility count/km distance)"                         
#   
  
  
  ##################### #
  # INPUT FORMAT 
  ##################### #

  #   str(fulltable)  # format of circular buffer tabular view saved as csv and parsed/compiled by tabs.compile():
  #   'data.frame':	62 obs. of  17 variables:
  #   $ ID          : chr  "ejscreenbuffersavedfromtabular1" "ejscreenbuffersavedfromtabular1" "ejscreenbuffersavedfromtabular1" "ejscreenbuffersavedfromtabular1" ...
  #   $ lon         : num  -74 -74 -74 -74 -74 ...
  #   $ lat         : num  40.9 40.9 40.9 40.9 40.9 ...
  #   $ miles       : chr  "1" "1" "1" "1" ...
  #   $ State       : chr  "NEW JERSEY" "NEW JERSEY" "NEW JERSEY" "NEW JERSEY" ...
  #   $ REGION      : chr  "2" "2" "2" "2" ...
  #   $ pop         : num  19214 19214 19214 19214 19214 ...
  #   $ fieldgroup  : chr  "EJ" "EJ" "EJ" "EJ" ...
  #   $ fieldname   : chr  "EJ Index for Lead Paint Indicator" "EJ Index for NATA Air Toxics Cancer Risk*" "EJ Index for NATA Diesel PM*" "EJ Index for NATA Neurological Hazard Index*" ...
  #   $ raw         : chr  "" "" "" "" ...
  #   $ stateavg    : chr  "" "" "" "" ...
  #   $ statepctile : chr  "80" "N/A" "N/A" "N/A" ...
  #   $ regionavg   : chr  "" "" "" "" ...
  #   $ regionpctile: chr  "73" "N/A" "N/A" "N/A" ...
  #   $ usavg       : chr  "" "" "" "" ...
  #   $ uspctile    : chr  "85" "N/A" "N/A" "N/A" ...
  

  ##################### #
  # OUTPUT FORMAT:
  ##################### #
  # "Export_Output_Example2.txt" is an example of batch tool output:
  #
  # OBJECTID,FACID,NAME,LAT,LON,totpop,buff,stabbr,statename,region,S_E_TSDF_PER,R_P_TRAFFIC,S_E_PM25_PER,R_P_CANCER,S_P_DIESEL,N_D_INDEX,RAW_E_RMP,R_E_PM25,R_D_LESSHS,R_E_DIESEL,RAW_D_OVER64,N_E_TSDF,R_E_LEAD_PER,R_E_RMP_PER,S_E_DIESEL_PER,RAW_E_RESP,R_D_INDEX_PER,RAW_D_LESSHS,N_E_TRAFFIC,S_E_NEURO_PER,N_P_NPL,S_D_INDEX,S_D_MINOR,S_D_LESSHS,S_P_RESP,N_E_PM25_PER,RAW_D_INDEX,N_E_NEURO_PER,RAW_D_UNDER5,RAW_E_LEAD,R_E_NPL_PER,S_E_RESP_PER,S_E_O3_PER,N_P_PM25,S_D_LESSHS_PER,N_E_DIESEL_PER,S_D_INCOME_PER,RAW_E_NPL,R_D_MINOR_PER,S_E_TRAFFIC,R_P_TSDF,RAW_E_TSDF,N_P_CANCER,RAW_E_NEURO,S_E_DIESEL,RAW_D_INCOME,N_P_RMP,N_E_O3_PER,S_E_O3,R_E_RESP,S_E_RESP,N_E_DIESEL,N_D_INDEX_PER,N_E_RMP_PER,RAW_D_MINOR,N_E_CANCER_PER,R_E_O3_PER,S_D_INDEX_PER,N_E_RMP,R_P_LEAD,R_E_NEURO,N_E_LEAD,S_E_RMP_PER,R_E_RMP,RAW_E_DIESEL,R_D_LING_PER,R_E_TRAFFIC,R_E_LEAD,R_D_OVER64_PER,N_P_NEURO,R_E_CANCER_PER,R_E_NPDES_PER,N_E_CANCER,N_D_MINOR_PER,S_E_TSDF,S_E_NPL,R_D_OVER64,S_D_MINOR_PER,S_P_TSDF,S_P_RMP,N_E_PM25,R_E_TSDF,S_E_RMP,RAW_D_LING,S_E_TRAFFIC_PER,S_P_PM25,S_E_LEAD,R_P_NEURO,S_D_LING,N_E_NPL,R_E_DIESEL_PER,R_D_LESSHS_PER,R_P_O3,N_E_TRAFFIC_PER,RAW_E_NPDES,N_E_NPDES,N_E_NEURO,R_P_DIESEL,N_E_RESP_PER,R_E_TSDF_PER,RAW_E_TRAFFIC,R_D_INDEX,R_P_PM25,N_D_UNDER5_PER,N_D_LESSHS_PER,R_E_NPDES,N_D_LING,S_E_PM25,N_E_NPL_PER,R_E_NEURO_PER,R_D_MINOR,N_P_TSDF,S_D_LING_PER,R_P_NPL,S_P_NPDES,S_E_NPDES_PER,N_D_UNDER5,S_E_NPL_PER,S_E_CANCER_PER,N_E_RESP,N_D_LESSHS,S_D_UNDER5,N_P_LEAD,RAW_E_CANCER,S_P_TRAFFIC,N_E_NPDES_PER,R_E_TRAFFIC_PER,N_P_NPDES,RAW_E_O3,N_P_O3,R_E_O3,N_E_O3,N_E_TSDF_PER,R_E_RESP_PER,S_D_OVER64,N_D_INCOME,R_E_NPL,R_D_UNDER5_PER,R_P_RESP,R_P_NPDES,S_P_O3,N_P_DIESEL,N_D_OVER64_PER,R_P_RMP,N_P_TRAFFIC,N_E_LEAD_PER,S_E_NPDES,S_D_OVER64_PER,S_P_NPL,N_D_MINOR,RAW_E_PM25,N_D_LING_PER,S_D_INCOME,S_P_NEURO,N_P_RESP,N_D_OVER64,S_D_UNDER5_PER,R_D_LING,R_E_CANCER,S_E_CANCER,S_P_CANCER,N_D_INCOME_PER,R_D_INCOME_PER,S_E_NEURO,R_D_INCOME,R_E_PM25_PER,R_D_UNDER5,S_E_LEAD_PER,S_P_LEAD
  # 1,1000,BRASW Facility,32.4,-94.7,"7,946",3 miles,TX,Texas,6,97,74,44,75,61,35%,1.2,9.44,19%,0.733,13%,0.054,74,92,37,1.5,72,23%,110,99,95,47%,55%,20%,66,27,61%,92,7%,0.27,97,54,51,77,64,49,70,0.29,69,91,96,0.33,80,0.12,0.913,52%,95,34,42.9,1.4,1.5,0.824,83,95,69%,72,47,68,0.31,84,0.043,0.3,91,0.42,0.478,66,81,0.18,63,90,87,73,49,79,0.073,0.067,11%,62,94,86,10.7,0.062,0.47,6%,60,61,0.17,92,9%,0.096,50,67,68,60,0.32,0.25,0.063,70,43,97,55,44%,68,62,78,0.35,5%,9.63,94,99,49%,97,56,95,74,71,7%,97,84,2.3,15%,8%,84,56,69,81,64,89,43.8,79,43.6,46.3,98,63,10%,34%,0.063,52,73,79,62,78,55,89,80,57,0.38,69,93,36%,9.44,74,39%,89,76,13%,49,7%,42,44,68,79,71,0.044,39%,47,7%,76,82
  ##################### #
  #header <- c('OBJECTID', 'FACID', 'NAME', 'LAT', 'LON', 'totpop', 'buff', 'stabbr', 'statename', 'region', 'S_E_TSDF_PER', 'R_P_TRAFFIC', 'S_E_PM25_PER', 'R_P_CANCER', 'S_P_DIESEL', 'N_D_INDEX', 'RAW_E_RMP', 'R_E_PM25', 'R_D_LESSHS', 'R_E_DIESEL', 'RAW_D_OVER64', 'N_E_TSDF', 'R_E_LEAD_PER', 'R_E_RMP_PER', 'S_E_DIESEL_PER', 'RAW_E_RESP', 'R_D_INDEX_PER', 'RAW_D_LESSHS', 'N_E_TRAFFIC', 'S_E_NEURO_PER', 'N_P_NPL', 'S_D_INDEX', 'S_D_MINOR', 'S_D_LESSHS', 'S_P_RESP', 'N_E_PM25_PER', 'RAW_D_INDEX', 'N_E_NEURO_PER', 'RAW_D_UNDER5', 'RAW_E_LEAD', 'R_E_NPL_PER', 'S_E_RESP_PER', 'S_E_O3_PER', 'N_P_PM25', 'S_D_LESSHS_PER', 'N_E_DIESEL_PER', 'S_D_INCOME_PER', 'RAW_E_NPL', 'R_D_MINOR_PER', 'S_E_TRAFFIC', 'R_P_TSDF', 'RAW_E_TSDF', 'N_P_CANCER', 'RAW_E_NEURO', 'S_E_DIESEL', 'RAW_D_INCOME', 'N_P_RMP', 'N_E_O3_PER', 'S_E_O3', 'R_E_RESP', 'S_E_RESP', 'N_E_DIESEL', 'N_D_INDEX_PER', 'N_E_RMP_PER', 'RAW_D_MINOR', 'N_E_CANCER_PER', 'R_E_O3_PER', 'S_D_INDEX_PER', 'N_E_RMP', 'R_P_LEAD', 'R_E_NEURO', 'N_E_LEAD', 'S_E_RMP_PER', 'R_E_RMP', 'RAW_E_DIESEL', 'R_D_LING_PER', 'R_E_TRAFFIC', 'R_E_LEAD', 'R_D_OVER64_PER', 'N_P_NEURO', 'R_E_CANCER_PER', 'R_E_NPDES_PER', 'N_E_CANCER', 'N_D_MINOR_PER', 'S_E_TSDF', 'S_E_NPL', 'R_D_OVER64', 'S_D_MINOR_PER', 'S_P_TSDF', 'S_P_RMP', 'N_E_PM25', 'R_E_TSDF', 'S_E_RMP', 'RAW_D_LING', 'S_E_TRAFFIC_PER', 'S_P_PM25', 'S_E_LEAD', 'R_P_NEURO', 'S_D_LING', 'N_E_NPL', 'R_E_DIESEL_PER', 'R_D_LESSHS_PER', 'R_P_O3', 'N_E_TRAFFIC_PER', 'RAW_E_NPDES', 'N_E_NPDES', 'N_E_NEURO', 'R_P_DIESEL', 'N_E_RESP_PER', 'R_E_TSDF_PER', 'RAW_E_TRAFFIC', 'R_D_INDEX', 'R_P_PM25', 'N_D_UNDER5_PER', 'N_D_LESSHS_PER', 'R_E_NPDES', 'N_D_LING', 'S_E_PM25', 'N_E_NPL_PER', 'R_E_NEURO_PER', 'R_D_MINOR', 'N_P_TSDF', 'S_D_LING_PER', 'R_P_NPL', 'S_P_NPDES', 'S_E_NPDES_PER', 'N_D_UNDER5', 'S_E_NPL_PER', 'S_E_CANCER_PER', 'N_E_RESP', 'N_D_LESSHS', 'S_D_UNDER5', 'N_P_LEAD', 'RAW_E_CANCER', 'S_P_TRAFFIC', 'N_E_NPDES_PER', 'R_E_TRAFFIC_PER', 'N_P_NPDES', 'RAW_E_O3', 'N_P_O3', 'R_E_O3', 'N_E_O3', 'N_E_TSDF_PER', 'R_E_RESP_PER', 'S_D_OVER64', 'N_D_INCOME', 'R_E_NPL', 'R_D_UNDER5_PER', 'R_P_RESP', 'R_P_NPDES', 'S_P_O3', 'N_P_DIESEL', 'N_D_OVER64_PER', 'R_P_RMP', 'N_P_TRAFFIC', 'N_E_LEAD_PER', 'S_E_NPDES', 'S_D_OVER64_PER', 'S_P_NPL', 'N_D_MINOR', 'RAW_E_PM25', 'N_D_LING_PER', 'S_D_INCOME', 'S_P_NEURO', 'N_P_RESP', 'N_D_OVER64', 'S_D_UNDER5_PER', 'R_D_LING', 'R_E_CANCER', 'S_E_CANCER', 'S_P_CANCER', 'N_D_INCOME_PER', 'R_D_INCOME_PER', 'S_E_NEURO', 'R_D_INCOME', 'R_E_PM25_PER', 'R_D_UNDER5', 'S_E_LEAD_PER', 'S_P_LEAD')
  
  if (missing(fulltable)) {stop('fulltable must be provided as output from tabs.compile()')}
  
  ################################################### #
  #  SHORTEN/WIDEN THE COMPILATION TO HAVE 1 ROW PER SITE, ALL OTHER DATA IN NUMEROUS COLUMNS
  ################################################### #
  
  # We have already checked to make sure every tabular txt file output has 
  # 31 rows, or varcount.typical, or rowcount, in case that may be set differently (31 means 12 EJ, 12 envt, 7 demog)

  # FIGURE OUT HOW MANY DATA ELEMENTS WERE IN THE REPORT SPLIT INTO ID.etc.rowcount and the rest of colcount 
  #
  #   $ ID          : chr  "ejscreenbuffersavedfromtabular1" "ejscreenbuffersavedfromtabular1" "ejscreenbuffersavedfromtabular1" "ejscreenbuffersavedfromtabular1" ...
  #   $ lon         : num  -74 -74 -74 -74 -74 ...
  #   $ lat         : num  40.9 40.9 40.9 40.9 40.9 ...
  #   $ miles       : chr  "1" "1" "1" "1" ...
  #   $ State       : chr  "NEW JERSEY" "NEW JERSEY" "NEW JERSEY" "NEW JERSEY" ...
  #   $ REGION      : chr  "2" "2" "2" "2" ...
  #   $ pop         : num  19214 19214 19214 19214 19214 ...

  #   $ fieldgroup  : chr  "EJ" "EJ" "EJ" "EJ" ...
  #   $ fieldname   : chr  "EJ Index for Lead Paint Indicator" "EJ Index for NATA Air Toxics Cancer Risk*" "EJ Index for NATA Diesel PM*" "EJ Index for NATA Neurological Hazard Index*" ...
  #   $ raw         : chr  "" "" "" "" ...
  #   $ stateavg    : chr  "" "" "" "" ...
  #   $ statepctile : chr  "80" "N/A" "N/A" "N/A" ...
  #   $ regionavg   : chr  "" "" "" "" ...
  #   $ regionpctile: chr  "73" "N/A" "N/A" "N/A" ...
  #   $ usavg       : chr  "" "" "" "" ...
  #   $ uspctile    : chr  "85" "N/A" "N/A" "N/A" ...
  #
  # The # of rows like ID, State, REGION, pop, etc. is different for buffer vs block group reports. (FIPS for a block group vs lat/lon/miles for buffers)
  # There are currently 9 data rows including fieldgroup: 
  #     fieldgroup, fieldname,   raw,   stateavg, statepctile,  regionavg, regionpctile,   usavg, uspctile
  # (row called fieldgroups says EJ, envt, or demog, etc.)
  # So the count of Basic rows like ID, State, etc. is found this way: (should be 5 if block groups and was 7 if buffers).
  
  varcount.typical <- 31 #??? how many indicators (e.g., env, demog, or EJ Indexes)
  #colcount <- 17 # how many stats on each indicator (e.g., US avg.) for circular buffers at least... not necessarily for block group reports or non circular buffers
  ID.etc.rowcount <- colcount - 9 # used to be 7 , if circular buffers

  # This pointcount should match the length of filelist, and is the number of points/buffers/zones/places analyzed.
  pointcount <- length(fulltable[, 1]) / varcount.typical
  if (floor(pointcount) != pointcount) {
    cat("\n ERROR - Table length is not an integer multiple of number of places/buffers.\n")
    #logwrite("ERROR - Table length is not an integer multiple of number of places/buffers.")
  }
  
  # make finaltable which will have only 1 row per ID instead of 31 (varcount.typical), 
  # but 31*7  or 31*5 columns to the right instead of just 7  if buffers (or 5 if these are block group not buffer reports).
  # Take every 31 or so rows and make those into one row of the shorter/wider table, by creating 7 (or 5) new columns for each of those rows.
  
  # keep rows unique to one ID (some of these are stored as factors but that is OK)
  # There are 7 (8 now) such rows currently if they are buffer reports:   ID, lon, lat, miles, State, REGION, pop  ***************** 
  # BUT there are 5 such rows if they are block group reports: ID, FIPS, State, REGION, pop
  # so but we will use variable varcount.typical or rowcount
  # print('fulltable str:')
  # print( str(fulltable) )
  
  finaltable <- data.frame(unique(fulltable[ , 1:ID.etc.rowcount]))
  
  # can assume that retains the original order of the rows, or use merge to join on ID
  row.names(finaltable) <- NULL
  
  # column 8 if buffer, col 6 if block group report, is fieldname- it gets the specific type of field appended to it.
  # note that last column is fieldgroup so leave that last one out
  # The result is for example, "Ozone (Env) regionavg"

  for (mycolname in names(fulltable[ , (ID.etc.rowcount + 2):(colcount - 1)]) ) {
    
    c.this <- matrix(fulltable[ , mycolname], ncol = varcount.typical, nrow = pointcount, byrow = TRUE)
    colnames(c.this) <- paste(fulltable$fieldname[1:varcount.typical], mycolname)
    finaltable <- cbind(finaltable, c.this, stringsAsFactors = FALSE)
  }
  
  # To view results so far:
  #   dim(finaltable)
  #   [1]  62 225      **** should only be 2 rows now? not 31+31 !
  #   t(head(finaltable,2))
  
  #header <- c('OBJECTID', 'FACID', 'NAME', 'LAT', 'LON', 'totpop', 'buff', 'stabbr', 'statename', 'region', 'S_E_TSDF_PER', 'R_P_TRAFFIC', 'S_E_PM25_PER', 'R_P_CANCER', 'S_P_DIESEL', 'N_D_INDEX', 'RAW_E_RMP', 'R_E_PM25', 'R_D_LESSHS', 'R_E_DIESEL', 'RAW_D_OVER64', 'N_E_TSDF', 'R_E_LEAD_PER', 'R_E_RMP_PER', 'S_E_DIESEL_PER', 'RAW_E_RESP', 'R_D_INDEX_PER', 'RAW_D_LESSHS', 'N_E_TRAFFIC', 'S_E_NEURO_PER', 'N_P_NPL', 'S_D_INDEX', 'S_D_MINOR', 'S_D_LESSHS', 'S_P_RESP', 'N_E_PM25_PER', 'RAW_D_INDEX', 'N_E_NEURO_PER', 'RAW_D_UNDER5', 'RAW_E_LEAD', 'R_E_NPL_PER', 'S_E_RESP_PER', 'S_E_O3_PER', 'N_P_PM25', 'S_D_LESSHS_PER', 'N_E_DIESEL_PER', 'S_D_INCOME_PER', 'RAW_E_NPL', 'R_D_MINOR_PER', 'S_E_TRAFFIC', 'R_P_TSDF', 'RAW_E_TSDF', 'N_P_CANCER', 'RAW_E_NEURO', 'S_E_DIESEL', 'RAW_D_INCOME', 'N_P_RMP', 'N_E_O3_PER', 'S_E_O3', 'R_E_RESP', 'S_E_RESP', 'N_E_DIESEL', 'N_D_INDEX_PER', 'N_E_RMP_PER', 'RAW_D_MINOR', 'N_E_CANCER_PER', 'R_E_O3_PER', 'S_D_INDEX_PER', 'N_E_RMP', 'R_P_LEAD', 'R_E_NEURO', 'N_E_LEAD', 'S_E_RMP_PER', 'R_E_RMP', 'RAW_E_DIESEL', 'R_D_LING_PER', 'R_E_TRAFFIC', 'R_E_LEAD', 'R_D_OVER64_PER', 'N_P_NEURO', 'R_E_CANCER_PER', 'R_E_NPDES_PER', 'N_E_CANCER', 'N_D_MINOR_PER', 'S_E_TSDF', 'S_E_NPL', 'R_D_OVER64', 'S_D_MINOR_PER', 'S_P_TSDF', 'S_P_RMP', 'N_E_PM25', 'R_E_TSDF', 'S_E_RMP', 'RAW_D_LING', 'S_E_TRAFFIC_PER', 'S_P_PM25', 'S_E_LEAD', 'R_P_NEURO', 'S_D_LING', 'N_E_NPL', 'R_E_DIESEL_PER', 'R_D_LESSHS_PER', 'R_P_O3', 'N_E_TRAFFIC_PER', 'RAW_E_NPDES', 'N_E_NPDES', 'N_E_NEURO', 'R_P_DIESEL', 'N_E_RESP_PER', 'R_E_TSDF_PER', 'RAW_E_TRAFFIC', 'R_D_INDEX', 'R_P_PM25', 'N_D_UNDER5_PER', 'N_D_LESSHS_PER', 'R_E_NPDES', 'N_D_LING', 'S_E_PM25', 'N_E_NPL_PER', 'R_E_NEURO_PER', 'R_D_MINOR', 'N_P_TSDF', 'S_D_LING_PER', 'R_P_NPL', 'S_P_NPDES', 'S_E_NPDES_PER', 'N_D_UNDER5', 'S_E_NPL_PER', 'S_E_CANCER_PER', 'N_E_RESP', 'N_D_LESSHS', 'S_D_UNDER5', 'N_P_LEAD', 'RAW_E_CANCER', 'S_P_TRAFFIC', 'N_E_NPDES_PER', 'R_E_TRAFFIC_PER', 'N_P_NPDES', 'RAW_E_O3', 'N_P_O3', 'R_E_O3', 'N_E_O3', 'N_E_TSDF_PER', 'R_E_RESP_PER', 'S_D_OVER64', 'N_D_INCOME', 'R_E_NPL', 'R_D_UNDER5_PER', 'R_P_RESP', 'R_P_NPDES', 'S_P_O3', 'N_P_DIESEL', 'N_D_OVER64_PER', 'R_P_RMP', 'N_P_TRAFFIC', 'N_E_LEAD_PER', 'S_E_NPDES', 'S_D_OVER64_PER', 'S_P_NPL', 'N_D_MINOR', 'RAW_E_PM25', 'N_D_LING_PER', 'S_D_INCOME', 'S_P_NEURO', 'N_P_RESP', 'N_D_OVER64', 'S_D_UNDER5_PER', 'R_D_LING', 'R_E_CANCER', 'S_E_CANCER', 'S_P_CANCER', 'N_D_INCOME_PER', 'R_D_INCOME_PER', 'S_E_NEURO', 'R_D_INCOME', 'R_E_PM25_PER', 'R_D_UNDER5', 'S_E_LEAD_PER', 'S_P_LEAD')
  
  return(finaltable)  
}

