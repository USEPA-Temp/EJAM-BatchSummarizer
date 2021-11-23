#' Format EJSCREEN API batch results to upload to batch.summarizer
#'
#' @param output_of_bufferapi Result of proxistat::bufferapi() 
#' @param lat latitudes vector as sent to bufferapi
#' @param lon same but longitudes
#' @param radius Radius of circular buffers in miles
#'
#' @export
#'
#' @examples
#'   # EXAMPLE OF USING EJSCREEN API outputs in batch.summarizer
#'   ## How to get batch results from EJSCREEN API, 
#'   ## prepare them for use in the batch.summarizer, save as csv file,
#'   ## and then be able to run the R Shiny app batch.summarizer 
#'   ## and use app to upload that csv, to get a summary of those batch results.
#'   ## 
#'   ## Specify size of buffer circle and pick random points as example data
#'   # myradius <- 1
#'   # n <- 10
#'   # pts <- proxistat::testpoints_block2010(n)
#'   
#'   ## RUN BATCH BUFFER USING EJSCREEN API
#'   # outlist <- proxistat::bufferapi(pts$lon, lat=pts$lat, radius = myradius)
#'   # 
#'   # api.out.table <- prep.bufferapi.for.summarizer(outlist, lat = pts$lat, lon = pts$lon, radius = myradius)
#'   #
#'   ## SAVE FILE OF BATCH RESULTS FOR IMPORT TO SUMMARIZER
#'   # write.csv(api.out.table, file = '~/Downloads/api.out.table.csv')


prep.bufferapi.for.summarizer <- function(output_of_bufferapi, lat, lon, radius) {
  
  ########################################################################## #
  # SCRIPT EXAMPLE OF LINKING EJSCREEN API outputs to batch.summarizer
  ## example of how to get batch results from EJSCREEN API, 
  ## prepare them for use in the batch.summarizer, save as csv file,
  ## and then be able to run the R Shiny app batch.summarizer 
  ## and upload that csv, to get a summary of those batch results.
  ## 
  ## Specify size of buffer circle and pick random points as example data
  # myradius <- 1
  # n <- 10
  # pts <- proxistat::testpoints_block2010(n)
  #
  ## RUN BATCH BUFFER USING EJSCREEN API
  # outlist <- proxistat::bufferapi(pts$lon, lat=pts$lat, radius = myradius)
  # 
  # api.out.table <- prep.bufferapi.for.summarizer(outlist, lat = pts$lat, lon = pts$lon, radius = myradius)
  #
  ## SAVE FILE OF BATCH RESULTS FOR IMPORT TO SUMMARIZER
  # write.csv(api.out.table, file = '~/Downloads/api.out.table.csv')
  ########################################################################## #
  
  # ReFormat results as a single table - easier to work with
  api.out.table <- data.table::rbindlist(outlist, fill = T, idcol = 'id')
  # dim(api.out.table)  #  10 by 174 # NOTE IT IS A DATA.TABLE !
  
  # Add lat lon etc. since API does not return those and summarizer needs them
  mysitesdata <- data.frame(OBJECTID=1:n, name= paste('Facility', 1:n), lon=lon, lat=lat)
  api.out.table <- cbind(mysitesdata, api.out.table)
  
  # Rename some fields and alter some to conform to the names and formats used in the summarizer
  names(api.out.table) <- gsub(pattern = 'epaRegion', replacement = 'region', x=names(api.out.table))
  names(api.out.table) <- gsub(pattern = 'stateAbbr', replacement = 'stabbr', x=names(api.out.table))
  names(api.out.table) <- gsub(pattern = 'totalPop', replacement = 'ACSTOTPOP', x=names(api.out.table))
  names(api.out.table) <- gsub(pattern = 'distance', replacement = 'buff', x=names(api.out.table))
  api.out.table$buff <- paste(api.out.table$buff, 'miles')
  names(api.out.table) <- gsub(pattern = 'stateName', replacement = 'statename', x=names(api.out.table))
  TitleCase <- function(x) {
    s <- strsplit(tolower(x), " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  }
  TitleCase <- Vectorize(FUN = TitleCase, USE.NAMES = FALSE)
  api.out.table$statename <- TitleCase(api.out.table$statename)
  
  return(api.out.table)  
}



################################################################################
# NOTES 
############################################### #

# NUM_NPL               "0"         Not used by summarizer now
# NUM_TSDF              "5"         Not used by summarizer now
# statLayerCount        "79"        Not used by summarizer now
# statLayerZeroPopCount "1"         Not used by summarizer now
# weightLayerCount      "455"       Not used by summarizer now
# timeSeconds           "16.7685563"Not used by summarizer now
# unit                  "9035"      ****  this might refer to miles vs km??
# statlevel             "blockgroup"Not used by summarizer now
# inputAreaMiles        "3.14"      Not used by summarizer now

# ACSIPOVBAS      "539"               MISSING- povknownratio  count NOT ACTUALLY NEEDED BY SUMMARIZER
# ACSTOTHH        "203"               MISSING- hhlds  count NOT ACTUALLY NEEDED BY SUMMARIZER
# ACSEDUCBAS      "392"               MISSING-  age25up count NOT ACTUALLY NEEDED BY SUMMARIZER
# PRE1960         "40"                MISSING-  pre1960 count NOT ACTUALLY NEEDED BY SUMMARIZER
# buff            "1 miles"          ****** ** need to get from distance number and add "miles"

# these field names do work with the latest batch.summarizer code:
# names.in  <- 
# c("OBJECTID", "id", "name", "lat", "lon", "ACSTOTPOP", "ACSIPOVBAS", 
#   "ACSTOTHH", "ACSEDUCBAS", "PRE1960", "buff", "stabbr", "statename", 
#   "region", "S_P_PM25", "R_P_PM25", "N_P_PM25", "S_P_O3", "R_P_O3", 
#   "N_P_O3", "S_P_DIESEL", "R_P_DIESEL", "N_P_DIESEL", "S_P_CANCER", 
#   "R_P_CANCER", "N_P_CANCER", "S_P_RESP", "R_P_RESP", "N_P_RESP", 
#   "S_P_TRAFFIC", "R_P_TRAFFIC", "N_P_TRAFFIC", "S_P_LEAD", "R_P_LEAD", 
#   "N_P_LEAD", "S_P_NPL", "R_P_NPL", "N_P_NPL", "S_P_RMP", "R_P_RMP", 
#   "N_P_RMP", "S_P_TSDF", "R_P_TSDF", "N_P_TSDF", "S_P_NPDES", "R_P_NPDES", 
#   "N_P_NPDES", "RAW_E_PM25", "S_E_PM25_PER", "R_E_PM25_PER", "N_E_PM25_PER", 
#   "S_E_PM25", "R_E_PM25", "N_E_PM25", "RAW_E_O3", "S_E_O3_PER", 
#   "R_E_O3_PER", "N_E_O3_PER", "S_E_O3", "R_E_O3", "N_E_O3", "RAW_E_DIESEL", 
#   "S_E_DIESEL_PER", "R_E_DIESEL_PER", "N_E_DIESEL_PER", "S_E_DIESEL", 
#   "R_E_DIESEL", "N_E_DIESEL", "RAW_E_CANCER", "S_E_CANCER_PER", 
#   "R_E_CANCER_PER", "N_E_CANCER_PER", "S_E_CANCER", "R_E_CANCER", 
#   "N_E_CANCER", "RAW_E_RESP", "S_E_RESP_PER", "R_E_RESP_PER", "N_E_RESP_PER", 
#   "S_E_RESP", "R_E_RESP", "N_E_RESP", "RAW_E_TRAFFIC", "S_E_TRAFFIC_PER", 
#   "R_E_TRAFFIC_PER", "N_E_TRAFFIC_PER", "S_E_TRAFFIC", "R_E_TRAFFIC", 
#   "N_E_TRAFFIC", "RAW_E_LEAD", "S_E_LEAD_PER", "R_E_LEAD_PER", 
#   "N_E_LEAD_PER", "S_E_LEAD", "R_E_LEAD", "N_E_LEAD", "RAW_E_NPL", 
#   "S_E_NPL_PER", "R_E_NPL_PER", "N_E_NPL_PER", "S_E_NPL", "R_E_NPL", 
#   "N_E_NPL", "RAW_E_RMP", "S_E_RMP_PER", "R_E_RMP_PER", "N_E_RMP_PER", 
#   "S_E_RMP", "R_E_RMP", "N_E_RMP", "RAW_E_TSDF", "S_E_TSDF_PER", 
#   "R_E_TSDF_PER", "N_E_TSDF_PER", "S_E_TSDF", "R_E_TSDF", "N_E_TSDF", 
#   "RAW_E_NPDES", "S_E_NPDES_PER", "R_E_NPDES_PER", "N_E_NPDES_PER", 
#   "S_E_NPDES", "R_E_NPDES", "N_E_NPDES", "RAW_D_INDEX", "S_D_INDEX_PER", 
#   "R_D_INDEX_PER", "N_D_INDEX_PER", "S_D_INDEX", "R_D_INDEX", "N_D_INDEX", 
#   "RAW_D_MINOR", "S_D_MINOR_PER", "R_D_MINOR_PER", "N_D_MINOR_PER", 
#   "S_D_MINOR", "R_D_MINOR", "N_D_MINOR", "RAW_D_INCOME", "S_D_INCOME_PER", 
#   "R_D_INCOME_PER", "N_D_INCOME_PER", "S_D_INCOME", "R_D_INCOME", 
#   "N_D_INCOME", "RAW_D_LING", "S_D_LING_PER", "R_D_LING_PER", "N_D_LING_PER", 
#   "S_D_LING", "R_D_LING", "N_D_LING", "RAW_D_LESSHS", "S_D_LESSHS_PER", 
#   "R_D_LESSHS_PER", "N_D_LESSHS_PER", "S_D_LESSHS", "R_D_LESSHS", 
#   "N_D_LESSHS", "RAW_D_UNDER5", "S_D_UNDER5_PER", "R_D_UNDER5_PER", 
#   "N_D_UNDER5_PER", "S_D_UNDER5", "R_D_UNDER5", "N_D_UNDER5", "RAW_D_OVER64", 
#   "S_D_OVER64_PER", "R_D_OVER64_PER", "N_D_OVER64_PER", "S_D_OVER64", 
#   "R_D_OVER64", "N_D_OVER64")

