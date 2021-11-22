stop()


if ( 1 == 0) {

  
  
  # all below might be obsolete since I in 2019-05-28 got arcgis batch processor working and 
  # saved output file exported as csv 
  # and read those field names (and also the long format gdb names from the xml file)
  # and saved that cleaned up info in a new file called 
  # "map_batch_to_friendly_fieldnames_2018.csv"
  # and changed global.R to use that now as default? or if specified? 
  

# clean up what I exported from arcgis:
x <- read.csv('Export_Output_3points_2018-04.txt', stringsAsFactors = FALSE)
# Warning message: 
# ... incomplete final line found ... on 'Export_Output_3points_2018-04.txt' 
#
# also, header of colnames has underscore as first character in almost every column!
# Is that just how the latest ArcGIS exports the latest batch processor output???
colnames(x) <- gsub('^X_', '', unlist(strsplit(colnames(x), ',')))
colnames(x)[1] <- 'number'
write.csv(x, file = 'Export_Output_cleaned_2018-04.csv', row.names = FALSE)

stop('stopped')

#'############################################################
# COMPARE the old vs cleaned up new names:
#'############################################################

# header row of ESRI/EPA batch processor output as exported to csv from arcgis 2018-04
cols2 <- 'OBJECTID,facid,name,lat,lon,_ACSTOTPOP,_ACSIPOVBAS,_ACSTOTHH,_ACSEDUCBAS,_PRE1960,_buff,_stabbr,_statename,_region,_S_P_PM25,_R_P_PM25,_N_P_PM25,_S_P_O3,_R_P_O3,_N_P_O3,_S_P_DIESEL,_R_P_DIESEL,_N_P_DIESEL,_S_P_CANCER,_R_P_CANCER,_N_P_CANCER,_S_P_RESP,_R_P_RESP,_N_P_RESP,_S_P_TRAFFIC,_R_P_TRAFFIC,_N_P_TRAFFIC,_S_P_LEAD,_R_P_LEAD,_N_P_LEAD,_S_P_NPL,_R_P_NPL,_N_P_NPL,_S_P_RMP,_R_P_RMP,_N_P_RMP,_S_P_TSDF,_R_P_TSDF,_N_P_TSDF,_S_P_NPDES,_R_P_NPDES,_N_P_NPDES,_RAW_E_PM25,_S_E_PM25_PER,_R_E_PM25_PER,_N_E_PM25_PER,_S_E_PM25,_R_E_PM25,_N_E_PM25,_RAW_E_O3,_S_E_O3_PER,_R_E_O3_PER,_N_E_O3_PER,_S_E_O3,_R_E_O3,_N_E_O3,_RAW_E_DIESEL,_S_E_DIESEL_PER,_R_E_DIESEL_PER,_N_E_DIESEL_PER,_S_E_DIESEL,_R_E_DIESEL,_N_E_DIESEL,_RAW_E_CANCER,_S_E_CANCER_PER,_R_E_CANCER_PER,_N_E_CANCER_PER,_S_E_CANCER,_R_E_CANCER,_N_E_CANCER,_RAW_E_RESP,_S_E_RESP_PER,_R_E_RESP_PER,_N_E_RESP_PER,_S_E_RESP,_R_E_RESP,_N_E_RESP,_RAW_E_TRAFFIC,_S_E_TRAFFIC_PER,_R_E_TRAFFIC_PER,_N_E_TRAFFIC_PER,_S_E_TRAFFIC,_R_E_TRAFFIC,_N_E_TRAFFIC,_RAW_E_LEAD,_S_E_LEAD_PER,_R_E_LEAD_PER,_N_E_LEAD_PER,_S_E_LEAD,_R_E_LEAD,_N_E_LEAD,_RAW_E_NPL,_S_E_NPL_PER,_R_E_NPL_PER,_N_E_NPL_PER,_S_E_NPL,_R_E_NPL,_N_E_NPL,_RAW_E_RMP,_S_E_RMP_PER,_R_E_RMP_PER,_N_E_RMP_PER,_S_E_RMP,_R_E_RMP,_N_E_RMP,_RAW_E_TSDF,_S_E_TSDF_PER,_R_E_TSDF_PER,_N_E_TSDF_PER,_S_E_TSDF,_R_E_TSDF,_N_E_TSDF,_RAW_E_NPDES,_S_E_NPDES_PER,_R_E_NPDES_PER,_N_E_NPDES_PER,_S_E_NPDES,_R_E_NPDES,_N_E_NPDES,_RAW_D_INDEX,_S_D_INDEX_PER,_R_D_INDEX_PER,_N_D_INDEX_PER,_S_D_INDEX,_R_D_INDEX,_N_D_INDEX,_RAW_D_MINOR,_S_D_MINOR_PER,_R_D_MINOR_PER,_N_D_MINOR_PER,_S_D_MINOR,_R_D_MINOR,_N_D_MINOR,_RAW_D_INCOME,_S_D_INCOME_PER,_R_D_INCOME_PER,_N_D_INCOME_PER,_S_D_INCOME,_R_D_INCOME,_N_D_INCOME,_RAW_D_LING,_S_D_LING_PER,_R_D_LING_PER,_N_D_LING_PER,_S_D_LING,_R_D_LING,_N_D_LING,_RAW_D_LESSHS,_S_D_LESSHS_PER,_R_D_LESSHS_PER,_N_D_LESSHS_PER,_S_D_LESSHS,_R_D_LESSHS,_N_D_LESSHS,_RAW_D_UNDER5,_S_D_UNDER5_PER,_R_D_UNDER5_PER,_N_D_UNDER5_PER,_S_D_UNDER5,_R_D_UNDER5,_N_D_UNDER5,_RAW_D_OVER64,_S_D_OVER64_PER,_R_D_OVER64_PER,_N_D_OVER64_PER,_S_D_OVER64,_R_D_OVER64,_N_D_OVER64'
cols2 <- gsub('^_', '', unlist(strsplit(cols2, ',')))

# versus from example used in 2015/2016 version of batch summarizer that was working
cols1 <- ('OBJECTID,FACID,NAME,LAT,LON,totpop,buff,stabbr,statename,region,S_E_TSDF_PER,R_P_TRAFFIC,S_E_PM25_PER,R_P_CANCER,S_P_DIESEL,N_D_INDEX,RAW_E_RMP,R_E_PM25,R_D_LESSHS,R_E_DIESEL,RAW_D_OVER64,N_E_TSDF,R_E_LEAD_PER,R_E_RMP_PER,S_E_DIESEL_PER,RAW_E_RESP,R_D_INDEX_PER,RAW_D_LESSHS,N_E_TRAFFIC,S_E_NEURO_PER,N_P_NPL,S_D_INDEX,S_D_MINOR,S_D_LESSHS,S_P_RESP,N_E_PM25_PER,RAW_D_INDEX,N_E_NEURO_PER,RAW_D_UNDER5,RAW_E_LEAD,R_E_NPL_PER,S_E_RESP_PER,S_E_O3_PER,N_P_PM25,S_D_LESSHS_PER,N_E_DIESEL_PER,S_D_INCOME_PER,RAW_E_NPL,R_D_MINOR_PER,S_E_TRAFFIC,R_P_TSDF,RAW_E_TSDF,N_P_CANCER,RAW_E_NEURO,S_E_DIESEL,RAW_D_INCOME,N_P_RMP,N_E_O3_PER,S_E_O3,R_E_RESP,S_E_RESP,N_E_DIESEL,N_D_INDEX_PER,N_E_RMP_PER,RAW_D_MINOR,N_E_CANCER_PER,R_E_O3_PER,S_D_INDEX_PER,N_E_RMP,R_P_LEAD,R_E_NEURO,N_E_LEAD,S_E_RMP_PER,R_E_RMP,RAW_E_DIESEL,R_D_LING_PER,R_E_TRAFFIC,R_E_LEAD,R_D_OVER64_PER,N_P_NEURO,R_E_CANCER_PER,R_E_NPDES_PER,N_E_CANCER,N_D_MINOR_PER,S_E_TSDF,S_E_NPL,R_D_OVER64,S_D_MINOR_PER,S_P_TSDF,S_P_RMP,N_E_PM25,R_E_TSDF,S_E_RMP,RAW_D_LING,S_E_TRAFFIC_PER,S_P_PM25,S_E_LEAD,R_P_NEURO,S_D_LING,N_E_NPL,R_E_DIESEL_PER,R_D_LESSHS_PER,R_P_O3,N_E_TRAFFIC_PER,RAW_E_NPDES,N_E_NPDES,N_E_NEURO,R_P_DIESEL,N_E_RESP_PER,R_E_TSDF_PER,RAW_E_TRAFFIC,R_D_INDEX,R_P_PM25,N_D_UNDER5_PER,N_D_LESSHS_PER,R_E_NPDES,N_D_LING,S_E_PM25,N_E_NPL_PER,R_E_NEURO_PER,R_D_MINOR,N_P_TSDF,S_D_LING_PER,R_P_NPL,S_P_NPDES,S_E_NPDES_PER,N_D_UNDER5,S_E_NPL_PER,S_E_CANCER_PER,N_E_RESP,N_D_LESSHS,S_D_UNDER5,N_P_LEAD,RAW_E_CANCER,S_P_TRAFFIC,N_E_NPDES_PER,R_E_TRAFFIC_PER,N_P_NPDES,RAW_E_O3,N_P_O3,R_E_O3,N_E_O3,N_E_TSDF_PER,R_E_RESP_PER,S_D_OVER64,N_D_INCOME,R_E_NPL,R_D_UNDER5_PER,R_P_RESP,R_P_NPDES,S_P_O3,N_P_DIESEL,N_D_OVER64_PER,R_P_RMP,N_P_TRAFFIC,N_E_LEAD_PER,S_E_NPDES,S_D_OVER64_PER,S_P_NPL,N_D_MINOR,RAW_E_PM25,N_D_LING_PER,S_D_INCOME,S_P_NEURO,N_P_RESP,N_D_OVER64,S_D_UNDER5_PER,R_D_LING,R_E_CANCER,S_E_CANCER,S_P_CANCER,N_D_INCOME_PER,R_D_INCOME_PER,S_E_NEURO,R_D_INCOME,R_E_PM25_PER,R_D_UNDER5,S_E_LEAD_PER,S_P_LEAD')
cols1 <- unlist(strsplit(cols1, ','))

# 
# > overlaps(cols1, cols2)
# in.a in.b in.a.only in.b.only in.one.only overlap union
# unique  179  173        15         9          24     164   188
# total   179  173        15         9          24     328   352
# > 

setdiff2(cols1, cols2)
setdiff(cols1, cols2)
setdiff(cols2, cols1)

# UNIQUE TO OLD OR NEW COLNAMES
#
# > setdiff2(cols1, cols2)
# [1] "FACID"         "NAME"          "LAT"           "LON"           "totpop"       
# [6] "S_E_NEURO_PER" "N_E_NEURO_PER" "RAW_E_NEURO"   "R_E_NEURO"     "N_P_NEURO"    
# [11] "R_P_NEURO"     "N_E_NEURO"     "R_E_NEURO_PER" "S_P_NEURO"     "S_E_NEURO"    
# [16] "facid"         "name"          "lat"           "lon"           "ACSTOTPOP"    
# [21] "ACSIPOVBAS"    "ACSTOTHH"      "ACSEDUCBAS"    "PRE1960"      

# ONLY IN OLD ONE
#
# > setdiff(cols1, cols2)
# [1] "FACID"         "NAME"          "LAT"           "LON"           "totpop"       
# [6] "S_E_NEURO_PER" "N_E_NEURO_PER" "RAW_E_NEURO"   "R_E_NEURO"     "N_P_NEURO"    
# [11] "R_P_NEURO"     "N_E_NEURO"     "R_E_NEURO_PER" "S_P_NEURO"     "S_E_NEURO"    

# ONLY IN NEW ONE
#
# > setdiff(cols2, cols1)
# [1] "facid"      "name"       "lat"        "lon"        "ACSTOTPOP"  "ACSIPOVBAS"
# [7] "ACSTOTHH"   "ACSEDUCBAS" "PRE1960"   
# > 

namemap <- read.csv("map_batch_to_friendly_fieldnames_2016.csv", stringsAsFactors = FALSE, as.is = TRUE)
# > names(namemap)
# [1] "oldnames"     "newnames"     "gdbfieldname" "vartype"      "varcategory" 
# [6] "longname"     "example"      "order0"       "order1"       "order2"      
# > 
#   



}