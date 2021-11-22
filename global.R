rm(list = ls())

testing <- TRUE # ********** while debugging: testing <- FALSE
if (testing) {cat("Starting global\n")}

# **** MUST UPDATE ACS-related CODE BELOW YEARLY FOR NEW ACS DATASET!**

##'##########################################################################
# # *** To run this App locally and specify a local large custom file, 
# # uncomment one of the following lines: 
# mydemofile <- 'path/hugebatchoutput.csv' 
# # for example mydemofile <- 'ejtest2.csv'
#'###########################################################################

#'###########################################################################
# DATA ANALYSIS FUNCTIONS
#'###########################################################################

requireFewerPackages <- TRUE

# Get packages and source code to get functions needed ***
#
# Building them into this package or sourcing them in this package
# *** lets web app require less time and space than relying on the packages 
# ejscreen, ejanalysis, analyze.stuff, proxistat
# # available at
# # http://ejanalysis.github.io 
# *** BUT harder to keep in sync with latest updates in those packages.
# # ejscreen pkg provides popupunits, for example, and proxistat has counties

if (!requireFewerPackages) {
  require(analyze.stuff)
  analyze.stuff::required.packages()
  require(ejscreen); require(ejanalysis)  
} else {
  ### do I really want to source these instead of having them  be available by loading this batch.summarizer package which exports those functions?
  ### If this pkg is not on CRAN and only on github, it makes it slightly more complicated if expect the package to be installed, but still possible.
  # anyway, for now, these are sourced here:
  source("pct.above.R")  # returns percent of rows (or wtd people) that have value above specified cutoff (or mean as default), for each column of data.frame
  source("count.above.R")  # returns count of how many rows (or wtd people) have value above specified cutoff (or mean as default), for each column of data.frame
  source("cols.above.count.R")  # returns count of how many cols (e.g. how many variables or indicators) have value above specified cutoff (or mean as default), for each row of data.frame
  source("flagged.R")  # creates flag that is TRUE if at least one of 12 indicators is > cutoff (EJ index >50th or 80th or 95th%ile, for example), for each of many rows of a data.frame
  source("rowMaxs.R")  # returns Max of each row
  source("rowMins.R")  # returns Min of each row
  source("colMaxs.R")  # returns Max of each col
  source("colMins.R")  # returns Min of each col
  source("wtd.colMeans.R")  # returns wtd.mean of each col
  source("lead.zeroes.R")  # add leading zeroes as needed to fix FIPS that had been stored as numeric
  
  source("change.fieldnames.R")  # updated function that helps change or resort fieldnames using a map file mapping old to new names
  # **** Might shift to using the version from  the packages
  # analyze.stuff::change.fieldnames	Change some or all of the colnames of a data.frame or matrix via a 1-1 map
  # ejscreen::change.fieldnames.ejscreen.csv		Change colnames of csv file on EJSCREEN FTP site to nicer colnames
  
  # source('pct.below.R') # returns percent of rows (or wtd people)
  # that have value below specified cutoff (or mean as default),
  # for each column of data.frame
  # source('pop.ecdf.R') # plot pop-wtd ecdf(s) for one
  # demographic group vs others, etc., for comparing conditions
  # between groups across many Census areal units (e.g. tracts)
}
#'###########################################################################

# COUNTY DATA   # make sure this is updated to latest ... counties change 

## *** could get this from proxistat package, 
##   or keep in this package and then could just use lazy loading
# data(counties, package='proxistat')
data(counties, package = 'batch.summarizer')
counties$nonwhite <- round(100 - counties$white, 1)



#'###########################################################################

require(batch.summarizer)
source("batch.summarize.R")  # will be part of package but had trouble with export tag bug
# FUNCTIONS only in this batch.summarizer package,
# now loaded as part of this package rather than sourced:
# source('batch.read.R') # now as package
# source('batch.clean.R') # now as package
# source("maphelpers.R")  # if we want percent choropleths of county data
# source("wilcoxon.pvalues.r")  # for stat significance testing - from air
# source("plotlyGraphWidget.R") ## for interactive plots/charts/graphs
# # see https://plot.ly/r/getting-started/
#
# load gomap.js ??

#'###########################################################################
#
# # OTHER PACKAGES USED:
library(Hmisc)  # various useful functions for data analysis
library(plotrix)  # for better weighted.hist than ggplot2 can provide.
library(ggplot2)  # for geom_histogram() that allows weights to be used. plotrix package also does wtd hist, but better.
library(shiny)  # http://shiny.rstudio.com
# library(data.table) # used by analyze.stuff or newer ver of wtd.colMeans()?
# library(dplyr) # might not need this

# MAPS PACKAGES TO OBTAIN THE leaflet PACKAGE (IF VERSION NOT ON CRAN?):
#
library(maps)  # for static maps; choropleth of counties, etc.
library(mapproj)
library(leaflet)  # for interactive maps
# # if need to get from github...
# library(devtools); devtools::install_github('rstudio/leaflet')
# library(leafletR) # for interactive maps? (different than leaflet)
#
# For leaflet maps, might need to define these here until
# latest version of leaflet package is on cran:
require(htmlwidgets)
leafletOutput = function(outputId, width = "100%", height = 400) {
  htmlwidgets::shinyWidgetOutput(outputId, "leaflet", width, 
                                 height, "leaflet")  #, error.label = NULL)
}
renderLeaflet = function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) 
    expr = substitute(expr)  # force quoted
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env, 
                                 quoted = TRUE)
}
########################################################################### #


# *** COULD SPECIFY LARGE FILE THAT IS OUTPUT OF BATCH RUN
# AND INPUT TO THIS SUMMARIZER here if huge
# file too large for Shiny's file upload limit - 
# For large file, edit code & run a local copy of this summarizer.


#'###################################################################################
# 
# *** SPECIFY YEAR/VERSION/VINTAGE 
# OF EJSCREEN & ACS DATA & BATCH TOOL (2019, 2020, etc.)
# and names of columns/fields/variables analyzed
# 
#'###################################################################################

suppressWarnings(rm(mydemofile))  # just in case testing and had already specified this
# Default example of export of batch results, for use in
# testing/demonstrating summarizer:
if (!exists("mydemofile")) {
  mydemofile <- 'default_example_2020-03_EJSCREEN_BATCH_Export_Output.csv' # "Export_Output_Example2.csv" # 'Export_Output_2019-08.txt' # 
}  
if (!exists("mydemofile.pop")) {
  mydemofile.pop <- 'default_example_2020-03_EJSCREEN_BATCH_Export_Output.csv' # "Export_Output_Example2.pop.csv" # 'Export_Output_2019-08.txt' #
}  # not realistic - just smaller pop numbers!!

# Default File that has default input and output and friendly
# fieldnames & var type & var category:
mynamesfile.default <- 'map_batch_to_friendly_fieldnames_2020.csv' 
# mynamesfile.default   <- 'map_batch_to_friendly_fieldnames_2018.csv'
# mynamesfile.default <- 'map_batch_to_friendly_fieldnames_2016.csv'

mywtsname <- "pop"  
# used for weighted means to get 
# stats on the average person in all these zones (e.g. avg person nearby any site)

#'########################################################
#
# *** These are all in package called ejscreen now and maybe can transition to that:
# so that ejscreen::names.d  might not be needed?
names.d.batch <- c("VSI.eo", "pctlowinc", "pctmin", "pctlths", 
                   "pctlingiso", "pctunder5", "pctover64")


source('ACS_US_TOTALS_2014-2018.R')


names.d.friendly <- c("Demog.Ind.", "% Low-inc.", "% Minority", 
                      "% <High School", "% Linguistic Isol.", "% < age 5", "% > age 64")
# *** should I add these:?  'ACSIPOVBAS', 'ACSTOTHH',
# 'ACSEDUCBAS', 'PRE1960'

# so that ejscreen::names.e  might not be needed?
names.e.batch <- c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", 
                   "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", 
                   "proximity.npdes")
#'neuro', 
names.e.friendly <- c("PM2.5", "Ozone", "Cancer risk", "Respiratory", 
                      "Diesel PM", "% built pre-1960", "Traffic", "NPL proximity", 
                      "RMP proximity", "TSDF proximity", "NPDES proximity")
#'Neuro.',
# so that ejscreen::names.ej  might not be needed?
names.ej.batch <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", 
                    "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
                    "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
                    "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", 
                    "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo")
#'EJ.DISPARITY.neuro.eo', 
names.ej.friendly <- paste("EJ:", names.e.friendly)

names.all <- c(names.d.batch, names.e.batch, names.ej.batch)
names.all.friendly <- c(names.d.friendly, names.e.friendly, names.ej.friendly)

# ***************
# *** neuro-related fields dropped in 2016 may need to be removed from 
# these in batch summarizer global.R:
# names.e.batch names.e.friendly names.ej.batch names.ej.friendly
# ***************
#'########################################################
# WHICH FIELDS TO COMPARE TO THRESHOLDS

threshgroup.default <- list(
  "EJ US pctiles", "EJ Region pctiles", "EJ State pctiles"
)
threshold.default <- list(50, 50, 50)  
# a default for cutoff in at/above threshold stat summarizing EJ US percentiles
#
# Allows user to specify user-specified # of groups of
# user-specified fields to compare to user-specified
# thresholds.  Initially, just the 3 thresholds can be
# altered, not which fields are compared or how many groups
# or what the groups are called.
# 
# NOTE: server.R creates threshold names at the moment
# threshnames.default <- list( grep('^pctile.EJ.DISPARITY.',
# colnames(fulltabler()), value=TRUE) ,
# grep('regionpctile.EJ.DISPARITY.', colnames(fulltabler()),
# value=TRUE) , grep('statepctile.EJ.DISPARITY.',
# colnames(fulltabler()), value=TRUE) )

#'########################################################
# Specify units that go with each environmental indicator.
# This is also available via 
#   data(popupunits, package='ejscreen') 
# except that package has to be obtained from github.

popupunits <- structure(
  list(evar = c("pm", "o3", "cancer", "resp", 
                "dpm", "pctpre1960", "traffic.score", "proximity.npl", "proximity.rmp", 
                "proximity.tsdf", "proximity.npdes"), 
       units = c("ug/m3", 
                 "ppb", "lifetime risk per million", "", "ug/m3", "= fraction pre-1960", 
                 "daily vehicles/meters distance", "sites/km distance", "facilities/km distance", 
                 "facilities/km distance", 
                 "wtd facilities/km distance")), 
  .Names = c("evar", 
             "units"), row.names = c(NA, -12L), class = "data.frame")
#'neuro', # neuro was dropped from EJSCREEN around 2016
#'############################


#'########################'########################'#######################

#'############################
# Useful open map layers
#'############################
# layer.admin <- 'OpenMapSurfer.AdminBounds' # neighborhoods,
# counties, etc.  layer.houses <- 'HERE.hybridDay'
# layer.street1 <- 'Esri.WorldStreetMap' layer.street2 <-
# 'OpenStreetMap.HOT' layer.street3 <- 'Stamen.TonerHybrid'
# layer.topo <- 'Esri.WorldTopoMap' layer.delorme <-
# 'Esri.DeLorme' layer.natgeo <- 'Esri.NatGeoWorldMap'
# layer.sat1 <- 'Esri.WorldImagery' layer.sat2 <-
# 'MapQuestOpen.Aerial' layer.esrigray <-
# 'Esri.WorldGrayCanvas'

mapserver1 = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer"
mapserver2 = "http://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer"

# var MapQuestOpen_Aerial =
# L.tileLayer('http://oatile{s}.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg',
# { attribution: 'Tiles Courtesy of <a
# href='http://www.mapquest.com/'>MapQuest</a> &mdash;
# Portions Courtesy NASA/JPL-Caltech and U.S. Depart. of
# Agriculture, Farm Service Agency', subdomains: '1234' });

#'########################'########################'#######################

#'############################
# MISC DEFAULT VALUES
#'############################

default.tab <- "Details"  # for some reason this has to be used to update info so barplot etc works on first click
default.tab.start <- "Upload"

#'############################
# WHICH QUANTILES TO USE IN SUMMARY STATS
#'############################
# Defaults for quantiles summary stats, with character string
# version used in ui to present options in checkboxes (probs
# of 0, 0.50, 1 are redundant since min, median, max are
# already separately shown)
probs.default.choices <- c("0", "0.25", "0.50", "0.75", "0.80", 
                           "0.90", "0.95", "0.99", "1.00")
probs.default <- c(0.25, 0.75, 0.95)
# as.numeric(probs.default.choices) # to have it default to
# all of these summary stats of the distributions across
# sites and people c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)

# Defaults for which of the predefined functions to use for
# summary stats (logical vector or vector of names?)
colfun.picked.default = "all"
rowfun.picked.default = "all"

# Default for all functions so can get stats even if one site
# (or one indicator at one site) has no data
na.rm = TRUE

#'############################
# MISC DEFAULTS
#'############################
pixels.per.char <- 10  #????
max.allowed = 30  # max length of site name we want displayed before wrapping in sites table
# define scaling for text on barplot
bar.cex <- 1.1
# size of marker for sites on map:
circle.marker.radius <- 6
meters.per.mile = 1609.344  # proxistat::convert(1, from = 'miles', towhat = 'meters') 

######################################################## #






############################################################### #
############################################################### #

# NOTES ON FIELD NAMES OF IMPORTED DATA - 
# batchfields2019 etc are not actually used...
# csv files are used to specify mapping of field names (colnames) 
# from batch output to what is used in R here
#
# removed batchfields2019 etc. from here

#' ####################################################################################
# FIELDNAMES USED BY CODE TO REFER TO SPECIFIC TYPES OF
# FIELDS THIS COULD BE REPLACED BY JUST REQUIRING THE
# RELEVANT GROUPS OF NAMES BE PROVIDED IN A PARTICULAR SORT
# ORDER IN THE newnames COLUMN OF THE LOOKUP TABLE (CSV READ
# IN AS mynamesfile.default or user-uploaded version of that)
# 
# AS WRITTEN CURRENTLY, THE non-friendly versions of the
# NAMES BELOW *MUST* MATCH THOSE IN THE newnames COLUMN OF
# THE LOOKUP TABLE and the friendly versions are read from
# the csv file for table outputs but from the defaults below
# for use in the graphics (hist/bar) labeling I believe.
#' ####################################################################################

############################################################### #
############################################################### #

####### MORE NOTES 

# Only some of these maps between versions of field names
# could be handled using the ejscreen package now, via
# data('ejscreenformulas'), with e.g., gdbfieldname 'CANCER'
# Rfieldname 'cancer' ***** acsfieldname NA type
# 'Environmental' glossaryfieldname 'Air toxics cancer risk'
# ***** e.g., change.fieldnames(ejscreenformulas$) BUT, Many
# fields used just for batch processing outputs are not in
# ejscreenformulas at all, such as oldnames newnames
# gdbfieldname 1 OBJECTID OBJECTID OBJECTID ...  12
# RAW_D_INDEX VSI.eo VULEOPCT 13 N_D_INDEX us.avg.VSI.eo <NA>
# 14 R_D_INDEX region.avg.VSI.eo <NA> 15 S_D_INDEX
# state.avg.VSI.eo <NA> 16 N_D_INDEX_PER pctile.VSI.eo
# P_VULEOPCT 17 R_D_INDEX_PER region.pctile.VSI.eo <NA> 18
# S_D_INDEX_PER state.pctile.VSI.eo <NA> 19 RAW_D_INCOME
# pctlowinc LOWINCPCT ...

# Also note not all the fields in lookup.fieldnames() are in
# names.all below.

# *** after reading defaults or user versions from the
# default or specified map names file - Read into the
# reactive lookup.fieldnames() BUT note these friendly names
# aren't same as longname in default lookup.fieldnames() and
# also they lack full unique name like 'Ozone US percentile'
# or 'Cancer risk state average' The ones below are short and
# friendly for graph labels.

# *** if using names from data(names.evars,
# package='ejscreen') etc., >
# ejscreenformulas$glossaryfieldname[match(names.d ,
# ejscreenformulas$Rfieldname)] [1] 'Demographic Index (based
# on 2 factors, % low-income and % minority)' [2]
# 'Supplementary Demographic Index (based on 6 factors)' [3]
# '% minority' [4] '% low-income' [5] '% less than high
# school' [6] '% of households (interpreted as individuals)
# in linguistic isolation' [7] '% under age 5' [8] '% over
# age 64' > ejscreenformulas$glossaryfieldname[match(names.e,
# ejscreenformulas$Rfieldname)] [1] 'PM2.5 level in air' [2]
# 'Ozone level in air' [3] 'Air toxics cancer risk' [4] 'Air
# toxics neurological hazard index' [5] 'Air toxics
# respiratory hazard index' [6] 'Diesel particulate matter
# level in air' [7] '% pre-1960 housing (lead paint
# indicator)' [8] 'Traffic proximity and volume' [9]
# 'Proximity to National Priorities List (NPL) sites' [10]
# 'Proximity to Risk Management Plan (RMP) facilities' [11]
# 'Proximity to Treatment Storage and Disposal (TSDF)
# facilities' [12] 'Proximity to major direct dischargers to
# water' > ejscreenformulas$glossaryfieldname[match(names.ej,
# ejscreenformulas$Rfieldname)] [1] 'EJ Index for PM2.5 level
# in air' [2] 'EJ Index for Ozone level in air' [3] 'EJ Index
# for Air toxics cancer risk' [4] 'EJ Index for Air toxics
# neurological hazard index' [5] 'EJ Index for Air toxics
# respiratory hazard index' [6] 'EJ Index for Diesel
# particulate matter level in air' [7] 'EJ Index for %
# pre-1960 housing (lead paint indicator)' [8] 'EJ Index for
# Traffic proximity and volume' [9] 'EJ Index for Proximity
# to National Priorities List (NPL) sites' [10] 'EJ Index for
# Proximity to Risk Management Plan (RMP) facilities' [11]
# 'EJ Index for Proximity to Treatment Storage and Disposal
# (TSDF) facilities' [12] 'EJ Index for Proximity to major
# direct dischargers to water'

# Roughly the lists of fieldnames below, names.e.batch ,
# names.d.batch , and names.ej.batch , are already available
# as data(names.evars, names.dvars, names.ejvars, package =
# 'ejscreen') BUT the names.d in ejscreen pkg has had
# VSI.svi6, which is removed here and for 2016 anyway and
# sort order differs here

# BUT THESE GLOSSARY NAMES WOULD BE TOO LONG FOR GRAPHICS:
# names.e.friendly <-
# ejscreenformulas$glossaryfieldname[match(names.e.batch ,
# ejscreenformulas$Rfieldname)] names.d.friendly <-
# ejscreenformulas$glossaryfieldname[match(names.d.batch ,
# ejscreenformulas$Rfieldname)] names.ej.friendly <-
# ejscreenformulas$glossaryfieldname[match(names.ej.batch ,
# ejscreenformulas$Rfieldname)]


############################################################### #
############################################################### #


##################### #
# ### TESTING / DEBUGGING ###
##################### #

if (testing) {
  options(shiny.reactlog = TRUE)  # If TRUE, enable logging of reactive events, which can be 
  # viewed later with the showReactLog function. This incurs a
  # substantial performance penalty and should not be used in
  # production.
  options(shiny.trace = TRUE)
  options(shiny.stacktraceoffset = TRUE)  # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = TRUE)  # instead of shorter prettier view
  options(shiny.error = browser)  # or options(shiny.error = recover) gives debugger prompt on err
  options(error = recover)  # The functions dump.frames and recover provide alternatives that allow post-mortem debugging. 
  options(verbose = TRUE)
  options(shiny.deprecation.messages = TRUE)
} else {
  options(shiny.reactlog = FALSE)
  options(shiny.trace = FALSE)
  options(shiny.stacktraceoffset = FALSE)  # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = FALSE)  # instead of shorter prettier view
  options(shiny.error = NULL)  # or options(shiny.error = recover) gives debugger prompt on err
  options(error = NULL)  # resets behavior to normal when error hit
  options(verbose = FALSE)
  options(shiny.deprecation.messages = FALSE)
}
on.exit({
  options(shiny.reactlog = FALSE)
  options(shiny.trace = FALSE)
  options(shiny.stacktraceoffset = FALSE)  # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = FALSE)  # instead of shorter prettier view
  options(shiny.error = NULL)  # or options(shiny.error = recover) gives debugger prompt on err
  options(error = NULL)  # resets behavior to normal when error hit
  options(verbose = FALSE)
  options(shiny.deprecation.messages = FALSE)
})
