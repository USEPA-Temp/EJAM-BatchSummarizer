######################################################################################## #
# DEMOGRAPHIC US TOTALS/US OVERALL PERCENTS --  ####
# *** MUST BE UPDATED WHEN NEW ACS DATA IS USED !
######################################################################################## #


# ACS 2016-2020 is to be released Dec 2021.

# The 2021 version of EJSCREEN actually may be released very late 2021 or early 2022 ?
#  and will likely use ACS 2015-2019 data (released December 2020 by Census) 

# The 2020 version of EJSCREEN actually released in 2021 avail as ejscreen::bg20
#  uses ACS 2014-2018 (released December 2019 by Census) 

# see the help file for bg20 now ************* and

# see  ACS_US_TOTALS_2014-2018.R
# see NOTES-on-reading-EJSCREEN-2019-from-gdb-on-FTP-site.R
# see updating EJSCREEN tech doc 2019.R
# see notes-on-how-names-e-RData-and-names-ejvars-updated-to-2016.R

# ejscreen::bg20 is the latest in EJSCREEN as of 9/2021

message("*** Note: MUST UPDATE ACS DATA IN CODE, WHEN SWITCHING TO NEW ACS DATASET")
# browseURL('https://www.census.gov/programs-surveys/acs/news/data-releases.html')

# US & REGION & STATE **MEDIANS** are available from ejscreen package:
# library(ejscreen)   # also, names.e etc are from that package
# round(  ejscreen::lookupUSA[ejscreen::lookupUSA$PCTILE == '50', ejscreen::names.d]  ,3)
# the bg19 data (ACS 2003-2017) showed this:
#    VSI.eo VSI.svi6 pctmin pctlowinc pctlths pctlingiso pctunder5 pctover64
# 51  0.299    0.159  0.298     0.294   0.094      0.012     0.057     0.134
# > round(  ejscreen::lookupUSA[ejscreen::lookupUSA$PCTILE == '50', ejscreen::names.e]  ,1)
#     pm   o3 cancer resp dpm pctpre1960 traffic.score proximity.npl proximity.rmp proximity.tsdf proximity.npdes # for 2019 version of EJSCREEN, acs 2013-2017
# 51 8.3 43.8   31.4  0.4 0.4        0.2         214.1           0.1           0.3            0.4               0


# US & REGION & STATE **AVERAGES** are available from ejscreen package:
# 
# > round(ejscreen::lookupUSA[ejscreen::lookupUSA$PCTILE == 'mean', ejscreen::names.d]  ,3)
# VSI.eo VSI.svi6 pctmin pctlowinc pctlths pctlingiso pctunder5 pctover64
# 102  0.356    0.182  0.385     0.328   0.127      0.045     0.062     0.149
# > round(ejscreen::lookupUSA[ejscreen::lookupUSA$PCTILE == 'mean', ejscreen::names.e]  ,1)
# pm o3 cancer resp dpm pctpre1960 traffic.score proximity.npl proximity.rmp proximity.tsdf proximity.npdes
# 102 8.3 43   31.9  0.4 0.5        0.3         753.7           0.1           0.7              4            13.6
# > 

# for one state:
# 
# >  ejscreen::lookupStates[ ejscreen::lookupStates$PCTILE == 'mean' &  ejscreen::lookupStates$REGION == 'NY', 'pm']
# [1] 7.6138
# >  ejscreen::lookupStates[ ejscreen::lookupStates$PCTILE == 'mean' &  ejscreen::lookupStates$REGION == 'NY', ejscreen::names.e]
# pm       o3   cancer      resp     dpm pctpre1960 traffic.score proximity.npl proximity.rmp
# 3570 7.6138 43.97403 32.06106 0.4904412 1.05207  0.5555662      1684.205     0.2245075     0.5028817
# proximity.tsdf proximity.npdes
# 3570       41.54222        1.222635


# THESE nationwide percents (us.avg) 
# also ARE IN THE OUTPUT OF THE BATCH TOOL:
#
# us.avg.VSI.eo  # name not used in package
# us.avg.pctlowinc
# us.avg.pctmin
# us.avg.pctlingiso
# us.avg.pctlths
# us.avg.pctunder5
# us.avg.pctover64
# us.avg.pctpre1960

# US COUNTS
#
# But the summarizer also needs to know COUNTS
# that are available from the ejscreen package 
# but need to be renamed for this package:
# 
# require(ejscreen); cbind(ustotals(bg20)) # used to say bg19
#
#  #junk <- ustotals(bg20); cbind(us.count.or.percent = junk[junk > 0]);rm(junk)
#   # ejscreen::bg19 has been replaced by ejscreen::bg20
#               us.count.or.percent  for 2019v of EJSCREEN was: (ACS 2013-2017)
# POP.US        324473370          
# LOWINC.US     105031889          
# MINS.US       127169165          
# UNDER5.US     20025714           
# OVER64.US     48362010           
# LTHS.US       28042734           
# LINGISO.US    6143833            

# PRE1960.US    38811968           

# PCTLOWINC.US  0.3318694          
# PCTMIN.US     0.3919248          
# PCTUNDER5.US  0.06171759         
# PCTOVER64.US  0.1490477          
# PCTLTHS.US    0.1282467          
# PCTLINGISO.US 0.05117791         

# PCTPRE1960.US 0.28338 

############################# #
# converting format in ejscreen pkg to format in batch.summarizer pkg:
############################# #

require(ejscreen)
us <- ustotals(ejscreen::bg20)  # used to say bg19
popus <- us$POP.US
pop.US <- popus
lowinc.US <- us$LOWINC.US
# povknownratio.US <- us$POVKNOWNRATIO # count from ACS ********** ADD TO EJSCREEN PKG
pctlowinc.US <- us$PCTLOWINC.US # calc
 
mins.US <- us$MINS.US # count from ACS or calc from nhwa.US
nhwa.US <- pop.US - mins.US # count from ACS or calc
pctmin.US <- us$PCTMIN.US # calc
VSI.eo.US <- (pctmin.US + pctlowinc.US)/2 # calc ## **** should verify formula works at US level like this 

lths.US <- us$LTHS.US # count from ACS
# age25up.US <- us$AGE25UP.US  # count from ACS ********** ADD TO EJSCREEN PKG
pctlths.US <- us$PCTLTHS.US # calc
 
lingiso.US <- us$LINGISO.US # count from ACS
# hhlds.US <- us$HHLDS.US  # count from ACS ********** ADD TO EJSCREEN PKG
pctlingiso.US <- us$PCTLINGISO.US # count from ACS
 
under5.US <-  # count from ACS
# over64.US  # count from ACS
#   pctunder5.US # calc
#   pctover64.US
#   
# pre1960.US # count from ACS
#   pctpre1960.US # calc
# 
# # then most are compiled here:
#   us.percents  # calc
#   us.percents.2016  # calc
#   us.counts.names  # calc
#   us.counts  # calc








##########      names.d.batch





# Names as used in the batch.summarizer package:

# us.counts
#    VSI.eo  pctlowinc     pctmin    pctlths pctlingiso  pctunder5  pctover64 
# 112721531  105773407  116947592   28587748    5275272   19973711   43177961 

# us.percents
#    VSI.eo  pctlowinc     pctmin    pctlths pctlingiso  pctunder5  pctover64 
# 35.886338  34.540918  37.231759  13.674676   4.539388   6.358886  13.746255 

# pop.US <-  # count from ACS
# popus  # count from ACS or same as pop.US
# 
# lowinc.US  # count from ACS
# povknownratio.US  # count from ACS
#   pctlowinc.US # calc
# 
# nhwa.US  # count from ACS
#   mins.US # calc from nhwa.US
#   pctmin.US # calc
#   VSI.eo.US # calc
#   
# lths.US  # count from ACS
# age25up.US  # count from ACS
#   pctlths.US # calc
# 
# lingiso.US  # count from ACS
# hhlds.US  # count from ACS
#   pctlingiso.US  # count from ACS
# 
# under5.US  # count from ACS
# over64.US  # count from ACS
#   pctunder5.US # calc
#   pctover64.US
#   
# pre1960.US # count from ACS
#   pctpre1960.US # calc
# 
# # then most are compiled here:
#   us.percents  # calc
#   us.percents.2016  # calc
#   us.counts.names  # calc
#   us.counts  # calc


#################################### #

# 2015-2018



popus <- 321004407  # from 2013-2017  
# browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B01001/0100000US')
pop.US <- 0
under5.US <- 0 + 0
pctunder5.US <- under5.US/pop.US
over64.US <- (0) + (0)

pctover64.US <- over64.US/pop.US

nhwa.US <-   197159492
#etc
#etc
#etc

#etc
#etc
#etc



# older ###################################################

# popus <- 309138711 # 309,138,711 is from
# http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/12_5YR/B01001/0100000US
popus <- 314107084  # from 2010-2014 ??
# http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B01001/0100000US
# browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B01001/0100000US')
pop.US <- 314107084
under5.US <- 10205881 + 9767830
pctunder5.US <- under5.US/pop.US
over64.US <- (2915262 + 3616479 + 4666294 + 3328014 + 2362325 + 
                1936823) + (3223178 + 4077987 + 5494784 + 4231547 + 3442927 + 
                              3882341)

pctover64.US <- over64.US/pop.US
# 65 and 66 years 2,915,262 +/-9,504 67 to 69 years 3,616,479
# +/-9,539 70 to 74 years 4,666,294 +/-9,753 75 to 79 years
# 3,328,014 +/-9,188 80 to 84 years 2,362,325 +/-7,350 85
# years and over 1,936,823 65 and 66 years 3,223,178 +/-9,504
# 67 to 69 years 4,077,987 +/-10,262 70 to 74 years 5,494,784
# +/-11,596 75 to 79 years 4,231,547 +/-8,771 80 to 84 years
# 3,442,927 +/-9,460 85 years and over 3,882,341

# browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B03002/0100000US')
nhwa.US <- 197159492
mins.US <- pop.US - nhwa.US
pctmin.US <- mins.US/pop.US
# browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B15002/0100000US')
lths.US <- 28587748
age25up.US <- 209056129
pctlths.US <- lths.US/age25up.US
# browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B16002/0100000US')
lingiso.US <- 5275272
hhlds.US <- 116211092
pctlingiso.US <- lingiso.US/hhlds.US
# browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/C17002/0100000US')
lowinc.US <- 105773407
povknownratio.US <- 306226394
pctlowinc.US <- lowinc.US/povknownratio.US
# browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B25034/0100000US')
pre1960.US <- 14374462 + 7119373 + 17665365
pctpre1960.US <- pre1960.US/132741033

VSI.eo.US <- (pctmin.US + pctlowinc.US)/2

# THESE SHOULD BE AVAILABLE IN THE BATCH ANALYSIS OUTPUTS?

# this seems wrong: it is counts not percents??? 
us.percents.2016 <- 100 * sapply(paste(names.d.batch, ".US", sep = ""), get)

# Calculated here from Census website tables: >
# cbind(us.percents.2016) us.percents.2016 VSI.eo.US
# 35.886338 pctlowinc.US 34.540918 pctmin.US 37.231759
# pctlths.US 13.674676 pctlingiso.US 4.539388 pctunder5.US
# 6.358886 pctover64.US 13.746255 those match the numbers
# from LOOKUP TABLES IN GDB: VULEOPCT 0.358863383 LOWINCPCT
# 0.345409178 MINORPCT 0.372317588 LESSHSPCT 0.136746759
# LINGISOPCT 0.045393877 UNDER5PCT 0.063588859 OVER64PCT
# 0.137462551 VULSVI6PCT 0.183486469

us.percents.2015 <- 100 * c(0.35015068301904, 0.337245110039133, 
                            0.363056255998945, 0.147948736834136, 0.0514480674233393, 
                            0.0651419032409694, 0.131563727067491)

us.percents <- us.percents.2016

# The actual exact counts are obtained above, like lowinc.US,
# but that is not quite the same as pctlowinc.US * pop.US,
# because denominator was not pop!  Early versions of
# summarizer used the approximation of pct*pop, but might
# want to use actual count?
us.counts.names <- paste(gsub("pct", "", names.d.batch), ".US", sep = "")
us.counts.names <- gsub("min.US", "mins.US", us.counts.names)
us.counts <- sapply(us.counts.names, get)
# for count of VSI.eo.US, do we want true count or pct * pop?
# or is VSI.eo.US count even used here at all?
# us.counts['VSI.eo.US'] <- mins.US + lowinc.US # ???
us.counts["VSI.eo.US"] <- VSI.eo.US * pop.US  # ???
# us.counts <- popus * us.percents # was the prior version's
# approach rename them because later code refers to them
# using the simple ejscreen::names.d or names.d.batch, not with .US on
# the names
names(us.percents) <- names.d.batch
names(us.counts) <- names.d.batch

# usavg.e <- c(x, x, x, x, x, x, x, x, x, x, xxxxxx) # should
# be in table??  what is in lookup tables in gdb 2016: PM25
# 9.318031237 OZONE 47.40943204 DSLPM 0.936750327 CANCER
# 40.03084161 RESP 1.838053631 PRE1960PCT 0.295004484 PTRAF
# 592.7652763 PNPL 0.129302536 PRMP 0.428815643 PTSDF
# 0.071994194 PWDIS 0.307163434
############################################################### #
