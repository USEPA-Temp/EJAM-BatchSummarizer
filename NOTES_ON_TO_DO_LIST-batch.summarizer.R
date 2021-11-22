# TO DO NOTES
#  

# Eventually should add ability to 
#
# use rmarkdown with parameters to create pdf report summary with graphics etc.
# see https://bookdown.org/yihui/rmarkdown/params-knit.html 
# something like e.g., 
# render_report = function(batchresults) {
#   rmarkdown::render(
#     "BatchReport.Rmd", 
#     params = list(
#       bx = batchresults
#     ),
#     output_file = paste0("BatchReport-", bx$title, "-", bx$radius, ".pdf")
#   )
# }
#

# Confirm Which functions or packages it should depend on and where to put those.  
# see requireFewerPackages and
# clarify if this package will rely on the complete ejscreen and ejanalysis packages
# or if it will be kept smaller by including copies of just the functions or data needed,
# like a few functions and names.e.batch instead of ejscreen::names.e, etc.
# Note that a Shiny app should not be a package of functions- 
# just put the functions needed into R folder and running Shiny app will source those.
# For datasets needed, maybe include them as separate packages this app would require,
# referring to them as packagename::datafromthatpackage 


## Need latest US overall reference values: 
# 
# move this code from server.R to elsewhere ... only need to do it once ever, and 
## it should be based on correct year version ofejscreen::lookupUSA which it does not right now:
#
################################### #  MAYBE THIS NEEDS TO BE DONE WITHIN batch.summarize() ??? 
### add US medians to the summary rows (to compare to avg or median site or person nearby)
### but note this is in summary rows (outlist) but (unlike other summary stats?) is NOT in the fulltable 
# us.med.names <- paste('us.med.', c(names.e, names.d, names.ej), sep = '')
# us.med.names <- us.med.names[!grepl('svi6', us.med.names)]
# us.med.values <-ejscreen::lookupUSA[ejscreen::lookupUSA$PCTILE == '50', c(names.e, names.d, names.ej)] # this needs to be for the right year ****
# us.med.values[names(us.med.values) %in% names.d] <- 100 * us.med.values[names(us.med.values) %in% names.d]
# us.med.values <- us.med.values[!grepl('svi6', names(us.med.values))]
# names(us.med.values) <- us.med.names




# ****** FIX AND/OR ENHANCE TABLES
#
# - Fix tables that don't display - full 2 big tables of details and also states sum counts
# 
# - exec sum tab could show table ranked E by ratio to US mean? (as in tables tab)
# - exec sum and/or barplot tab: add a barplot of RATIO local to US avg (as in tables 1, 1e, 2) *** would be nice to have. sorted.
# - exec sum could show other barplot on same page? or pick a standard key plot or two?
# 
# - ensure using latest pop totals/ ACS data - is any needed now other than what ejscreen dataset already has from package ejscreen?? just median and mean were the issue, not counts??
# 
# - fix Table 3. Do most of these sites have demographics above the US average (not median)?
#   to compare to median not avg
# 
# - maybe clarify / calc raw value for US median person? 
# is avg of percentiles the same as percentile of the avg person's raw score? probably not 
# ... in barplots, summary tables, and/or detail tables?
# 
# *************************
# ***- clarify if can correctly report those stats using EPA batch tool since it double counts people near >1 site. 
# (& R batch fast tool fixed that but needs repair to work with latest vintage dataset)
# Cannot do stats right for people near 2+ sites, with epa batch tool, but only rarely matters:
# Tables below show how many blockgroups with x nearby TSDF, NPL sites.
# *** There can be 2 nearby but rarely more NPLs or even TSDFs overall:
#   ** just over 1% (1 in 78) places have 2+ TSDF nearby
#   ** only 1/30 of 1% (1 in 3400) have 2+ NPL nearby
# Details: 
# *************************
# cbind(sort(table(bg19$statename[bg19$TSDF_CNT > 1])))
# ...
# Maryland               62
# Washington             67
# Massachusetts          72
# Michigan               78
# New Jersey             81
# Indiana                91
# Pennsylvania          103
# Illinois              122
# Texas                 147
# Ohio                  173
# California            381
# New York              444  
# > 
# Ohio                 0.061500178
# California           0.135442588  CA accounts for 14% of the US blockgroups near 2+ TSDF
# New York             0.157838606  NY accounts for 16% of the US blockgroups near 2+ TSDF
# and almost 3% of NY blockgroups have 2+ near, in NH it is 2.8%, rarely in FL/NH.
# 
# states_w_tsdf_near <- aggregate(bg19$TSDF_CNT > 1, by = list(bg19$ST), FUN = function(z) round(100* sum(z)/length(z),1))
# 49      WA 1.4
# 5       CA 1.6
# 21      MD 1.6
# 41      RI 1.6
# 42      SC 1.6
# 19      LA 1.7
# 16      IN 1.9
# 36      OH 1.9
# 9       DE 2.1
# 31      NH 2.8
# 35      NY 2.9
# > 
#  pct.above(bg19[,c('NPL_CNT', 'TSDF_CNT')], benchmarks = 0, or.tied = T)*100
#  pct.of.all.above.or.tied.with.cutoff.for.NPL_CNT 
#                                               100 
# pct.of.all.above.or.tied.with.cutoff.for.TSDF_CNT 
#                                               100 
# > pct.above(bg19[,c('NPL_CNT', 'TSDF_CNT')], benchmarks = 1, or.tied = T)*100
#  pct.of.all.above.or.tied.with.cutoff.for.NPL_CNT 
#                                         0.5950085 
# pct.of.all.above.or.tied.with.cutoff.for.TSDF_CNT 
#                                         5.4040929  ** 5% of places have any TSDF nearby
# 
# > pct.above(bg19[,c('NPL_CNT', 'TSDF_CNT')], benchmarks = 2, or.tied = T)*100
#  pct.of.all.above.or.tied.with.cutoff.for.NPL_CNT 
#                                        0.02904694  ** only 1/30 of 1% (1 in 3400) have 2+ NPL nearby
# pct.of.all.above.or.tied.with.cutoff.for.TSDF_CNT 
#                                        1.27670390 ** just over 1% (1 in 78) places have 2+ TSDF nearby
#                                        
# > pct.above(bg19[,c('NPL_CNT', 'TSDF_CNT')], benchmarks = 3, or.tied = T)*100
#  pct.of.all.above.or.tied.with.cutoff.for.NPL_CNT 
#                                       0.003630868 
# pct.of.all.above.or.tied.with.cutoff.for.TSDF_CNT 
#                                       0.462935647 ** only 2,813 places have 3+ nearby
# > 
# >          count.above(bg19[,c('NPL_CNT', 'TSDF_CNT')], benchmarks = 2, or.tied = T)
#  count.above.or.tied.with.cutoff.for.NPL_CNT 
#                                           64 
# count.above.or.tied.with.cutoff.for.TSDF_CNT 
#                                         2813 
#                                         
# > cbind(table(bg19$TSDF_CNT))
#      [,1]
# 0  208426
# 1    9094
# 2    1793  2813 places have at least 2 nearby
# 3     499
# 4     219
# 5     111
# 6      57
# 7      49
# 8      19
# 9      19
# 10     14
# 11      8
# 12      3
# 13      1
# 14      3
# 15      3
# 16      4
# 17      1
# 18      1
# 19      2
# 20      1
# 21      1
# 23      1
# 25      2
# 37      1
# 38      1
# > cbind(table(bg19$NPL_CNT))
#     [,1]
# 0 219022
# 1   1247
# 2     56
# 3      6
# 4      1
# 6      1
# 
# 
