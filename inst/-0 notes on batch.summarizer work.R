# OLDER NOTES ON TESTING batch.summarize function 

 x1 = read.csv("Export_Output_Example2.pop.csv", stringsAsFactors = FALSE)
 x2 = read.csv("Export_Output_Example2.csv", stringsAsFactors = FALSE)
dim(x1)
 mymap <- read.csv('map batch to friendly fieldnames 2016.csv', stringsAsFactors = FALSE)
 x1 <- batch.clean(x1, oldcolnames = mymap$oldnames, newcolnames = mymap$newnames )
 x2 <- batch.clean(x2, oldcolnames = mymap$oldnames, newcolnames = mymap$newnames )
dim(x1)
  if ('name' %in% colnames(x1) ) { rownames(x1) <- x1[ , 'name'] } # become colnames when transposed for viewing? no.
if ('name' %in% colnames(x2) ) { rownames(x2) <- x2[ , 'name'] } # become colnames when transposed for viewing? no.
dim(x1)

 
mythreshnames = list( 
  group1 = grep('^pctile.EJ.DISPARITY.', colnames(x1), value = TRUE) , 
  group2 = grep('region.pctile.EJ.DISPARITY.', colnames(x1), value = TRUE) , 
  group3 = grep('state.pctile.EJ.DISPARITY.', colnames(x1), value = TRUE) 
)


x <- batch.summarize(
  sitestats = x2, popstats = x1,
  wtscolname = 'pop', 
  cols = colnames(x1), 
  threshnames = mythreshnames, threshold = list(95,95,95), threshgroup = list('group1', 'group2', 'group3'),
  #probs = as.numeric( input$probs ), 
  na.rm = na.rm
)

# x <- batch.summarize(
#   sitestats = fulltabler(), popstats = fulltabler.pop(),
#   wtscolname = mywtsname, 
#   #wts = fulltabler()[ , mywtsname],  
#   cols = mycolnames(), 
#   threshnames = mythreshnames(), threshold = mythresholds(), threshgroup = mythreshgroups(),
#   colfun.picked = colfun.picked, rowfun.picked = rowfun.picked,
#   probs = as.numeric( input$probs ), na.rm = na.rm
# )

