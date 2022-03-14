EJSCREEN_API_output_example <- function(n=10, radius=1) {
  
  pts <- proxistat::testpoints_block2010(n)
  
  benchmark.start <- Sys.time()
  outlist <- batch.summarizer::ejscreenapi(pts$lon, lat=pts$lat, radius = radius)
  benchmark.end <- Sys.time()
  
  # Format results as a single table
  api.out.table <- data.table::rbindlist(outlist, fill = T, idcol = 'id')
  
  # # Average speed
  perhour <- proxistat::speedsummary(benchmark.start, benchmark.end, NROW(pts))
  cat('\n', perhour, 'buffers per hour \n')
  # # Variability in speed, visualized
  # hist(as.numeric(unlist(lapply(outlist, FUN =function(x) x$timeSeconds)), na.rm = T),100,
  #    main = 'Histogram of seconds per buffer', xlab='Seconds elapsed for a buffer query of API',
  #    sub = paste('(Overall rate =', perhour, 'buffers per hour)') )
  
  
  return(api.out.table)
}
