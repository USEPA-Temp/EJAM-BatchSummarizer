batch.read <- function(file, ...) {
# not really needed, not used now
  x <- read.csv(file = file, stringsAsFactors = FALSE, ...)
  
  return(x)
}
