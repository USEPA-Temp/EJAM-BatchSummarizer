#' Color-coded map of US counties (choropleth)
#'
#' Uses \code{\link[maps]{map}} function from maps package, to draw a map of US counties.
#'  
#'  PROBABLY WILL REPLACE THIS WITH \code{\link[choroplethr]{county_choropleth}} county_choropleth {choroplethr}
#'   or countypointmap in proxistat package
#'   
#' Note: percent map is designed to work with the counties data set
#' It will not work correctly with other data sets if their row order does
#' not exactly match the order in which the maps package plots counties.
#' It is hardcodedd to show quartiles as specific percentile bins, cutpoints in the legend.
#' 
#' @param var vector of numbers to use for county map
#' @param color Default is 'black' - Color like 'blue' for \code{\link[grDevices]{colorRamp}} from white to color to generate a vector of gradations of fill colors for the map
#' @param legend.title text for legend
#' @param min Default is 0
#' @param max Default is 100
#' @param projection Default is 'polyconic' - for \link[maps]{map}
#'
percent_map <- function(var, color='black', legend.title, min = 0, max = 100, projection='polyconic') {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)

  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100,
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # plot choropleth map
  maps::map("county", fill = TRUE, col = fills,
    resolution = 0, lty = 0, projection = projection,
    myborder = 0, mar = c(0,0,0,0))

  # overlay state borders
  maps::map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = projection,
    myborder = 0, mar = c(0,0,0,0))

  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))

  legend("bottomleft",
    legend = legend.text,
    fill = shades[c(1, 25, 50, 75, 100)],
    title = legend.title)
}
