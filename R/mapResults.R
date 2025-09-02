#' mapResults
#'
#' @description
#' Maps the results of a survey, including surveyed lines, pronghorn observation points, and the herd unit boundary.
#'
#' @param preppedData List object returned from \code{\link{prepDataForAnalysis}}.
#'
#' @return Returns a leaflet map.
#'
#' @examples
#' \dontrun{
#'
#' # Prep data
#' dat <- prepDataForAnalysis("example.xlsx")
#'
#' # Map
#' mapResults(dat)
#' }
#'
#' @author Garrett Catlin, Jason Carlisle
#' @export

mapResults <- function(preppedData) {

  # extract lines, points
  l <- st_transform(preppedData$sfLines, crs = 4326)
  l <- arrange(l, begin)
  p <- st_transform(preppedData$sfPoints, crs = 4326)

  # add date, time
  l$Date <- as_date(l$begin)
  l$Begin <- format(as.POSIXct(l$begin), format = "%H:%M")
  l$End <- format(as.POSIXct(l$end), format = "%H:%M")
  p$Time <- format(as.POSIXct(p$DateTime), format = "%H:%M")
  p$Date <- as_date(p$DateTime)


  # label points
  p$popup_text <-
    paste0(
      "Transect ID: ", p$siteID, "<br/>",
      "Altitude: <strong>", p$agl, '</strong> <br/>',
      'Band: ', '<strong>', p$band, '</strong> <br/>',
      'Group Size: ', '<strong>', p$s, '</strong> <br/>',
      'Date: ', p$Date, "<br/>",
      "Time: ", p$Time) %>%
    lapply(HTML)

  # label transects
  l$popup_text <- paste0(
    "Transect ID: <strong>",
    l$siteID,
    "</strong> <br/>",
    "Transect Length: <strong>",
    round(l$Length, 2),
    " km </strong> <br/>",
    "Date: ", l$Date, "<br/>",
    "Start: ", l$Begin, "<br/>",
    "End: ", l$End
  ) %>%
    lapply(HTML)

  # color transects
  l$color <- magma(nrow(l), end = .8)

  # radius of points
  p$radius <- fcase(
    p$s %in% 1:3, 3,
    p$s %in% 4:10, 5,
    p$s %in% 11:20, 7,
    p$s >= 21, 9
  )

  # herd units for reference
  # from package's /data folder
  hu <- st_transform(herdUnits, crs = 4326)

  # Keep the herd unit that contains the points' centroid
  p_centroid <- p %>%
    st_combine() %>%
    st_centroid()

  hu <- hu %>%
    st_filter(p_centroid)


  # leaflet
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = hu,
                color = "darkslategray",
                fillOpacity = 0) %>%
    addPolylines(data = l,
                 weight = 4,
                 label = ~popup_text,
                 color = ~color,
                 opacity = .75) %>%
    addCircleMarkers(data = p,
                     radius = ~radius,
                     label = ~popup_text,
                     stroke = F,
                     fillOpacity = .45)

  # return
  return(map)
}
