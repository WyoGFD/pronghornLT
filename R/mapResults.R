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
#'
#' @import leaflet
#' @importFrom htmltools HTML
#' @importFrom lubridate as_date
#' @importFrom sf st_transform st_as_sf st_centroid st_combine st_filter
#' @importFrom viridisLite magma
#' @importFrom data.table fcase
#'
#' @export

mapResults <- function(preppedData) {

  # extract lines, points
  l <- sf::st_transform(preppedData$sfLines, crs = 4326)
  l <- dplyr::arrange(l, begin)
  p <- sf::st_transform(preppedData$sfPoints, crs = 4326)

  # add date, time
  l$Date <- lubridate::as_date(l$begin)
  l$Begin <- format(as.POSIXct(l$begin), format = "%H:%M")
  l$End <- format(as.POSIXct(l$end), format = "%H:%M")
  p$Time <- format(as.POSIXct(p$DateTime), format = "%H:%M")
  p$Date <- lubridate::as_date(p$DateTime)


  # label points
  p$popup_text <-
    paste0(
      "Transect ID: ", p$siteID, "<br/>",
      "Altitude: <strong>", p$agl, '</strong> <br/>',
      'Band: ', '<strong>', p$band, '</strong> <br/>',
      'Group Size: ', '<strong>', p$s, '</strong> <br/>',
      'Date: ', p$Date, "<br/>",
      "Time: ", p$Time) %>%
    lapply(htmltools::HTML)

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
    lapply(htmltools::HTML)

  # color transects
  l$color <- viridisLite::magma(nrow(l), end = .8)

  # radius of points
  p$radius <- data.table::fcase(
    p$s %in% 1:3, 3,
    p$s %in% 4:10, 5,
    p$s %in% 11:20, 7,
    p$s >= 21, 9
  )

  # herd units for reference
  # from package's /data folder
  hu <- sf::st_transform(herdUnits, crs = 4326)

  # Keep the herd unit that contains the points' centroid
  p_centroid <- p %>%
    sf::st_combine() %>%
    sf::st_centroid()

  hu <- hu %>%
    sf::st_filter(p_centroid)


  # leaflet
  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data = hu,
                         color = "darkslategray",
                         fillOpacity = 0) %>%
    leaflet::addPolylines(data = l,
                          weight = 4,
                          label = ~popup_text,
                          color = ~color,
                          opacity = .75) %>%
    leaflet::addCircleMarkers(data = p,
                              radius = ~radius,
                              label = ~popup_text,
                              stroke = F,
                              fillOpacity = .45)

  # return
  return(map)
}
