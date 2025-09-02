#' cutPoly
#'
#' @description
#' This function cuts out holes in a given polygon by transforming the crs' to
#' match and then by differencing the 2 polygons.
#'
#' @param sPoly \code{sf} object, the input polygon within which line transects
#'  will be generated. The coordinate system
#'  must use meters as the linear unit (e.g. EPSG=26913)
#' @param xPoly \code{sf} object, optional input polygon that defines an
#' exclusion area where line transects will not be generated.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # get hunt area polygons
#' poly <- huntAreas %>%
#'   filter(HERDNAME %in% c("Badwater", "North Natrona"))
#'
#' # map
#' mapLT(poly)
#'
#' # get cutout
#' x <- st_read("example_cutout.kml")
#'
#' # map poly and cutouts
#' mapLT(poly, x)
#'
#' # get poly with cutouts
#' poly <- cutPoly(poly, x)
#'
#' # calculate recommended line length
#' recLength <- calcLineLength(
#'   poly,
#'   N = 20000
#' )
#'
#' # makeLines
#' lines <- makeLines(
#'   poly,
#'   recLength
#' )
#'
#' # map
#' mapLT(poly, x, lines$lines)
#' }
#'
#' @author Garrett Catlin
#' @return A polygon with cutouts
#' @export

cutPoly <- function(sPoly, xPoly) {

  # transform to same crs
  xPoly <- st_transform(xPoly, st_crs(sPoly))

  # combine xPoly
  xPoly <- st_combine(xPoly)

  # take difference
  st_agr(sPoly) = "constant"
  diffPoly <- st_difference(sPoly, xPoly)

  # return
  return(diffPoly)
}
