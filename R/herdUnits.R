#' Spatial data for antelope (pronghorn) herd units in Wyoming
#'
#' @description Herd units for antelope (pronghorn) in Wyoming.
#' Each herd unit contains one or more hunt areas.  Current as of August 2025.
#'
#' @format \code{sf} object with \code{POLYGON} features,
#' 40 rows and 4 columns.
#' Each row is a distinct herd, columns as follows:
#' \enumerate{
#'    \item{\code{HERDUNIT}: unique numeric identifier for herd, key to \code{\link{huntAreas}}}
#'    \item{\code{HERDNAME}: unique name for herd}
#'    \item{\code{AREA}: area in square miles using \code{st_crs(4326)}}
#'    \item{\code{geometry}: \code{sf} specification of polygon coordinates}
#' }
#'
#' @source https://wyoming-wgfd.opendata.arcgis.com/
#'
#' @seealso \code{\link{huntAreas}}
"herdUnits"
