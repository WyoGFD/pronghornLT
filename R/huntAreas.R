#' Spatial data for antelope (pronghorn) hunt areas in Wyoming
#'
#' @description Hunt areas for antelope (pronghorn) in Wyoming.
#' Each herd unit contains one or more hunt areas.  Current as of August 2025.
#' @format \code{sf} object with \code{MULTIPOLYGON} features,
#' 102 rows and 6 columns.
#' Each row is a distinct hunt area, columns as follows:
#' \enumerate{
#'    \item{\code{HUNTAREA}: unique numeric identifier for hunt area}
#'    \item{\code{HUNTNAME}: unique name for hunt area}
#'    \item{\code{HERDUNIT}: numeric identifier of herd to which the hunt area belongs, key to \code{\link{herdUnits}}}
#'    \item{\code{HERDNAME}: name of herd to which the hunt area belongs}
#'    \item{\code{AREA}: area in square miles using \code{st_crs(4326)}}
#'    \item{\code{geometry}: \code{sf} specification of polygon coordinates}
#' }
#'
#' @source https://wyoming-wgfd.opendata.arcgis.com/
#'
#' @seealso \code{\link{herdUnits}}
"huntAreas"
