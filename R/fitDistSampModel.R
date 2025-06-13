#' Fit distance-sampling model to pronghorn line-transect data
#'
#' Estimate abundance of pronghorn using distance sampling.
#' Distance-sampling models are fit using the Rdistance package.
#' This is a thin wrapper around \code{Rdistance::dfuncEstim} and
#' \code{Rdistance::abundEstim}.
#'
#' @param ddf Detection data.  See \code{Rdistance::dfuncEstim} and
#' \code{Rdistance::abundEstim}.
#' @param sdf Site data.  See \code{Rdistance::abundEstim}.
#' @param keyFun Shape of detection curve.  See \code{Rdistance::dfuncEstim}.
#' @param wHi Right truncation distance.  See \code{Rdistance::dfuncEstim}.
#' Default is 200 m, given the pronghorn survey strip is 200 m wide.
#' @param sidesSurveyed Sides of the aircraft surveyed.  Default is 1, given the
#' current protocol specifies 1 observer looking out 1 side of the aircraft.
#' Rdistance assumes 2 sides of the line are surveyed, by default, so this
#' allows the area of the survey strip to be adjusted accordingly.
#' @param areaMi2 Size of the area (in square miles) that the abundance estimate
#' is extrapolated to.  This is often the total area of the herd unit, but areas
#' not occupiable by pronghorn (e.g., forested areas) should be excluded.
#' @param bootIterations Number of bootstrap iterations to use to calculate
#' confidence intervals.  See \code{Rdistance::abundEstim}.  If 0 (the default),
#' no bootstrap is run and no CIs are generated.
#'
#' @return An 'abundance estimate' object.  Same as returned by
#' \code{Rdistance::abundEstim}.
#'
#' @author Jason Carlisle
#'
#' @importFrom units set_units
#' @importFrom Rdistance RdistDf dfuncEstim abundEstim
#' @importFrom dplyr mutate select
#' @export
#'
#' @examples
#' \dontrun{
#' # Prep data
#' dataPath <- "C:/Users/jadcarlisle/Desktop/demo"
#' x <- prepDataForAnalysis(inputFile = file.path(dataPath,
#'                                               "Data_PronghornLT_Rattlesnake_2022.xlsx"),
#'                        inputSheet = 1)
#'
#' # Fit distance-sampling model to estimate abundance
#' # Key input data are the x$ddf and x$sdf data.frames from prepDataForAnalysis
#' fit <- fitDistSampModel(ddf = x$ddf,
#'                        sdf = x$sdf,
#'                        keyFun = "hazrate",
#'                        sidesSurveyed = 1,
#'                        areaMi2 = 884,
#'                        bootIterations = 50)
#'
#' # Print results
#' fit
#' }

fitDistSampModel <- function(ddf,
                             sdf,
                             keyFun = "halfnorm",
                             wHi = 200,
                             sidesSurveyed = 1,
                             areaMi2,
                             bootIterations = 0) {

  # Set seed to make bootstrapping results reproducible
  set.seed(82070)







  # Set units (required by Rdistance as of v2.2)
  # And Rdistance works best if the distance column is named "dist"
  wHi <- units::set_units(wHi, "m")
  areaMi2 <- units::set_units(areaMi2, "mi2")

  ddf <- ddf |>
    mutate(dist = units::set_units(adjustedDist, "m")) |>
    select(-adjustedDist)

  sdf <- sdf |>
    mutate(length = units::set_units(lengthKm, "km")) |>
    select(-lengthKm)


  # Make nested data.frame (required by Rdistance as of v4.0)
  df <- Rdistance::RdistDf(transectDf = sdf,
                           detectionDf = ddf,
                           by = "siteID",
                           pointSurvey = FALSE,
                           observer = "single",
                           .detectionCol = "detections",
                           .effortCol = "length")

  # Summary
  # summary(df, formula = dist ~ groupsize(s))



  # Fit detection function
  # Specifying outputUnits = "m" will provide distance measures in m
  # and density estimate in mi^2 (we'll convert those to mi^2 in the app)
  dfunc <- df |>
    Rdistance::dfuncEstim(dist ~ 1 + groupsize(s),
                          likelihood = keyFun,
                          expansions = 0,
                          w.hi = wHi,
                          outputUnits = "m")

  # plot(dfunc)


  # Estimate abundance
  # Turn off the bootstrap feature if bootIterations input is 0
  if (bootIterations == 0) {
    ci <- NULL  # Rdistance input to skip bootstrap and not calculate CIs
  } else {
    ci <- 0.95  # default 95% CI
  }

  fit <- dfunc |>
    Rdistance::abundEstim(area = areaMi2,
                          propUnitSurveyed = sidesSurveyed / 2,
                          ci = ci,
                          R = bootIterations,
                          plot.bs = FALSE,
                          showProgress = FALSE)

  summary(fit)
  data.frame(fit$estimates)

  return(fit)

}
