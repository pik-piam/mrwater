#' @title readCalibratedArea
#' @description Read Calibrated Area
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("CalibratedArea", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readCalibratedArea <- function(){

  x <- read.magpie("calibrated_area_0.5.mz")

  return(x)
}
