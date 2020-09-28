#' @title readBphTCRE
#' @description Read Transient Climate Response to accumulated doubling of CO2. File based on CMIP5 +1perc CO2 per year experiment. To be used in the translation to carbon equivalents of BphEffect
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("BphTCRE", convert="onlycorrect")
#' }
#'

readBphTCRE <- function(){

  x <- read.magpie("f32_localTCRE_0.5.mz")

  return(x)
}
