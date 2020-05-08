#' @title readProtectArea
#' @description Read Protected Area
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("ProtectArea", convert="onlycorrect")
#' }
#'

readProtectArea <- function(){

  x <- read.magpie("protect_area_0.5.mz")

  return(x)
}
