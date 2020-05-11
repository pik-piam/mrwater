#' @title readCshareReleased
#' @description Read "Share of soil carbon that is released on cropland during the harvest. Calculated by Benjamin Bodirsky"
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("CshareReleased", convert="onlycorrect")
#' }
#'

readCshareReleased <- function(){

  x <- read.magpie("cshare_released_0.5.mz")

  return(x)
}
