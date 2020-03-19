#' @title readAff_unrestricted
#' @description Read Afforestation only tropical dataset
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Aff_unrestricted", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readAff_unrestricted <- function(){

  x <- read.magpie("aff_unrestricted_0.5.mz")

  return(x)
}
