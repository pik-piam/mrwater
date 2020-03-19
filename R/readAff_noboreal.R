#' @title readAff_noboreal
#' @description Read Afforestation No Boreal dataset
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Aff_noboreal", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readAff_noboreal <- function(){

  x <- read.magpie("aff_noboreal_0.5.mz")

  return(x)
}
