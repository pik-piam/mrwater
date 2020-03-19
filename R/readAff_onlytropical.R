#' @title readAff_onlytropical
#' @description Read Afforestation only tropical dataset
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Aff_onlytropical", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readAff_onlytropical <- function(){

  x <- read.magpie("aff_onlytropical_0.5.mz")

  return(x)
}
