#' @title readRrLayer
#' @description Read range-rarity layer
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("RrLayer", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readRrLayer <- function(){

  x <- read.magpie("rr_layer_0.5.mz")

  return(x)
}
