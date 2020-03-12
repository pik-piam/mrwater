#' @title readAvl_irrig
#' @description Read Available Irrigation
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Avl_irrig", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readAvl_irrig <- function(){

  x <- read.magpie("avl_irrig_0.5.mz")

  return(x)
}
