#' @title readAvlIrrig
#' @description Read Available Irrigation
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("AvlIrrig", convert="onlycorrect")
#' }
#'

readAvlIrrig <- function(){

  x <- read.magpie("avl_irrig_0.5.mz")

  return(x)
}
