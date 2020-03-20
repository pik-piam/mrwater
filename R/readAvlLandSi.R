#' @title readAvl_Land_Si
#' @description Read si0 and nsi0 areas based on Ramankutty dataset"
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("AvlLandSi", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readAvlLandSi <- function(){

  x <- read.magpie("avl_land_si_0.5.mz")

  return(x)
}
