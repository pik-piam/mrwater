#' @title readKoeppen_geiger
#' @description Read koeppen geiger areas cellular
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Koeppen_geiger", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readKoeppen_geiger <- function(){

  x <- read.magpie("koeppen_geiger_0.5.mz")

  return(x)
}
