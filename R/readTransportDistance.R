#' @title readTransportDistance
#' @description Read transport distance
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("TransportDistance", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readTransportDistance <- function(){

  x <- read.magpie("transport_distance_0.5.mz")

  return(x)
}
