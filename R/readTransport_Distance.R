#' @title readTransport_Distance
#' @description Read transport distance (no source information available)
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Transport_Distance", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readTransport_Distance <- function(){

  x <- read.magpie("C:/PIK/inputdata/sources/Transport_Distance/transport_distance_0.5.mz")

  return(x)
}
