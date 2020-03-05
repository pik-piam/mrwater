#' @title calcTransport_Distance
#' @description Function extracts travel time to major cities in minutes
#'
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("Transport_Distance", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcTransport_Distance <-function(x){

x <- readSource("Transport_Distance", convert="onlycorrect")

return(list(
  x=x,
  weight=NULL,
  unit="Travel Time (minutes)",
  description="Travel time to major cities Nelson 2008 EC JRC, see model documentation",
  isocountries=FALSE))
}
