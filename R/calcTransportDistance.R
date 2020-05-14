#' @title calcTransportDistance
#' @description Function extracts travel time to major cities in minutes
#'
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("TransportDistance", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#'

calcTransportDistance <-function(){

x <- readSource("TransportDistance", convert="onlycorrect")

return(list(
  x=x,
  weight=NULL,
  unit="Travel Time (minutes)",
  description="Travel time to major cities Nelson 2008 EC JRC, see model documentation",
  isocountries=FALSE))
}
