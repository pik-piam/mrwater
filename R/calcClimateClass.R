#' @title calcClimateClass
#' @description fraction of a cell belonging to a given climate classification based on Koeppen Geiger Classification. http://koeppen-geiger.vu-wien.ac.at/.
#'
#' @return Clustered MAgPIE object on requested resolution
#' @author Abhijeet Mishra
#'
#' @examples
#' \dontrun{ calcOutput("ClimateClass", aggregate = FALSE) }
#'
#' @export

calcClimateClass <-function(){

  x <- readSource("Koeppen", subtype="cellular",convert = FALSE)

  return(list(
    x=x,
    weight=NULL,
    unit="share",
    description="share of koeppen geiger area",
    isocountries=FALSE))
}
