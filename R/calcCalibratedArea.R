#' @title calcCalibratedArea
#' @description Function extracts calibrated area
#'
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("CalibratedArea", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import mrcommons
#' @import mrland
#' @importFrom magpiesets findset
#'

calcCalibratedArea <-function(){

  x <- readSource("CalibratedArea", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="",
    isocountries=FALSE))
}
