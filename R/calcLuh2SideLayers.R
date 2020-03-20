#' @title calcLuh2SideLayers
#' @description Function extracts biodiversity data for LUH2 land cover types
#'
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("Luh2SideLayers", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcLuh2SideLayers <-function(){

x <- readSource("Luh2SideLayers", convert="onlycorrect")

return(list(
  x=x,
  weight=NULL,
  unit="bool (none)",
  description="Data from LUH2 provided by David Leclere from IIASA, Bending the curve on biodiversity loss",
  isocountries=FALSE))
}
