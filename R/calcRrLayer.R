#' @title calcRrLayer
#' @description Function extracts range-rarity as used for biodiversity loss
#'
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("RrLayer", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset
#'

calcRrLayer <-function(){

x <- readSource("RrLayer", convert="onlycorrect")

return(list(
  x=x,
  weight=NULL,
  unit="Range-Rarity (?)",
  description="range-rarity layer provided by David Leclere from IIASA, Bending the curve on biodiversity loss",
  isocountries=FALSE))
}
