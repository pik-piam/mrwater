#' @title calcCO2Atmosphere
#' @description Disaggregate CO2 global atmospheric concentration to cellular level
#' @param rcp specify the RCP (rcp85, rcp60, rcp45 or rcp26)
#' @param level specify the spatial output level
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("CO2Atmosphere", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset
#'

calcCO2Atmosphere <-function(rcp = "rcp85", level="cellular"){

  x <- readSource("CO2Atmosphere", subtype=rcp, convert="onlycorrect")

  if(level=="cellular"){

    cells <- toolGetMapping("CountryToCellMapping.csv", type="cell")
    cells$glo <- "GLO"
    x <- toolAggregate(x, rel=cells, from="glo", to="celliso")

  }

  return(list(
    x=x,
    weight=NULL,
    unit="ppm",
    description="Atmosphere CO2 concentration",
    isocountries=FALSE))
}
