#' @title calcRrLayer
#' @description Function extracts range-rarity as used for biodiversity loss
#' @param rcp specify the RCP
#' @param level specify the spatial output level
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

calcCO2Atmosphere <-function(rcp="rcp8p5", level="cellular"){

  x <- readSource("CO2Atmosphere", subtype=rcp, convert=FALSE)

  if(level=="cellular"){

    cells <- toolGetMapping("CountryToCellMapping.csv", type="cell")
    cells$glo <- "GLO"
    x <- toolAggregate(x, rel=cells, from="glo", to="celliso")

  }

  return(list(
    x=x,
    weight=NULL,
    unit="Range-Rarity (?)",
    description="range-rarity layer provided by David Leclere from IIASA, Bending the curve on biodiversity loss",
    isocountries=FALSE))

  "C:/magpie_inputdata/sources"
}
