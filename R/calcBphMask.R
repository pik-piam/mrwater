#' @title calcBphMask
#' @description Mask of Datapoints of biogeophysical temperature change of afforestation (degree C) to be used as weight. File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018
#'# @param
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("BphMask", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom madrat readSource

calcBphMask <-function(){

  x <- readSource("BphMask", convert="onlycorrect")

  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, land="fao", input_magpie=TRUE, years="y1995", round=6), dim=3)

  return(list(
    x=x,
    weight=weight,
    unit="none",
    description="Nonan Mask of BPH Effect Dataset",
    isocountries=FALSE))
}
