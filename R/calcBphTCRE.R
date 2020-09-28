#' @title calcBphTCRE
#' @description Transient Climate Response to accumulated doubling of CO2. File based on CMIP5 +1perc CO2 per year experiment. To be used in the translation to carbon equivalents of BphEffect
#'# @param
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("BphTCRE", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom madrat readSource

calcBphTCRE <-function(){

  x <- readSource("BphTCRE", convert="onlycorrect")

  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, land="fao", input_magpie=TRUE, years="y1995", round=6), dim=3)

  return(list(
    x=x,
    weight=weight,
    unit="degC per tC per ha",
    description="Local Transient Climate Response to tc per ha in degC",
    isocountries=FALSE))
}
