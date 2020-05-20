#' @title calcAtmosphericDepositionRates
#' @description Function that calculates Atmospheric deposition rates
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#' @examples
#' \dontrun{ calcOutput("AtmosphericDepositionRates", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset

calcAtmosphericDepositionRates <-function(){

  x <- readSource("AtmosphericDepositionRates", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="tNr / ha",
    description="Atmospheric deposition rate",
    isocountries=FALSE))
}
