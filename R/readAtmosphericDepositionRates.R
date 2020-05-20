#' @title readAtmosphericDepositionRates
#' @description Read Atmospheric deposition rate
#' @return Magpie objects with results on cellular level.
#' @author Marcos Alves
#' @examples
#'
#' \dontrun{
#'   readSource("AtmosphericDepositionRates", convert="onlycorrect")
#' }
#'

readAtmosphericDepositionRates <- function(){

  x <- read.magpie("f50_AtmosphericDepositionRates_0.5.mz")

  return(x)
}
