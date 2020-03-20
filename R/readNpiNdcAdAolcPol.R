#' @title readNpiNdcAdAolcPol
#' @description Read NPI/NDC dataset
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("NpiNdcAdAolcPol", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readNpiNdcAdAolcPol <- function(){

  x <- read.magpie("npi_ndc_ad_aolc_pol_0.5.mz")

  return(x)
}
