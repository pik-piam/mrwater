#' @title readNpiNdcAffPol
#' @description Read dummy NPI/NDC policies
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Michael Windisch
#' @examples
#'
#' \dontrun{
#'   readSource("NpiNdcAffPol", convert="onlycorrect")
#' }
#'

readNpiNdcAffPol <- function(){

  x <- read.magpie("npi_ndc_aff_pol_0.5.mz")

  return(x)
}
