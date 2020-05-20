#' @title NitrogenFixationRateNatural
#' @description Read Nitrogen fixation rate of natural vegetation
#' @return Magpie objects with results on cellular level.
#' @author Marcos Alves
#' @examples
#'
#' \dontrun{
#'   readSource("NitrogenFixationRateNatural", convert="onlycorrect")
#' }
#'

readNitrogenFixationRateNatural <- function(){

  x <- read.magpie("f50_NitrogenFixationRateNatural_0.5.mz")

  return(x)
}
