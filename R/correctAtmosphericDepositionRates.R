#' @title correctAtmosphericDepositionRates
#' @description Correct Atmospheric deposition rate
#' @return Magpie object with results on cellular level
#' @param x magpie object provided by the read function
#' @author Marcos Alves
#' @seealso
#'   \code{\link{readAtmosphericDepositionRates}}
#' @examples
#'
#' \dontrun{
#'   readSource("AtmosphericDepositionRates", convert="onlycorrect")
#' }
#'

correctAtmosphericDepositionRates <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
