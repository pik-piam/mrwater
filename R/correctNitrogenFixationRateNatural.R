#' @title correctNitrogenFixationRateNatural
#' @description Read Nitrogen fixation rate of natural vegetation
#' @return Magpie object with results on cellular level
#' @param x magpie object provided by the read function
#' @author Marcos Alves
#' @seealso
#'   \code{\link{readNitrogenFixationRateNatural}}
#' @examples
#'
#' \dontrun{
#'   readSource("NitrogenFixationRateNatural", convert="onlycorrect")
#' }
#'

correctNitrogenFixationRateNatural <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
