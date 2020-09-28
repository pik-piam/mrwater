#' @title correctBphTCRE
#' @description Transient Climate Response to accumulated doubling of CO2. File based on CMIP5 +1perc CO2 per year experiment. To be used in the translation to carbon equivalents of BphEffect
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Michael Windisch
#' @seealso
#'   \code{\link{readBphTCRE}}
#' @examples
#'
#' \dontrun{
#'   readSource("BphTCRE", convert="onlycorrect")
#' }
#'

correctBphTCRE <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
