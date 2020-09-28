#' @title correctBphEffect
#' @description Biogeophysical temperature change of afforestation (degree C). File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Michael Windisch
#' @seealso
#'   \code{\link{readBphEffect}}
#' @examples
#'
#' \dontrun{
#'   readSource("BphEffect", convert="onlycorrect")
#' }
#'

correctBphEffect <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
