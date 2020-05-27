#' @title correctHalfEarth
#' @description correct HalfEarth data
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Felicitas Beier
#' @seealso
#'   \code{\link{readHalfEarth}}
#' @examples
#'
#' \dontrun{
#'   readSource("HalfEarth", convert="onlycorrect")
#' }
#'

correctHalfEarth <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
