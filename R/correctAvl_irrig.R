#' @title correctAvl_irrig
#' @description Read available irrigation file
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author David Chen
#' @seealso
#'   \code{\link{readAvl_irrig}}
#' @examples
#'
#' \dontrun{
#'   readSource("Avl_irrig", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

correctAvl_irrig <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
