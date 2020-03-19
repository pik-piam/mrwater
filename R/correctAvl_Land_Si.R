#' @title correctAvl_Land_Si
#' @description Read Available Land Si
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author David Chen
#' @seealso
#'   \code{\link{readAvl_Land_Si}}
#' @examples
#'
#' \dontrun{
#'   readSource("Avl_Land_Si", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

correctAvl_Land_Si <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
