#' @title correctGFADArea
#' @description Correct Global forest age dataset
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Abhijeet Mishra, Felicitas Beier
#' @seealso
#'   \code{\link{readGFADArea}}
#' @examples
#'
#' \dontrun{
#'   readSource("GFADArea", convert="onlycorrect")
#' }
#'

correctGFADArea <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
