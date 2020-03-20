#' @title correctNpiNdcAffPol
#' @description Read dummy NPI/NDC policies file
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Michael Windisch
#' @seealso
#'   \code{\link{readNpiNdcAffPol}}
#' @examples
#'
#' \dontrun{
#'   readSource("NpiNdcAffPol", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

correctNpiNdcAffPol <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
