#' @title correctCalibratedArea
#' @description Read calibrated area file
#' #' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author David Chen
#' @seealso
#'   \code{\link{readCalibratedArea}}
#' @examples
#'
#' \dontrun{
#'   readSource("CalibratedArea", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

correctCalibratedArea <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
