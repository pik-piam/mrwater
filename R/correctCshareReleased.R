#' @title correctCshareReleased
#' @description Read "Share of soil carbon that is released on cropland during the harvest. Calculated by Benjamin Bodirsky"
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author David Chen
#' @seealso
#'   \code{\link{readCshareReleased}}
#' @examples
#'
#' \dontrun{
#'   readSource("CshareReleased", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

correctCshareReleased <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
