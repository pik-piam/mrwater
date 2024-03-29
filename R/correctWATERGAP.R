#' @title correctWATERGAP
#' @description Correct WATERGAP data
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x MAgPIE object provided by the read function
#' @author Felicitas Beier
#' @seealso
#'   \code{\link{readWATERGAP}}
#' @examples
#' \dontrun{
#'   readSource("WATERGAP", convert="onlycorrect")
#' }
#'
#' @import magclass
#' @importFrom madrat toolConditionalReplace

correctWATERGAP <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
