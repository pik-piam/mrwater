#' @title correctTransport_Distance
#' @description Read transport distance file (no source information available)
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Kristine Karstens
#' @seealso
#'   \code{\link{readDams}}
#' @examples
#'
#' \dontrun{
#'   readSource("Transport_Distance", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

correctTransport_Distance <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
