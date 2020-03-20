#' @title correctAfforestationMask
#' @description correct Afforestation Mask where afforestation is possible
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author David Chen
#' @seealso
#'   \code{\link{readAfforestationMask}}
#' @examples
#'
#' \dontrun{
#'   readSource("AfforestationMask",subtype="unrestricted", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

correctAfforestationMask <- function(x,subtype){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
