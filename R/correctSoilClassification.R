#' @title correctSoilClassification
#' @description Correct soil content
#' @param x magpie object provided by the read function
#' @param subtype Switch between different inputs
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos alves, Kristine Karstens
#' @seealso
#' \code{\link{readSoilClassification}},
#' @examples
#'
#' \dontrun{
#'   readSource("SoilClassification", subtype="HWSD.soil", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass read.LPJ_input

correctSoilClassification <- function(x, subtype){
  
  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)
  x <- toolCell2isoCell(x)
  
  return(x)
}
