#' @title calcNpiNdcAffPol
#' @description Function extracts dummy NPI/NDC policies
#'
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("NpiNdcAffPol", aggregate = FALSE) }
#' @importFrom magpiesets findset
#'

calcNpiNdcAffPol <-function(){

x <- readSource("NpiNdcAffPol", convert="onlycorrect")

return(list(
  x=x,
  weight=NULL,
  unit="dumy (none)",
  description="Dummy file for NPI/INDC policies",
  isocountries=FALSE))
}
