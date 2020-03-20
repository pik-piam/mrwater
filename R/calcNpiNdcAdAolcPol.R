#' @title calcNpiNdcAdAolcPol
#' @description Function extracts NPI/NDC forestry (area?)
#'
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("NpiNdcAdAolcPol", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import moinput
#' @importFrom magpiesets findset
#'

calcNpiNdcAdAolcPol <-function(){

x <- readSource("NpiNdcAdAolcPol", convert="onlycorrect")

return(list(
  x=x,
  weight=NULL,
  unit="Area (Mha)",
  description="Forestry prescribed exogoneously by NDC/NPI policy as opposed to CDR GHG prices",
  isocountries=FALSE))
}
