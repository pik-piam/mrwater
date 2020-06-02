#' @title calcNpiNdcAffPol
#' @description Function creates dummy NPI/NDC policies
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze, Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("NpiNdcAffPol", aggregate = FALSE) }
#' @importFrom magpiesets findset
#'

calcNpiNdcAffPol <-function(){

# create a dummy data set, which is later used to define NDC and NPI policies
x <- new.magpie(cells_and_regions=toolGetMapping("CountryToCellMapping.csv", type="cell")$celliso, years=seq(1995,2150,5),
                names = c("none", "npi", "ndc"), fill = 0, sets = c("region.cell","year","data1"))

return(list(
  x=x,
  weight=NULL,
  unit="dummy (none)",
  description="Dummy file for NPI/INDC policies",
  isocountries=FALSE))
}
