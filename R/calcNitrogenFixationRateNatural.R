#' @title calcNitrogenFixationRateNatural
#' @description Function that extracts Nitrogen fixation rate of natural vegetation
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{ calcOutput("NitrogenFixationRateNatural", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#'

calcNitrogenFixationRateNatural <-function(){

  x <- readSource("NitrogenFixationRateNatural", convert="onlycorrect")

  return(list(
    x=x,
    weight=NULL,
    unit="tNr / ha",
    description="Nitrogen fixation rate of natural vegetation",
    isocountries=FALSE))
}
