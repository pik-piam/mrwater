#' @title convertGPD
#' @description convert GPD
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on iso level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#'
#' \dontrun{
#'   readSource("GPD", convert=TRUE)
#' }
#' @importFrom readxl read_xls

convertGPD <- function(x){

  #convert to Mha
  x <- x/1000

  getCells(x) <- toolCountry2isocode(getCells(x))   #replace country names with iso3 codes
  x           <- toolCountryFill(x)                 #expand to all countries
  x[is.na(x)] <- 0                                  #replace empty spots with zeros

  return(x)
}
