#' @title calcIrrigation
#' @description This function extracts irrigation water (airrig: water applied additionally to rainfall) from LPJmL to MAgPIE
#' @param years years to be returned
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Abhijeet Mishra
#'
#' @examples
#' \dontrun{ calcOutput("Irrigation", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset

calcIrrigation <- function(years = seq(1995,2095,by=5)){

  # Read in airrig:
  lpj_airrig <- readSource("LPJmL", subtype="LPJmL5:CRU_4.irrig",convert="onlycorrect")
  ### calc + arguments
  # Note: airrig: irrigation water applied additionally to rainfall
    # rainfed=0
  #### READSOURCE DOES NOT WORK YET... CHECK AGAIN!

  # Load LPJmL to MAgPIE mapping to aggregate to MAgPIE crops
  LPJ2MAG      <- toolGetMapping( "MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")

  # Aggregate to MAgPIE crops
  mag_airrig   <- toolAggregate(lpj_airrig, LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel=TRUE)

  # Remove negative airrig
  mag_airrig[mag_airrig < 0] <- 0

  # Check for NAs
  if(any(is.na(mag_airrig))){
    stop("produced NA airrig")
  }

  return(list(
    x=mag_airrig,
    weight=NULL,
    unit="m^3 per ha per yr",
    description="Irrigation water (water applied in addition to rainfall) in m^3 per hectar per year for different crop types following LPJmL irrigation system assumptions.",
    isocountries=FALSE))
}
