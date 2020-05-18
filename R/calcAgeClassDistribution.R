#' @title calcAgeClassDistribution
#' @description This function calculates the share of each age class in secondary forests in each MAgPIE simulation cluster based on Global Forest Age Dataset from Poulter et al. 2019
#'
#' @return magpie object in cluster resolution
#' @author Abhijeet Mishra, Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AgeClassDistribution", aggregate = FALSE) }
#'

calcAgeClassDistribution <- function(){

  poulter_dataset <- readSource("GFAD", convert="onlycorrect")

  soilc_layer_natveg <-  calcOutput("LPJmL", version=version, climatetype=climatetype, subtype="soilc_layer",
                              time=time, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                              aggregate=FALSE, years=lpjml_years)
  topsoilc           <- soilc_layer_natveg[,,1] + 1/3 * soilc_layer_natveg[,,2]
  getNames(topsoilc) <- "topsoilc"

  # Check for NAs
  if(any(is.na(topsoilc))){
    stop("produced NA Carbon")
  }

  return(list(
    x=topsoilc,
    weight=NULL,
    unit="t per ha",
    description="Topsoil carbon in tons per hectar for natural vegetation.",
    isocountries=FALSE))
}
