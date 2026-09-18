#' @title calcGrassET
#'
#' @description Calculates evapotranspiration (ET) of grassland
#'              under irrigated and rainfed conditions based on LPJmL inputs.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param season        "wholeYear":  grass ET in the entire year (main + off season)
#'                      "mainSeason": grass ET in the crop-specific growing
#'                                    period of LPJmL (main season)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("GrassET", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums getItems new.magpie getSets add_dimension
#'

calcGrassET <- function(selectyears, lpjml, climatetype, season) {

  ####################
  ### Read in data ###
  ####################
  #### To Do (discuss with Jens): yearly grass ET (for cropsIr and for cropsRf)

  # irrigated grass ET in entire year
  yearlyIrrigated <- calcOutput("LPJmLHarmonize", subtype = "cropsIR:et_grass_ir",
                                lpjmlversion = lpjml, climatetype = climatetype,
                                monthly = FALSE,
                                aggregate = FALSE)[, selectyears, "irrigated"]
  # rainfed grass ET in entire year
  yearlyRainfed <- calcOutput("LPJmLHarmonize", subtype = "cropsRF:et_grass_rf",
                              lpjmlversion = lpjml, climatetype = climatetype,
                              monthly = FALSE,
                              aggregate = FALSE)[, selectyears, "rainfed"]
  # irrigated grass ET in irrigated growing period of crop
  grperIrrigated <- calcOutput("LPJmLHarmonize", subtype = "cropsIR:cft_et_grass_ir",
                               lpjmlversion = lpjml, climatetype = climatetype,
                               monthly = FALSE,
                               aggregate = FALSE)[, selectyears, "irrigated"]
  # rainfed grass ET in rainfed growing period of crop
  grperRainfed <- calcOutput("LPJmLHarmonize", subtype = "cropsRF:cft_et_grass_rf",
                             lpjmlversion = lpjml, climatetype = climatetype,
                             monthly = FALSE,
                             aggregate = FALSE)[, selectyears, "rainfed"]

  ########################
  ### Data preparation ###
  ########################
  # Annual grass ET
  grassETannual <- mbind(yearlyIrrigated, yearlyRainfed)
  # Grass ET in respective growing period of crop
  grassETgrper <- mbind(grperIrrigated, grperRainfed)

  ### To Do: check ordering of dimensions and check which ordering is expected in follow-up functions

  # Name dimensions
  getSets(grassETannual) <- c("x", "y", "iso", "year", "crop", "irrigation")
  getSets(grassETgrper)  <- c("x", "y", "iso", "year", "crop", "irrigation")

  ##############
  ### Return ###
  ##############
  unit        <- "tDM per ha"
  description <- "irrigated and rainfed evapotranspiration of grass"

  if (season == "mainSeason") {

    out         <- grassETgrper
    description <- paste0(description, " in growing season of LPJmL")

  } else if (season == "wholeYear") {

    out         <- grassETannual
    description <- paste0(description, "in the entire year")

  } else {
    stop("Please specify output to be returned by function calcGrasset:
         mainSeason or wholeYear")
  }

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("calcGrassET produced NA values")
  }
  if (any(out < 0)) {
    stop("calcGrassET produced negative values")
  }
  out[out < 0] <- 0

  if (any((grassETannual - grassETgrper) < 0)) {
    warning("Annual grass ET < grass ET in growing period")
    ### To Do (double-check with Jens): Are negatives ok here?
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
