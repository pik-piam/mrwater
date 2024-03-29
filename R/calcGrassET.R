#' @title calcGrassET
#'
#' @description Calculates evapotranspiration (ET) of grassland
#'              under irrigated and rainfed conditions based on LPJmL inputs.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param season        "wholeYear":  grass et in the entire year (main + off season)
#'                      "mainSeason": grass etP in the crop-specific growing
#'                                    period of LPJmL (main season)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
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

  # Read in input data already time-smoothed and for climate scenarios harmonized to the baseline
  if (grepl("historical", climatetype)) {
    # Baseline is only smoothed (not harmonized)
    stage <- "smoothed"
  } else {
    # Climate scenarios are harmonized to baseline
    stage <- "harmonized2020"
  }

  ####################
  ### Read in data ###
  ####################
  # monthly irrigated grass ET
  monthlyIrrigated <- calcOutput("LPJmL_new", subtype = "met_grass_ir",
                                 years = selectyears,
                                 stage = stage,
                                 version = lpjml[["crop"]], climatetype = climatetype,
                                 aggregate = FALSE)
  # monthly irrigated grass ET
  monthlyRainfed <- calcOutput("LPJmL_new", subtype = "met_grass_rf",
                               years = selectyears,
                               stage = stage,
                               version = lpjml[["crop"]], climatetype = climatetype,
                               aggregate = FALSE)


  # irrigated grass ET in irrigated growing period of crop
  grperIrrigated <- calcOutput("LPJmL_new", subtype = "cft_et_grass_ir",
                               years = selectyears,
                               stage = stage,
                               version = lpjml[["crop"]], climatetype = climatetype,
                               aggregate = FALSE)
  # rainfed grass ET in rainfed growing period of crop
  grperRainfed <- calcOutput("LPJmL_new", subtype = "cft_et_grass_rf",
                             years = selectyears,
                             stage = stage,
                             version = lpjml[["crop"]], climatetype = climatetype,
                             aggregate = FALSE)

  ########################
  ### Data preparation ###
  ########################
  # Empty objects to be filled
  grassETannual <- grassETgrper <- new.magpie(cells_and_regions = getItems(grperIrrigated, dim = 1),
                                              years = getItems(grperIrrigated, dim = 2),
                                              names = getItems(grperIrrigated, dim = 3),
                                              fill = NA)
  # Name dimensions
  getSets(grassETannual) <- c("x", "y", "iso", "year", "crop", "irrigation")
  getSets(grassETgrper)  <- c("x", "y", "iso", "year", "crop", "irrigation")

  # Extract rainfed grass ET in rainfed growing period of crop
  grassETgrper[, , "rainfed"]   <- grperRainfed[, , "rainfed"]
  # Extract irrigated grass ET in irrigated growing period of crop
  grassETgrper[, , "irrigated"] <- grperIrrigated[, , "irrigated"]

  ### @JENS: I don't need irrigated grass et in rainfed growing period of crop, right?
  ### @JENS: I don't need rainfed grass et in irrigated growing period of crop, right?

  ####################
  ### Calculations ###
  ####################

  # Calculate annual rainfed grass ET
  grassETannual[, , "rainfed"]   <- add_dimension(dimSums(monthlyRainfed,
                                                           dim = 3),
                                                   add = "irrigation", nm = "rainfed")
  # Calculate annual irrigated grass ET
  grassETannual[, , "irrigated"] <- add_dimension(dimSums(monthlyIrrigated,
                                                           dim = 3),
                                                   add = "irrigation", nm = "irrigated")

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
  if (any((grassETannual - grassETgrper) < 0)) {
    warning("Annual grass ET < grass ET in growing period")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
