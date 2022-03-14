#' @title       calcBlueWaterConsumption
#' @description This function calculates consumptive blue water use for the whole year based on
#'              LPJmL blue water consumption of crops and the difference between rainfed and irrigated
#'              evapotranspiration of grass
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model or historical baseline "GSWP3-W5E5:historical"
#' @param fallowFactor  Factor determining water requirement reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#' @param output        output to be returned by the function: combination of
#'                      crop type ("crops" or "grass") and
#'                      season ("main" (LPJmL growing period), "year" (entire year)),
#'                      separated by ":"
#'                      ("crops:main", "crops:year", "grass:main", "grass:year")
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("BlueWaterConsumption", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames setYears getSets
#' @importFrom madrat calcOutput
#' @importFrom stats lm

calcBlueWaterConsumption <- function(selectyears, lpjml, climatetype,
                                     fallowFactor = 0.75, output) {

  ####################
  ### Read in data ###
  ####################
  # Grass ET in the entire year (main + off season) (in m^3/ha)
  grassETannual <- calcOutput("GrassET", season = "wholeYear",
                               lpjml = lpjml, climatetype = climatetype,
                               selectyears = selectyears, aggregate = FALSE)

  # Grass ET in the growing period of LPJmL (main season) (in m^3/ha)
  grassETgrper  <- calcOutput("GrassET", season = "mainSeason",
                               lpjml = lpjml, climatetype = climatetype,
                               selectyears = selectyears, aggregate = FALSE)

  # Cells that are suitable for multiple cropping under irrigated/rainfed conditions
  suitMC <- calcOutput("MulticroppingYieldIncrease", output = "multicroppingSuitability",
                       lpjml = lpjml["crop"],
                       climatetype = "GSWP3-W5E5:historical", ###ToDo: Switch to flexible climatetype argument (once LPJmL runs are ready)
                       selectyears = selectyears,
                       aggregate = FALSE)

  # Crop blue water consumption in growing period (main season)
  bwc1st <- collapseNames(setYears(calcOutput("LPJmL_new", subtype = "cwater_b",
                                version = lpjml["crop"], climatetype = climatetype,
                                stage = "smoothed", years = selectyears,
                                aggregate = FALSE),
                     selectyears)[, , "irrigated"])
  getSets(bwc1st)["d3.1"] <- "crop"


  ####################
  ### Calculations ###
  ####################
  # Delta ET (irrigated ET - rainfed ET) as proxy for blue water consumption (BWC)
  # of grass throughout the whole year
  annualBWCgrass <- grassETannual[, , "irrigated"] - grassETannual[, , "rainfed"]

  # Delta ET (irrigated ET - rainfed ET) as proxy for BWC
  # of grass in growing period
  grperBWCgrass <- grassETgrper[, , "irrigated"] - grassETgrper[, , "rainfed"]

  # Calculate grass BWC in off season where multiple cropping is suitable
  grassBWC2nd <- (annualBWCgrass - grperBWCgrass) * fallowFactor * suitMC

  # Relationship between derived grass BWC in growing period of crop and crop BWC
  # (Linear model with intercept at 0)
  coeff <- lm(y ~ x + 0, data = data.frame(y = as.vector(bwc1st),
                                           x = as.vector(grperBWCgrass)))$coefficients[1]

  # crop blue water consumption in off season
  bwc2nd   <- coeff * grassBWC2nd
  bwcTotal <- bwc1st + bwc2nd


  ##############
  ### Return ###
  ##############
  description <- "Blue water consumption of "
  unit        <- " m^3/ha per year"

  if (output == "crops:main") {

    # main season BWC for crops (single cropping case)
    out         <- bwc1st
    description <- paste0(description, " crops in LPJmL growing period")

  } else if (output == "grass:main") {

    out         <- grperBWCgrass
    description <- paste0(description, " grass in LPJmL growing period of crops")


  } else if (output == "crops:year") {

    out         <- bwcTotal
    description <- paste0(description, " crops throughout the entire year")


  } else if (output == "grass:year") {

    out         <- annualBWCgrass
    description <- paste0(description, " grass throughout the entire year")


  } else {
    stop("Please select valid output type for calcBlueWaterConsumption:
         crops:main, grass:main, crops:year, grass:year")
  }

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(out < 0)) {
    stop("produced negative irrigation water requirements")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
