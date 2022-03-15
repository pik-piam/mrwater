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
  # Cells that are suitable for multiple cropping under irrigated/rainfed conditions
  suitMC <- setYears(collapseNames(calcOutput("MulticroppingYieldIncrease", output = "multicroppingSuitability",
                                  lpjml = lpjml["crop"],
                                  climatetype = "GSWP3-W5E5:historical", ### ToDo: Switch to flexible climatetype argument (once LPJmL runs are ready)
                                  selectyears = selectyears,
                                  aggregate = FALSE)[, , "irrigated"]),
                     selectyears)

  # Grass ET in the entire year (main + off season) (in m^3/ha)
  grassETannual <- setYears(calcOutput("GrassET", season = "wholeYear",
                                       lpjml = lpjml["crop"], climatetype = climatetype,
                                       selectyears = selectyears, aggregate = FALSE),
                            selectyears)

  # Grass ET in the growing period of LPJmL (main season) (in m^3/ha)
  grassETgrper  <- setYears(calcOutput("GrassET", season = "mainSeason",
                                       lpjml = lpjml["crop"], climatetype = climatetype,
                                       selectyears = selectyears, aggregate = FALSE),
                            selectyears)

  # Add missing crops (betr, begr, mgrass) [Note: perennials]
  missingCrops <- new.magpie(cells_and_regions = getItems(suitMC, dim = 1),
                             years = getItems(suitMC, dim = 2),
                             names = c("betr.irrigated", "betr.rainfed",
                                       "begr.irrigated", "begr.rainfed",
                                       "mgrass.irrigated", "mgrass.rainfed"),
                             fill = 0)
  getSets(missingCrops) <- getSets(grassETannual)
  grassETannual         <- mbind(grassETannual, missingCrops)
  grassETgrper          <- mbind(grassETgrper, missingCrops)

  # Crop blue water consumption in growing period (main season)
  bwc1st <- collapseNames(setYears(calcOutput("LPJmL_new", subtype = "cwater_b",
                                version = lpjml["crop"], climatetype = climatetype,
                                stage = "smoothed", years = selectyears,
                                aggregate = FALSE),
                     selectyears)[, , "irrigated"])
  getSets(bwc1st)["d3.1"] <- "crop"
  # Drop "other" category from LPJmL (not part of crop mapping)
  bwc1st <- bwc1st[, , "others", invert = TRUE]


  ####################
  ### Calculations ###
  ####################
  # Delta ET (irrigated ET - rainfed ET) as proxy for blue water consumption (BWC)
  # of grass throughout the whole year
  annualBWCgrass <- collapseNames(grassETannual[, , "irrigated"]) -
                     collapseNames(grassETannual[, , "rainfed"])

  # Delta ET (irrigated ET - rainfed ET) as proxy for BWC
  # of grass in growing period
  grperBWCgrass <- collapseNames(grassETgrper[, , "irrigated"]) -
                    collapseNames(grassETgrper[, , "rainfed"])

  # Off season grass BWC
  grassBWC2nd <- (annualBWCgrass - grperBWCgrass)
  if (any(grassBWC2nd < 0)) {
    warning("Annual grass BWC < grass BWC in growing period. This may happen
            when using raw rather than smoothed LPJmL inputs due to growing
            periods that can span over two years. It should, however, even out
            when time smoothing is applied.
            Negatives are set to 0.")
    # ToDo: adjust argument when LPJmL runs are ready such that stop() when smoothing activated and just warning with raw data
    # ToDo: Check whether same where annual GPP < grper GPP
    grassBWC2nd[grassBWC2nd < 0] <- 0
  }

  # Calculate grass BWC in off season where multiple cropping is suitable
  grassBWC2nd <- grassBWC2nd * fallowFactor * suitMC

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
    stop("calcBlueWaterConsumption produced NA irrigation water requirements")
  }
  if (any(out < 0)) {
    warning("calcBlueWaterConsumption produced negative irrigation water requirements")
    #ToDo: Change to stop() when LPJmL runs are ready and smoothing can be activated
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
