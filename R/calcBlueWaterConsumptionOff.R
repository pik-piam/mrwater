#' @title       calcBlueWaterConsumptionOff
#' @description This function calculates consumptive blue water use for the whole year based on
#'              LPJmL blue water consumption of crops and the difference between rainfed and irrigated
#'              evapotranspiration of grass
#'
#' @param selectyears   Years to be returned
#' @param iniyear       Initialization year for filtering rules of data for regression
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model (e.g., "MRI-ESM2-0:ssp370")
#'                      or historical baseline (e.g., "GSWP3-W5E5:historical")
#' @param interim       Interim output, i.e. the inputs to the blue water consumption regression
#'                      (TRUE:bconsCrop or TRUE:bconsGrass).
#'                      This is optional and only required for visualization purposes.
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("BlueWaterConsumptionOff", aggregate = FALSE)
#' }
#'
#' @importFrom magclass getItems collapseNames
#' @importFrom madrat calcOutput getFromComment
#' @importFrom stats lm coef

calcBlueWaterConsumptionOff <- function(selectyears, iniyear,
                                        lpjml, climatetype,
                                        interim = "FALSE") {
  ####################
  ### Read in data ###
  ####################
  # crop blue water consumption in main irrigated growing period (in m^3/ha)
  bwc1st <- calcOutput("BlueWaterConsumptionMain", selectyears = selectyears,
                       lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
  unit <- getFromComment(bwc1st, "unit")
  # crop evapotranspiration under irrigated conditions in irrigated season
  etCropIRgrper <- calcOutput("Evapotranspiration", runtype = "cft:ir",
                              lpjml = lpjml, climatetype = climatetype,
                              selectyears = selectyears,
                              aggregate = FALSE)
  # crop evapotranspiration without irrigation in irrigated season
  etCropNOIRgrper <- calcOutput("Evapotranspiration", runtype = "cft:noir",
                                lpjml = lpjml, climatetype = climatetype,
                                selectyears = selectyears,
                                aggregate = FALSE)
  # grass evapotranspiration under irrigated conditions in irrigated season
  ### To Do: will be replaced with calcGrassET
  etGrassIRgrper <- calcOutput("Evapotranspiration",
                               lpjml = lpjml, climatetype = climatetype,
                               runtype = "grass:ir",
                               selectyears = selectyears,
                               aggregate = FALSE)
  # grass evapotranspiration without irrigation in irrigated season
  etGrassNOIRgrper <- calcOutput("Evapotranspiration",
                                 lpjml = lpjml, climatetype = climatetype,
                                 runtype = "grass:noir",
                                 selectyears = selectyears,
                                 aggregate = FALSE)
  # yearly grass evapotranspiration (grassland cft grows throughout the whole year)
  etGrassIRyear <- collapseNames(etCropIRgrper[, , "grassland"])
  etGrassNOIRyear <- collapseNames(etCropNOIRgrper[, , "grassland"])

  ##########################
  ### Data for filtering ###
  ##########################
  # To ensure that the sample size doesn't change over the time series,
  # filtering data is read in for iniyear only

  # Multiple cropping suitability under irrigated conditions
  mcSuit <- collapseNames(calcOutput("MulticroppingSuitability", selectyears = iniyear,
                                     lpjml = lpjml, climatetype = climatetype,
                                     suitability = "endogenous", sectoral = "lpj",
                                     aggregate = FALSE)[, , "irrigated"])
  mcSuit <- toolHoldConstant(add_dimension(mcSuit, dim = 3.1, add = "system",
                                           nm = c("drip", "sprinkler", "surface")),
                             selectyears)
  getSets(mcSuit) <- c("x", "y", "iso", "year", "system", "crop")

  # Off-season yields
  yldSingle <- setYears(calcOutput("YieldsLPJmL", selectyears = iniyear,
                                   lpjml = lpjml, climatetype = climatetype,
                                   multicropping = FALSE,
                                   aggregate = FALSE)[, , "irrigated"],
                        iniyear)
  yldMultiple <- setYears(calcOutput("YieldsLPJmL", selectyears = iniyear,
                                     lpjml = lpjml, climatetype = climatetype,
                                     multicropping = "TRUE:potential:endogenous",
                                     aggregate = FALSE)[, , "irrigated"],
                          iniyear)
  yldOffSeason <- collapseNames(yldMultiple - yldSingle)
  yldOffSeason <- toolHoldConstant(add_dimension(yldOffSeason, dim = 3.1, add = "system",
                                                 nm = c("drip", "sprinkler", "surface"))[, , getItems(mcSuit, dim = 3)],
                                   selectyears)
  getSets(yldOffSeason) <- c("x", "y", "iso", "year", "system", "crop")


  ####################
  ### Calculations ###
  ####################
  # crop blue water consumption in main growing season
  bconsCrop <- bwc1st
  # grass blue water consumption in main growing season
  bconsGrass <- collapseNames(etGrassIRgrper - etGrassNOIRgrper)
  bconsGrass <- add_dimension(bconsGrass, dim = 3.1, add = "system",
                              nm = c("drip", "sprinkler", "surface"))
  getSets(bconsGrass) <- c("x", "y", "iso", "year", "system", "crop")

  # crop lists
  missingCrps <- setdiff(getItems(bconsCrop, dim = "crop"), getItems(bconsGrass, dim = "crop"))
  lpj         <- intersect(getItems(bconsCrop, dim = "crop"), getItems(bconsGrass, dim = "crop"))

  # select crops
  bconsCrop    <- bconsCrop[, , lpj]
  bconsGrass   <- bconsGrass[, , lpj]
  mcSuit       <- mcSuit[, , lpj]
  yldOffSeason <- yldOffSeason[, , lpj]

  # filtering
  naCells <- bconsCrop
  naCells[, , ] <- 0
  # where no multicropping suitability under irrigated conditions
  naCells[mcSuit != 1] <- 1
  # where no yield increase through multiple cropping
  naCells[yldOffSeason <= 0] <- 1
  # apply filtering
  bconsCrop[naCells == 1] <- NA
  bconsGrass[naCells == 1] <- NA

  if (any(naCells[, , "drip"] != naCells[, , "sprinkler"])) {
    stop("There must be a dimension mismatch in the filtering in calcBlueWaterConsumptionOff")
  }

  # Regression (linear fit between crop blue water consumption
  # by irrigation system and grass evapotranspiration) to derive
  # coefficient used to derive off-season blue water consumption
  # Dependent variable (y): crop blue water consumption for given system in main season
  # Independent variable (x): delta grass ET in irrigated growing period of respective crop
  fit <- toolBWCregression(y = bconsCrop, x = bconsGrass)

  ##JENS: bconsGrass can have negative values. Should I set them to zero before the regression?
  # Note: the negative values of bconsCrop are already set to 0 in calcBlueWaterConsumptionMain

  # grass blue water consumption in the entire year
  bconsGrassYr <- collapseNames(etGrassIRyear) - collapseNames(etGrassNOIRyear)

  # set negative values to zero:
  #bconsGrassYr[bconsGrassYr < 0] <- 0
  #bconsGrass[bconsGrass < 0] <- 0

  # Second season blue water consumption of grass ("off season")
  grassBWC2nd <- bconsGrassYr - bconsGrass

  ### JENS: both bconsGrassYr and bconsGrass have negative values
  ### I'm considering to set them to 0 before making the difference and then set the yields
  ### for the second season to 0 (or the multiple cropping suitability under irrigated conditions?)
  ### Check whether bconsGrassYr and bconsGrass are negative: set to 0 (before making difference)
  ### Also: if this is the case: find handling of setting respective yields to zero as well.
  ### Note: even when setting them individually to zero, negative values can occur for grassBWC2nd and need to be corrected

  # Set negative grass BWC to 0
  grassBWC2nd[grassBWC2nd < 0] <- 0
  ### To Do: double-check with Jens!!!! (see examples below)
  ### This may lead to an over-estimation of 2nd season water demand in
  ### locations where grass doesn't really need irrigation in the second season

  # Second season blue water consumption of crop ("off season")
  bwc2nd <- fit$a + fit$b * grassBWC2nd
  # Exclude cells with low yields in second season
  bwc2nd[naCells == 1] <- 0

  # add missing crops and assign them 0 (no multiple cropping for these)
  noBWC2nd <- new.magpie(cells_and_regions = getItems(bwc2nd, dim = 1),
                         years = getItems(bwc2nd, dim = 2),
                         names = missingCrps,
                         fill = 0)
  noBWC2nd <- add_dimension(noBWC2nd, dim = 3.1, add = "system",
                            nm = c("drip", "sprinkler", "surface"))
  getSets(noBWC2nd) <- getSets(bwc2nd)
  bwc2nd <- mbind(bwc2nd, noBWC2nd)
  getSets(bwc2nd) <- c("x", "y", "iso", "year", "system", "crop")

  ##############
  ### Checks ###
  ##############
  if (any(bwc2nd[, , c("sugarcane", "biomass tree", "biomass grass")] != 0)) {
    stop("second season blue water consumption for perennials should be 0")
  }
  if (any(is.na(bwc2nd))) {
    stop("calcBlueWaterConsumptionOff produced NA values")
  }
  if (any(bwc2nd < 0)) {
    # warning only if more than 1% are negative
    for (i in getItems(bwc2nd, dim = 3)) {
      condition <- bwc2nd[, , i]
      if (sum(condition) / sum(!condition) > 0.01) {
        stop(paste0("More than 1% of the grid cells have negative
                    blue water consumption in the off season
                    for crop ", i, ".
                    Check calcBlueWaterConsumptionOff!"))
      }
    }
  }
  # Correction of negative values
  bwc2nd[bwc2nd < 0] <- 0

  # Check system blue water consumption
  if (any(bwc2nd[, , "sprinkler"] < bwc2nd[, , "surface"])) {
    stop(paste0("Problem in calcBlueWaterConsumptionOff: ",
                "Sprinkler should always have greater blue water consumption than surface."))
  }
  if (any(bwc2nd[, , "surface"] < bwc2nd[, , "drip"])) {
    stop(paste0("Problem in calcBlueWaterConsumptionOff: ",
                "Surface should always have greater blue water consumption than drip"))
  }
  ### Jens: These checks fail.
  ### For the coefficients (a, b), everything is fine,
  ### but grassBWC2nd is negative and therefore order changes.
  ### Better to set grassBWC2nd already to 0 when negative?
  # Example 1:
  # bwc2nd["102p25.-5p25.IDN", "y2100", "temperate cereals"]
  # a["102p25.-5p25.IDN", "y2100", "temperate cereals"] #+
  # b["102p25.-5p25.IDN", "y2100", "temperate cereals"] #*
  # grassBWC2nd["102p25.-5p25.IDN", "y2100", "temperate cereals"]
  # Example 2:
  # bwc2nd["130p25.-12p75.AUS", "y1995", "oil crops rapeseed"]
  # a["130p25.-12p75.AUS", "y1995", "oil crops rapeseed"]
  # b["130p25.-12p75.AUS", "y1995", "oil crops rapeseed"]
  # grassBWC2nd["130p25.-12p75.AUS","y1995","oil crops rapeseed"]

  # Choose output that is returned by this function
  bool <- (as.logical(stringr::str_split(interim, ":")[[1]][1]))
  if (!bool) {
    ### Main output ###
    # Crop blue water consumption in off-season
    out <- bwc2nd
  } else {
    if (grepl("bconsCrop", interim)) {
      ### Auxilary output ###
      # Blue water consumption of crop in main season
      out <- bconsCrop
    } else if (grepl("bconsGrass", interim)) {
      ### Auxilary output ###
      # Blue water consumption of grass in main season of crop
      out <- bconsGrass
    }
  }

  return(list(x = out,
              weight = NULL,
              unit = unit,
              description = paste0("blue water consumption ",
                                   "in the off season ",
                                   "for three different irrigation systems"),
              isocountries = FALSE))
}
