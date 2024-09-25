#' @title       calcBlueWaterConsumption
#' @description This function calculates consumptive blue water use for the whole year based on
#'              LPJmL blue water consumption of crops and the difference between rainfed and irrigated
#'              evapotranspiration of grass
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model (e.g., "MRI-ESM2-0:ssp370")
#'                      or historical baseline (e.g., "GSWP3-W5E5:historical")
#' @param fallowFactor  Factor determining water requirement reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#' @param output        output to be returned by the function: combination of
#'                      crop type ("crops" or "grass") and
#'                      season ("main" (LPJmL growing period), "year" (entire year)),
#'                      separated by ":"
#'                      ("crops:main", "crops:year", "grass:main", "grass:year")
#' @param areaMask      Multicropping area mask to be used
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification
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
                                     fallowFactor = 0.75, areaMask,
                                     output) {

  # Read in input data already time-smoothed and for climate scenarios harmonized to the baseline
  if (grepl("historical", climatetype)) {
    # Baseline is only smoothed (not harmonized)
    stage <- "smoothed"
  } else {
    # Climate scenarios are harmonized to baseline
    stage <- "harmonized2020"
  }

  # Crop mapping
  lpj2mag <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral",
                            where = "mappingfolder")
  kcr     <- lpj2mag$MAgPIE
  kcr     <- kcr[kcr != "pasture"]

  ####################
  ### Read in data ###
  ####################
  # Crop blue water consumption in growing period (main season)
  bwc1st <- collapseNames(setYears(calcOutput("LPJmL_new", subtype = "cwater_b",
                                              version = lpjml[["crop"]], climatetype = climatetype,
                                              stage = stage, years = selectyears,
                                              aggregate = FALSE),
                                   selectyears)[, , "irrigated"])
  getSets(bwc1st)["d3.1"] <- "crop"
  # Drop "other" category from LPJmL (not part of crop mapping)
  bwc1st <- bwc1st[, , "others", invert = TRUE]

  # Grass ET in the entire year (main + off season) (in m^3/ha)
  grassETannual <- setYears(calcOutput("GrassET", season = "wholeYear",
                                       lpjml = lpjml, climatetype = climatetype,
                                       selectyears = selectyears,
                                       aggregate = FALSE),
                            selectyears)

  # Grass ET in the growing period of LPJmL by crop (main season) (in m^3/ha)
  grassETgrper  <- setYears(calcOutput("GrassET", season = "mainSeason",
                                       lpjml = lpjml, climatetype = climatetype,
                                       selectyears = selectyears,
                                       aggregate = FALSE),
                            selectyears)

  # Extract croplist and order
  crops <- getItems(grassETannual, dim = "crop")

  ####################
  ### Calculations ###
  ####################
  # Delta ET (irrigated ET - rainfed ET) as proxy for blue water consumption (BWC)
  # of grass throughout the whole year
  annualBWCgrass <- collapseNames(grassETannual[, , "irrigated"]) -
    collapseNames(grassETannual[, , "rainfed"]) # To Do: transpiration instead of ET
  annualBWCgrass[annualBWCgrass < 0] <- 0

  # Delta ET (irrigated ET - rainfed ET) as proxy for BWC
  # of grass in growing period
  grperBWCgrass <- collapseNames(grassETgrper[, , "irrigated"]) -
    collapseNames(grassETgrper[, , "rainfed"])

  # Off season grass BWC
  grassBWC2nd <- (annualBWCgrass - grperBWCgrass)
  grassBWC2nd[grassBWC2nd < 0] <- 0

  # Calculate grass BWC in off season
  grassBWC2nd <- grassBWC2nd

  # Relationship between derived grass BWC in growing period of crop and crop BWC
  # (Linear model with intercept at 0)
  # This is necessary because we used the difference between irrigated and rainfed ET
  # for grass, but BWC for crops
  coeff <- new.magpie(cells_and_regions = getItems(grassBWC2nd, dim = 1),
                      years = getItems(grassBWC2nd, dim = 2),
                      names = crops,
                      fill = NA)
  for (y in selectyears) {
    for (i in crops) {
      tmp <- lm(y ~ x + 0, data = data.frame(y = as.vector(bwc1st[, y, i]),
                                             x = as.vector(grperBWCgrass[, y, i])))$coefficients[1]
      coeff[, y, i] <- tmp
    }
  }

  # Crop blue water consumption in off season
  bwc2nd   <- grassBWC2nd * coeff
  # Add missing crops
  # Note: betr and begr are perennials and do not have additional water requirements
  #       outside the main growing season.
  missingCrops <- new.magpie(cells_and_regions = getItems(bwc2nd, dim = 1),
                             years = getItems(bwc2nd, dim = 2),
                             names = c("betr", "begr"),
                             fill = 0)
  getSets(missingCrops) <- getSets(bwc2nd)
  bwc2nd <- mbind(bwc2nd, missingCrops)

  # Transformation from lpj to kcr crops
  bwc2nd <- toolAggregate(bwc2nd, lpj2mag, from = "LPJmL",
                          to = "MAgPIE", dim = "crop", partrel = TRUE)[, , kcr]
  bwc1st <- toolAggregate(bwc1st, lpj2mag, from = "LPJmL",
                          to = "MAgPIE", dim = "crop", partrel = TRUE)[, , kcr]
  # The MAgPIE perennial crop "oilpalm" is grown throughout the whole year
  # but proxied with an LPJmL crop with seasonaility ("groundnut").
  # Therefore, both single and multiple cropping blue water consumption has be adjusted
  if (lpj2mag$LPJmL[lpj2mag$MAgPIE == "oilpalm"] == "groundnut") {
    bwc1st[, , "oilpalm"] <- bwc1st[, , "oilpalm"] + bwc2nd[, , "oilpalm"]
  }

  ##############
  ### Return ###
  ##############
  description <- "Blue water consumption of "
  unit        <- " m^3/ha per year"

  if (output == "crops:main") {
    # main season BWC for crops (single cropping case)
    out         <- bwc1st[, , kcr]
    description <- paste0(description, "crops in LPJmL growing period")

  } else if (output == "grass:main") {

    out         <- grperBWCgrass
    description <- paste0(description, "grass in LPJmL growing period of crops")

  } else if (output == "crops:year") {

    # Water requirements for multiple cropping case are only calculated for areas
    # where multiple cropping is possible in case of irrigation
    suitMC <- collapseNames(calcOutput("MulticroppingCells", sectoral = "kcr",
                                       scenario = "potential:endogenous",
                                       selectyears = selectyears,
                                       lpjml = lpjml, climatetype = climatetype,
                                       aggregate = FALSE)[, , "irrigated"][, , kcr])

    # Special case: current multicropping according to LandInG
    if (grepl(pattern = "actual", x = areaMask)) {

      # Cropping intensity
      ci <- collapseNames(calcOutput("MulticroppingIntensity", sectoral = "kcr",
                                     scenario = strsplit(areaMask, split = ":")[[1]][2],
                                     selectyears = selectyears,
                                     aggregate = FALSE)[, , "irrigated"][, , kcr])
      # Share of area that is multicropped
      shrMC <- (ci - 1)

    } else {

      # For potential case, the whole area is fully multicropped
      shrMC <- suitMC
      shrMC[, , ] <- 1

    }

    # Total blue water consumption considering multiple cropping suitability
    # (and if applicable share that is multiple cropped)
    bwcTotal <- bwc1st + bwc2nd * fallowFactor * shrMC * suitMC

    # whole year BWC for crops (multiple cropping case)
    out         <- bwcTotal[, , kcr]
    description <- paste0(description, "crops throughout the entire year")

  } else if (output == "grass:year") {

    out         <- annualBWCgrass
    description <- paste0(description, "grass throughout the entire year")

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
    # ToDo: Change to stop() when LPJmL runs are ready and smoothing can be activated
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
