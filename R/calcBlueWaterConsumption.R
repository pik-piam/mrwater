#' @title       calcBlueWaterConsumption
#' @description This function calculates consumptive blue water use for the whole year based on
#'              LPJmL blue water consumption of crops and the difference between rainfed and irrigated
#'              evapotranspiration of grass
#'
#' @param selectyears   Years to be returned
#' @param iniyear       Initialization year for filtering rules of data for regression
#'                      that determines second season yield
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model (e.g., "MRI-ESM2-0:ssp370")
#'                      or historical baseline (e.g., "GSWP3-W5E5:historical")
#' @param fallowFactor  Factor determining water requirement reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#' @param season        Season to be returned: "main" (LPJmL growing period),
#'                      "year" (entire year))
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
calcBlueWaterConsumption <- function(selectyears, iniyear,
                                     lpjml, climatetype,
                                     fallowFactor = 0.75, areaMask,
                                     season) {
  # Crop mapping
  lpj2mag <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mrlandcore")
  kcr <- setdiff(lpj2mag$MAgPIE, "pasture")

  # Read in data
  bwc1st <- calcOutput("BlueWaterConsumptionMain",
                       selectyears = selectyears,
                       lpjml = lpjml,
                       climatetype = climatetype,
                       aggregate = FALSE)
  bwc2nd <- calcOutput("BlueWaterConsumptionOff",
                       selectyears = selectyears,
                       iniyear = iniyear,
                       lpjml = lpjml,
                       climatetype = climatetype,
                       aggregate = FALSE)

  # Transformation from lpj to kcr crops
  bwc2nd <- toolAggregate(bwc2nd, lpj2mag,
                          from = "LPJmL5", to = "MAgPIE",
                          dim = "crop", partrel = TRUE)[, , kcr]
  bwc1st <- toolAggregate(bwc1st, lpj2mag,
                          from = "LPJmL5", to = "MAgPIE",
                          dim = "crop", partrel = TRUE)[, , kcr]
  # The MAgPIE perennial crop "oilpalm" is grown throughout the whole year
  # but proxied with an LPJmL crop with seasonality ("groundnut").
  # Therefore, both single and multiple cropping blue water consumption has to be adjusted
  # by assigning both seasons water requirements to this crop
  if (lpj2mag$LPJmL5[lpj2mag$MAgPIE == "oilpalm"] == "groundnut") {
    bwc1st[, , "oilpalm"] <- bwc1st[, , "oilpalm"] + bwc2nd[, , "oilpalm"]
  }

  ##############
  ### Return ###
  ##############
  description <- "Blue water consumption of "
  unit <- " m^3/ha per year"

  if (season == "main") {
    # main season BWC for crops (single cropping case)
    out <- bwc1st[, , kcr]
    description <- paste0(description, "crops in LPJmL growing period")

  } else if (season == "year") {
    # Water requirements for multiple cropping case are only returned for areas
    # where multiple cropping is possible in case of irrigation
    # Note: for the selected years since it can change over time
    suitMC <- collapseNames(calcOutput("MulticroppingCells",
                                       sectoral = "kcr",
                                       scenario = "potential:endogenous",
                                       selectyears = selectyears,
                                       lpjml = lpjml, climatetype = climatetype,
                                       aggregate = FALSE)[, , "irrigated"][, , kcr])

    # Special case: current multicropping according to LandInG
    if (grepl(pattern = "actual", x = areaMask)) {
      # Cropping intensity
      ci <- collapseNames(calcOutput("MulticroppingIntensity",
                                     sectoral = "kcr",
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
    out <- bwcTotal[, , kcr]
    description <- paste0(description, "crops throughout the entire year")

  } else {
    stop("Please select valid season argument for calcBlueWaterConsumption:
         `main`, `year`")
  }

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("calcBlueWaterConsumption produced NA irrigation water requirements")
  }
  if (any(out < 0)) {
    stop("calcBlueWaterConsumption produced negative irrigation water requirements")
  }

  return(list(x = out,
              weight = NULL,
              unit = unit,
              description = description,
              isocountries = FALSE))
}
