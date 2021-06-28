#' @title       calcIrrigWatValue
#' @description This function calculates the value of irrigation water
#'              (added value of water to the production process)
#'              based on yield gain, crop prices and irrigation water requirements
#'
#' @param lpjml         LPJmL version to be used
#' @param selectyears   Years to be returned
#' @param climatetype   Climate model or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Initialization year for price in price-weighted normalization
#'                      of meanpricedcroprank and yield improvement potential prices
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or
#'                      calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                      smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                      smoothed_calib
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames dimSums getYears getCells
#' @importFrom mrcommons toolGetMappingCoord2Country
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigWatValue", aggregate = FALSE)
#' }
#'
calcIrrigWatValue <- function(lpjml, selectyears, climatetype, iniyear,
                              cropmix, yieldcalib, multicropping) {
  ## Note: Methodology for calculating value of water following D'Odorico et al. (2020)

  # Read in potential yield gain per cell (USD05 per ha)
  yield_gain <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml,
                           climatetype = climatetype, selectyears = selectyears,
                           cropmix = NULL, monetary = TRUE, iniyear = iniyear,
                           yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)

  # Read in irrigation water requirement (withdrawals) (in m^3 per hectar per year) [smoothed and harmonized]
  # Note: Following D'Odorico et al. (2020), results refer to water withdrawals (because that's what one would pay for rather than for consumption)
  irrig_withdrawal <- calcOutput("IrrigWatRequirements", lpjml = lpjml,
                                 climatetype = climatetype, selectyears = selectyears, aggregate = FALSE)
  irrig_withdrawal <- collapseNames(irrig_withdrawal[, , "withdrawal"])
  irrig_withdrawal <- irrig_withdrawal[, , intersect(gsub("[.].*", "", getNames(irrig_withdrawal)), getNames(yield_gain))]

  # Read in irrigation system area initialization
  irrigation_system <- calcOutput("IrrigationSystem", source = "Jaegermeyr", aggregate = FALSE)

  # Calculate irrigation water requirements
  IWR <- dimSums((irrigation_system[, , ] * irrig_withdrawal[, , ]), dim = 3.1)
  IWR <- IWR[, , getNames(yield_gain)]

  # Correction of small yield gains and small IWR: where smaller (a) or (b) set to 0
  # (a) 1 mm = l/m^2 = 10 m^3/ha
  # (b) 10 USD /ha
  setToZero <- IWR < 10 | yield_gain < 10

  IWR[setToZero]        <- 0
  yield_gain[setToZero] <- 0

  # Calculate value of water
  watvalue <- ifelse(IWR > 0, yield_gain / IWR, 0)

  # Selected crops
  if (!is.null(cropmix)) {

    # share of corp area by crop type
    if (length(cropmix) == 1 && grepl("hist", cropmix)) {
      # historical crop mix
      # read in total (irrigated + rainfed) croparea
      croparea <- calcOutput("Croparea", years = iniyear, sectoral = "kcr", cells = "lpjcell",
                             physical = TRUE, cellular = TRUE, irrigation = TRUE, aggregate = FALSE)

      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
      map                          <- toolGetMappingCoord2Country()
      getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
      names(dimnames(croparea))[1] <- "x.y.iso"
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

      if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "irrig") {
        # irrigated croparea
        croparea <- collapseNames(croparea[, , "irrigated"])
      } else if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "total") {
        # total croparea (irrigated + rainfed)
        croparea <- dimSums(croparea, dim = "irrigation")
      } else {
        stop("Please select hist_irrig or hist_total when selecting the historical cropmix")
      }

      # historical share of crop types in cropland per cell
      croparea_shr <- croparea / dimSums(croparea, dim = 3)
      # correct NAs: where no current cropland available -> representative crops (maize, rapeseed, pulses) assumed as proxy
      rep_crops   <- c("maiz", "rapeseed", "puls_pro")
      other_crops <- setdiff(getNames(croparea), rep_crops)
      croparea_shr[, , rep_crops][dimSums(croparea, dim = 3) == 0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
      croparea_shr[, , other_crops][dimSums(croparea, dim = 3) == 0] <- 0

      # average water value over historical crops weighted with their croparea share
      watvalue <- dimSums(croparea_shr * watvalue, dim = 3)

      description <- "Average yield improvement potential for crop types weighted with historical croparea share"

    } else {
      # Note: equal crop area share for each proxycrop assumed
      watvalue    <- watvalue[, , cropmix]
      # calculate average water value over proxy crops
      watvalue    <- dimSums(watvalue, dim = 3) / length(getNames(watvalue))

      description <- "Value of irrigation water for selection of crop types"
    }

  } else {
    description <- "Value of irrigation water for all different crop types"
  }

  # Check for NAs
  if (any(is.na(watvalue))) {
    stop("Function calcIrrigWatValue produced NAs")
  }

  # Check for negatives
  if (any(round(watvalue) < 0)) {
    stop("Function calcIrrigWatValue produced negative values")
  }

  return(list(
    x            = watvalue,
    weight       = NULL,
    unit         = "USD05 per m^3",
    description  = description,
    isocountries = FALSE))
}
