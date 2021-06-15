#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential of irrigation for different crops
#'
#' @param lpjml         LPJmL version required for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears   Years to be returned by the function
#' @param monetary      Yield improvement potential in tDM (FALSE, default) or
#'                      priced yield improvement potential in USD05 (TRUE)
#' @param iniyear       initialization year for food price and cropmix area
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#'                      NULL returns all crops individually
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or
#'                      calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                      smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                      smoothed_calib (not harmonized, but calibrated)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigYieldImprovementPotential", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells getSets dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, monetary, iniyear, selectyears, cropmix, yieldcalib, multicropping) {

  # read in cellular lpjml yields [in tons/ha]
  yields <- calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype, iniyear = iniyear, selectyears = selectyears, yieldcalib = yieldcalib, aggregate = FALSE)

  if (multicropping) {
    # read in multiple cropping zones [3 layers: single, double, triple cropping]
    mc     <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
    # correct yield potential for multicropping
    yields <- yields * mc
  }

  # yield gap (irrigated vs. rainfed) [in tons/ha]
  yield_gain <- collapseNames(yields[,,"irrigated"]) - collapseNames(yields[,,"rainfed"])
  # (Note: irrigation may lead to shift in growing period -> can have negative values; also: under N-stress, irrigation may lead to lower yields, the latter is only relevant for limited-N-LPJmL version, default: unlimited N)

  # magpie crops
  croplist <- getNames(yield_gain)

  if (monetary) {
    # Read in crop output price in initialization (USD05/tDM)
    p           <- calcOutput("IniFoodPrice", datasource = "FAO", products = "kcr", aggregate = FALSE, years = NULL, year = iniyear)
    croplist    <- intersect(croplist, getNames(p))

    # set negative yield gains to 0
    yield_gain[yield_gain < 0] <- 0

    # Calculate monetary yield gain (in USD05/ha)
    yield_gain  <- yield_gain[,,croplist] * p[,,croplist]
    unit        <- "USD05 per ha"
  } else {
    unit        <- "tons per ha"
  }

  # Selected crops
  if (!is.null(cropmix)) {

    # share of corp area by crop type
    if (length(cropmix) == 1 && grepl("hist", cropmix)) {

      # read in relevant croparea: total (irrigated + rainfed) or irrigated depending on chosen cropmix
      croparea <- setYears(calcOutput("Croparea", years = iniyear, sectoral = "kcr", cells = "lpjcell", physical = TRUE, cellular = TRUE,
                                      irrigation = TRUE, aggregate = FALSE), NULL)
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
      map                          <- toolGetMappingCoord2Country()
      getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
      names(dimnames(croparea))[1] <- "x.y.iso"
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

      if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "irrig") {
        # irrigated croparea
        croparea <- collapseNames(croparea[,,"irrigated"])
      } else if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "total") {
        # total croparea (irrigated + rainfed)
        croparea <- dimSums(croparea, dim = "irrigation")
      } else {
        stop("Please select hist_irrig or hist_total when selecting the historical cropmix")
      }

      # historical share of crop types in cropland per cell
      croparea_shr <- croparea / dimSums(croparea, dim = 3)
      # correct NAs: where no current cropland available -> representative crops (maize, rapeseed, pulses) assumed as proxy
      rep_crops    <- c("maiz", "rapeseed", "puls_pro")
      other_crops  <- setdiff(getNames(croparea), rep_crops)
      croparea_shr[,,rep_crops][dimSums(croparea, dim = 3) == 0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
      croparea_shr[,,other_crops][dimSums(croparea, dim = 3) == 0] <- 0

      # average yield gain over historical crops weighted with their croparea share
      croplist    <- intersect(croplist, getNames(croparea_shr))
      yield_gain  <- dimSums(croparea_shr[,,croplist] * yield_gain[,,croplist], dim = 3)

      description <- "Average yield improvement potential for crop types weighted with historical croparea share"

    } else {
      # Note: equal crop area share for each proxycrop assumed
      # select proxy crops
      yield_gain  <- yield_gain[,,cropmix]
      # average over proxy crops
      yield_gain  <- dimSums(yield_gain, dim = 3) / length(getNames(yield_gain))
      description <- "Average yield improvement potential for selection of crop types"
    }

  } else {
    description <- "Yield improvement potential by irrigation for all different crop types"
  }

  # Check for NAs
  if (any(is.na(yield_gain))) {
    stop("Function YieldImprovementPotential produced NAs")
  }

  return(list(
    x = yield_gain,
    weight = NULL,
    unit = unit,
    description = description,
    isocountries = FALSE))
}
