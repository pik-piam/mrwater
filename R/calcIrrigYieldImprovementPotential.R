#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential
#'              through irrigation for all grid cells given a certain crop mix
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned by the function
#' @param unit          Unit of yield improvement potential to be returned and
#'                      level of price aggregation used, separated by ":".
#'                      Unit:
#'                      USD_ha (USD per hectare) for relative area return, or
#'                      USD_m3 (USD per cubic meter) for relative volumetric return;
#'                      USD for absolute return (total profit);
#'                      for relative return according to area and volume.
#'                      Price aggregation:
#'                      "GLO" for global average prices, or
#'                      "ISO" for country-level prices
#' @param iniyear       initialization year for food price and cropmix area
#' @param comagyear     if !NULL: already irrigated area is subtracted;
#'                      if NULL: total potential land area is used;
#'                      year specified here is the year of the initialization
#'                      used for cropland area initialization in calcIrrigatedArea
#' @param cropmix       Selected cropmix for which yield improvement potential
#'                      is calculated (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#'                      NULL returns all crops individually
#' @param irrigationsystem Irrigation system used: system share as in initialization year,
#'                         or drip, surface, sprinkler for full irrigation by selected system
#' @param landScen         Land availability scenario consisting of two parts separated by ":":
#'                         1. available land scenario (currCropland, currIrrig, potCropland)
#'                         2. protection scenario (WDPA, or one of the scenarios available in calcConservationPriorities,
#'                            e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
#'                            or NA for no protection).
#'                         For case of no land protection select "NA" in second part of argument
#'                         or do not specify second part of the argument
#' @param yieldcalib       If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                         Also needs specification of refYields, separated by ":".
#'                         Options: FALSE (for single cropping analyses) or
#'                                  "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                         If FALSE: uncalibrated LPJmL yields are used
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
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
#' @importFrom magclass collapseNames getNames getCells dimSums
#' @importFrom stringr str_split

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, unit,
                                               iniyear, selectyears,
                                               comagyear,
                                               cropmix, landScen, irrigationsystem,
                                               yieldcalib, multicropping) {

  if (!is.logical(multicropping)) {
    stop("calcIrrigYieldImprovementPotential requires logical
         in multicropping argument.")
  }

  # retrieve arguments
  priceAgg <- str_split(unit, pattern = ":")[[1]][2]
  unit     <- str_split(unit, pattern = ":")[[1]][1]

  # read in yield gain (in USD/ha)
  yieldGain <- calcOutput("IrrigCropYieldGain", priceAgg = priceAgg,
                          lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib,
                          multicropping = multicropping, aggregate = FALSE)

  # set negative yield gains to 0
  yieldGain[yieldGain < 0] <- 0
  # (Note: irrigation may lead to shift in growing period -> can have negative values
  # (in cropping calendar version);
  # also: under N-stress, irrigation may lead to lower yields,
  # (the latter is only relevant for limited-N-LPJmL version, default: unlimited N))

  # Selected crops
  if (!is.null(cropmix)) {

    # description of output
    description <- "Yield improvement potential through irrigation
                    given cropmix croparea share"

    # share of crop area by crop type (same for all years)
    cropareaShr <- setYears(calcOutput("CropAreaShare",
                                       iniyear = iniyear, cropmix = cropmix,
                                       aggregate = FALSE),
                            NULL)
    # extract crops and sort objects
    croplist  <- getItems(cropareaShr, dim = "crop")
    yieldGain <- yieldGain[, , croplist]

    # Unit of irrigation yield gain to be returned
    if (unit == "USD_ha") {

      # Relative yield gain (in terms of area)
      u <- "USD05 per ha"

      # average irrigation crop yield gain weighted with their croparea share
      yieldGain <- dimSums(cropareaShr * yieldGain, dim = "crop")

    } else if (unit == "USD") {

      # Absolute yield gain (total profit on land)
      u <- "mio. USD05"

      # croparea per crop given chosen land scenario (in Mha)
      # (excluding already committed agricultural areas if comagyear != NULL)
      croparea <- calcOutput("CropAreaPotIrrig",
                              selectyears = selectyears, iniyear = iniyear,
                              comagyear = comagyear,
                              cropmix = cropmix, landScen = landScen,
                              aggregate = FALSE)

      # absolute irrigation yield gain on available area
      yieldGain <- dimSums(yieldGain * croparea, dim = "crop")

    } else if (unit == "USD_m3") {

      # Relative yield gain (in terms of volume)
      u <- "USD05 per m^3"

      # Select multiple cropping argument
      if (multicropping) {
        # For case of yield gain: potential multiple croppping is the reference
        m <- "TRUE:potential:endogenous"
      } else {
        m <- FALSE
      }

      # irrigation water requirements for given irrigation system
      # per crop (in m^3 per hectare per year)
      irrigWat <- setYears(collapseNames(calcOutput("ActualIrrigWatRequirements",
                                                    selectyears = selectyears, iniyear = iniyear,
                                                    lpjml = lpjml, climatetype = climatetype,
                                                    irrigationsystem = irrigationsystem,
                                                    multicropping = m,
                                                    aggregate = FALSE)[, , "withdrawal"]),
                           selectyears)
      # Correction of small irrigWatReq: where < 10 m^3/ha (= 1mm = 1 l/m^2 = 10 m^3/ha): 0
      irrigWat[irrigWat < 10] <- 0

      # relative yield gain in terms of volume
      yieldGain <- ifelse(dimSums(cropareaShr * irrigWat, dim = "crop") > 0,
                          dimSums(cropareaShr * yieldGain, dim = "crop") /
                            dimSums(cropareaShr * irrigWat, dim = "crop"),
                          0)

    } else {
      stop("Please define unit of yield improvement potential via unit argument:
           unit (USD_ha, USD_m3, USD) and price aggregation (GLO, ISO, CONST),
           separated by :")
    }
  } else {

    description <- paste0("Yield improvement potential through irrigation ",
                          "for all different crop types")
    u           <- "USD05 per hectare"

  }

  # Check for NAs
  if (any(is.na(yieldGain))) {
    stop("Function YieldImprovementPotential produced NAs")
  }

  # Check for negatives
  if (any(round(yieldGain) < 0)) {
    stop("Function YieldImprovementPotential produced negative values")
  }

  return(list(x            = yieldGain,
              weight       = NULL,
              unit         = u,
              description  = description,
              isocountries = FALSE))
}
