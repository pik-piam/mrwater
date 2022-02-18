#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential
#'              through irrigation for different crops
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned by the function
#' @param unit          Unit of yield improvement potential to be returned:
#'                      tDM (tons per dry matter),
#'                      USD_ha (USD per hectare) for area return, or
#'                      USD_m3 (USD per cubic meter) for volumetric return
#' @param iniyear       initialization year for food price and cropmix area
#' @param cropmix       Selected cropmix for which yield improvement potential
#'                      is calculated (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#'                      NULL returns all crops individually
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                      If FALSE: uncalibrated LPJmL yields are used
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
#' @importFrom magclass collapseNames getNames getCells dimSums

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, unit,
                                               iniyear, selectyears, cropmix,
                                               yieldcalib, multicropping) {

  # read in cellular lpjml yields for each season
  yields    <- calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, unit = unit,
                          multicropping = multicropping, cropmix = cropmix,
                          aggregate = FALSE)
  # read in yield gain
  yieldGain <- calcOutput("IrrigCropYieldGain", unit = unit,
                          lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, cropmix = cropmix,
                          multicropping = multicropping, aggregate = FALSE)

  # set negative yield gains to 0
  yieldGain[yieldGain < 0] <- 0
  # (Note: irrigation may lead to shift in growing period -> can have negative values
  # (in cropping calendar version);
  # also: under N-stress, irrigation may lead to lower yields,
  # (the latter is only relevant for limited-N-LPJmL version, default: unlimited N))

  if (multicropping) {

    # Cells are bijectively assigned according to their single and multicropping
    # potentials under irrigated and rainfed conditions
    mc   <- calcOutput("MultipleCroppingZones", layers = 2, aggregate = FALSE)
    rSiS <- new.magpie(cells_and_regions = getCells(yieldGain),
                       years = getYears(yieldGain),
                       fill = 0)
    rMiS <- rSiM <- rMiM <- rSiS
    rSiS[mc[, , "rainfed"] == 0 & mc[, , "irrigated"] == 0] <- 1
    rMiS[mc[, , "rainfed"] == 1 & mc[, , "irrigated"] == 0] <- 1
    rSiM[mc[, , "rainfed"] == 0 & mc[, , "irrigated"] == 1] <- 1
    rMiM[mc[, , "rainfed"] == 1 & mc[, , "irrigated"] == 1] <- 1

    tmp             <- yieldGain
    yieldGain       <- dimSums(yieldGain, dim = "season")
    yieldGain[, , ] <- NA

    # cells where single cropping under rainfed conditions and
    # single cropping under irrigated conditions (rSiS)
    yieldGain[rSiS == 1] <- collapseNames(tmp[, , "first"])[rSiS == 1]

    # cells where single cropping under rainfed conditions and
    # multiple cropping under irrigated conditions (rSiM)
    yieldGain[rSiM == 1] <- (collapseNames(tmp[, , "first"]) +
                             collapseNames(yields[, , "second"][, , "irrigated"]))[rSiM == 1]

    # cells where multiple cropping under rainfed conditions and
    # multiple cropping under irrigated conditions (rMiM)
    yieldGain[rMiM == 1] <- dimSums(tmp, dim = "season")[rMiM == 1]

  } else {

    # only one cropping season: off-season ("second") set to 0
    yieldGain[, , "second"] <- 0
    yieldGain <- dimSums(yieldGain, dim = "season")

  }

  # Selected crops
  if (!is.null(cropmix)) {

    # share of crop area by crop type
    cropareaShr <- calcOutput("CropAreaShare", iniyear = iniyear, cropmix = cropmix,
                              aggregate = FALSE)

    # average (rf/irr) yields over crops weighted with their croparea share
    yieldGain <- dimSums(yieldGain * cropareaShr, dim = "crop")

    # description of output
    description <- "Yield improvement potential through irrigation given cropmix croparea share"

  } else {

    description <- "Yield improvement potential through irrigation for all different crop types and seasons"

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
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
