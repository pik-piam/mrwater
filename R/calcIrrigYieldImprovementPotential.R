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
#' @importFrom magclass collapseNames getNames getCells getSets dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, unit,
                                               iniyear, selectyears, cropmix,
                                               yieldcalib, multicropping) {

  # read in cellular lpjml yields for each season [in tons/ha]
  yields   <- calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
                         iniyear = iniyear, selectyears = selectyears,
                         yieldcalib = yieldcalib, multicropping = multicropping,
                         aggregate = FALSE)

  # magpie crops
  croplist <- getNames(collapseNames(yields[, , "irrigated"][, , "first"]))

  # read in crop output price in initialization year (USD05/tDM)
  p        <- calcOutput("IniFoodPrice", datasource = "FAO", products = "kcr",
                         years = NULL, year = iniyear, aggregate = FALSE)

  if (unit == "tDM") {

    unit <- "tons per ha"

  } else if (unit == "USD_ha") {

    croplist <- intersect(croplist, getNames(p))

    # Calculate monetary yield gain (in USD05/ha)
    yields   <- yields[, , croplist] * p[, , croplist]
    unit     <- "USD05 per ha"

  } else if (unit == "USD_m3") {

    croplist <- intersect(croplist, getNames(p))

    # Calculate monetary yield gain (in USD05/ha)
    yields   <- yields[, , croplist] * p[, , croplist]

    # Read in irrigation water requirements (withdrawals) for all crops
    # (in m^3 per hectare per year) [smoothed and harmonized]
    # Note: users would pay for consumption rather than withdrawals [D'Odorico et al. (2020)]
    irrigReqWW <- calcOutput("IrrigWatRequirements", selectyears = selectyears,
                             lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
    irrigReqWW <- collapseNames(irrigReqWW[, , "withdrawal"])
    irrigReqWW <- irrigReqWW[, , intersect(gsub("[.].*", "", getNames(irrigReqWW)), croplist)]

    # Read in irrigation system area initialization
    irrigationSystem <- calcOutput("IrrigationSystem", datasource = "Jaegermeyr", aggregate = FALSE)

    # Calculate irrigation water requirements
    irrigWatReq  <- dimSums((irrigationSystem[, , ] * irrigReqWW[, , ]), dim = 3.1)
    irrigWatReq  <- irrigWatReq[, , croplist]

    # Correction of small irrigWatReq: where < 10 m^3/ha (= 1mm = 1 l/m^2 = 10 m^3/ha): 0
    irrigWatReq[irrigWatReq < 10] <- 10
    # Correction of very small yields: where < 10 USD/ha: 0
    tmp <- calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
                      lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
                      iniyear = iniyear, selectyears = selectyears, yieldcalib = yieldcalib,
                      multicropping = multicropping, aggregate = FALSE)
    irrigWatReq[dimSums(tmp, dim = 3) < 10] <- 0
    # Yields in USD/m^3
    yields           <- yields / irrigWatReq
    yields[irrigWatReq <= 0] <- 0
    unit             <- "USD05 per m^3"

  }

  # Selected crops

  if (!is.null(cropmix)) {

    # share of crop area by crop type
    cropareaShr <- calcOutput("CropAreaShare", iniyear = iniyear, cropmix = cropmix,
                              aggregate = FALSE)

    # average (rf/irr) yields over crops weighted with their croparea share
    croplist <- intersect(croplist, getNames(cropareaShr))
    yields   <- dimSums(yields[, , croplist] * cropareaShr[, , croplist], dim = "crop")

    # description of output
    description <- "Average yield improvement potential for crop types weighted cropmix croparea share"

    # Cells are bijectively assigned according to their single and multicropping
    # potentials under irrigated and rainfed conditions
    mc   <- calcOutput("MultipleCroppingZones", layers = 2, aggregate = FALSE)
    rSiS <- new.magpie(cells_and_regions = getCells(yields),
                       years = getYears(yields),
                       fill = 0)
    rMiS <- rSiM <- rMiM <- rSiS
    rSiS[mc[, , "rainfed"] == 0 & mc[, , "irrigated"] == 0] <- 1
    rMiS[mc[, , "rainfed"] == 1 & mc[, , "irrigated"] == 0] <- 1
    rSiM[mc[, , "rainfed"] == 0 & mc[, , "irrigated"] == 1] <- 1
    rMiM[mc[, , "rainfed"] == 1 & mc[, , "irrigated"] == 1] <- 1

    # yield gain in main season:
    yieldGain <- (collapseNames(yields[, , "first"][, , "irrigated"]) -
                    collapseNames(yields[, , "first"][, , "rainfed"]))

    # additional yield gains through irrigation
    if (multicropping) {

      # cells where single cropping under rainfed conditions and
      # multiple cropping under irrigated conditions (rSiM)
      tmp <- yieldGain + collapseNames(yields[, , "second"][, , "irrigated"])
      yieldGain[rSiM == 1] <- tmp[rSiM == 1]
      # cells where multiple cropping under rainfed conditions and
      # multiple cropping under irrigated conditions (rMiM)
      tmp <- yieldGain + (collapseNames(yields[, , "second"][, , "irrigated"]) -
                            collapseNames(yields[, , "second"][, , "rainfed"]))
      yieldGain[rMiM == 1] <- tmp[rMiM == 1]
    }

  } else {

    yieldGain   <- collapseNames(yields[, , "irrigated"]) - collapseNames(yields[, , "rainfed"])
    description <- "Yield improvement potential through irrigation for all different crop types (optionally: by season)"

  }

  # set negative yield gains to 0
  yieldGain[yieldGain < 0] <- 0
  # (Note: irrigation may lead to shift in growing period -> can have negative values
  # (in cropping calendar version);
  # also: under N-stress, irrigation may lead to lower yields,
  # (the latter is only relevant for limited-N-LPJmL version, default: unlimited N))

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
