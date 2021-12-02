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
                                               iniyear, selectyears, cropmix, yieldcalib, multicropping) {

  # yield reduction parameters (share of yield potential that can be reached in second / third season):
  yldShr2 <- 0.8
  yldShr3 <- 0.7

  # read in cellular lpjml yields [in tons/ha]
  yields   <- calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
                         iniyear = iniyear, selectyears = selectyears,
                         yieldcalib = yieldcalib, aggregate = FALSE)

  # magpie crops
  croplist <- getNames(collapseNames(yields[, , "irrigated"]))

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
    irrigReqWW <- calcOutput("IrrigWatRequirements", lpjml = lpjml,
                                   climatetype = climatetype, selectyears = selectyears, aggregate = FALSE)
    irrigReqWW <- collapseNames(irrigReqWW[, , "withdrawal"])
    irrigReqWW <- irrigReqWW[, , intersect(gsub("[.].*", "", getNames(irrigReqWW)), croplist)]

    # Read in irrigation system area initialization
    irrigationSystem <- calcOutput("IrrigationSystem", source = "Jaegermeyr", aggregate = FALSE)

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
    yields   <- dimSums(yields[, , croplist] * cropareaShr[, , croplist], dim = "MAG")

    # description of output
    description <- "Average yield improvement potential for crop types weighted cropmix croparea share"

    # Account for multicropping potential
    mc   <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)

    i1r1 <- new.magpie(cells_and_regions = getCells(yields),
                       years = getYears(yields),
                       fill = 0)
    i3r1 <- i3r2 <- i3r3 <- i2r1 <- i2r2 <- i2r3 <- i1r2 <- i1r3 <- i1r1
    i1r1[mc[, , "irrigated"] == 1 & mc[, , "rainfed"] == 1] <- 1
    i1r2[mc[, , "irrigated"] == 1 & mc[, , "rainfed"] == 2] <- 1
    i1r3[mc[, , "irrigated"] == 1 & mc[, , "rainfed"] == 3] <- 1
    i2r1[mc[, , "irrigated"] == 2 & mc[, , "rainfed"] == 1] <- 1
    i2r2[mc[, , "irrigated"] == 2 & mc[, , "rainfed"] == 2] <- 1
    i2r3[mc[, , "irrigated"] == 2 & mc[, , "rainfed"] == 3] <- 1
    i3r1[mc[, , "irrigated"] == 3 & mc[, , "rainfed"] == 1] <- 1
    i3r2[mc[, , "irrigated"] == 3 & mc[, , "rainfed"] == 2] <- 1
    i3r3[mc[, , "irrigated"] == 3 & mc[, , "rainfed"] == 3] <- 1

    # additional seasonal gains
    yieldGainAdd <- new.magpie(cells_and_regions = getCells(yields),
                                 years = getYears(yields),
                                 names = c("ss", "sd", "st", "ds", "dd", "dt", "ts", "td", "tt"),
                                 fill = 0)

    yieldGainAdd[, , "ss"] <- collapseNames(yields[, , "irrigated"]) - collapseNames(yields[, , "rainfed"])
    yieldGainAdd[, , "ds"] <- yldShr2 * collapseNames(yields[, , "irrigated"])
    yieldGainAdd[, , "dd"] <- yldShr2 * (collapseNames(yields[, , "irrigated"]) -
                                                     collapseNames(yields[, , "rainfed"]))
    yieldGainAdd[, , "ts"] <- yldShr3 * collapseNames(yields[, , "irrigated"])
    yieldGainAdd[, , "td"] <- yldShr3 * collapseNames(yields[, , "irrigated"])
    yieldGainAdd[, , "tt"] <- yldShr3 * (collapseNames(yields[, , "irrigated"]) -
                                                     collapseNames(yields[, , "rainfed"]))

    # report seasonal gains for three seasons
    yieldGain <- new.magpie(cells_and_regions = getCells(yields),
                             years = getYears(yields),
                             names = c("single", "double", "triple"),
                             fill = 0)

    if (multicropping) {

      yieldGain[, , "single"] <- collapseNames(yieldGainAdd[, , "ss"])
      yieldGain[, , "single"] <- yieldGain[, , "single"] * (1 - i1r2) * (1 - i1r3)

      yieldGain[, , "double"] <- collapseNames(yieldGainAdd[, , "ds"]) * i2r1 +
                                  collapseNames(yieldGainAdd[, , "dd"]) * i2r2 +
                                  collapseNames(yieldGainAdd[, , "dt"]) * i2r3

      yieldGain[, , "triple"] <- collapseNames(yieldGainAdd[, , "ts"]) * i3r1 +
                                  collapseNames(yieldGainAdd[, , "td"]) * i3r2 +
                                  collapseNames(yieldGainAdd[, , "tt"]) * i3r3

    } else {

      # yield gain through irrigation for each crop [in tons/ha]
      yieldGain[, , "single"] <- collapseNames(yieldGainAdd[, , "ss"])
      yieldGain[, , c("double", "triple")] <- 0

    }

  } else {

    if (!multicropping) {

      yieldGain   <- collapseNames(yields[, , "irrigated"]) - collapseNames(yields[, , "rainfed"])
      description <- "Yield improvement potential by irrigation for all different crop types"

    } else {
      stop("Yield gain potential for different crop types not implemented for
           case of multicropping")
    }
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
