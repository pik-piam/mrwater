#' @title       calcYieldsValued
#' @description This function calculates yields per crop and season
#'              valued at FAO prices (optionally per area unit (ha)
#'              or water volume unit (m3))
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
#' calcOutput("YieldsValued", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells dimSums

calcYieldsValued <- function(lpjml, climatetype, unit,
                             iniyear, selectyears, cropmix,
                             yieldcalib, multicropping) {

  # read in cellular lpjml yields for each season [in tDM/ha]
  yields    <- calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, multicropping = multicropping,
                          aggregate = FALSE)
  # extract magpie crops
  croplist  <- getNames(collapseNames(yields[, , "irrigated"][, , "first"]))

  # read in crop output price in initialization year (USD05/tDM)
  p         <- calcOutput("IniFoodPrice", datasource = "FAO", products = "kcr",
                          years = NULL, year = iniyear, aggregate = FALSE)[, , croplist]

  # Unit of yield gain to be returned
  if (unit == "tDM") {

    unit <- "tons per ha"

  } else if (unit == "USD_ha") {

    # Calculate yields per ha valued at prices (in USD05/ha)
    yields <- yields * p
    unit   <- "USD05 per ha"

  } else if (unit == "USD_m3") {

    yields <- yields * p
    unit   <- "USD05 per m^3"

    # Read in irrigation water requirements (withdrawals) for all crops
    # (in m^3 per hectare per year) [smoothed and harmonized]
    # Note: users would pay for consumption rather than withdrawals [D'Odorico et al. (2020)]
    # Note: iniyear argument only relevant for regional aggregation weight (can be ignored here)
    irrigReqWW <- collapseNames(calcOutput("ActualIrrigWatRequirements", iniyear = 2010,
                                           selectyears = selectyears, multicropping = multicropping,
                                           lpjml = lpjml, climatetype = climatetype,
                                           aggregate = FALSE)[, , "withdrawal"])
    # expand dimension such that same as yields
    irrigReqWW <- add_dimension(irrigReqWW, dim = 3.3, add = "irrigation",
                                nm = c("irrigated", "rainfed"))[, , getNames(yields)]

    # Correction of small irrigWatReq: where < 10 m^3/ha (= 1mm = 1 l/m^2 = 10 m^3/ha): 0
    irrigReqWW[irrigReqWW < 10] <- 0
    # Correction of very small yields: where < 10 USD/ha: 0
    tmp <- calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
                      lpjml = lpjml, climatetype = climatetype,
                      cropmix = cropmix, yieldcalib = yieldcalib,
                      iniyear = iniyear, selectyears = selectyears,
                      multicropping = multicropping, aggregate = FALSE)
    tmp <- add_dimension(tmp, dim = 3.1, add = "crop",
                         nm = getNames(dimSums(irrigReqWW, dim = c(3.2, 3.3))))
    tmp <- add_dimension(tmp, dim = 3.2, add = "season",
                         nm = getNames(dimSums(irrigReqWW, dim = c(3.1, 3.3))))
    tmp <- add_dimension(tmp, dim = 3.3, add = "irrigation",
                         nm = getNames(dimSums(irrigReqWW, dim = c(3.1, 3.2))))[, , getNames(yields)]
    irrigReqWW[tmp < 10] <- 0

    # yield to water ratio per season [tDM / m^3]
    yields                  <- yields / irrigReqWW
    yields[irrigReqWW <= 0] <- 0

  }

  # Check for NAs
  if (any(is.na(yields))) {
    stop("Function calcYieldsValued produced NAs")
  }

  # Check for negatives
  if (any(round(yields) < 0)) {
    stop("Function calcYieldsValued produced negative values")
  }

  return(list(x            = yields,
              weight       = NULL,
              unit         = unit,
              description  = "Yields for all different crop types and seasons",
              isocountries = FALSE))
}
