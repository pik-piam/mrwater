#' @title       calcYieldsValued
#' @description This function calculates yields per crop
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
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLanduseToolbox
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:cropIrrig" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
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

  # read in cellular lpjml yields [in tDM/ha]
  yields    <- calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, multicropping = multicropping,
                          aggregate = FALSE)
  # extract magpie crops
  croplist  <- getNames(collapseNames(yields[, , "irrigated"]))

  # read in global average crop output price averaging price fluctuations
  # around the initialization year (USD05/tDM)
  if (class(iniyear) == "character") {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }
  avgYears <- c((iniyear - 1):(iniyear + 2))
  i        <- iniyear - 2
  p        <- setYears(calcOutput("IniFoodPrice", datasource = "FAO", products = "kcr",
                                  years = NULL, year = i, aggregate = FALSE)[, , croplist], i)
  for (i in avgYears) {
    tmp <- setYears(calcOutput("IniFoodPrice", datasource = "FAO", products = "kcr",
                                years = NULL, year = i, aggregate = FALSE)[, , croplist], i)
    p   <- mbind(p, tmp)
  }
  p <- dimSums(p, dim = "Year") / length(getItems(p, dim = 2))

  # @KRISTINE: better way to average over IniFoodPrice (problem: can only read in one year at a time...)

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
                         nm = getItems(irrigReqWW, dim = "crop"))
    tmp <- add_dimension(tmp, dim = 3.2, add = "irrigation",
                         nm = getItems(irrigReqWW, dim = "irrigation"))[, , getNames(yields)]
    irrigReqWW[tmp < 10] <- 0

    # yield to water ratio [tDM / m^3]
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
              description  = "Yields for all different crop types",
              isocountries = FALSE))
}
