#' @title       calcPotMulticroppingShare
#' @description Calculates share of currently irrigated areas that is multiple cropped.
#'              On areas where irrigation expansion takes place, full multiple cropping is
#'              assumed.
#'
#' @param lpjml             LPJmL version used
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#' @param selectyears       Years for which irrigatable area is calculated
#' @param iniyear           Initialization year for initial croparea
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          USD_ha (USD per hectare) for relative area return, or
#'                          USD_m3 (USD per cubic meter) for relative volumetric return;
#'                          USD for absolute return (total profit);
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (in USD per hectare)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param comAg             If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          (mask can be:
#'                          "none": no mask applied (only for development purposes)
#'                          "actual:total": currently multicropped areas calculated from total harvested areas
#'                                          and total physical areas per cell from LandInG
#'                          "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                          "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                          "potential:endogenous": potentially multicropped areas given
#'                                                  temperature and productivity limits
#'                          "potential:exogenous": potentially multicropped areas given
#'                                                 GAEZ suitability classification)
#'                          (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("PotMulticroppingShare", aggregate = FALSE)
#' }
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames add_dimension add_columns mbind

calcPotMulticroppingShare <- function(lpjml, climatetype,
                                      selectyears, iniyear,
                                      efrMethod, accessibilityrule,
                                      rankmethod, yieldcalib, allocationrule,
                                      gainthreshold, irrigationsystem, landScen,
                                      cropmix, comAg, fossilGW,
                                      multicropping, transDist) {


  ### Read in data ###
  # Irrigation water requirements in main season (in m^3 per ha per yr):
  watReqFirst <- calcOutput("ActualIrrigWatRequirements",
                            multicropping = FALSE,
                            selectyears = selectyears, iniyear = iniyear,
                            lpjml = lpjml, climatetype = climatetype,
                            irrigationsystem = irrigationsystem,
                            aggregate = FALSE)
  # Irrigation water requirements in the entire year under multiple cropping (in m^3 per ha per yr):
  watReqYear  <- calcOutput("ActualIrrigWatRequirements",
                            multicropping = multicropping,
                            selectyears = selectyears, iniyear = iniyear,
                            lpjml = lpjml, climatetype = climatetype,
                            irrigationsystem = irrigationsystem,
                            aggregate = FALSE)
  crops <- getItems(watReqFirst, dim = "crop")

  # Potential irrigation water use (in mio. m^3 per year):
  # This includes committed agricultural water use, multiple cropping expansion on irrigated areas,
  # fossil groundwater and additional irrigation water potential
  watPotAvl <- calcOutput("PotWater",
                          lpjml = lpjml, climatetype = climatetype,
                          selectyears = selectyears, iniyear = iniyear,
                          efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                          rankmethod = rankmethod, yieldcalib = yieldcalib,
                          allocationrule = allocationrule, gainthreshold = gainthreshold,
                          irrigationsystem = irrigationsystem, landScen = landScen,
                          cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
                          multicropping = multicropping, transDist = transDist,
                          aggregate = FALSE)
  watPotAvlWW <- collapseNames(watPotAvl[, , "wat_ag_ww"])
  watPotAvlWC <- collapseNames(watPotAvl[, , "wat_ag_wc"])
  rm(watPotAvl)

  # Multiple cropping suitability
  suitMC <- calcOutput("MulticroppingCells",
                       sectoral = "kcr",
                       scenario = "potential:endogenous",
                       selectyears = selectyears,
                       lpjml = lpjml, climatetype = climatetype,
                       aggregate = FALSE)[, , crops]
  suitMCir <- collapseNames(suitMC[, , "irrigated"])

  # Where multiple cropping is possible, full multiple cropping is assumed
  # for both irrigated and rainfed areas for the potential multicropping share
  out <- suitMC

  # For case of committed agriculture, expansion of multiple cropping
  # on irrigated areas is only possible if enough water is available
  if (comAg && multicropping != FALSE) {

    # multiple cropping as of current multiple cropping pattern
    m <- as.logical(stringr::str_split(multicropping, ":")[[1]][1])
    if (m) {
      m <- "TRUE:actual:irrig_crop"
    } else {
      m <- FALSE
    }

    # Actually committed irrigated area (crop-specific) (in Mha)
    # including non-renewable groundwater (if activated)
    comAgArea <- calcOutput("IrrigAreaActuallyCommitted",
                            fossilGW = fossilGW,
                            lpjml = lpjml, climatetype = climatetype,
                            selectyears = selectyears, iniyear = iniyear,
                            efrMethod = efrMethod, multicropping = m,
                            transDist = transDist, aggregate = FALSE)
    getSets(comAgArea) <- c("x", "y", "iso", "year", "crop", "EFP", "scen")
    comAgArea <- collapseNames(dimOrder(comAgArea, perm = c(2, 3, 1), dim = 3))

    # Water use for committed agricultural areas
    # in main season
    comAgWatFirstWW  <- comAgArea * collapseNames(watReqFirst[, , "withdrawal"])
    comAgWatFirstWC  <- comAgArea * collapseNames(watReqFirst[, , "consumption"])
    # in entire year
    comAgWatYearWW  <- comAgArea * collapseNames(watReqYear[, , "withdrawal"])
    comAgWatYearWC  <- comAgArea * collapseNames(watReqYear[, , "consumption"])
    # in off season
    comAgWatSecondWW <- comAgWatYearWW - comAgWatFirstWW
    comAgWatSecondWC <- comAgWatYearWC - comAgWatFirstWC

    # Check: watPotAvl > comAgWatFirst for case of comAg=TRUE
    if (any(round(watPotAvlWW - comAgWatFirstWW, digits = 6) < 0)) {
      stop("When comAg is activated, there should be enough water for the irrigation of the
           main season of currently irrigated areas.
           Please check what's wrong in calcPotMulticroppingShare!")
    }
    # Check: comAgWatSecond should be 0 for perennials and where not suitable for MC under irrigated conditions
    perennials <- c("oilpalm", "sugr_cane") # what about "others" and "cottn_pro"?
    if (any(round(comAgWatSecondWW[, , perennials], digits = 6) != 0)) {
      # for sugarcane it works, but oilpalm proxied by
      warning("Committed water requirements in the off season should be zero for perennials.
           Please check what's wrong in calcPotMulticroppingShare!
           This originates in the crop mapping issue from LPJmL to MAgPIE crops where
           different crops are perennials")
      # Fix in calcIrrigWatRequirements and calcBlueWaterConsumption
    }
    if (any(comAgWatSecondWW[suitMCir == 0] != 0)) {
      warning("Committed water requirements in the off season should be zero where multiple
           cropping is not possible.
           Please check what's wrong in calcPotMulticroppingShare!
           This is related to the mapping of LPJmL annuals to MAgPIE perennials.
           It's only the case for oilpalm, others and cottn_pro")
      # Fix in calcIrrigWatRequirements and calcBlueWaterConsumption
    }
    rm(comAgArea, comAgWatYearWW, comAgWatYearWC)

    # total required water in main and off-season respectively
    comAgWatSecondWW <- dimSums(comAgWatSecondWW, dim = "crop")
    comAgWatSecondWC <- dimSums(comAgWatSecondWC, dim = "crop")
    comAgWatFirstWW  <- dimSums(comAgWatFirstWW, dim = "crop")
    comAgWatFirstWC  <- dimSums(comAgWatFirstWC, dim = "crop")

    # Share of water for second season that can be fulfilled
    # with remaining water after first season irrigation.
    outWW <- ifelse(comAgWatSecondWW > 0,
                    (watPotAvlWW - comAgWatFirstWW) / comAgWatSecondWW,
                    NA)
    outWC <- ifelse(comAgWatSecondWC > 0,
                    (watPotAvlWC - comAgWatFirstWC) / comAgWatSecondWC,
                    NA)

    outWW[outWW > 1] <- 1
    outWC[outWC > 1] <- 1

    # Where no committed agriculture:
    # full multiple cropping is assumed where it is suitable
    outWW[is.na(outWW)] <- suitMCir[is.na(outWW)]
    outWC[is.na(outWC)] <- suitMCir[is.na(outWC)]

    if (any(round(outWW - outWC, digits = 6) != 0)) {
      warning("outWW and outWC should be the same. check what's wrong")
    }

    out[, , "irrigated"] <- pmin(outWW, outWC)

  }

  # For single cropping case: no areas are suitable for multiple cropping
  if (!(as.logical(stringr::str_split(multicropping, ":")[[1]][1]))) {
    out[, , ] <- 0
  }

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcPotMulticroppingShare produced NAs")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcPotMulticroppingShare produced negative values")
  } else {
    # correct negatives due to numerical reasons
    out[out < 0] <- 0
  }
  if (any(out > 1)) {
    stop("Problem in mrwater::calcPotMulticroppingShare: Value should be between 0 or 1!")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "share",
              description  = paste0("share of irrigated area that can be multiple cropped ",
                                    "given water limitation"),
              isocountries = FALSE))
}