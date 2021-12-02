#' @title       calcRiverSurplusDischargeAllocation_final
#' @description This function distributes surplus basin discharge after the
#'              previous river routings following certain management assumptions
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param output            Output to be reported
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return;
#'                          and boolean indicating fullpotential (TRUE, i.e. cell receives full
#'                                                                irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                                                receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          ("optimization" or "upstreamfirst")
#' @param thresholdtype     Unit of yield improvement potential used as threshold:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return
#' @param gainthreshold     Threshold of yield improvement potential
#'                          (same unit as thresholdtype)
#' @param irrigationsystem  Irrigation system to be used for river basin discharge
#'                          allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param com_ag            if TRUE: the currently already irrigated areas
#'                                   in initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverSurplusDischargeAllocation_final", aggregate = FALSE)
#' }
#'
calcRiverSurplusDischargeAllocation_final <- function(lpjml, climatetype,
                                                      selectyears, output, efrMethod, accessibilityrule,
                                                      rankmethod, yieldcalib, allocationrule, thresholdtype,
                                                      gainthreshold, irrigationsystem, iniyear, landScen,
                                                      cropmix, com_ag, multicropping) {

  # Check
  if (!is.numeric(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }

  # Retrieve arguments
  if (com_ag == TRUE) {
    comagyear <- iniyear
  } else if (com_ag == FALSE) {
    comagyear <- NULL
  }

  #######################################
  ###### Read in Required Inputs ########
  #######################################
  # River Structure
  rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))

  # Minimum reserved flow requirements: Inaccessible discharge +
  #                                     Environmental Flow Requirements (adjusted for
  #                                     part that is fulfilled by inaccessible water) +
  #                                     Reserved for Non-Agricultural Uses +
  #                                     [Reserved Committed Agricultural Uses, if activated] (in mio. m^3 / yr)
  required_wat_min_allocation <- calcOutput("RiverWatReserved", aggregate = FALSE,
                                            selectyears = selectyears, iniyear = iniyear,
                                            lpjml = lpjml, climatetype = climatetype, com_ag = com_ag,
                                            efrMethod = efrMethod, accessibilityrule = accessibilityrule)

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge                   <- calcOutput("RiverDischargeNatAndHuman", selectyears = selectyears, iniyear = iniyear,
                                            lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
                                            com_ag = com_ag, aggregate = FALSE)

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig      <- calcOutput("FullIrrigationRequirement", selectyears = selectyears, iniyear = iniyear,
                                            lpjml = lpjml, climatetype = climatetype, comagyear = comagyear,
                                            irrigationsystem = irrigationsystem, landScen = landScen,
                                            cropmix = cropmix, multicropping = multicropping, aggregate = FALSE)
  required_wat_fullirrig_ww   <- pmax(collapseNames(required_wat_fullirrig[, , "withdrawal"]), 0)
  required_wat_fullirrig_wc   <- pmax(collapseNames(required_wat_fullirrig[, , "consumption"]), 0)

  # Yield gain potential through irrigation of proxy crops
  irrig_yieldgainpotential    <- calcOutput("IrrigYieldImprovementPotential", selectyears = selectyears,
                                            lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
                                            unit = thresholdtype, iniyear = iniyear, yieldcalib = yieldcalib,
                                            multicropping = multicropping, aggregate = FALSE)

  # Initialization of fraction of full irrigation requirements that can be fulfilled
  frac_fullirrig              <- new.magpie(cells_and_regions = getCells(discharge),
                                            years = getYears(discharge),
                                            names = getNames(irrig_yieldgainpotential),
                                            fill = 0,
                                            sets = c("x.y.iso", "year", "EFP.scen.season"))

  # Global cell rank based on yield gain potential by irrigation of proxy crops: maize, rapeseed, pulses
  glocellrank     <- setYears(calcOutput("IrrigCellranking", cellrankyear = selectyears,
                                         lpjml = lpjml, climatetype = climatetype, method = rankmethod,
                                         cropmix = cropmix, iniyear = iniyear, yieldcalib = yieldcalib,
                                         multicropping = multicropping, aggregate = FALSE),
                              selectyears)

  # Initialization of objects
  tmp <- as.magpie(discharge)

  .transformObject <- function(x) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = getCells(tmp),
                          years = getYears(tmp),
                          names = getNames(tmp),
                          fill = 0,
                          sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out     <- object0 + x
    return(out)
  }

  discharge                   <- as.array(discharge)
  required_wat_min_allocation <- as.array(required_wat_min_allocation)
  frac_fullirrig              <- as.array(.transformObject(frac_fullirrig))
  required_wat_fullirrig_ww   <- as.array(.transformObject(required_wat_fullirrig_ww))
  required_wat_fullirrig_wc   <- as.array(.transformObject(required_wat_fullirrig_wc))
  irrig_yieldgainpotential    <- as.array(.transformObject(irrig_yieldgainpotential))
  avl_wat_ww                  <- as.array(.transformObject(0))
  avl_wat_wc                  <- as.array(.transformObject(0))
  glocellrank                 <- as.array(glocellrank)

  scenarios <- getNames(discharge)

  ################################################
  ####### River basin discharge allocation #######
  ################################################
  if (class(selectyears) == "numeric") {
    selectyears <- paste0("y", selectyears)
  }

  # list of objects that are inputs and outputs to the allocation function
  l_inout <- list(discharge = discharge,
                  required_wat_min_allocation = required_wat_min_allocation,
                  frac_fullirrig = frac_fullirrig)

  # list of objects that are inputs to the allocation function
  l_in    <- list(irrig_yieldgainpotential = irrig_yieldgainpotential,
                  required_wat_fullirrig_ww = required_wat_fullirrig_ww,
                  required_wat_fullirrig_wc = required_wat_fullirrig_wc,
                  gainthreshold = gainthreshold,
                  avl_wat_ww = avl_wat_ww,
                  avl_wat_wc = avl_wat_wc,
                  multicropping = multicropping,
                  scenarios = scenarios)

  for (y in selectyears) {

    l_inout <- toolDischargeAllocation(y = y, rs = rs, l_inout = l_inout,
                                       l_in = l_in, glocellrank = glocellrank,
                                       allocationrule = allocationrule)
  }

  if (output == "discharge") {

    # Main output for MAgPIE: water available for agricultural consumption
    out         <- as.magpie(l_inout$discharge, spatial = 1)
    description <- "Cellular discharge after surplus discharge allocation algorithm"

  } else if (output == "potIrrigWat") {

    # Main output for MAgPIE: water available for agricultural withdrawal
    frac        <- as.magpie(l_inout$frac_fullirrig, spatial = 1)
    out         <- frac * required_wat_fullirrig

    description <- "Full irrigation requirements that can be fulfilled (consumption and withdrawal)"

  } else {
    stop("specify outputtype")
  }

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("calcRiverSurplusDischargeAllocation produced NAs")
  }
  if (any(round(out) < 0)) {
    stop("calcRiverSurplusDischargeAllocation produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
