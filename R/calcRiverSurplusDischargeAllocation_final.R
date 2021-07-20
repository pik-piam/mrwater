#' @title       calcRiverSurplusDischargeAllocation_final
#' @description This function distributes surplus basin discharge after the
#'              previous river routings following certain management assumptions
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years to be returned
#'                          (Note: does not affect years of harmonization or smoothing)
#' @param output            Output to be reported
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
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
#'                          and boolean indicating fullpotential (TRUE, i.e. cell receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param yieldcalib        FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param allocationrule    Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype     Unit of yield improvement potential used as threshold:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return
#' @param gainthreshold     Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem  Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param avlland_scen      Land availability scenario (currCropland, currIrrig, potIrrig)
#'                          combination of land availability scenario and initialization year separated by ":".
#'                          protection scenario separated by "_" (only relevant when potIrrig selected):
#'                          WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix           cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
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
                                                      selectyears, output, EFRmethod, accessibilityrule,
                                                      rankmethod, yieldcalib, allocationrule, thresholdtype,
                                                      gainthreshold, irrigationsystem, iniyear, avlland_scen,
                                                      cropmix, com_ag, multicropping) {

  # Check
  if (!is.na(as.list(strsplit(avlland_scen, split = ":"))[[1]][2]) &&
      iniyear != as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])) {
    stop("Initialization year in calcRiverSurplusDischargeAllocation does not match:
         iniyear and avlland_scen should have same initialization year")
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
                                            EFRmethod = EFRmethod, accessibilityrule = accessibilityrule)

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  discharge                   <- calcOutput("RiverDischargeNatAndHuman", selectyears = selectyears, iniyear = iniyear,
                                            lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
                                            com_ag = com_ag, aggregate = FALSE)

  # Required water for full irrigation per cell (in mio. m^3)
  required_wat_fullirrig      <- calcOutput("FullIrrigationRequirement", selectyears = selectyears,
                                            lpjml = lpjml, climatetype = climatetype, comagyear = comagyear,
                                            irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
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
                                            names = getNames(discharge), fill = 0,
                                            sets = c("x.y.iso", "year", "EFP.scen"))

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

  ################################################
  ####### River basin discharge allocation #######
  ################################################
  out     <- NULL
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
                  multicropping = multicropping)

  for (y in selectyears) {

    l_inout <- toolDischargeAllocation(y = y, rs = rs, l_inout = l_inout,
                                       l_in = l_in, glocellrank = glocellrank,
                                       allocationrule = allocationrule)
  }

  if (output == "discharge") {

    # Main output for MAgPIE: water available for agricultural consumption
    out         <- as.magpie(l_inout$discharge, spatial = 1)
    description <- "Cellular discharge after surplus discharge allocation algorithm"

  } else if (output == "frac_fullirrig") {

    # Main output for MAgPIE: water available for agricultural withdrawal
    out         <- as.magpie(l_inout$frac_fullirrig, spatial = 1)
    description <- "Fraction of full irrigation requirements that can be fulfilled"

  } else {
    stop("specify outputtype")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
