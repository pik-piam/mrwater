#' @title fullWATER
#' @description Function that produces the objects for water outputs that can
#'              be plotted with the mrwaterPlots functions.
#'
#' @param efrMethod         EFR method used including selected strictness of EFRs
#'                          (Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst")
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return;
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param thresholdtype     Unit of yield improvement potential used as threshold:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return
#' @param gainthreshold     Threshold of yield improvement potential required for
#'                          water allocation in upstreamfirst algorithm
#'                          (in same unit as thresholdtype)
#' @param currland          Current land availability scenario: land scenario and initialization year
#' @param potland           Potential land availability scenario: specify suitability (Ramankutty or Zabel),
#'                          protection scenario and initialization year after potIrrig
#'                          (e.g. potCropland_HalfEarth:2010)
#' @param yieldcalib        Boolean for whether LPJmL should be calibrated to FAO country yields (TRUE or FALSE)
#'
#' @author Felicitas Beier
#'
#' @export

fullWATER <- function(efrMethod = "VMF:fair", accessibilityrule = "CV:2",
                      allocationrule = "optimization", rankmethod = "USD_ha:TRUE",
                      thresholdtype = "USD_ha", gainthreshold = 500,
                      currland = "currCropland:2010", potland = "potIrrig:HalfEarth", yieldcalib = TRUE) {

  # Standard settings
  iniyear          <- 2010
  selectyears      <- c(2010, 2050)
  plotyear         <- 2010
  ssp              <- "ssp2"
  multicropping    <- FALSE

  lpjml            <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                        crop = "ggcmi_phase3_nchecks_9ca735cb")
  climatetype      <- "GFDL-ESM4:ssp126"

  irrigationsystem <- "initialization"
  cropmix          <- "hist_total"
  #* #*#*# @KRISTINE/JENS/BENNI: Does is make sense to use "hist_total" everywhere or should I use "hist_irrig" sometimes (e.g. for committed uses) or would that create mismatch?

  # Multiple Cropping Zones
  calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE,
             file = "multicroppingZones.mz")

  # LUH croparea
  calcOutput("CropareaAdjusted", years = iniyear, sectoral = "kcr", cells = "lpjcell",
             physical = TRUE, cellular = TRUE, irrigation = TRUE, aggregate = FALSE,
             file = "cropareaLUH.mz")

  # Share current irrigation water that can be fulfilled by available water resources
  calcOutput("ShrCurrIrrigFulfilled", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, aggregate = FALSE,
             file = "shrCurrIrrigFulfilled.mz")

  calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
             lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
             selectyears = selectyears, iniyear = iniyear, aggregate = FALSE,
             file = "fulfilledComAg.mz")

  calcOutput("WaterUseCommittedAg", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear, aggregate = FALSE,
             file = "ComAg.mz")

  calcOutput("WaterPotUse", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = currland, cropmix = cropmix,
             com_ag = TRUE, multicropping = multicropping, aggregate = FALSE,
             file = "WatPotUse_curr.mz")

  calcOutput("WaterPotUse", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = potland, cropmix = cropmix,
             com_ag = TRUE, multicropping = multicropping, aggregate = FALSE,
             file = "WatPotUse_pot.mz")
  calcOutput("WaterPotUse", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = paste0(gsub("_.*", "", potland), ":", iniyear), cropmix = cropmix,
             com_ag = TRUE, multicropping = multicropping, aggregate = FALSE,
             file = "WatPotUse_pot_Unprotect.mz")

  ### Main Outputs ###
  # Potentially irrigated area
  calcOutput("IrrigatableArea", selectyears = selectyears, iniyear = iniyear,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = gainthreshold, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, landScen = currland,
             cropmix = cropmix, potential_wat = TRUE, com_ag = FALSE,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigatableArea_potential.mz")

  # Potentially irrigated area for different thresholds
  # (only for sustainable scenario and only for multicropping = FALSE!)
  calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000),
             selectyears = plotyear, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, landScen = currland, cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_curr_single.mz")

  calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000),
             selectyears = plotyear, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, landScen = potland, cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_pot_single.mz")

  # Reference data
  calcOutput("YieldgainArea", rangeGT = c(0, 250, 500, 1000, 2000, 3000), lpjml = lpjml,
             selectyears = plotyear, iniyear = iniyear,
             climatetype = climatetype, efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype, landScen = currland,
             cropmix = cropmix, multicropping = multicropping, aggregate = FALSE,
             file = paste0("yieldgainarea_curr", ".mz"))

  calcOutput("YieldgainArea", rangeGT = c(0, 250, 500, 1000, 2000, 3000), lpjml = lpjml,
             selectyears = plotyear, iniyear = iniyear,
             climatetype = climatetype, efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype, landScen = potland,
             cropmix = cropmix, multicropping = multicropping, aggregate = FALSE,
             file = paste0("yieldgainarea_pot", ".mz"))
  calcOutput("YieldgainArea", rangeGT = c(0, 250, 500, 1000, 2000, 3000), lpjml = lpjml,
             selectyears = plotyear, iniyear = iniyear,
             climatetype = climatetype, efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0(gsub("_.*", "", potland), ":", iniyear),
             cropmix = cropmix, multicropping = multicropping, aggregate = FALSE,
             file = paste0("yieldgainarea_pot_Unprotect", ".mz"))

  # Yield gain through irrigation
  calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype,
             unit = "USD_ha", iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib, multicropping = multicropping,
             aggregate = FALSE, file = paste0("yieldgain_USDha", ".mz"))
  calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype,
             unit = "USD_m3", iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib, multicropping = multicropping,
             aggregate = FALSE, file = paste0("yieldgain_USDm3", ".mz"))

  # Area that is potentially available for irrigated agriculture
  calcOutput("AreaPotIrrig", selectyears = plotyear, comagyear = NULL,
             landScen = potland, aggregate = FALSE,
             file = "avlIrrigarea_pot.mz")
  calcOutput("AreaPotIrrig", selectyears = plotyear, comagyear = NULL,
             landScen = paste0(gsub("_.*", "", potland), ":", iniyear), aggregate = FALSE,
             file = "avlIrrigarea_pot_Unprotect.mz")
  calcOutput("AreaPotIrrig", selectyears = plotyear, comagyear = NULL,
             landScen = currland, aggregate = FALSE,
             file = "avlIrrigarea_curr.mz")

  # Physical Potential considering committed uses
  # Potentially irrigated area
  calcOutput("IrrigatableArea", selectyears = plotyear, iniyear = iniyear,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = 0, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, landScen = currland,
             cropmix = cropmix, potential_wat = TRUE, com_ag = TRUE,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigArea_currCropland_comag.mz")

  # Potentially irrigated area
  calcOutput("IrrigatableArea", selectyears = plotyear, iniyear = iniyear,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = 0, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, landScen = potland,
             cropmix = cropmix, potential_wat = TRUE, com_ag = TRUE,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigArea_potCropland_comag.mz")

  # LUH fulfilled
  calcOutput("IrrigatableArea", selectyears = plotyear, iniyear = iniyear,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = 0, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, landScen = potland,
             cropmix = cropmix, potential_wat = FALSE, com_ag = TRUE,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "LUHfulfilled_comag.mz")

  # Basin violations
  calcOutput("EFRviolations", lpjml = lpjml, selectyears = plotyear,
             climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = potland, cropmix = cropmix, com_ag = TRUE, multicropping = FALSE,
             scenario = "off.ssp2", aggregate = FALSE,
             file = "basinViolation_LANDPROTECT_comag.mz")

  calcOutput("EFRviolations", lpjml = lpjml, selectyears = plotyear,
             climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = potland, cropmix = cropmix, com_ag = FALSE, multicropping = FALSE,
             scenario = "off.ssp2", aggregate = FALSE,
             file = "basinViolation_LANDPROTECT.mz")

  calcOutput("EFRviolations", lpjml = lpjml, selectyears = plotyear,
             climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = paste0(gsub("_.*", "", potland), ":", iniyear),
             cropmix = cropmix, com_ag = TRUE, multicropping = FALSE,
             scenario = "off.ssp2", aggregate = FALSE,
             file = "basinViolation_comag.mz")

  calcOutput("EFRviolations", lpjml = lpjml, selectyears = plotyear,
             climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = paste0(gsub("_.*", "", potland), ":", iniyear),
             cropmix = cropmix, com_ag = FALSE, multicropping = FALSE,
             scenario = "off.ssp2", aggregate = FALSE,
             file = "basinViolation.mz")

  # Accessibility graph
  calcOutput("LPJmL_new", subtype = "mdischarge",
                                        version = lpjml["natveg"], climatetype = climatetype,
                                        stage = "raw", aggregate = FALSE,
                                        file = "LPJmL_monthlyDischarge.mz")
  # Validation
  for (efrMethod in c("VMF:fair", "Smakhtin:fair", "Smakhtin:good")) {
    for (accessibilityrule in c("CV:2", "Q:1", "Q:0.9", "Q:0.75", "Q:0.5")) {

      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000),
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 landScen = currland, cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidCurrcropland", gsub(":", "", efrMethod), gsub(":", "", accessibilityrule), ".mz"))

      # Potentially irrigated area
      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000),
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 landScen = potland, cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidPotcropland", gsub(":", "", efrMethod), gsub(":", "", accessibilityrule), ".mz"))

      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000),
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 landScen = paste0(gsub("_.*", "", potland), ":", iniyear), cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidPotcropland_Unprotect", gsub(":", "", efrMethod),
                               gsub(":", "", accessibilityrule), ".mz"))
    }
  }

  # Half-Earth protection map
  calcOutput("ProtectArea", cells = "lpjcell", aggregate = FALSE, file = "protectedAreas.mz")

}
