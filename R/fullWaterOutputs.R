#' @title fullWaterOutputs
#' @description Function that produces the objects for water outputs that can
#'              be plotted with the mrwaterPlots functions.
#'
#' @param EFRmethod         EFR method used including selected strictness of EFRs
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
#' @param avlland_scen      Land availability scenario: current or potential;
#'                          optional additionally: protection scenario in case of potential
#'                          (when left empty: no protection)
#'                          and initialization year of cropland area
#'                          combination of land availability scenario and initialization year separated by ":".
#'                          land availability scenario: currIrrig (only currently
#'                          irrigated cropland available for irrigated agriculture),
#'                          currCropland (only current cropland areas available for irrigated agriculture),
#'                          potIrrig (suitable land is available for irrigated agriculture,
#'                          potentially land restrictions activated through protect_scen argument)
#'                          protection scenario separated by "_" (only relevant when potIrrig selected):
#'                          WDPA, BH, FF, CPD, LW, HalfEarth
#' @param multicropping     Boolean of multiple cropping per year (TRUE, FALSE)
#'
#' @author Felicitas Beier
#'
#' @export

fullWaterOutputs <- function(EFRmethod = "VMF:fair", accessibilityrule = "CV:2",
                             allocationrule = "optimization", rankmethod = "USD_ha:TRUE",
                             thresholdtype = "USD_ha", gainthreshold = 500,
                             avlland_scen = "currCropland:2010",
                             multicropping = FALSE) {

  # Standard settings
  iniyear          <- 2010
  selectyears      <- c(2010, 2050)
  plotyear         <- 2010
  ssp              <- "ssp2"

  lpjml            <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                        crop = "ggcmi_phase3_nchecks_9ca735cb")
  climatetype      <- "GFDL-ESM4:ssp126"

  irrigationsystem <- "initialization"
  yieldcalib       <- TRUE
  cropmix          <- "hist_total"
  #* #*#*# @KRISTINE/JENS/BENNI: Does is make sense to use "hist_total" everywhere or should I use "hist_irrig" sometimes (e.g. for committed uses) or would that create mismatch?
  #* #*#*# @JENS/BENNI: We'll only use the proxycrops version for MAgPIE runs, not for the stand-alone-paper, right?

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
             EFRmethod = EFRmethod, aggregate = FALSE,
             file = "shrCurrIrrigFulfilled.mz")

  calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
             selectyears = selectyears, iniyear = iniyear, aggregate = FALSE,
             file = "fulfilledComAg.mz")

  calcOutput("WaterUseCommittedAg", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear, aggregate = FALSE,
             file = "ComAg.mz")

  calcOutput("WaterPotUse", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             avlland_scen = avlland_scen, cropmix = cropmix,
             com_ag = TRUE, multicropping = multicropping, aggregate = FALSE,
             file = "WatPotUse_curr.mz")

  calcOutput("WaterPotUse", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             avlland_scen = avlland_scen, cropmix = cropmix,
             com_ag = TRUE, multicropping = multicropping, aggregate = FALSE,
             file = "WatPotUse_pot.mz")

  # Yield gain through irrigation in USD per hectare
  calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype,
             unit = "USD_ha", iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib, multicropping = multicropping,
             aggregate = FALSE, file = "yieldgain_USDha.mz")

  ### Main Outputs ###
  # Potentially irrigated area
  calcOutput("IrrigatableArea", selectyears = selectyears,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = gainthreshold, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
             cropmix = cropmix, potential_wat = TRUE, com_ag = FALSE,
             accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigatableArea_potential.mz")

  # Potentially irrigated area for different thresholds
  calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "currCropland:2010", cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_curr_single.mz")
  calcOutput("EconOfIrrig", scenario = ssp, season = "double", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "currCropland:2010", cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_curr_double.mz")
  calcOutput("EconOfIrrig", scenario = ssp, season = "triple", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "currCropland:2010", cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_curr_triple.mz")

  calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_pot_single.mz")
  calcOutput("EconOfIrrig", scenario = ssp, season = "double", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_pot_double.mz")
  calcOutput("EconOfIrrig", scenario = ssp, season = "triple", output = "IrrigArea",
             GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
             lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix,
             potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
             file = "DemandCurve_pot_triple.mz")

  # Reference data
  calcOutput("YieldgainArea", rangeGT = c(0, 250, 500, 1000, 2000, 3000), lpjml = lpjml,
             selectyears = plotyear, climatetype = climatetype, EFRmethod = EFRmethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype, avlland_scen = "currCropland:2010",
             cropmix = cropmix, multicropping = multicropping, aggregate = FALSE,
             file = "yieldgainarea_curr.mz")

  calcOutput("YieldgainArea", rangeGT = c(0, 250, 500, 1000, 2000, 3000), lpjml = lpjml,
             selectyears = plotyear, climatetype = climatetype, EFRmethod = EFRmethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype, avlland_scen = "potIrrig_HalfEarth:2010",
             cropmix = cropmix, multicropping = multicropping, aggregate = FALSE,
             file = "yieldgainarea_pot.mz")

  # Area that is potentially available for irrigated agriculture
  calcOutput("AreaPotIrrig", selectyears = plotyear, comagyear = NULL,
             avlland_scen = "potIrrig_HalfEarth:2010", aggregate = FALSE,
             file = "avlIrrigarea_pot.mz")
  calcOutput("AreaPotIrrig", selectyears = plotyear, comagyear = NULL,
             avlland_scen = "currCropland:2010", aggregate = FALSE,
             file = "avlIrrigarea_curr.mz")

  # Physical Potential considering committed uses
  # Potentially irrigated area
  calcOutput("IrrigatableArea", selectyears = plotyear,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = 0, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "currCropland:2010",
             cropmix = cropmix, potential_wat = TRUE, com_ag = TRUE,
             accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigArea_currCropland_comag.mz")

  # Potentially irrigated area
  calcOutput("IrrigatableArea", selectyears = plotyear,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = 0, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "potIrrig_HalfEarth:2010",
             cropmix = cropmix, potential_wat = TRUE, com_ag = TRUE,
             accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigArea_potCropland_comag.mz")

  # LUH fulfilled
  calcOutput("IrrigatableArea", selectyears = plotyear,
             climatetype = climatetype, lpjml = lpjml,
             gainthreshold = 0, rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,  thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, avlland_scen = "potIrrig_HalfEarth:2010",
             cropmix = cropmix, potential_wat = FALSE, com_ag = TRUE,
             accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
             multicropping = multicropping, aggregate = FALSE,
             file = "LUHfulfilled_comag.mz")

  # Validation

  for (EFRmethod in c("VMF:fair", "Smakhtin:fair", "Smakhtin:good")) {
    for (accessibilityrule in c("CV:2", "Q:1", "Q:0.9", "Q:0.75", "Q:0.5")) {

      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
                 lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 avlland_scen = "currCropland:2010", cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidCurrcropland", gsub(":", "", EFRmethod), gsub(":", "", accessibilityrule), ".mz"))

      # Potentially irrigated area
      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = plotyear,
                 lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidPotcropland", gsub(":", "", EFRmethod), gsub(":", "", accessibilityrule), ".mz"))
    }
  }
}
