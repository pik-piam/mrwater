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
#' @param protectLand       Land protection scenario (e.g. HalfEarth, BH_FF, NULL)
#' @param yieldcalib        Boolean for whether LPJmL should be calibrated to FAO country yields (TRUE or FALSE)
#'
#' @author Felicitas Beier
#'
#' @export

fullWATER <- function(efrMethod = "VMF:fair", accessibilityrule = "CV:2",
                      allocationrule = "optimization", rankmethod = "USD_ha:TRUE",
                      thresholdtype = "USD_ha", gainthreshold = 500,
                      protectLand = "HalfEarth", yieldcalib = TRUE) {

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

  gtrange          <- c(0, 250, 500, 1000, 2000, 3000)

  ################
  # MAIN RESULTS #
  ################

  # Irrigation potentials (IAP and IWP (consumption and withdrawal))
  # for range of yield value gain thresholds and different scenarios
  for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {

   # SUS and LANDPROTECT scenarios:
   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("potCropland:", protectLand), potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currCropland:", protectLand), potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currIrrig:", protectLand), potential_wat = FALSE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("potCropland:", protectLand), potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currCropland:", protectLand), potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currIrrig:", protectLand), potential_wat = FALSE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTSUScomAg.mz"))

   # UNSUS and WATPROTECT scenarios:
   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "potCropland:NULL", potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currCropland:NULL", potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currIrrig:NULL", potential_wat = FALSE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "potCropland:NULL", potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTUNSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currCropland:NULL", potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURUNSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currIrrig:NULL", potential_wat = FALSE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTUNSUScomAg.mz"))

  }

  # Yield gain area
  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("currCropland:", protectLand),
             aggregate = FALSE,
             file = paste0("yieldgainarea_currSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("currCropland:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_currUNSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("potCropland:", protectLand),
             aggregate = FALSE,
             file = paste0("yieldgainarea_potSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("potCropland:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_potUNSUS", ".mz"))

  # Half-Earth protection map
  calcOutput("ProtectArea", cells = "lpjcell", aggregate = FALSE,
             file = "protectedAreas.mz")

  # Accessibility graph
  calcOutput("LPJmL_new", subtype = "mdischarge",
             version = lpjml["natveg"], climatetype = climatetype,
             stage = "raw", aggregate = FALSE,
             file = "LPJmL_monthlyDischarge.mz")

  # Share current irrigation water that can be fulfilled by available water resources
  calcOutput("ShrCurrIrrigFulfilled",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, aggregate = FALSE,
             file = "shrCurrIrrigFulfilled.mz")

  # LUH croparea
  calcOutput("CropareaAdjusted", years = iniyear,
             sectoral = "kcr", cells = "lpjcell",
             physical = TRUE, cellular = TRUE,
             irrigation = TRUE, aggregate = FALSE,
             file = "cropareaLUH.mz")

  # Water use (withdrawal and consumption) on current irrigated area
  calcOutput("WaterUseCommittedAg",
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             aggregate = FALSE, file = "watComAg.mz")

  ##########
  # EXTRAS #
  ##########

  # Yield gain through irrigation
  calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype,
             unit = "USD_ha", iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib, multicropping = multicropping,
             aggregate = FALSE, file = paste0("yieldgain_USDha", ".mz"))
  calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype,
             unit = "USD_m3", iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib, multicropping = multicropping,
             aggregate = FALSE, file = paste0("yieldgain_USDm3", ".mz"))

  # Validation
  for (efrMethod in c("VMF:fair", "Smakhtin:fair", "Smakhtin:good")) {
    for (accessibilityrule in c("CV:2", "Q:1", "Q:0.9", "Q:0.75", "Q:0.5")) {

      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000),
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 landScen = "currCropland:NULL", cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidCurrcropland", gsub(":", "", efrMethod), gsub(":", "", accessibilityrule), ".mz"))

      # Potentially irrigated area
      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000),
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 landScen = paste0("potCropland:", protectLand), cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidPotcropland", gsub(":", "", efrMethod), gsub(":", "", accessibilityrule), ".mz"))

      calcOutput("EconOfIrrig", scenario = ssp, season = "single", output = "IrrigArea",
                 GT_range = c(0, 250, 500, 1000, 2000, 3000),
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                 thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                 landScen = "potCropland:NULL", cropmix = cropmix, potential_wat = TRUE,
                 com_ag = FALSE, multicropping = multicropping, aggregate = FALSE,
                 file = paste0("ValidPotcropland_Unprotect", gsub(":", "", efrMethod),
                               gsub(":", "", accessibilityrule), ".mz"))
    }
  }
}
