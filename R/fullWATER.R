#' @title fullWATER
#' @description Function that produces the objects for water outputs
#'              on cellular resolution.
#'
#' @param efrMethod         EFR method used including selected strictness of EFRs
#'                          (Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or
#'                          Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness
#'                          of accessibility restriction:
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
#' @param protectLand       Land protection scenario (e.g. HalfEarth, BH_IFL, NULL)
#' @param yieldcalib        Boolean for whether LPJmL should be calibrated
#'                          to FAO country yields (TRUE or FALSE)
#' @param multicropping     Boolean for whether multicropping is activated (TRUE)
#'                          or not (FALSE)
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#'
#' @author Felicitas Beier
#'
#' @export

fullWATER <- function(efrMethod = "VMF:fair", accessibilityrule = "CV:2",
                      allocationrule = "optimization", rankmethod = "USD_ha:TRUE",
                      thresholdtype = "USD_ha", gainthreshold = 500,
                      protectLand = "HalfEarth", yieldcalib = TRUE,
                      multicropping = FALSE, cropmix = "hist_total") {

  # Standard settings
  iniyear          <- 2010
  selectyears      <- 2010
  plotyear         <- 2010
  ssp              <- "ssp2"

  lpjml            <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                        crop = "ggcmi_phase3_nchecks_9ca735cb")
  # climatetype      <- "GFDL-ESM4:ssp126"
  climatetype      <- "MRI-ESM2-0:ssp370"


  irrigationsystem <- "initialization"

  gtrange          <- c(0, 250, 500, 1000, 2000, 3000)

  ################
  # MAIN RESULTS #
  ################

  # Irrigation potentials (IAP and IWP (consumption and withdrawal))
  # for range of yield value gain thresholds and different scenarios
  for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {

   # SUS and LANDPROTECT scenarios:
   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("potCropland:", protectLand), potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currCropland:", protectLand), potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currIrrig:", protectLand), potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("potCropland:", protectLand), potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currCropland:", protectLand), potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currIrrig:", protectLand), potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTSUScomAg.mz"))

   # UNSUS and WATPROTECT scenarios:
   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "potCropland:NULL", potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currCropland:NULL", potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currIrrig:NULL", potential_wat = TRUE, com_ag = FALSE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "potCropland:NULL", potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTUNSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currCropland:NULL", potential_wat = TRUE, com_ag = TRUE,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURUNSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule, thresholdtype = thresholdtype,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currIrrig:NULL", potential_wat = TRUE, com_ag = TRUE,
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
             landScen = paste0("currIrrig:", protectLand),
             aggregate = FALSE,
             file = paste0("yieldgainarea_actSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("currIrrig:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_actUNSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("currCropland:", protectLand),
             aggregate = FALSE,
             file = paste0("yieldgainarea_curSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             efrMethod = efrMethod,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("currCropland:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_curUNSUS", ".mz"))

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

  # Water requirements for irrigation of selected areas
  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currIrrig:", protectLand),
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_actSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currIrrig:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_actUNSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currCropland:", protectLand),
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_curSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currCropland:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_curUNSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("potCropland:", protectLand),
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_potSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("potCropland:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_potUNSUS.mz")

  # Half-Earth protection map
  calcOutput("ProtectArea", cells = "lpjcell", aggregate = FALSE,
             file = "protectedAreas.mz")

  # Area that is potentially available for irrigated agriculture
  calcOutput("AreaPotIrrig", selectyears = plotyear, comagyear = NULL,
             landScen = paste0("potCropland:", protectLand), aggregate = FALSE,
             file = "avlIrrigarea_pot.mz")

  # Accessibility graph
  calcOutput("LPJmL_new", subtype = "mdischarge",
             version = lpjml[["natveg"]], climatetype = climatetype,
             stage = "raw", aggregate = FALSE,
             file = "LPJmL_monthlyDischarge.mz")

  # Share current irrigation water that can be fulfilled by available water resources
  calcOutput("ShrCurrIrrigFulfilled", multicropping = multicropping,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, aggregate = FALSE,
             file = "shrCurrIrrigFulfilled.mz")

  # LUH croparea
  calcOutput("CropareaAdjusted", iniyear = iniyear,
             aggregate = FALSE,
             file = "cropareaLUH.mz")

  # Water use (withdrawal and consumption) on current irrigated area
  calcOutput("WaterUseCommittedAg", multicropping = multicropping,
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             aggregate = FALSE, file = "watComAg.mz")
  # Committed agricultural water uses that can be fulfilled by local resources
  calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, multicropping = multicropping,
             aggregate = FALSE,
             file = "comAguses.mz")
  # Non-Agricultural water uses (in mio. m^3 / yr) [smoothed]
  calcOutput("RiverHumanUses", humanuse = "non_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, multicropping = multicropping,
             aggregate = FALSE,
             file = "nonAguses.mz")
  # Irrigatable areas with committed agricultural uses
  calcOutput("IrrigAreaPotential", selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             yieldcalib = yieldcalib, allocationrule = allocationrule,
             thresholdtype = thresholdtype, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, cropmix = cropmix,
             landScen = paste0("currIrrig:", "NULL"),
             potential_wat = FALSE, com_ag = TRUE,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigAreaCurrent.mz")

  # Yield gain through irrigation
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = multicropping, aggregate = FALSE,
             file = paste0("yieldgain_USDha", ".mz"))

}
