#' @title fullIRRIGATIONPOTENTIAL
#' @description Function that produces the objects for Technical and Economic
#'              Irrigation Potentials within land and water boundaries
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
#' @param gainthreshold     Threshold of yield improvement potential required for
#'                          water allocation in upstreamfirst algorithm
#'                          (in same unit as in rankmethod)
#' @param protectLand       Land protection scenario (e.g. HalfEarth, BH_IFL, NULL)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          (mask can be:
#'                          "none": no mask applied (only for development purposes)
#'                          "actual:total": currently multicropped areas calculated from total harvested areas
#'                                          and total physical areas per cell from readLanduseToolbox
#'                          "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                          "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                          "potential:endogenous": potentially multicropped areas given
#'                                                  temperature and productivity limits
#'                          "potential:exogenous": potentially multicropped areas given
#'                                                 GAEZ suitability classification)
#'                          (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
#'
#' @author Felicitas Beier
#'
#' @importFrom stringr str_split
#'
#' @export

fullIRRIGATIONPOTENTIAL <- function(efrMethod = "VMF:fair", accessibilityrule = "CV:2",
                                    transDist = 0, fossilGW = FALSE,
                                    allocationrule = "optimization", rankmethod = "USD_ha:GLO:TRUE",
                                    gainthreshold = 500,
                                    protectLand = "HalfEarth", yieldcalib = "TRUE:FALSE",
                                    multicropping = FALSE, cropmix = "hist_total",
                                    climatetype = "MRI-ESM2-0:ssp370",
                                    lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                              crop = "ggcmi_phase3_nchecks_9ca735cb")) {

  # Standard settings
  iniyear          <- 2010
  selectyears      <- 2010
  plotyear         <- 2010
  ssp              <- "ssp2"

  irrigationsystem <- "initialization"

  gtrange          <- c(0, 10, 50, 100, 250, 300, 500, 600, 750, 900, 1000, 1500, 2000, 3000)

  # retrieve arguments
  thresholdtype <- paste(str_split(rankmethod, pattern = ":")[[1]][1],
                         str_split(rankmethod, pattern = ":")[[1]][2],
                         sep = ":")

  ################
  # MAIN RESULTS #
  ################

  # Irrigation potentials (IAP and IWP (consumption and withdrawal))
  # for range of yield value gain thresholds and different scenarios
  for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {

   # SUS and LANDPROTECT scenarios:
   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("potCropland:", protectLand),
              comAg = FALSE,  transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currCropland:", protectLand),
              comAg = FALSE,  transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currIrrig:", protectLand),
              comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("potCropland:", protectLand),
              comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currCropland:", protectLand),
              comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = paste0("currIrrig:", protectLand),
              comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTSUScomAg.mz"))

   # UNSUS and WATPROTECT scenarios:
   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "potCropland:NULL",
              comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currCropland:NULL",
              comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currIrrig:NULL",
              comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTUNSUS.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "potCropland:NULL",
              comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconPOTUNSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currCropland:NULL",
              comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconCURUNSUScomAg.mz"))

   calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
              selectyears = plotyear, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              efrMethod = efrMethod, accessibilityrule = accessibilityrule,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, cropmix = cropmix,
              landScen = "currIrrig:NULL",
              comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
              multicropping = multicropping, aggregate = FALSE,
              file = paste0(o, "EconACTUNSUScomAg.mz"))

  }

  # Yield gain area
  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             yieldcalib = yieldcalib, unit = thresholdtype,
             irrigationsystem = irrigationsystem,
             landScen = paste0("currIrrig:", protectLand),
             aggregate = FALSE,
             file = paste0("yieldgainarea_actSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             yieldcalib = yieldcalib, unit = thresholdtype,
             irrigationsystem = irrigationsystem,
             landScen = paste0("currIrrig:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_actUNSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             yieldcalib = yieldcalib, unit = thresholdtype,
             irrigationsystem = irrigationsystem,
             landScen = paste0("currCropland:", protectLand),
             aggregate = FALSE,
             file = paste0("yieldgainarea_curSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             yieldcalib = yieldcalib, unit = thresholdtype,
             irrigationsystem = irrigationsystem,
             landScen = paste0("currCropland:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_curUNSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             yieldcalib = yieldcalib, unit = thresholdtype,
             irrigationsystem = irrigationsystem,
             landScen = paste0("potCropland:", protectLand),
             aggregate = FALSE,
             file = paste0("yieldgainarea_potSUS", ".mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = multicropping,
             yieldcalib = yieldcalib, unit = thresholdtype,
             irrigationsystem = irrigationsystem,
             landScen = paste0("potCropland:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_potUNSUS", ".mz"))

  # Water requirements for irrigation of selected areas
  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currIrrig:", protectLand),
             cropmix = cropmix, yieldcalib = yieldcalib,
             irrigationsystem = irrigationsystem,
             unit = thresholdtype,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_actSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currIrrig:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             irrigationsystem = irrigationsystem,
             unit = thresholdtype,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_actUNSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currCropland:", protectLand),
             cropmix = cropmix, yieldcalib = yieldcalib,
             irrigationsystem = irrigationsystem,
             unit = thresholdtype,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_curSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currCropland:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             irrigationsystem = irrigationsystem,
             unit = thresholdtype,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_curUNSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("potCropland:", protectLand),
             cropmix = cropmix, yieldcalib = yieldcalib,
             irrigationsystem = irrigationsystem,
             unit = thresholdtype,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_potSUS.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("potCropland:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             irrigationsystem = irrigationsystem,
             unit = thresholdtype,
             multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_potUNSUS.mz")

  # Half-Earth protection map
  calcOutput("ProtectArea", cells = "lpjcell", aggregate = FALSE,
             file = "protectedAreas.mz")

  # Area that is potentially available for irrigated agriculture
  calcOutput("AreaPotIrrig", selectyears = plotyear, comagyear = NULL,
             landScen = paste0("potCropland:", protectLand),
             lpjml = NULL, climatetype = NULL,
             efrMethod = NULL, fossilGW = FALSE,
             multicropping = NULL, transDist = NULL,
             aggregate = FALSE,
             file = "avlIrrigarea_pot.mz")

  # Accessibility graph
  calcOutput("LPJmL_new", subtype = "mdischarge",
             version = lpjml[["natveg"]], climatetype = climatetype,
             stage = "raw", aggregate = FALSE,
             file = "LPJmL_monthlyDischarge.mz")

  # Share current irrigation water that can be fulfilled by available water resources
  calcOutput("ShrHumanUsesFulfilled",
             multicropping = multicropping, transDist = transDist,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, aggregate = FALSE,
             file = "shrHumanUsesFulfilled.mz")

  # LUH croparea
  calcOutput("CropareaAdjusted", iniyear = iniyear,
             aggregate = FALSE,
             file = "cropareaLUH.mz")
  # Cropmix of LUH
  calcOutput("CropAreaShare", iniyear = iniyear,
             cropmix = cropmix, aggregate = FALSE,
             file = "cropareaShr.mz")

  # Water use (withdrawal and consumption) on current irrigated area
  calcOutput("WaterUseCommittedAg", multicropping = multicropping,
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             aggregate = FALSE, file = "watComAg.mz")
  # Committed agricultural water uses that can be fulfilled by local resources
  calcOutput("RiverHumanUseAccounting",
             iteration = "committed_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, multicropping = multicropping,
             transDist = transDist, comAg = FALSE,
             accessibilityrule = NULL, fossilGW = NULL,
             rankmethod = NULL, gainthreshold = NULL,
             cropmix = NULL, yieldcalib = NULL,
             irrigationsystem = NULL, landScen = NULL,
             aggregate = FALSE,
             file = "comAguses.mz")
  # Non-Agricultural water uses (in mio. m^3 / yr) [smoothed]
  calcOutput("RiverHumanUseAccounting",
             iteration = "non_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, multicropping = multicropping,
             transDist = transDist, comAg = FALSE,
             accessibilityrule = NULL, fossilGW = NULL,
             rankmethod = NULL, gainthreshold = NULL,
             cropmix = NULL, yieldcalib = NULL,
             irrigationsystem = NULL, landScen = NULL,
             aggregate = FALSE,
             file = "nonAguses.mz")
  # Irrigatable areas with committed agricultural uses
  calcOutput("IrrigAreaActuallyCommitted",
             fossilGW = FALSE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, transDist = transDist,
             multicropping = multicropping, aggregate = FALSE,
             file = "irrigAreaCurrent.mz")

  # Yield gain through irrigation
  # With global crop prices
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             comagyear = NULL, fossilGW = NULL,
             efrMethod = NULL, transDist = NULL,
             irrigationsystem = irrigationsystem,
             landScen = paste0("potCropland:", "NULL"),
             multicropping = multicropping, aggregate = FALSE,
             file = paste0("yieldgain_USDha_GLO", ".mz"))
  # with regional crop prices
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:ISO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             comagyear = NULL, fossilGW = NULL,
             efrMethod = NULL, transDist = NULL,
             irrigationsystem = irrigationsystem,
             landScen = paste0("potCropland:", "NULL"),
             multicropping = multicropping, aggregate = FALSE,
             file = paste0("yieldgain_USDha_ISO", ".mz"))
  # with one price for all crops
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:CONST",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             comagyear = NULL, fossilGW = NULL,
             efrMethod = NULL, transDist = NULL,
             irrigationsystem = irrigationsystem,
             landScen = paste0("potCropland:", "NULL"),
             multicropping = multicropping, aggregate = FALSE,
             file = paste0("yieldgain_USDha_constant", ".mz"))

  # Rainfed and irrigated crop yield valued at crop-specific prices [in USD/ha]
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib,
             priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
             multicropping = multicropping,
             aggregate = FALSE,
             file = "yieldsValued.mz")
}
