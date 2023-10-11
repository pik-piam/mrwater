#' @title fullSIMPLECLIMATE
#' @description Function that produces gridded outputs for usage
#'              in the SIMPLE model (Baldos & Hertel 2012) and
#'              the SIMPLE-G model (Baldos et al. 2020)
#'
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

fullSIMPLECLIMATE <- function(transDist = 100, fossilGW = TRUE,
                       allocationrule = "optimization",
                       rankmethod = "USD_m3:GLO:TRUE") {

  # The analysis is calculated for the 2010 and 2050
  iniyear           <- 2010
  selectyears       <- c(2010, 2050)
  # The analysis is conducted for two climate scenarios
  climatescens      <- c("MRI-ESM2-0:ssp126", "MRI-ESM2-0:ssp585")
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_9ca735cb")

  # Non-agricultural water usage follows a BAU trajectory
  ssp               <- "ssp2"

  # Standard settings
  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  multicropping     <- FALSE
  yieldcalib        <- "TRUE:FALSE"
  cropmix           <- "hist_total"
  protectLand       <- "HalfEarth"
  irrigationsystem  <- "initialization"
  gtrange           <- c(0, 10, 50, 100, 250, 300, 500, 600, 750, 900, 1000, 1500, 2000, 3000)

  # retrieve arguments
  thresholdtype <- paste(str_split(rankmethod, pattern = ":")[[1]][1],
                         str_split(rankmethod, pattern = ":")[[1]][2],
                         sep = ":")

  ################
  # MAIN RESULTS #
  ################

  # Area that is potentially available for irrigated agriculture
  calcOutput("AreaPotIrrig", selectyears = selectyears, comagyear = NULL,
             landScen = paste0("potCropland:", protectLand),
             lpjml = NULL, climatetype = NULL,
             efrMethod = NULL, fossilGW = FALSE,
             multicropping = FALSE, transDist = NULL,
             aggregate = FALSE,
             file = "avlIrrigarea_pot.mz")

  # Irrigation potentials (IAP and IWP (consumption and withdrawal))
  # for range of yield value gain thresholds and different scenarios
  for (c in climatescens) {

    # Rainfed and irrigated crop yield valued at crop-specific prices [in USD/ha]
    calcOutput("YieldsValued",
               lpjml = lpjml, climatetype = c,
               iniyear = iniyear, selectyears = selectyears,
               yieldcalib = yieldcalib,
               priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
               multicropping = multicropping,
               aggregate = FALSE,
               file = paste0("yieldsValued",
                             "_", c, ".mz"))
    # Yield gain through irrigation
    # With global crop prices
    calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
               lpjml = lpjml, climatetype = c,
               iniyear = iniyear, selectyears = selectyears,
               cropmix = cropmix, yieldcalib = yieldcalib,
               comagyear = NULL, fossilGW = NULL,
               efrMethod = NULL, transDist = NULL,
               irrigationsystem = irrigationsystem,
               landScen = paste0("potCropland:", "NULL"),
               multicropping = multicropping, aggregate = FALSE,
               file = paste0("yieldgain_USDha_GLO",
                             "_", c, ".mz"))

    # Irrigatable areas with committed agricultural uses
    calcOutput("IrrigAreaActuallyCommitted",
               fossilGW = fossilGW,
               lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = transDist,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0("irrigAreaCurrent",
                             "_", c, ".mz"))

    # Water use (withdrawal and consumption) on current irrigated area
    calcOutput("WaterUseCommittedAg", multicropping = multicropping,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = c,
               aggregate = FALSE,
               file = paste0("watComAg",
                             "_", c, ".mz"))

    for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {
      for (y in selectyears) {

        # SUS and LANDPROTECT scenarios:
        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = paste0("potCropland:", protectLand),
                   comAg = FALSE,  transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconPOTSUS",
                                 "_", y, "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = paste0("currCropland:", protectLand),
                   comAg = FALSE,  transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconCURSUS",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = paste0("currIrrig:", protectLand),
                   comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconACTSUS",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = paste0("potCropland:", protectLand),
                   comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconPOTSUScomAg",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = paste0("currCropland:", protectLand),
                   comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconCURSUScomAg",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = paste0("currIrrig:", protectLand),
                   comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconACTSUScomAg",
                                 "_", y,
                                 "_", c, ".mz"))

        # UNSUS and WATPROTECT scenarios:
        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "potCropland:NULL",
                   comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconPOTUNSUS",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "currCropland:NULL",
                   comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconCURUNSUS",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "currIrrig:NULL",
                   comAg = FALSE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconACTUNSUS",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "potCropland:NULL",
                   comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconPOTUNSUScomAg",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "currCropland:NULL",
                   comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconCURUNSUScomAg",
                                 "_", y,
                                 "_", c, ".mz"))

        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = y, iniyear = iniyear,
                   lpjml = lpjml, climatetype = c,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "currIrrig:NULL",
                   comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
                   multicropping = multicropping, aggregate = FALSE,
                   file = paste0(o, "EconACTUNSUScomAg",
                                 "_", y,
                                 "_", c, ".mz"))
      }
    }

    # Yield gain area
    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               cropmix = cropmix, multicropping = multicropping,
               yieldcalib = yieldcalib, unit = thresholdtype,
               irrigationsystem = irrigationsystem,
               landScen = paste0("currIrrig:", protectLand),
               aggregate = FALSE,
               file = paste0("yieldgainarea_actSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               cropmix = cropmix, multicropping = multicropping,
               yieldcalib = yieldcalib, unit = thresholdtype,
               irrigationsystem = irrigationsystem,
               landScen = paste0("currIrrig:", "NULL"),
               aggregate = FALSE,
               file = paste0("yieldgainarea_actUNSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               cropmix = cropmix, multicropping = multicropping,
               yieldcalib = yieldcalib, unit = thresholdtype,
               irrigationsystem = irrigationsystem,
               landScen = paste0("currCropland:", protectLand),
               aggregate = FALSE,
               file = paste0("yieldgainarea_curSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               cropmix = cropmix, multicropping = multicropping,
               yieldcalib = yieldcalib, unit = thresholdtype,
               irrigationsystem = irrigationsystem,
               landScen = paste0("currCropland:", "NULL"),
               aggregate = FALSE,
               file = paste0("yieldgainarea_curUNSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               cropmix = cropmix, multicropping = multicropping,
               yieldcalib = yieldcalib, unit = thresholdtype,
               irrigationsystem = irrigationsystem,
               landScen = paste0("potCropland:", protectLand),
               aggregate = FALSE,
               file = paste0("yieldgainarea_potSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               cropmix = cropmix, multicropping = multicropping,
               yieldcalib = yieldcalib, unit = thresholdtype,
               irrigationsystem = irrigationsystem,
               landScen = paste0("potCropland:", "NULL"),
               aggregate = FALSE,
               file = paste0("yieldgainarea_potUNSUS",
                             "_", c, ".mz"))

    # Water requirements for irrigation of selected areas
    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currIrrig:", protectLand),
               cropmix = cropmix, yieldcalib = yieldcalib,
               irrigationsystem = irrigationsystem,
               unit = thresholdtype,
               multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldgainwater_actSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currIrrig:", "NULL"),
               cropmix = cropmix, yieldcalib = yieldcalib,
               irrigationsystem = irrigationsystem,
               unit = thresholdtype,
               multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldgainwater_actUNSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currCropland:", protectLand),
               cropmix = cropmix, yieldcalib = yieldcalib,
               irrigationsystem = irrigationsystem,
               unit = thresholdtype,
               multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldgainwater_curSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currCropland:", "NULL"),
               cropmix = cropmix, yieldcalib = yieldcalib,
               irrigationsystem = irrigationsystem,
               unit = thresholdtype,
               multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldgainwater_curUNSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("potCropland:", protectLand),
               cropmix = cropmix, yieldcalib = yieldcalib,
               irrigationsystem = irrigationsystem,
               unit = thresholdtype,
               multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldgainwater_potSUS",
                             "_", c, ".mz"))

    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = c,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("potCropland:", "NULL"),
               cropmix = cropmix, yieldcalib = yieldcalib,
               irrigationsystem = irrigationsystem,
               unit = thresholdtype,
               multicropping = multicropping, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldgainwater_potUNSUS",
                             "_", c, ".mz"))
  }
}
