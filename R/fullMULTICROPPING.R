#' @title fullMULTICROPPING
#' @description Function that produces output for irrigation potentials
#'              under multiple cropping on cellular resolution.
#'
#' @param cropmix        Selected cropmix (options:
#'                       "hist_irrig" for historical cropmix on currently irrigated area,
#'                       "hist_total" for historical cropmix on total cropland,
#'                       or selection of proxycrops)
#' @param yieldcalib     If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                       If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule Rule to be applied for river basin discharge allocation
#'                       across cells of river basin ("optimization", "upstreamfirst")
#' @param rankmethod     Rank and optimization method consisting of
#'                       Unit according to which rank is calculated:
#'                       tDM (tons per dry matter),
#'                       USD_ha (USD per hectare) for area return, or
#'                       USD_m3 (USD per cubic meter) for volumetric return;
#'                       Unit of yield improvement potential to be returned;
#'                       Level of price aggregation used:
#'                       "GLO" for global average prices, or
#'                       "ISO" for country-level prices;
#'                       and boolean indicating fullpotential (TRUE, i.e. cell
#'                       receives full irrigation requirements in total area)
#'                       or reduced potential (FALSE, reduced potential of cell
#'                       receives at later stage in allocation algorithm);
#'                       separated by ":"
#' @param thresholdtype  Unit of yield improvement potential used as threshold,
#'                       consisting of two components:
#'                       Unit:
#'                       tDM (tons per dry matter),
#'                       USD_ha (USD per hectare) for area return, or
#'                       USD_m3 (USD per cubic meter) for volumetric return.
#'                       Price aggregation:
#'                       "GLO" for global average prices, or
#'                       "ISO" for country-level prices
#'
#' @author Felicitas Beier
#'
#' @export

fullMULTICROPPING <- function(cropmix = c("maiz", "rapeseed", "puls_pro"),
                              yieldcalib = "TRUE:TRUE:actual:irrig_crop",
                              allocationrule = "optimization",
                              rankmethod = "USD_ha:GLO:TRUE",
                              thresholdtype = "USD_ha:GLO") {

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010"
  plotyear          <- "y2010"
  ssp               <- "ssp2"

  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_9ca735cb")
  climatetype       <- "GSWP3-W5E5:historical"

  irrigationsystem  <- "initialization"
  gtrange          <- c(0, 10, 50, 100, 250, 300, 500, 600,
                        750, 900, 1000, 1500, 2000, 3000)
  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"

  ################
  # MAIN RESULTS #
  ################

  # Non-Agricultural water uses (in mio. m^3 / yr) [smoothed]
  calcOutput("RiverHumanUses", humanuse = "non_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, multicropping = FALSE,
             aggregate = FALSE,
             file = "nonAguses.mz")

  # Yields
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0("Yields", "_single.mz"))
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0("calibYields", "_single.mz"))
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
             yieldcalib = FALSE,
             multicropping = "TRUE:potential:endogenous",
             file = paste0("Yields", "_multiplePOT.mz"))
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
             yieldcalib = yieldcalib,
             multicropping = "TRUE:potential:endogenous",
             file = paste0("calibYields", "_multiplePOT.mz"))
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
             yieldcalib = yieldcalib,
             multicropping = "TRUE:actual:irrig_crop",
             file = paste0("calibYields", "_multipleACT.mz"))

  # Yield gain through irrigation under multiple cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
             file = paste0("yieldgain_USDha_multipleACT", ".mz"))

  # Yield gain through irrigation under multiple cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = paste0("yieldgain_USDha_multiplePOT", ".mz"))

  # Yield gain through irrigation under multiple cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = "TRUE:potential:exogenous", aggregate = FALSE,
             file = paste0("yieldgain_USDha_multipleExogenous", ".mz"))

  # Yield gain through irrigation under single cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0("yieldgain_USDha_single", ".mz"))

  # Yield gain analysis
  for (m in c("TRUE:actual:total", "TRUE:actual:irrig_crop", "TRUE:potential:endogenous")) {

    calcOutput("YieldImprovementPotential", unit = "USD_ha:GLO",
               yieldgaintype = "irrigation_singlecropping",
               lpjml = lpjml, climatetype = climatetype,
               iniyear = iniyear, selectyears = selectyears,
               cropmix = cropmix, yieldcalib = yieldcalib,
               multicropping = m, aggregate = FALSE,
               file = paste0("yieldgain_IrrigationSingle_",
                             as.list(strsplit(m, split = ":"))[[1]][2],
                             as.list(strsplit(m, split = ":"))[[1]][3], ".mz"))
    calcOutput("YieldImprovementPotential", unit = "USD_ha:GLO",
               yieldgaintype = "multicropping_rf",
               lpjml = lpjml, climatetype = climatetype,
               iniyear = iniyear, selectyears = selectyears,
               cropmix = cropmix, yieldcalib = yieldcalib,
               multicropping = m, aggregate = FALSE,
               file = paste0("yieldgain_MulticroppingRainfed_",
                             as.list(strsplit(m, split = ":"))[[1]][2],
                             as.list(strsplit(m, split = ":"))[[1]][3], ".mz"))
    calcOutput("YieldImprovementPotential", unit = "USD_ha:GLO",
               yieldgaintype = "multicropping_ir",
               lpjml = lpjml, climatetype = climatetype,
               iniyear = iniyear, selectyears = selectyears,
               cropmix = cropmix, yieldcalib = yieldcalib,
               multicropping = m, aggregate = FALSE,
               file = paste0("yieldgain_MulticroppingIrrigated_",
                             as.list(strsplit(m, split = ":"))[[1]][2],
                             as.list(strsplit(m, split = ":"))[[1]][3], ".mz"))
    calcOutput("YieldImprovementPotential", unit = "USD_ha:GLO",
               yieldgaintype = "irrigation_multicropping",
               lpjml = lpjml, climatetype = climatetype,
               iniyear = iniyear, selectyears = selectyears,
               cropmix = cropmix, yieldcalib = yieldcalib,
               multicropping = m, aggregate = FALSE,
               file = paste0("yieldgain_IrrigationAndMulticropping_",
                             as.list(strsplit(m, split = ":"))[[1]][2],
                             as.list(strsplit(m, split = ":"))[[1]][3], ".mz"))
    calcOutput("YieldImprovementPotential", unit = "USD_ha:GLO",
               yieldgaintype = "irrigation_multicropping",
               lpjml = lpjml, climatetype = climatetype,
               iniyear = iniyear, selectyears = selectyears,
               cropmix = cropmix, yieldcalib = yieldcalib,
               multicropping = m, aggregate = FALSE,
               file = paste0("yieldgain_IrrigationMulticropping_",
                             as.list(strsplit(m, split = ":"))[[1]][2],
                             as.list(strsplit(m, split = ":"))[[1]][3], ".mz"))
  }

  # Discharge determined by previous river routings (in mio. m^3 / yr)
  calcOutput("RiverDischargeNatAndHuman", selectyears = selectyears, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod,
              multicropping = "TRUE:actual:irrig_crop",
              comAg = TRUE, aggregate = FALSE,
              file = "comAgdischarge_multi.mz")

  calcOutput("RiverDischargeNatAndHuman", selectyears = selectyears, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, multicropping = FALSE,
              comAg = TRUE, aggregate = FALSE,
              file = "comAgdischarge_single.mz")

  # Required water for full irrigation per cell (in mio. m^3)
  calcOutput("FullIrrigationRequirement",
              selectyears = selectyears, iniyear = iniyear, comagyear = iniyear,
              lpjml = lpjml, climatetype = climatetype,
              irrigationsystem = irrigationsystem, landScen = "currCropland:NULL",
              cropmix = cropmix, yieldcalib = yieldcalib,
              multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
              file = "reqWatFullirrig_multi.mz")

  calcOutput("FullIrrigationRequirement",
             selectyears = selectyears, iniyear = iniyear, comagyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             irrigationsystem = irrigationsystem, landScen = "currCropland:NULL",
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = "reqWatFullirrig_single.mz")

  # Share current irrigation water that can be fulfilled by available water resources
  calcOutput("ShrCurrIrrigFulfilled", multicropping = FALSE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, aggregate = FALSE,
             file = "shrCurrIrrigFulfilledSingle.mz")
  calcOutput("ShrCurrIrrigFulfilled", multicropping = "TRUE:actual:irrig_crop",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, aggregate = FALSE,
             file = "shrCurrIrrigFulfilledMultiple.mz")

  for (committed in c(TRUE, FALSE)) {
    for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {
    # Potentially irrigated area on current cropland under single cropping
    calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
               selectyears = plotyear, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               landScen = "currCropland:NULL", potentialWat = TRUE, comAg = committed,
               multicropping = FALSE, aggregate = FALSE,
               file = paste0(o, "EconCURUNSUS", "comAg", as.character(committed), "_single.mz"))

    # Current irrigated area under single cropping
    calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
               selectyears = plotyear, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               landScen = "currIrrig:NULL", potentialWat = TRUE, comAg = committed,
               multicropping = FALSE, aggregate = FALSE,
               file = paste0(o, "EconACTUNSUS", "comAg", as.character(committed), "_single.mz"))
    }
  }

  # Yield gain area (single cropping)
  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = FALSE,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("currIrrig:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_actUNSUS", "_single.mz"))

  calcOutput("YieldgainArea", rangeGT = gtrange,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = plotyear, iniyear = iniyear,
             cropmix = cropmix, multicropping = FALSE,
             yieldcalib = yieldcalib, thresholdtype = thresholdtype,
             landScen = paste0("currCropland:", "NULL"),
             aggregate = FALSE,
             file = paste0("yieldgainarea_curUNSUS", "_single.mz"))

  # Water requirements for irrigation of selected areas
  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currIrrig:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             unit = thresholdtype,
             multicropping = FALSE, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_actUNSUS_single.mz")

  calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             landScen = paste0("currCropland:", "NULL"),
             cropmix = cropmix, yieldcalib = yieldcalib,
             unit = thresholdtype,
             multicropping = FALSE, rangeGT = gtrange, aggregate = FALSE,
             file = "yieldgainwater_curUNSUS_single.mz")

  # Irrigation potentials (IAP and IWP (consumption and withdrawal))
  # for range of yield value gain thresholds and different scenarios

  for (m in c("TRUE:actual:total", "TRUE:actual:irrig_crop", "TRUE:potential:endogenous")) {

    # Yield gain area (multiple cropping)
    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = plotyear, iniyear = iniyear,
               cropmix = cropmix, multicropping = m,
               yieldcalib = yieldcalib, thresholdtype = thresholdtype,
               landScen = paste0("currIrrig:", "NULL"),
               aggregate = FALSE,
               file = paste0("yieldGainArea_IRRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz"))

    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = plotyear, iniyear = iniyear,
               cropmix = cropmix, multicropping = m,
               yieldcalib = yieldcalib, thresholdtype = thresholdtype,
               landScen = paste0("currCropland:", "NULL"),
               aggregate = FALSE,
               file = paste0("yieldGainArea_CURRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz"))

    # Water requirements for irrigation of selected areas
    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currIrrig:", "NULL"),
               cropmix = cropmix, yieldcalib = yieldcalib,
               unit = thresholdtype,
               multicropping = m, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldGainWater_IRRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz"))

    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currCropland:", "NULL"),
               cropmix = cropmix, yieldcalib = yieldcalib,
               unit = thresholdtype,
               multicropping = m, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldGainWater_CURRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz"))

    for (committed in c(TRUE, FALSE)) {
      for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {

        # Potential cropland, but with protection
        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = plotyear, iniyear = iniyear,
                   lpjml = lpjml, climatetype = climatetype,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule, thresholdtype = thresholdtype,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "potCropland:HalfEarth",
                   potentialWat = TRUE, comAg = committed,
                   multicropping = m, aggregate = FALSE,
                   file = paste0(o, "EconPOTSUS", "comAg",
                                 as.character(committed),
                                 as.list(strsplit(m, split = ":"))[[1]][2], ".mz"))

        # Current cropland
        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = plotyear, iniyear = iniyear,
                   lpjml = lpjml, climatetype = climatetype,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule, thresholdtype = thresholdtype,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "currCropland:NULL", potentialWat = TRUE, comAg = committed,
                   multicropping = m, aggregate = FALSE,
                   file = paste0(o, "EconCURUNSUS", "comAg",
                                 as.character(committed),
                                 as.list(strsplit(m, split = ":"))[[1]][2], ".mz"))

        # Current irrigated area
        calcOutput("EconOfIrrig", scenario = ssp, output = o, gtrange = gtrange,
                   selectyears = plotyear, iniyear = iniyear,
                   lpjml = lpjml, climatetype = climatetype,
                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                   rankmethod = rankmethod, yieldcalib = yieldcalib,
                   allocationrule = allocationrule, thresholdtype = thresholdtype,
                   irrigationsystem = irrigationsystem, cropmix = cropmix,
                   landScen = "currIrrig:NULL", potentialWat = TRUE, comAg = committed,
                   multicropping = m, aggregate = FALSE,
                   file = paste0(o, "EconIRRUNSUS", "comAg",
                                 as.character(committed),
                                 as.list(strsplit(m, split = ":"))[[1]][2], ".mz"))
      }
    }
  }

  ##############
  # VALIDATION #
  ##############

  # Multiple cropping suitability per crop calculated based on crop and grass productivity
  calcOutput("MulticroppingSuitability",
              lpjml = lpjml[["crop"]], climatetype = climatetype,
              selectyears = selectyears, suitability = "endogenous",
              aggregate = FALSE, file = "suitMC_LPJmL.mz")

  # Multiple cropping suitability according to GAEZ (Boolean)
  calcOutput("MulticroppingSuitability",
             lpjml = lpjml[["crop"]], climatetype = climatetype,
             selectyears = selectyears, suitability = "exogenous",
             aggregate = FALSE, file = "suitMC_GAEZ2.mz")

  # Multiple cropping suitability according to GAEZ
  calcOutput("MultipleCroppingZones", layers = 8,
             aggregate = FALSE, file = "suitMC_GAEZ8.mz")

  # Toolbox
  calcOutput("CropareaToolbox", physical = TRUE, sectoral = "kcr",
             cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "ToolboxPHYS.mz")
  calcOutput("CropareaToolbox", physical = FALSE, sectoral = "kcr",
             cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "ToolboxHARV.mz")

  calcOutput("MulticroppingCells", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype, scenario = "actual:irrig_crop",
             aggregate = FALSE, file = "ToolboxMulticropping.mz")

}
