#' @title fullMULTICROPPING
#' @description Function that produces output for irrigation potentials
#'              under multiple cropping on cellular resolution.
#'
#' @param cropmix       Selected cropmix (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                      If FALSE: uncalibrated LPJmL yields are used
#'
#' @author Felicitas Beier
#'
#' @export

fullMULTICROPPING <- function(cropmix = c("maiz", "rapeseed", "puls_pro"),
                              yieldcalib = TRUE) {

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010"
  plotyear          <- "y2010"
  ssp               <- "ssp2"

  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_9ca735cb")
  climatetype       <- "GSWP3-W5E5:historical"

  irrigationsystem  <- "initialization"
  gtrange          <- c(0, 10, 50, 100, 250, 300, 500, 600, 750, 900, 1000, 1500, 2000, 3000)
  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  allocationrule    <- "optimization"
  rankmethod        <- "USD_ha:GLO:TRUE"
  thresholdtype     <- "USD_ha:GLO"

  ################
  # MAIN RESULTS #
  ################

  # Yields
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE, multicropping = FALSE, aggregate = FALSE,
             file = paste0("Yields", "_single.mz"))
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = TRUE, multicropping = FALSE, aggregate = FALSE,
             file = paste0("calibYields", "_single.mz"))
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
             yieldcalib = FALSE, multicropping = "potential:endogenous",
             file = paste0("Yields", "_multiplePOT.mz"))
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
             yieldcalib = TRUE, multicropping = "potential:endogenous",
             file = paste0("calibYields", "_multiplePOT.mz"))

  # Yield gain through irrigation under multiple cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = paste0("yieldgain_USDha_multiple", ".mz"))

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

  # Physical croparea
  calcOutput("CropareaAdjusted", iniyear = iniyear,
             aggregate = FALSE, file = "croparea_physical.mz")


  for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {
  # Potentially irrigated area on current cropland under single cropping
  calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
             selectyears = plotyear, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, cropmix = cropmix,
             landScen = "currCropland:NULL", potential_wat = TRUE, com_ag = FALSE,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0(o, "EconCURUNSUS", "_single.mz"))

  # Current irrigated area under single cropping
  calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
             selectyears = plotyear, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, cropmix = cropmix,
             landScen = "currIrrig:NULL", potential_wat = TRUE, com_ag = FALSE,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0(o, "EconACTUNSUS", "_single.mz"))
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

  for (m in c("TRUE:actual:total", "TRUE:potential:endogenous")) {

    # Yield gain area (multiple cropping)
    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = plotyear, iniyear = iniyear,
               cropmix = cropmix, multicropping = m,
               yieldcalib = yieldcalib, thresholdtype = thresholdtype,
               landScen = paste0("currIrrig:", "NULL"),
               aggregate = FALSE,
               file = paste0("yieldGainArea_IRRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz", ))

    calcOutput("YieldgainArea", rangeGT = gtrange,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = plotyear, iniyear = iniyear,
               cropmix = cropmix, multicropping = m,
               yieldcalib = yieldcalib, thresholdtype = thresholdtype,
               landScen = paste0("currCropland:", "NULL"),
               aggregate = FALSE,
               file = paste0("yieldGainArea_CURRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz", ))

    # Water requirements for irrigation of selected areas
    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currIrrig:", "NULL"),
               cropmix = cropmix, yieldcalib = yieldcalib,
               unit = thresholdtype,
               multicropping = m, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldGainWater_IRRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz", ))

    calcOutput("YieldgainWatUse", lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               landScen = paste0("currCropland:", "NULL"),
               cropmix = cropmix, yieldcalib = yieldcalib,
               unit = thresholdtype,
               multicropping = m, rangeGT = gtrange, aggregate = FALSE,
               file = paste0("yieldGainWater_CURRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz", ))


    for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {

      # Current cropland
      calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule, thresholdtype = thresholdtype,
                 irrigationsystem = irrigationsystem, cropmix = cropmix,
                 landScen = "currCropland:NULL", potential_wat = TRUE, com_ag = FALSE,
                 multicropping = m, aggregate = FALSE,
                 file = paste0(o, "EconCURUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz", ))

      # Current irrigated area
      calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule, thresholdtype = thresholdtype,
                 irrigationsystem = irrigationsystem, cropmix = cropmix,
                 landScen = "currIrrig:NULL", potential_wat = TRUE, com_ag = FALSE,
                 multicropping = m, aggregate = FALSE,
                 file = paste0(o, "EconIRRUNSUS", as.list(strsplit(m, split = ":"))[[1]][2], ".mz", ))
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
  calcOutput("CropareaToolbox", physical = TRUE, sectoral = "lpj",
             cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "ToolboxPHYS.mz")
  calcOutput("CropareaToolbox", physical = FALSE, sectoral = "lpj",
                     cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                     selectyears = selectyears, aggregate = FALSE,
             file = "ToolboxHARV.mz")

  calcOutput("MulticroppingCells", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype, scenario = "actual:irrigation",
             aggregate = FALSE, file = "ToolboxMulticropping.mz")

}
