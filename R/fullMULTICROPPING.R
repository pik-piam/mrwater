#' @title fullMULTICROPPING
#' @description Function that produces output for irrigation potentials
#'              under multipl cropping on cellular resolution.
#'
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#'
#' @author Felicitas Beier
#'
#' @export

fullMULTICROPPING <- function(multicropping = "TRUE:endogenous") {

  # Standard settings
  iniyear           <- 2010
  selectyears       <- 2010
  plotyear          <- 2010
  ssp               <- "ssp2"

  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_9ca735cb")
  climatetype       <- "GSWP3-W5E5:historical"

  irrigationsystem  <- "initialization"
  gtrange           <- c(0, 250, 500, 1000, 2000, 3000)
  efrMethod         <-  "VMF:fair"
  accessibilityrule <- "CV:2"
  allocationrule    <- "optimization"
  rankmethod        <- "USD_ha:TRUE"
  thresholdtype     <- "USD_ha"
  protectLand       <- "HalfEarth"
  yieldcalib        <- TRUE
  cropmix           <- "hist_total"


  ################
  # MAIN RESULTS #
  ################

  # Yield gain through irrigation under multiple cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = TRUE, aggregate = FALSE,
             file = paste0("yieldgain_USDha_multiple", ".mz"))

  # Yield gain through irrigation under single cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0("yieldgain_USDha_single", ".mz"))

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
               landScen = paste0("potCropland:", protectLand),
               potential_wat = TRUE, com_ag = FALSE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0(o, "EconPOTSUS.mz"))

    calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
               selectyears = plotyear, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               landScen = paste0("currCropland:", protectLand),
               potential_wat = TRUE, com_ag = FALSE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0(o, "EconCURSUS.mz"))

    calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
               selectyears = plotyear, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               landScen = paste0("currIrrig:", protectLand),
               potential_wat = TRUE, com_ag = FALSE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0(o, "EconACTSUS.mz"))

    calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
               selectyears = plotyear, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               landScen = paste0("potCropland:", protectLand),
               potential_wat = TRUE, com_ag = TRUE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0(o, "EconPOTSUScomAg.mz"))

    calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
               selectyears = plotyear, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               landScen = paste0("currCropland:", protectLand),
               potential_wat = TRUE, com_ag = TRUE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0(o, "EconCURSUScomAg.mz"))

    calcOutput("EconOfIrrig", scenario = ssp, output = o, GT_range = gtrange,
               selectyears = plotyear, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               landScen = paste0("currIrrig:", protectLand),
               potential_wat = TRUE, com_ag = TRUE,
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
             aggregate = FALSE, file = "suitMC_LPJmL.mz")

  # Multiple cropping suitability according to GAEZ
  calcOutput("MultipleCroppingZones", layers = 8,
             aggregate = FALSE, file = "suitMC_GAEZ.mz")


}
