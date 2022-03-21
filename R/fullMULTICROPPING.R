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

fullMULTICROPPING <- function(multicropping = "TRUE:endogenous",
                              cropmix = c("maiz", "rapeseed", "puls_pro"),
                              yieldcalib = TRUE) {

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


  ################
  # MAIN RESULTS #
  ################

  # Physical croparea
  calcOutput("Croparea", years = iniyear, physical = TRUE, sectoral = "kcr",
             irrigation = TRUE, cells = "lpjcell", cellular = TRUE,
             aggregate = FALSE, file = "croparea_physical.mz")

  # Potentially irrigated area on current cropland under single cropping
  calcOutput("EconOfIrrig", scenario = ssp, output = "IrrigArea", GT_range = gtrange,
             selectyears = plotyear, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, thresholdtype = thresholdtype,
             irrigationsystem = irrigationsystem, cropmix = cropmix,
             landScen = "currCropland:NULL", potential_wat = TRUE, com_ag = FALSE,
             multicropping = FALSE, aggregate = FALSE,
             file = paste0("IrrigArea", "EconCURUNSUS", "_single.mz"))


  # Yield gain through irrigation under multiple cropping
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = plotyear,
             cropmix = cropmix, yieldcalib = yieldcalib,
             multicropping = "TRUE:endogenous", aggregate = FALSE,
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

    ## Land protection
    # Current cropland
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

    # Current irrigated area
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

    # Current cropland (committed agricultural uses reserved)
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

    # Current irrigated area (committed agricultural uses reserved)
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

    ## No land protection
    # Current cropland
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

    # Current irrigated area
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

    # Current cropland (committed agricultural uses reserved)
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

    # Current irrigated area (committed agricultural uses reserved)
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
             aggregate = FALSE, file = "suitMC_GAEZ2.mz")

  # Multiple cropping suitability according to GAEZ
  calcOutput("MultipleCroppingZones", layers = 8,
             aggregate = FALSE, file = "suitMC_GAEZ8.mz")


}
