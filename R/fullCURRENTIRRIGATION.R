#' @title fullCURRENTIRRIGATION
#' @description Function that produces output for irrigation potentials
#'              under multiple cropping on cellular resolution.
#'
#' @param yieldcalib     If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                       If FALSE: uncalibrated LPJmL yields are used
#'
#' @author Felicitas Beier
#'
#' @importFrom stringr str_split
#'
#' @export

fullCURRENTIRRIGATION <- function(yieldcalib = "TRUE:TRUE:actual:irrig_crop") {

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010"

  # Newest LPJmL runs
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_bft_e511ac58")
  climatetype       <- "MRI-ESM2-0:ssp370"

  irrigationsystem  <- "initialization"
  efrMethod         <- "VMF:fair"

  ################
  # MAIN RESULTS #
  ################

  ### Croparea ###
  # share of crop area by crop type (chosen cropmix)
  calcOutput("CropAreaShare", iniyear = iniyear, cropmix = "hist_irrig",
             aggregate = FALSE, file = "cropareaShr.mz")

  calcOutput("CropareaAdjusted", iniyear = iniyear, dataset = "Toolbox",
             aggregate = FALSE, file = "cropareaToolbox.mz")

  calcOutput("CropareaToolbox", physical = TRUE, sectoral = "kcr",
             cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "ToolboxPHYS.mz")
  calcOutput("CropareaToolbox", physical = FALSE, sectoral = "kcr",
             cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "ToolboxHARV.mz")

  ### Yield Gain ###
  # Single cropping yield gain
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL, efrMethod = efrMethod, transDist = 0,
             irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
             cropmix = "hist_irrig", yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = "yieldgain_single.mz")

  # Multiple cropping yield gain
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL, efrMethod = efrMethod, transDist = 0,
             irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
             cropmix = "hist_irrig", yieldcalib = yieldcalib,
             multicropping = TRUE, aggregate = FALSE,
             file = "yieldgain_multiple.mz")

  # Agricultural Water Consumption (NOLIM)
  calcOutput("WaterUseCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = FALSE, aggregate = FALSE,
             file = "comAg_single_NOLIM.mz")
  calcOutput("WaterUseCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
             file = "comAg_multipleACT_NOLIM.mz")
  calcOutput("WaterUseCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "comAg_multiplePOT_NOLIM.mz")

  for (t in c(0, 100, 200)) {

    # Committed agricultural area
    calcOutput("IrrigAreaActuallyCommitted",
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = t,
               multicropping = FALSE, aggregate = FALSE,
               file = paste0("comAgAreaACT_single_", t, ".mz"))

    calcOutput("IrrigAreaActuallyCommitted",
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = t,
               multicropping = "TRUE:actual:irrig_crop",
               aggregate = FALSE,
               file = paste0("comAgAreaACT_multipleACT_", t, ".mz"))

    calcOutput("IrrigAreaActuallyCommitted",
               iteration = "committed_agriculture_fullPotential",
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = t,
               multicropping = "TRUE:potential:endogenous",
               aggregate = FALSE,
               file = paste0("comAgAreaACT_multiplePOT_", t, ".mz"))


    # Committed Agricultural water uses
    calcOutput("RiverHumanUseAccounting",
               iteration = "committed_agriculture",
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod,
               selectyears = selectyears, iniyear = iniyear,
               transDist = t, comAg = TRUE,
               accessibilityrule = NULL,
               rankmethod = NULL, gainthreshold = NULL,
               cropmix = NULL, yieldcalib = NULL,
               irrigationsystem = NULL, landScen = NULL,
               multicropping = FALSE,
               aggregate = FALSE,
               file = paste0("comAgWatACT_single_", t, ".mz"))
    calcOutput("RiverHumanUseAccounting",
               iteration = "committed_agriculture",
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod,
               selectyears = selectyears, iniyear = iniyear,
               transDist = t, comAg = TRUE,
               accessibilityrule = NULL,
               rankmethod = NULL, gainthreshold = NULL,
               cropmix = NULL, yieldcalib = NULL,
               irrigationsystem = NULL, landScen = NULL,
               multicropping = "TRUE:actual:irrig_crop",
               aggregate = FALSE,
               file = paste0("comAgWatACT_multipleACT_", t, ".mz"))
    calcOutput("RiverHumanUseAccounting",
               iteration = "committed_agriculture_fullPotential",
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod,
               selectyears = selectyears, iniyear = iniyear,
               transDist = t, comAg = TRUE,
               accessibilityrule = NULL,
               rankmethod = NULL, gainthreshold = NULL,
               cropmix = NULL, yieldcalib = NULL,
               irrigationsystem = NULL, landScen = NULL,
               multicropping = "TRUE:potential:endogenous",
               aggregate = FALSE,
               file = paste0("comAgWatACT_multiplePOT_", t, ".mz"))


    # Share current irrigation water that can be fulfilled by available water resources
    calcOutput("ShrHumanUsesFulfilled",
               transDist = t,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = "TRUE:actual:irrig_crop",
               file = paste0("shrHumanUsesFulfilledMultiple_", t, ".mz"))

  }

  ##############
  # VALIDATION #
  ##############
  # Multiple cropping suitability per crop calculated based on crop and grass productivity
  calcOutput("MulticroppingSuitability",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, suitability = "endogenous",
             aggregate = FALSE, file = "suitMC_LPJmL.mz")

  # Inverted Growing Period Runs
  # crop yields

}
