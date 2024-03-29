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

  calcOutput("CropareaAdjusted", iniyear = iniyear,
             dataset = "LandInG", sectoral = "kcr",
             aggregate = FALSE, file = "cropareaLandInG.mz")
  calcOutput("CropareaAdjusted", iniyear = iniyear,
             dataset = "LandInG", sectoral = "lpj",
             aggregate = FALSE, file = "cropareaLandInG_lpj.mz")

  calcOutput("CropareaLandInG", physical = TRUE, sectoral = "kcr",
             cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "LandingPHYS.mz")
  calcOutput("CropareaLandInG", physical = FALSE, sectoral = "kcr",
             cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "LandingHARV.mz")

  ### Crop Yields ###
  # single cropping yields (in USD/ha)
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib, priceAgg = "GLO",
             multicropping = FALSE, aggregate = FALSE,
             file = "yield_single.mz")
  # multiple cropping yields (in USD/ha)
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib, priceAgg = "GLO",
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "yield_multiple.mz")

  ### Yield Gain ###
  # Single cropping yield gain
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL,
             irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
             cropmix = "hist_irrig", yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = "yieldgain_single.mz")

  # Multiple cropping yield gain
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL,
             irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
             cropmix = "hist_irrig", yieldcalib = yieldcalib,
             multicropping = TRUE, aggregate = FALSE,
             file = "yieldgain_multiple.mz")

  # Agricultural Water Consumption (NOLIM) [in mio. m^3 per year]
  calcOutput("WaterUseCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = FALSE, aggregate = FALSE,
             file = "comAgWat_single_NOLIM.mz")
  calcOutput("WaterUseCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
             file = "comAgWat_multipleACT_NOLIM.mz")
  calcOutput("WaterUseCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "comAgWat_multiplePOT_NOLIM.mz")

  for (t in c(0, 100, 200)) {

    # Committed agricultural area
    calcOutput("IrrigAreaActuallyCommitted",
               fossilGW = FALSE,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = t,
               multicropping = FALSE, aggregate = FALSE,
               file = paste0("comAgAreaACT_single_", t, ".mz"))

    calcOutput("IrrigAreaActuallyCommitted",
               fossilGW = FALSE,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = t,
               multicropping = "TRUE:actual:irrig_crop",
               aggregate = FALSE,
               file = paste0("comAgAreaACT_multipleACT_", t, ".mz"))

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


    # Share current irrigation water that can be fulfilled by available water resources
    calcOutput("ShrHumanUsesFulfilled",
               transDist = t,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = "TRUE:actual:irrig_crop",
               file = paste0("shrHumanUsesFulfilledMultiple_", t, ".mz"))
    calcOutput("ShrHumanUsesFulfilled",
               transDist = t,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = FALSE,
               file = paste0("shrHumanUsesFulfilledSingle_", t, ".mz"))

  }

  ##############
  # VALIDATION #
  ##############
  # Multiple cropping suitability per crop calculated based on crop and grass productivity
  # (for LPJmL crop types)
  calcOutput("MulticroppingSuitability", sectoral = "lpj",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, suitability = "endogenous",
             aggregate = FALSE, file = "suitMC_LPJmL.mz")

  # Multiple cropping zones according to GAEZ
  calcOutput("MultipleCroppingZones", layers = 8,
             aggregate = FALSE, file = "suitMC_GAEZ.mz")

  # Inverted Growing Period Runs
  # crop yields

}
