#' @title fullMULTICROPPING
#' @description Function that produces output for irrigation potentials
#'              under multiple cropping on cellular resolution.
#'
#' @param yieldcalib     If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                       If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule Rule to be applied for river basin discharge allocation
#'                       across cells of river basin ("optimization", "upstreamfirst")
#' @param rankmethod     Rank and optimization method consisting of
#'                       Unit according to which rank is calculated:
#'                       USD_ha (USD per hectare) for relative area return, or
#'                       USD_m3 (USD per cubic meter) for relative volumetric return;
#'                       USD for absolute return (total profit);
#'                       Price aggregation:
#'                       "GLO" for global average prices, or
#'                       "ISO" for country-level prices
#'                       and boolean indicating fullpotential (TRUE, i.e. cell
#'                       receives full irrigation requirements in total area)
#'                       or reduced potential (FALSE, reduced potential of cell
#'                       receives at later stage in allocation algorithm);
#'                       separated by ":"
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#'
#' @author Felicitas Beier
#'
#' @importFrom stringr str_split
#'
#' @export

fullMULTICROPPING <- function(yieldcalib = "TRUE:TRUE:actual:irrig_crop",
                              allocationrule = "optimization",
                              rankmethod = "USD_ha:GLO:TRUE",
                              transDist = 100) {

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010" # c("y2010", "y2050) --> with potential multicropping!
  plotyear          <- selectyears
  ssp               <- "ssp2"

  # Newest LPJmL runs
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_bft_e511ac58")
  climatetype       <- "MRI-ESM2-0:ssp370"

  irrigationsystem  <- "initialization"
  gtrange           <- c(0, 10, 50, 100, 250, 300, 500, 600,
                         750, 900, 1000, 1500, 2000, 3000)
  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"

  ################
  # MAIN RESULTS #
  ################

  ### Croparea ###
  # share of crop area by crop type (chosen cropmix)
  calcOutput("CropAreaShare", iniyear = iniyear, cropmix = "hist_irrig",
             aggregate = FALSE, file = "cropareaShr.mz")

  calcOutput("CropareaAdjusted", iniyear = iniyear, dataset = "Toolbox",
             aggregate = FALSE, file = "cropareaToolbox.mz")

  ### Yield Gain ###
  # Single cropping yield gain
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL, efrMethod = efrMethod, transDist = transDist,
             irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
             cropmix = "hist_irrig", yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = "yieldgain_single.mz")

  # Single cropping yield gain
  calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL, efrMethod = efrMethod, transDist = transDist,
             irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
             cropmix = "hist_irrig", yieldcalib = yieldcalib,
             multicropping = TRUE, aggregate = FALSE,
             file = "yieldgain_multiple.mz")


  ### Current Uses ###
  # Non-Agricultural water uses (in mio. m^3 / yr) [smoothed]
  calcOutput("RiverHumanUseAccounting",
             iteration = "non_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, multicropping = FALSE,
             transDist = transDist, comAg = FALSE,
             accessibilityrule = NULL,
             rankmethod = NULL, gainthreshold = NULL,
             cropmix = NULL, yieldcalib = NULL,
             irrigationsystem = NULL, landScen = NULL,
             aggregate = FALSE,
             file = "nonAgWatACT.mz")

  # Committed Agricultural water uses
  calcOutput("RiverHumanUseAccounting",
             iteration = "committed_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             efrMethod = efrMethod,
             selectyears = selectyears, iniyear = iniyear,
             transDist = transDist, comAg = TRUE,
             accessibilityrule = NULL,
             rankmethod = NULL, gainthreshold = NULL,
             cropmix = NULL, yieldcalib = NULL,
             irrigationsystem = NULL, landScen = NULL,
             multicropping = FALSE,
             aggregate = FALSE,
             file = "comAgWatACT_single.mz")
  calcOutput("RiverHumanUseAccounting",
             iteration = "committed_agriculture",
             lpjml = lpjml, climatetype = climatetype,
             efrMethod = efrMethod,
             selectyears = selectyears, iniyear = iniyear,
             transDist = transDist, comAg = TRUE,
             accessibilityrule = NULL,
             rankmethod = NULL, gainthreshold = NULL,
             cropmix = NULL, yieldcalib = NULL,
             irrigationsystem = NULL, landScen = NULL,
             multicropping = "TRUE:potential:endogenous",
             aggregate = FALSE,
             file = "comAgWatACT_multi.mz")

  for (t in c(0, 100, 200)) {

    # Committed agricultural area
    calcOutput("IrrigAreaActuallyCommitted",
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = transDist,
               multicropping = FALSE, aggregate = FALSE,
               file = paste0("comAgAreaACT_single_", t, ".mz"))

    calcOutput("IrrigAreaActuallyCommitted",
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = transDist,
               multicropping = "TRUE:potential:endogenous",
               aggregate = FALSE,
               file = paste0("comAgAreaACT_multiple_", t, ".mz"))

    # Share current irrigation water that can be fulfilled by available water resources
    calcOutput("ShrHumanUsesFulfilled",
               transDist = transDist,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = FALSE,
               file = paste0("shrHumanUsesFulfilledSingle_", t, ".mz"))
    calcOutput("ShrHumanUsesFulfilled",
               transDist = transDist,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = "TRUE:potential:endogenous",
               file = paste0("shrHumanUsesFulfilledMultiple_", t, ".mz"))

  }

  ### Yields ###
  # Yields in tDM [for development purposes only. ToDo: Remove]
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

  # Rainfed and irrigated crop yield valued at crop-specific prices [in USD/ha]
  # under single cropping:
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib,
             priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
             multicropping = FALSE,
             aggregate = FALSE,
             file = "yieldsValued_single.mz")
  # under multiple cropping (potential):
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib,
             priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
             multicropping = "TRUE:potential:endogenous",
             aggregate = FALSE,
             file = "yieldsValued_multiplePOT.mz")
  # under multiple cropping (actual):
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib,
             priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
             multicropping = "TRUE:actual:irrig_crop",
             aggregate = FALSE,
             file = "yieldsValued_multipleACT.mz")

  # Required water for full irrigation per cell (in mio. m^3)
  # (including already committed agricultural areas,
  # i.e. full irrigation on all available cropland under chosen landScen)
  calcOutput("FullIrrigationRequirement",
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL, efrMethod = NULL, transDist = NULL,
             lpjml = lpjml, climatetype = climatetype,
             irrigationsystem = irrigationsystem, landScen = "currCropland:NULL",
             cropmix = "hist_total", yieldcalib = yieldcalib,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "reqWatFullirrig_multi.mz")

  calcOutput("FullIrrigationRequirement",
             selectyears = selectyears, iniyear = iniyear,
             comagyear = NULL, efrMethod = NULL, transDist = NULL,
             lpjml = lpjml, climatetype = climatetype,
             irrigationsystem = irrigationsystem, landScen = "currCropland:NULL",
             cropmix = "hist_total", yieldcalib = yieldcalib,
             multicropping = FALSE, aggregate = FALSE,
             file = "reqWatFullirrig_single.mz")

  for (committed in c(TRUE, FALSE)) {
    for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {

      ### Current irrigated area (IRR) ###
      # under single cropping
      calcOutput("EconOfIrrig",
                 scenario = ssp, output = o, gtrange = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule,
                 irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
                 landScen = "currIrrig:NULL", comAg = committed,
                 transDist = transDist,
                 multicropping = FALSE, aggregate = FALSE,
                 file = paste0(o, "EconACTUNSUS", "comAg",
                               as.character(committed), "_single.mz"))
      # under multiple cropping
      calcOutput("EconOfIrrig",
                 scenario = ssp, output = o, gtrange = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule,
                 irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
                 landScen = "currIrrig:NULL", comAg = committed,
                 transDist = transDist,
                 multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
                 file = paste0(o, "EconACTUNSUS", "comAg",
                               as.character(committed), "_multiplePOT.mz"))
      calcOutput("EconOfIrrig",
                 scenario = ssp, output = o, gtrange = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule,
                 irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
                 landScen = "currIrrig:NULL", comAg = committed,
                 transDist = transDist,
                 multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
                 file = paste0(o, "EconACTUNSUS", "comAg",
                               as.character(committed), "_multipleACT.mz"))

      ### Current cropland (CUR) ###
      # under single cropping
      calcOutput("EconOfIrrig",
                 scenario = ssp, output = o, gtrange = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule,
                 irrigationsystem = irrigationsystem, cropmix = "hist_total",
                 landScen = "currCropland:NULL", comAg = committed,
                 transDist = transDist,
                 multicropping = FALSE, aggregate = FALSE,
                 file = paste0(o, "EconCURUNSUS", "comAg",
                               as.character(committed), "_single.mz"))
      # under multiple cropping
      calcOutput("EconOfIrrig",
                 scenario = ssp, output = o, gtrange = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule,
                 irrigationsystem = irrigationsystem, cropmix = "hist_total",
                 landScen = "currCropland:NULL", comAg = committed,
                 transDist = transDist,
                 multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
                 file = paste0(o, "EconCURUNSUS", "comAg",
                               as.character(committed), "_multiplePOT.mz"))
      calcOutput("EconOfIrrig",
                 scenario = ssp, output = o, gtrange = gtrange,
                 selectyears = plotyear, iniyear = iniyear,
                 lpjml = lpjml, climatetype = climatetype,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule,
                 irrigationsystem = irrigationsystem, cropmix = "hist_total",
                 landScen = "currCropland:NULL", comAg = committed,
                 transDist = transDist,
                 multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
                 file = paste0(o, "EconCURUNSUS", "comAg",
                               as.character(committed), "_multipleACT.mz"))
    }
  }

  ### Revenue achieved on respective land area ###
  for (man in c("single_rainfed", "single_irrigated",
                "multiple_rainfed", "multiple_irrigated")) {
    for (land in c("currIrrig:NULL", "currCropland:NULL", "potCropland:NULL")) {

      calcOutput("Revenue", management = man, landScen = land,
                 lpjml = lpjml, climatetype = climatetype,
                 selectyears = selectyears, iniyear = iniyear,
                 efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                 rankmethod = rankmethod, yieldcalib = yieldcalib,
                 allocationrule = allocationrule, gainthreshold = 0,
                 irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
                 comAg = FALSE,
                 transDist = transDist,
                 aggregate = FALSE,
                 file = paste0("Revenue_", strsplit(land, split = ":")[[1]][1], "_", man, ".mz"))
    }
  }

  calcOutput("Revenue",
             management = "actual", landScen = "currIrrig:NULL",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, gainthreshold = 0,
             irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
             comAg = FALSE,
             transDist = transDist,
             aggregate = FALSE,
             file = paste0("Revenue_", "currIrrig", "_", "actual", ".mz"))
  calcOutput("Revenue",
             management = "actual", landScen = "currCropland:NULL",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, gainthreshold = 0,
             irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
             comAg = FALSE,
             transDist = transDist,
             aggregate = FALSE,
             file = paste0("Revenue_", "currCropland", "_", "actual", ".mz"))

  ##############
  # VALIDATION #
  ##############

  # Multiple cropping suitability per crop calculated based on crop and grass productivity
  calcOutput("MulticroppingSuitability",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, suitability = "endogenous",
             aggregate = FALSE, file = "suitMC_LPJmL.mz")

  # Multiple cropping suitability according to GAEZ (Boolean)
  calcOutput("MulticroppingSuitability",
             lpjml = lpjml, climatetype = climatetype,
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
