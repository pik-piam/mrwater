#' @title fullTRANSPORT
#' @description Function that produces output for analysis of
#'              water transport for provision to cells in surrounding
#'
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
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
#'
#' @author Felicitas Beier
#'
#' @importFrom madrat calcOutput
#' @importFrom stringr str_split
#' @export

fullTRANSPORT <- function(multicropping, rankmethod = "USD_ha:GLO:TRUE") {

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010"
  cropmix           <- "hist_irrig"
  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  allocationrule    <- "optimization"
  irrigationsystem  <- "initialization"

  # Newest LPJmL runs
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_bft_e511ac58")
  climatetype       <- "MRI-ESM2-0:ssp370"

  # Technical potential chosen for Irrigation Potentials
  gainthreshold     <- 0

  # Selected transport distances
  distances         <- c(0, 80, 100, 200, 300, 500, 1000)

  if (as.logical(str_split(multicropping, ":")[[1]][1])) {
    yieldcalib        <- "TRUE:TRUE:actual:irrig_crop"
  } else {
    yieldcalib        <- "TRUE:FALSE"
  }

  ##################################
  ###      Current Croparea      ###
  ##################################
  # Landuse Toolbox Croparea
  # Unit: Mha
  calcOutput("CropareaAdjusted",
             iniyear = iniyear, dataset = "Toolbox",
             aggregate = FALSE,
             file = "cropareaToolbox.mz")

  # Cropmix that was basis for calculations
  calcOutput("CropAreaShare",
             iniyear = iniyear, cropmix = cropmix,
             aggregate = FALSE,
             file = "cropareaShare.mz")

  # Share of water consumption that can be fulfilled by local water resources
  # or resources in certain radius
  for (transDist in distances) {
    calcOutput("ShrHumanUsesFulfilled",
               multicropping = multicropping, transDist = transDist,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               file = paste0("shrHumanUsesFulfilled_", as.character(transDist), "km.mz"))
  }

  #####################################################
  ### Crop specific yields and water requirements   ###
  #####################################################
  calcOutput("IrrigCropYieldGain", priceAgg = "GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib,
             multicropping = multicropping, aggregate = FALSE,
             file = "cropyieldgain.mz")
  calcOutput("IrrigYieldImprovementPotential",
            selectyears = selectyears, iniyear = iniyear,
            lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
            unit = "USD_ha:GLO", yieldcalib = yieldcalib,
            comagyear = NULL, efrMethod = NULL, transDist = NULL,
            irrigationsystem = irrigationsystem,
            landScen = paste0("potCropland:", "NULL"),
            multicropping = multicropping, aggregate = FALSE,
            file = "irriggain.mz")

  calcOutput("ActualIrrigWatRequirements",
             irrigationsystem = irrigationsystem,
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             multicropping = multicropping, aggregate = FALSE,
             file = "cropwatrequirements.mz")

  # requested human uses
  calcOutput("WaterUseCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = multicropping, aggregate = FALSE,
             file = "comAgwatuse.mz")
  calcOutput("WaterUseNonAg",
              selectyears = selectyears, cells = "lpjcell",
              datasource = "WATERGAP_ISIMIP", usetype = "total",
              seasonality = "total", harmonType = "average",
              lpjml = NULL, climatetype = NULL, aggregate = FALSE,
              file = "nonAgwatuse.mz")

  ##################################
  ### Potentially Irrigated Area ###
  ##################################
  # Potentially irrigated area for different water provision distances
  # min distance: 0 (no water diversion)
  # max distance: 1000km (comparable to China South-to-North-Water-Diversion-Project 1155km)
  for (transDist in distances) {

    ### Current Human Uses ###
    # Non-Agricultural
    calcOutput("RiverHumanUseAccounting",
               iteration = "non_agriculture",
               lpjml = lpjml, climatetype = climatetype,
               transDist = transDist, comAg = NULL,
               efrMethod = efrMethod, multicropping = multicropping,
               selectyears = selectyears, iniyear = iniyear,
               accessibilityrule = NULL,
               rankmethod = NULL, gainthreshold = NULL,
               cropmix = NULL, yieldcalib = NULL,
               irrigationsystem = NULL, landScen = NULL,
               aggregate = FALSE,
               file = paste0("nonAgUses_", transDist, "km.mz"))

    # Agricultural
    calcOutput("RiverHumanUseAccounting",
                iteration = "committed_agriculture",
                lpjml = lpjml, climatetype = climatetype,
                transDist = transDist, comAg = NULL,
                efrMethod = efrMethod, multicropping = multicropping,
                selectyears = selectyears, iniyear = iniyear,
                accessibilityrule = NULL,
                rankmethod = NULL, gainthreshold = NULL,
                cropmix = NULL, yieldcalib = NULL,
                irrigationsystem = NULL, landScen = NULL,
                aggregate = FALSE,
                file = paste0("comAgUses_", transDist, "km.mz"))

    # Current State (Committed Agricultural Area)
    calcOutput("IrrigAreaPotential", gainthreshold = gainthreshold,
               landScen = paste0("currIrrig:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               comAg = TRUE, multicropping = multicropping,
               aggregate = FALSE,
               file = paste0("curpotIrrigArea_", transDist, "km.mz"))

    calcOutput("WaterUsePotential", gainthreshold = gainthreshold,
               landScen = paste0("currIrrig:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               multicropping = multicropping, comAg = TRUE,
               aggregate = FALSE,
               file = paste0("curpotIrrigWater_", transDist, "km.mz"))

    # Potentially Irrigated Area (with previously committed areas)
    calcOutput("IrrigAreaPotential", gainthreshold = gainthreshold,
               landScen = paste0("currCropland:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               comAg = TRUE, multicropping = multicropping,
               aggregate = FALSE,
               file = paste0("potIrrigArea_", transDist, "km.mz"))

    calcOutput("WaterUsePotential", gainthreshold = gainthreshold,
               landScen = paste0("currCropland:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               multicropping = multicropping, comAg = TRUE,
               aggregate = FALSE,
               file = paste0("potIrrigWater_", transDist, "km.mz"))

    # Potentially Irrigated Area (without committed areas)
    calcOutput("IrrigAreaPotential", gainthreshold = gainthreshold,
               landScen = paste0("currCropland:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               comAg = FALSE, multicropping = multicropping,
               aggregate = FALSE,
               file = paste0("fullpotIrrigArea_", transDist, "km.mz"))

    calcOutput("WaterUsePotential", gainthreshold = gainthreshold,
               landScen = paste0("currCropland:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               multicropping = multicropping, comAg = FALSE,
               aggregate = FALSE,
               file = paste0("fullpotIrrigWater_", transDist, "km.mz"))


    ### Return from Irrigation ###
    # global irrigation return
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currIrrig:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = TRUE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSD_currIRR_", transDist, "km_comAg.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currIrrig:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = FALSE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSD_currIRR_", transDist, "km.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currCropland:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = TRUE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSD_currCROP_", transDist, "km_comAg.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currCropland:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = FALSE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSD_currCROP_", transDist, "km.mz"))
    # global irrigation return per hectare
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currIrrig:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = TRUE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDha_currIRR_", transDist, "km_comAg.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currIrrig:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = FALSE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDha_currIRR_", transDist, "km.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currCropland:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = TRUE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDha_currCROP_", transDist, "km_comAg.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_ha",
               landScen = paste0("currCropland:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = FALSE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDha_currCROP_", transDist, "km.mz"))
    # global irrigation return per m3
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_m3",
               landScen = paste0("currIrrig:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = TRUE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDm3_currIRR_", transDist, "km_comAg.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_m3",
               landScen = paste0("currIrrig:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = FALSE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDm3_currIRR_", transDist, "km.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_m3",
               landScen = paste0("currCropland:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = TRUE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDm3_currCROP_", transDist, "km_comAg.mz"))
    calcOutput("IrrigationReturn", country = "GLO", profitType = "USD_m3",
               landScen = paste0("currCropland:", "NULL"),
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear, efrMethod = efrMethod,
               accessibilityrule = accessibilityrule, rankmethod = rankmethod,
               yieldcalib = yieldcalib, allocationrule = allocationrule,
               gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
               cropmix = cropmix, potentialWat = TRUE, comAg = FALSE,
               multicropping = multicropping, transDist = transDist,
               aggregate = FALSE,
               file = paste0("returnUSDm3_currCROP_", transDist, "km.mz"))
  }
}
