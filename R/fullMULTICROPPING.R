#' @title fullMULTICROPPING
#' @description Function that produces output for multiple cropping and
#'              irrigation potentials on current cropland
#'              at cellular resolution.
#'
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst")
#' @param comAg             if TRUE: the currently already irrigated areas
#'                                   in initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#'
#' @author Felicitas Beier
#'
#' @importFrom stringr str_split
#'
#' @export

fullMULTICROPPING <- function(allocationrule = "optimization",
                              comAg = TRUE,
                              fossilGW = TRUE,
                              transDist = 100) {
  # scenarios for paper: landScen <- "currCropland:NA", "currIrrig:NA"

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010"
  irrigationsystem  <- "initialization"
  efrMethod         <- "VMF:fair"

  # Newest LPJmL runs
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_bft_e511ac58")
  climatetype       <- "GSWP3-W5E5:historical"

  # Settings for optimization algorithm
  accessibilityrule <- "CV:2"
  rankmethod        <- "USD_m3:GLO:TRUE"
  ssp               <- "ssp2"
  efp               <- "off"

  # Assumption in this study:
  # only technical potential is reported for the purpose of this analysis:
  gtrange <- gainthreshold <- 0
  # potential yields from LPJmL to derive multiple cropping potentials
  yieldcalib        <- "TRUE:TRUE:actual:irrig_crop" # FALSE
  # Historical cropmix
  cropmix           <- "hist_total"

  #########################
  # Groundwater component #
  #########################
  for (t in c(0, 100, 200)) {
    calcOutput("NonrenGroundwatUse", output = "total",
               lpjml = lpjml, climatetype = climatetype,
               transDistGW = t, multicropping = "TRUE:actual:irrig_crop",
               selectyears = selectyears, iniyear = iniyear,
               aggregate = FALSE,
               file = paste0("groundwater_tD", as.character(t), ".mz"))

  }

  ####################
  # CURRENT CROPAREA #
  ####################
  # share of crop area by crop type used to determine potentially irrigated areas
  calcOutput("CropAreaShare", iniyear = iniyear, cropmix = cropmix,
             aggregate = FALSE, file = "cropareaShr.mz")

  # croparea in Mha
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

  # fallow land (in Mha)
  calcOutput("FallowLand", years = selectyears, aggregate = FALSE,
             file = "fallowLand.mz")

  ######################
  # WATER REQUIREMENTS #
  ######################
  calcOutput("ActualIrrigWatRequirements",
             irrigationsystem = irrigationsystem,
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             multicropping = FALSE, aggregate = FALSE,
             file = "watReq_single.mz")
  calcOutput("ActualIrrigWatRequirements",
             irrigationsystem = irrigationsystem,
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "watReq_multiple.mz")

  ####################
  #   MULTICROPPING  #
  ####################
  # current multiple cropping intensity
  calcOutput("MulticroppingIntensity",
             scenario = "irrig_crop",
             selectyears = selectyears, sectoral = "kcr",
             file = "croppingIntensity.mz", aggregate = FALSE)

  # cells where multiple cropping happens
  calcOutput("MulticroppingCells", scenario = "actual:irrig_crop", sectoral = "kcr",
             selectyears = selectyears, lpjml = lpjml, climatetype = climatetype,
             aggregate = FALSE, file = "currMC.mz")

  # potential multiple cropping suitability
  calcOutput("MulticroppingSuitability", selectyears = selectyears,
             lpjml = lpjml, climatetype = climatetype,
             suitability = "endogenous", sectoral = "kcr",
             file = "multicroppingSuitability.mz", aggregate = FALSE)

  ###############
  # CROP YIELDS #
  ###############
  # potential (non-calibrated) yields under single cropping (in USD/ha)
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE, priceAgg = "GLO",
             multicropping = FALSE, aggregate = FALSE,
             file = "yieldValued_single.mz")
  # potential (non-calibrated) yields under multiple cropping (in USD/ha)
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE, priceAgg = "GLO",
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "yieldValued_multiple.mz")
  # actual (calibrated) yields under single cropping (in USD/ha)
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", priceAgg = "GLO", #### or TRUE:FALSE?
             multicropping = FALSE, aggregate = FALSE,
             file = "yieldValued_single_calib.mz")
  # actual (calibrated) yields under multiple cropping (in USD/ha)
  calcOutput("YieldsValued",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", priceAgg = "GLO",
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "yieldValued_multiple_calib.mz")

  # potential (non-calibrated) yield under single cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE,
             multicropping = FALSE, aggregate = FALSE,
             file = "yield_single.mz")
  # potential (non-calibrated) yield under multiple cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "yield_multiple.mz")
  # actual (calibrated) yield under single cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", #### or TRUE:FALSE?
             multicropping = FALSE, aggregate = FALSE,
             file = "yield_single_calib.mz")
  # actual (calibrated) yield under multiple cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = "TRUE:TRUE:actual:irrig_crop",
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "yield_multiple_calib.mz")


  #########################
  # IRRIGATION POTENTIALS #
  #########################
  ### (a) Areas ###
  # potentially irrigated area on current cropland (under single cropping conditions)
  calcOutput("PotIrrigAreas", cropAggregation = FALSE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currCropland:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = FALSE, transDist = transDist,
             aggregate = FALSE,
             file = "piaCUR_single.mz")
  # potentially irrigated area on current cropland (under current multiple cropping conditions)
  calcOutput("PotIrrigAreas", cropAggregation = FALSE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currCropland:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = "TRUE:actual:irrig_crop", transDist = transDist,
             aggregate = FALSE,
             file = "piaCUR_multACT.mz")
  # potentially irrigated area on current cropland (under consideration of potential multiple cropping)
  calcOutput("PotIrrigAreas", cropAggregation = FALSE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currCropland:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = "TRUE:potential:endogenous", transDist = transDist,
             aggregate = FALSE,
             file = "piaCUR_multPOT.mz")
  # potentially irrigated area on currently irrigated cropland (under consideration of potential multiple cropping)
  calcOutput("PotIrrigAreas", cropAggregation = FALSE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currIrrig:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = "TRUE:potential:endogenous", transDist = transDist,
             aggregate = FALSE,
             file = "piaIRR_multPOT.mz")

  ### (B) Water Use ###
  # potentially irrigation water on current cropland (under single cropping conditions)
  calcOutput("PotWater",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currCropland:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = FALSE, transDist = transDist,
             aggregate = FALSE,
             file = "piwCUR_single.mz")
  # potentially irrigation water on current cropland (under current multiple cropping conditions)
  calcOutput("PotWater",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currCropland:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = "TRUE:actual:irrig_crop", transDist = transDist,
             aggregate = FALSE,
             file = "piwCUR_multACT.mz")
  # potentially irrigation water on current cropland (under consideration of potential multiple cropping)
  calcOutput("PotWater",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currCropland:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = "TRUE:potential:endogenous", transDist = transDist,
             aggregate = FALSE,
             file = "piwCUR_multPOT.mz")
  # potentially irrigation water on currently irrigated cropland (under consideration of potential multiple cropping)
  calcOutput("PotWater",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
             gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
             landScen = "currIrrig:NULL",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = "TRUE:potential:endogenous", transDist = transDist,
             aggregate = FALSE,
             file = "piwIRR_multPOT.mz")

  # Potential multiple cropping share
  calcOutput("PotMulticroppingShare", scenario = paste(efp, ssp, sep = "."),
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule, gainthreshold = gainthreshold,
             irrigationsystem = irrigationsystem, landScen = "currCropland:NA",
             cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
             multicropping = "TRUE:potential:endogenous", transDist = transDist,
             aggregate = FALSE,
             file = "potMCshare.mz")

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
               file = paste0("comAgAreaACT_single_", t, ".mz")
    )

    calcOutput("IrrigAreaActuallyCommitted",
               fossilGW = FALSE,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, transDist = t,
               multicropping = "TRUE:actual:irrig_crop",
               aggregate = FALSE,
               file = paste0("comAgAreaACT_multipleACT_", t, ".mz")
    )


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
               file = paste0("comAgWatACT_single_", t, ".mz")
    )
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
               file = paste0("comAgWatACT_multipleACT_", t, ".mz")
    )
    calcOutput("RiverHumanUseAccounting",
               iteration = "committed_agriculture_fullMulticropping",
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod,
               selectyears = selectyears, iniyear = iniyear,
               transDist = t, comAg = TRUE,
               accessibilityrule = accessibilityrule,
               rankmethod = NULL, gainthreshold = NULL,
               cropmix = NULL, yieldcalib = NULL,
               irrigationsystem = NULL, landScen = NULL,
               multicropping = "TRUE:potential:endogenous",
               aggregate = FALSE,
               file = paste0("comAgWatACT_multiplePOT_", t, ".mz")
    )


    # Share current irrigation water that can be fulfilled by available water resources
    calcOutput("ShrHumanUsesFulfilled",
               transDist = t,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = "TRUE:actual:irrig_crop",
               file = paste0("shrHumanUsesFulfilledMultiple_", t, ".mz")
    )
    calcOutput("ShrHumanUsesFulfilled",
               transDist = t,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = FALSE,
               file = paste0("shrHumanUsesFulfilledSingle_", t, ".mz")
    )
  }

  ##############
  # CORRECTION #
  ##############
  watReqFirst <- calcOutput("ActualIrrigWatRequirements",
                            multicropping = FALSE,
                            selectyears = selectyears, iniyear = iniyear,
                            lpjml = lpjml, climatetype = climatetype,
                            irrigationsystem = irrigationsystem,
                            aggregate = FALSE, file = "watReqFirst.mz")
  # Irrigation water requirements in the entire year under multiple cropping (in m^3 per ha per yr):
  watReqYear  <- calcOutput("ActualIrrigWatRequirements",
                            multicropping = "TRUE:potential:endogenous",
                            selectyears = selectyears, iniyear = iniyear,
                            lpjml = lpjml, climatetype = climatetype,
                            irrigationsystem = irrigationsystem,
                            aggregate = FALSE, file = "watReqYear.mz")

  ##############
  # VALIDATION #
  ##############
  # Multiple cropping suitability per crop calculated based on crop and grass productivity
  # (for LPJmL crop types)
  calcOutput("MulticroppingSuitability", sectoral = "lpj",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, suitability = "endogenous",
             aggregate = FALSE, file = "suitMC_LPJmL.mz"
  )

  # Multiple cropping zones according to GAEZ
  calcOutput("MultipleCroppingZones",
             layers = 8,
             aggregate = FALSE, file = "suitMC_GAEZ.mz"
  )

  # Inverted Growing Period Runs
  # crop yields



  ###########
  # Revenue #
  ###########
  ### Revenue achieved on respective land area ###
  # revenue unit: mio. USD
  # biomass runit: mio. tDM
  for (o in c("biomass", "revenue")) {
    for (man in c("single:potential", "single:counterfactual",
                  "actMC:potential", "actMC:counterfactual",
                  "potMC:potential", "potMC:counterfactual")) {
      for (a in c("actual", "currIrrig:NA", "currCropland:NA")) {
        for (calib in c(FALSE, TRUE)) {

          if (calib) {
            c <- "TRUE:TRUE:actual:irrig_crop"
          } else {
            c <- FALSE
          }

          calcOutput("CropProductionRevenue",
                     outputtype = o,
                     scenario = paste(efp, ssp, sep = "."),
                     management = man,
                     area = a,
                     yieldcalib = c,
                     lpjml = lpjml, climatetype = climatetype,
                     selectyears = selectyears, iniyear = iniyear,
                     efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                     rankmethod = rankmethod,
                     allocationrule = allocationrule, gainthreshold = gtrange,
                     irrigationsystem = irrigationsystem, cropmix = cropmix,
                     transDist = transDist, fossilGW = fossilGW, comAg = comAg,
                     file = paste0(o, "_",
                                   gsub(":", "_", man), "_",
                                   str_split(a, ":")[[1]][1],
                                   "calib", calib,
                                   ".mz"), aggregate = FALSE)
        }
      }
    }
  }


}



#
#   ################
#   # MAIN RESULTS #
#   ################
#
#   ### Croparea ###
#   # share of crop area by crop type (chosen cropmix)
#   calcOutput("CropAreaShare", iniyear = iniyear, cropmix = "hist_irrig",
#              aggregate = FALSE, file = "cropareaShr.mz")
#
#   calcOutput("CropareaAdjusted", iniyear = iniyear, dataset = "LandInG",
#              aggregate = FALSE, file = "cropareaLandInG.mz")
#
#   ### Yield Gain ###
#   # Single cropping yield gain
#   calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
#              lpjml = lpjml, climatetype = climatetype,
#              selectyears = selectyears, iniyear = iniyear,
#              comagyear = NULL,
#              irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
#              cropmix = "hist_irrig", yieldcalib = yieldcalib,
#              multicropping = FALSE, aggregate = FALSE,
#              file = "yieldgain_single.mz")
#
#   # Multiple cropping yield gain
#   calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha:GLO",
#              lpjml = lpjml, climatetype = climatetype,
#              selectyears = selectyears, iniyear = iniyear,
#              comagyear = NULL,
#              irrigationsystem = irrigationsystem, landScen = paste0("potCropland:", "NULL"),
#              cropmix = "hist_irrig", yieldcalib = yieldcalib,
#              multicropping = TRUE, aggregate = FALSE,
#              file = "yieldgain_multiple.mz")
#
#
#   ### Current Uses ###
#   # Non-Agricultural water uses (in mio. m^3 / yr) [smoothed]
#   calcOutput("RiverHumanUseAccounting",
#              iteration = "non_agriculture",
#              lpjml = lpjml, climatetype = climatetype,
#              selectyears = selectyears, iniyear = iniyear,
#              efrMethod = efrMethod, multicropping = FALSE,
#              transDist = transDist, comAg = FALSE,
#              accessibilityrule = NULL,
#              rankmethod = NULL, gainthreshold = NULL,
#              cropmix = NULL, yieldcalib = NULL,
#              irrigationsystem = NULL, landScen = NULL,
#              aggregate = FALSE,
#              file = "nonAgWatACT.mz")
#
#   # Committed Agricultural water uses
#   calcOutput("RiverHumanUseAccounting",
#              iteration = "committed_agriculture",
#              lpjml = lpjml, climatetype = climatetype,
#              efrMethod = efrMethod,
#              selectyears = selectyears, iniyear = iniyear,
#              transDist = transDist, comAg = TRUE,
#              accessibilityrule = NULL,
#              rankmethod = NULL, gainthreshold = NULL,
#              cropmix = NULL, yieldcalib = NULL,
#              irrigationsystem = NULL, landScen = NULL,
#              multicropping = FALSE,
#              aggregate = FALSE,
#              file = "comAgWatACT_single.mz")
#   calcOutput("RiverHumanUseAccounting",
#              iteration = "committed_agriculture",
#              lpjml = lpjml, climatetype = climatetype,
#              efrMethod = efrMethod,
#              selectyears = selectyears, iniyear = iniyear,
#              transDist = transDist, comAg = TRUE,
#              accessibilityrule = NULL,
#              rankmethod = NULL, gainthreshold = NULL,
#              cropmix = NULL, yieldcalib = NULL,
#              irrigationsystem = NULL, landScen = NULL,
#              multicropping = "TRUE:actual:irrig_crop",
#              aggregate = FALSE,
#              file = "comAgWatACT_multi.mz")
#
#   for (t in c(0, 100, 200)) {
#
#
#     # Share current irrigation water that can be fulfilled by available water resources
#     calcOutput("ShrHumanUsesFulfilled",
#                transDist = t,
#                lpjml = lpjml, climatetype = climatetype,
#                selectyears = selectyears, iniyear = iniyear,
#                efrMethod = efrMethod, aggregate = FALSE,
#                multicropping = FALSE,
#                file = paste0("shrHumanUsesFulfilledSingle_", t, ".mz"))
#     calcOutput("ShrHumanUsesFulfilled",
#                transDist = t,
#                lpjml = lpjml, climatetype = climatetype,
#                selectyears = selectyears, iniyear = iniyear,
#                efrMethod = efrMethod, aggregate = FALSE,
#                multicropping = "TRUE:actual:irrig_crop",
#                file = paste0("shrHumanUsesFulfilledMultiple_", t, ".mz"))
#
#   }
#
#   ### Yields ###
#   # Yields in tDM [for development purposes only. ToDo: Remove]
#   calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears,
#              yieldcalib = FALSE,
#              multicropping = FALSE, aggregate = FALSE,
#              file = paste0("Yields", "_single.mz"))
#   calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears,
#              yieldcalib = yieldcalib,
#              multicropping = FALSE, aggregate = FALSE,
#              file = paste0("calibYields", "_single.mz"))
#   calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
#              yieldcalib = FALSE,
#              multicropping = "TRUE:potential:endogenous",
#              file = paste0("Yields", "_multiplePOT.mz"))
#   calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
#              yieldcalib = yieldcalib,
#              multicropping = "TRUE:potential:endogenous",
#              file = paste0("calibYields", "_multiplePOT.mz"))
#   calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears, aggregate = FALSE,
#              yieldcalib = yieldcalib,
#              multicropping = "TRUE:actual:irrig_crop",
#              file = paste0("calibYields", "_multipleACT.mz"))
#
#   # Rainfed and irrigated crop yield valued at crop-specific prices [in USD/ha]
#   # under single cropping:
#   calcOutput("YieldsValued",
#              lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears,
#              yieldcalib = yieldcalib,
#              priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
#              multicropping = FALSE,
#              aggregate = FALSE,
#              file = "yieldsValued_single.mz")
#   # under multiple cropping (potential):
#   calcOutput("YieldsValued",
#              lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears,
#              yieldcalib = yieldcalib,
#              priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
#              multicropping = "TRUE:potential:endogenous",
#              aggregate = FALSE,
#              file = "yieldsValued_multiplePOT.mz")
#   # under multiple cropping (actual):
#   calcOutput("YieldsValued",
#              lpjml = lpjml, climatetype = climatetype,
#              iniyear = iniyear, selectyears = selectyears,
#              yieldcalib = yieldcalib,
#              priceAgg = unlist(strsplit(rankmethod, split = ":"))[2],
#              multicropping = "TRUE:actual:irrig_crop",
#              aggregate = FALSE,
#              file = "yieldsValued_multipleACT.mz")
#
#   for (committed in c(FALSE, TRUE)) {
#     for (o in c("IrrigArea", "wat_ag_ww", "wat_ag_wc")) {
#
#       ### Current irrigated area (IRR) ###
#       # under single cropping
#       calcOutput("EconOfIrrig",
#                  scenario = ssp, output = o, gtrange = gtrange,
#                  selectyears = plotyear, iniyear = iniyear,
#                  lpjml = lpjml, climatetype = climatetype,
#                  efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                  rankmethod = rankmethod, yieldcalib = yieldcalib,
#                  allocationrule = allocationrule,
#                  irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
#                  landScen = "currIrrig:NULL", comAg = committed,
#                  transDist = transDist,
#                  multicropping = FALSE, aggregate = FALSE,
#                  file = paste0(o, "EconACTUNSUS", "comAg",
#                                as.character(committed), "_single.mz"))
#       # under multiple cropping
#       calcOutput("EconOfIrrig",
#                  scenario = ssp, output = o, gtrange = gtrange,
#                  selectyears = plotyear, iniyear = iniyear,
#                  lpjml = lpjml, climatetype = climatetype,
#                  efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                  rankmethod = rankmethod, yieldcalib = yieldcalib,
#                  allocationrule = allocationrule,
#                  irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
#                  landScen = "currIrrig:NULL", comAg = committed,
#                  transDist = transDist,
#                  multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
#                  file = paste0(o, "EconACTUNSUS", "comAg",
#                                as.character(committed), "_multiplePOT.mz"))
#       calcOutput("EconOfIrrig",
#                  scenario = ssp, output = o, gtrange = gtrange,
#                  selectyears = plotyear, iniyear = iniyear,
#                  lpjml = lpjml, climatetype = climatetype,
#                  efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                  rankmethod = rankmethod, yieldcalib = yieldcalib,
#                  allocationrule = allocationrule,
#                  irrigationsystem = irrigationsystem, cropmix = "hist_irrig",
#                  landScen = "currIrrig:NULL", comAg = committed,
#                  transDist = transDist,
#                  multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
#                  file = paste0(o, "EconACTUNSUS", "comAg",
#                                as.character(committed), "_multipleACT.mz"))
#
#       ### Current cropland (CUR) ###
#       # under single cropping
#       calcOutput("EconOfIrrig",
#                  scenario = ssp, output = o, gtrange = gtrange,
#                  selectyears = plotyear, iniyear = iniyear,
#                  lpjml = lpjml, climatetype = climatetype,
#                  efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                  rankmethod = rankmethod, yieldcalib = yieldcalib,
#                  allocationrule = allocationrule,
#                  irrigationsystem = irrigationsystem, cropmix = "hist_total",
#                  landScen = "currCropland:NULL", comAg = committed,
#                  transDist = transDist,
#                  multicropping = FALSE, aggregate = FALSE,
#                  file = paste0(o, "EconCURUNSUS", "comAg",
#                                as.character(committed), "_single.mz"))
#       # under multiple cropping
#       calcOutput("EconOfIrrig",
#                  scenario = ssp, output = o, gtrange = gtrange,
#                  selectyears = plotyear, iniyear = iniyear,
#                  lpjml = lpjml, climatetype = climatetype,
#                  efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                  rankmethod = rankmethod, yieldcalib = yieldcalib,
#                  allocationrule = allocationrule,
#                  irrigationsystem = irrigationsystem, cropmix = "hist_total",
#                  landScen = "currCropland:NULL", comAg = committed,
#                  transDist = transDist,
#                  multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
#                  file = paste0(o, "EconCURUNSUS", "comAg",
#                                as.character(committed), "_multiplePOT.mz"))
#       calcOutput("EconOfIrrig",
#                  scenario = ssp, output = o, gtrange = gtrange,
#                  selectyears = plotyear, iniyear = iniyear,
#                  lpjml = lpjml, climatetype = climatetype,
#                  efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                  rankmethod = rankmethod, yieldcalib = yieldcalib,
#                  allocationrule = allocationrule,
#                  irrigationsystem = irrigationsystem, cropmix = "hist_total",
#                  landScen = "currCropland:NULL", comAg = committed,
#                  transDist = transDist,
#                  multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
#                  file = paste0(o, "EconCURUNSUS", "comAg",
#                                as.character(committed), "_multipleACT.mz"))
#     }
#   }
#
#
#   ##############
#   # VALIDATION #
#   ##############
#
#   # Multiple cropping suitability per crop calculated based on crop and grass productivity
#   calcOutput("MulticroppingSuitability",
#              lpjml = lpjml, climatetype = climatetype,
#              selectyears = selectyears, suitability = "endogenous",
#              aggregate = FALSE, file = "suitMC_LPJmL.mz")
#
#   # Multiple cropping suitability according to GAEZ (Boolean)
#   calcOutput("MulticroppingSuitability",
#              lpjml = lpjml, climatetype = climatetype,
#              selectyears = selectyears, suitability = "exogenous",
#              aggregate = FALSE, file = "suitMC_GAEZ2.mz")
#
#   # Multiple cropping suitability according to GAEZ
#   calcOutput("MultipleCroppingZones", layers = 8,
#              aggregate = FALSE, file = "suitMC_GAEZ8.mz")
#
#   # LandInG
#   calcOutput("CropareaLandInG", physical = TRUE, sectoral = "kcr",
#              cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
#              selectyears = selectyears, aggregate = FALSE,
#              file = "LandInGPHYS.mz")
#   calcOutput("CropareaLandInG", physical = FALSE, sectoral = "kcr",
#              cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
#              selectyears = selectyears, aggregate = FALSE,
#              file = "LandInGHARV.mz")
#
#   calcOutput("MulticroppingCells", selectyears = selectyears,
#              lpjml = lpjml, climatetype = climatetype, scenario = "actual:irrig_crop",
#              aggregate = FALSE, file = "LandInGMulticropping.mz")
#
# }
