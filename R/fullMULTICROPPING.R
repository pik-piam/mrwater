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
  lpjml             <- "lpjml5.9.16-m1"
  climatetype       <- "MRI-ESM2-0:ssp370"

  # Settings for optimization algorithm
  accessibilityrule <- "CV:2"
  rankmethod        <- "USD_m3:GLO:TRUE"
  ssp               <- "ssp2"
  efp               <- "off"

  # Assumption in this study:
  # only technical potential is reported for the purpose of this analysis:
  gtrange <- gainthreshold <- 0
  # potential yields from LPJmL to derive multiple cropping potentials:
  yieldcalib        <- FALSE
  # yield calibration setting for where calibrated yields are used in analysis:
  calibDetails      <- "country:5"
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

  ######################################
  # Blue water consumption regressions #
  ######################################
  calcOutput("BlueWaterConsumptionOff", selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype, interim = "TRUE:bconsCrop",
             aggregate = FALSE, file = "BWCcrop.mz")
  calcOutput("BlueWaterConsumptionOff", selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype, interim = "TRUE:bconsGrass",
             aggregate = FALSE, file = "BWCgrass.mz")

  ####################
  # CURRENT CROPAREA #
  ####################
  # share of crop area by crop type used to determine potentially irrigated areas
  #calcOutput("CropAreaShare", iniyear = iniyear, cropmix = cropmix,
  #           aggregate = FALSE, file = "cropareaShr.mz")

  # croparea in Mha
  calcOutput("CropareaAdjusted", iniyear = iniyear,
             dataset = "LandInG", sectoral = "kcr",
             aggregate = FALSE, file = "cropareaLandInG.mz")
  #calcOutput("CropareaAdjusted", iniyear = iniyear,
  #           dataset = "LandInG", sectoral = "lpj",
  #           aggregate = FALSE, file = "cropareaLandInG_lpj.mz")

  calcOutput("CropareaLandInG", physical = TRUE, sectoral = "kcr",
             cellular = TRUE, irrigation = TRUE,
             selectyears = selectyears, aggregate = FALSE,
             file = "LandingPHYS.mz")
  calcOutput("CropareaLandInG", physical = FALSE, sectoral = "kcr",
             cellular = TRUE, irrigation = TRUE,
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
  #calcOutput("YieldsValued",
  #           lpjml = lpjml, climatetype = climatetype,
  #           iniyear = iniyear, selectyears = selectyears,
  #           yieldcalib = FALSE, calibDetails = calibDetails,
  #           priceAgg = "GLO",
  #           multicropping = FALSE, aggregate = FALSE,
  #           file = "yieldValued_single.mz")
  # potential (non-calibrated) yields under multiple cropping (in USD/ha)
  #calcOutput("YieldsValued",
  #           lpjml = lpjml, climatetype = climatetype,
  #           iniyear = iniyear, selectyears = selectyears,
  #           yieldcalib = FALSE, calibDetails = calibDetails,
  #           priceAgg = "GLO",
  #           multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
  #           file = "yieldValued_multiple.mz")

  # actual (calibrated) yields under single cropping (in USD/ha)
  #calcOutput("YieldsValued",
  #           lpjml = lpjml, climatetype = climatetype,
  #           iniyear = iniyear, selectyears = selectyears,
  #           yieldcalib = "TRUE:TRUE:actual:irrig_crop", calibDetails = calibDetails,
  #           priceAgg = "GLO",
  #           multicropping = FALSE, aggregate = FALSE,
  #           file = "yieldValued_single_calib.mz")
  # actual (calibrated) yields under multiple cropping (in USD/ha)
  #calcOutput("YieldsValued",
  #           lpjml = lpjml, climatetype = climatetype,
  #           iniyear = iniyear, selectyears = selectyears,
  #           yieldcalib = "TRUE:TRUE:actual:irrig_crop", calibDetails = calibDetails,
  #           priceAgg = "GLO",
  #           multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
  #           file = "yieldValued_multiple_calib_act.mz")
  # potential (calibrated) yields under multiple cropping (in USD/ha)
  #calcOutput("YieldsValued",
  #           lpjml = lpjml, climatetype = climatetype,
  #           iniyear = iniyear, selectyears = selectyears,
  #           yieldcalib = "TRUE:TRUE:actual:irrig_crop", calibDetails = calibDetails,
  #           priceAgg = "GLO",
  #           multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
  #           file = "yieldValued_multiple_calib_pot.mz")

  # potential (non-calibrated) yield under single cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE, calibDetails = calibDetails,
             multicropping = FALSE, aggregate = FALSE,
             file = "yield_single.mz")
  # potential (non-calibrated) yield under actual multiple cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE, calibDetails = calibDetails,
             multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
             file = "yield_multiple_act.mz")
  # potential (non-calibrated) yield under potential multiple cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = FALSE, calibDetails = calibDetails,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "yield_multiple_pot.mz")

  # counterfactual (calibrated) yield under single cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", calibDetails = calibDetails,
             multicropping = FALSE, aggregate = FALSE,
             file = "yield_single_calib.mz")
  # actual (calibrated) yield under multiple cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", calibDetails = calibDetails,
             multicropping = "TRUE:actual:irrig_crop", aggregate = FALSE,
             file = "yield_multiple_calib_act.mz")
  # potential (calibrated) yield under multiple cropping (in tDM)
  calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", calibDetails = calibDetails,
             multicropping = "TRUE:potential:endogenous", aggregate = FALSE,
             file = "yield_multiple_calib_pot.mz")

  # FAO production (for comparison)
  calcOutput("Production", products = "kcr", attributes = "dm",
             irrigation = FALSE, cellular = FALSE,
             aggregate = FALSE, file = "FAOproduction.mz")


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
  #calcOutput("PotWater",
  #           lpjml = lpjml, climatetype = climatetype,
  #           selectyears = selectyears, iniyear = iniyear,
  #           efrMethod = efrMethod, accessibilityrule = accessibilityrule,
  #           rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
  #           gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
  #           landScen = "currIrrig:NULL",
  #           cropmix = cropmix, comAg = comAg, fossilGW = fossilGW,
  #           multicropping = "TRUE:potential:endogenous", transDist = transDist,
  #           aggregate = FALSE,
  #           file = "piwIRR_multPOT.mz")

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

  for (t in c(0, 100, 200)) {
    # Share current irrigation water that can be fulfilled by available water resources
    calcOutput("ShrHumanUsesFulfilled",
               transDist = t,
               lpjml = lpjml, climatetype = climatetype,
               selectyears = selectyears, iniyear = iniyear,
               efrMethod = efrMethod, aggregate = FALSE,
               multicropping = "TRUE:actual:irrig_crop",
               file = paste0("shrHumanUsesFulfilledMultiple_", t, ".mz"))
  }

  ###################
  # CORRECTION DATA #
  ###################
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
  # COMPARISON #
  ##############
  # Multiple cropping zones according to GAEZ
  suitMC_GAEZ <- calcOutput("MultipleCroppingZones",
                            layers = 8,
                            aggregate = FALSE, file = "suitMC_GAEZ.mz")

  ### Yields regression ###
  # grass GPP in the growing period of LPJmL (main season) (in tDM/ha)
  #calcOutput("GrassGPPyearly", season = "mainSeason",
  #           lpjml = lpjml, climatetype = climatetype,
  #           selectyears = selectyears,
  #           aggregate = FALSE, file = "grassGPP.mz")
  # crop yields in the growing period of LPJmL (main season) (in tDM/ha)
  #calcOutput("YieldsLPJmL", lpjml = lpjml, climatetype = climatetype,
  #           selectyears = iniyear, multicropping = FALSE,
  #           aggregate = FALSE, file = "cropYields_lpjml.mz")
  # For filtering out small off-season yields:
  # crop yields in entire year for LPJmL crops (in tDM/ha)
  #calcOutput("YieldsLPJmL", lpjml = lpjml, climatetype = climatetype,
  #           selectyears = iniyear, multicropping = "TRUE:potential:endogenous",
  #           aggregate = FALSE, file = "cropYields_lpjml_multiple.mz")

  ###########
  # Revenue #
  ###########
  ### Revenue achieved on respective land area ###
  # revenue unit: mio. USD
  # biomass runit: mio. tDM
  #for (o in c("biomass", "revenue")) {
  #  for (man in c("single:potential", "single:counterfactual",
  #                "actMC:potential", "actMC:counterfactual",
  #                "potMC:potential", "potMC:counterfactual")) {
  #    for (a in c("actual", "currIrrig:NA", "currCropland:NA")) {
  #      for (calib in c(FALSE, TRUE)) {
  #        if (calib) {
  #          c <- "TRUE:TRUE:actual:irrig_crop"
  #        } else {
  #          c <- FALSE
  #        }
  #        calcOutput("CropProductionRevenue",
  #                   outputtype = o,
  #                   scenario = paste(efp, ssp, sep = "."),
  #                   management = man,
  #                   area = a,
  #                   yieldcalib = c,
  #                   lpjml = lpjml, climatetype = climatetype,
  #                   selectyears = selectyears, iniyear = iniyear,
  #                   efrMethod = efrMethod, accessibilityrule = accessibilityrule,
  #                   rankmethod = rankmethod,
  #                   allocationrule = allocationrule, gainthreshold = gtrange,
  #                   irrigationsystem = irrigationsystem, cropmix = cropmix,
  #                   transDist = transDist, fossilGW = fossilGW, comAg = comAg,
  #                   file = paste0(o, "_",
  #                                 gsub(":", "_", man), "_",
  #                                 str_split(a, ":")[[1]][1],
  #                                 "calib", calib,
  #                                 ".mz"), aggregate = FALSE)
  #      }
  #    }
  #  }
  #}
}
