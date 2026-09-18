#' @title fullWATER4MAGPIE
#' @description Function that produces the complete cellular water data set
#'              for the MAgPIE model.
#'              Note: This is only needed temporarily until it is included
#'              in the fullCELLULARMAGPIE
#'
#' @param lpjml lpjml version
#' @param climatetype climate model and rcp
#' @author Felicitas Beier

fullWATER4MAGPIE <- function(lpjml = "lpjml5.9.16-m2", climatetype = "MRI-ESM2-0:ssp370") {

  #### Additional settings ####

  # water specific settings
  irrigationsystem  <- "initialization"
  efrMethod         <- "VMF:fair"
  multicropping     <- "TRUE:actual:irrig_crop"

  accessibilityrule <- "CV:2"
  rankmethod        <- "USD_m3:GLO:TRUE"
  gainthreshold     <- 10 # to remove unproductive areas form potential irrigation (Note: temporary until we found solution for accounting for regional costs)
  allocationrule    <- "optimization"

  comAg             <- TRUE # potential includes priority for committed agricultural areas
  yieldcalib        <- FALSE
  # Question: should we activate yield-calibration for determination of PIWW?
  #           In MAgPIE, yields are being calibrated, so I guess it would be more consistent
  #           If so: global or country-level calibration (MAgPIE is regional)
  fossilGW  <- TRUE
  transDist <- 100

  cropmix   <- "hist_total" # cropmix as of LandInG
  landScen  <- "potCropland:NULL" # potential cropland and no land protection (for testing)
  # To Do: different area protection scenario settings for different scenarios

  ### fullCELLULAR settings

  magYearsPastLong <- c("y1995", "y2000", "y2005", "y2010", "y2015")
  lpjYears         <- seq(1995, 2100, by = 5)
  iniyear         <- 1995

  # 14_yields
  # irrigated and rainfed yields with different aggregation weights
  # calcOutput("YieldsMAgPIE",
  #            datasource = c(lpjml = lpjml, isimip = isimip),
  #            climatetype = climatetype,
  #            multicropping = multicropping,
  #            calibration = NULL,
  #            round = 2, years = lpjYears,
  #            aggregate = "cluster",
  #            outputStatistics = stats, file = paste0("lpj_yields_", ctype, ".mz"),
  #            weighting = "totalLUspecific") # Note: We use a different weight than the default: "totalCrop"

  # 41 area equipped for irrigation
  # area committed for irrigation according to data in past
  # Note: currently, this is based on LandInG
  calcOutput("IrrigAreaCommitted",
             selectyears = magYearsPastLong, iniyear = iniyear,
             aggregate = FALSE, file = paste0("area_irrig_", "0.5", ".mz"))
  # Question (Benni): This used to be "avl_irrig_...", Should I rename such files, too?

  # 42 water demand
  # irrigation water requirements for chosen irrigation system settings
  # Question: Should we generate different scenarios for different irrigation efficiencies?
  #           (i.e., initialization, all_sprinkler, all_drip)
  #           This would mean an additional set/column in MAgPIE
  calcOutput("ActualIrrigWatRequirements", selectyears = lpjYears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             irrigationsystem = irrigationsystem, multicropping = multicropping,
             aggregate = FALSE, file = paste0("irrig_req_crop_", "0.5", ".mz"))

  # 43 water availability
  # Potentially irrigated areas with same settings as PIWW for disaggregation
  # Question (Jan, Jens, Benni): Do we need this?
  calcOutput("PotIrrigAreas", cropAggregation = TRUE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = lpjYears, iniyear = iniyear,
             efrMethod = efrMethod, irrigationsystem = irrigationsystem,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             gainthreshold = gainthreshold, allocationrule = allocationrule,
             yieldcalib = yieldcalib, comAg = comAg,
             fossilGW = fossilGW, transDist = transDist,
             multicropping = multicropping,
             landScen = landScen, cropmix = cropmix,
             aggregate = FALSE, file = paste0("pia", "_0.5", ".mz"))

  # Question (Jan): Can I remove one dimension here in full-function? or do I need a separate function for that?
  # To Do: argument to choose output [,, c("wat_ww", "wat_wc"]
  # To Do: scenario settings (tool function for water scenarios)
  calcOutput("PotWater", lpjml = lpjml, climatetype = climatetype,
             selectyears = lpjYears, iniyear = iniyear,
             efrMethod = efrMethod, irrigationsystem = irrigationsystem,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             gainthreshold = gainthreshold, allocationrule = allocationrule,
             yieldcalib = yieldcalib, comAg = comAg,
             fossilGW = fossilGW, transDist = transDist,
             multicropping = multicropping,
             landScen = landScen, cropmix = cropmix,
             aggregate = FALSE, file = paste0("pot_irr_wat_withdrawal", "_0.5", ".mz"))

  # To Do: remove non-renewable GW from this PIWW and report separately

  ### Question: Do we need to report EFRs (e.g., for postprocessing (e.g., PBs or disaggregation?))
  # calcOutput("EFRSmakthin", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            round = 6, seasonality = "grper",
  #            outputStatistics = stats, file = paste0("lpj_envflow_grper_", ctype, ".mz"))
  # calcOutput("EFRSmakthin", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            round = 6, seasonality = "total",
  #            outputStatistics = stats, file = paste0("lpj_envflow_total_", ctype, ".mz"))
  #
  # if (dev == "EFRtest") {
  #   calcOutput("EnvmtlFlow", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #              aggregate = "cluster",
  #              round = 6, seasonality = "grper",
  #              outputStatistics = stats, file = paste0("envflow_grper_", ctype, ".cs3"))
  #   calcOutput("EnvmtlFlow", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #              aggregate = "cluster",
  #              round = 6, seasonality = "total",
  #              outputStatistics = stats, file = paste0("envflow_total_", ctype, ".cs3"))
  # }

  ### Question: Do we need to report non-agricultural water use (for postprocessing)
  #### I think yes: for reporting of total water use (e.g., for water consumption PB)
  # To Do: make sure to harmonize with what entered in PIWW
  # calcOutput("WaterUseNonAg", datasource = "WATERGAP_ISIMIP", usetype = "all:all",
  #            selectyears = lpjYears, seasonality = "grper", lpjml = lpjml, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            outputStatistics = stats, file = paste0("watdem_nonagr_grper_", ctype, ".mz"))
  #
  # calcOutput("WaterUseNonAg", datasource = "WATERGAP_ISIMIP", usetype = "all:all",
  #            selectyears = lpjYears, seasonality = "total", lpjml = lpjml, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            outputStatistics = stats, file = paste0("watdem_nonagr_total_", ctype, ".mz"))
}
