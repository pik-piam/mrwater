#' @title fullSIMPLE
#' @description Function that produces gridded outputs for usage
#'              in the SIMPLE model (Baldos & Hertel 2012) and
#'              the SIMPLE-G model (Baldos et al. 2020)
#'
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst")
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
#' @param transDist         Water transport distance allowed to fulfill locally
#'                          unfulfilled water demand by surrounding cell water availability
#' @param fossilGW          If TRUE: non-renewable groundwater can be used.
#'                          If FALSE: non-renewable groundwater cannot be used.
#'
#' @author Felicitas Beier
#'
#' @importFrom stringr str_split
#'
#' @export

fullSIMPLE <- function(transDist = 100, fossilGW = TRUE,
                              allocationrule = "optimization",
                              rankmethod = "USD_m3:GLO:TRUE") {

  # The analysis is calculated for the 2010
  iniyear           <- 2010
  selectyears       <- 2010
  # Newest LPJmL runs
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_bft_e511ac58")
  climatetype       <- "GSWP3-W5E5:historical"

  # Non-agricultural water usage follows a BAU trajectory
  ssp               <- "ssp2"

  # Standard settings
  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  multicropping     <- FALSE
  yieldcalib        <- "TRUE:FALSE"
  cropmix           <- "hist_rainf"
  irrigationsystem  <- "initialization"

  # retrieve arguments
  thresholdtype <- paste(str_split(rankmethod, pattern = ":")[[1]][1],
                         str_split(rankmethod, pattern = ":")[[1]][2],
                         sep = ":")

  ################
  # MAIN RESULTS #
  ################

  # Environmental Flow Requirements
  # (share of discharge that has to be reserved for the environment)
  calcOutput("EnvmtlFlowRequirementsShare", lpjml = lpjml,
             climatetype = climatetype, efrMethod = efrMethod,
             aggregate = FALSE,
             file = "EFRshare.mz")

  # To derive the country-/basin-level transformation elasticity
  # for the CET function, PIA is calculated for different thresholds
  calcOutput("EconOfIrrig", scenario = ssp, output = "IrrigArea",
             gtrange = seq(0, 3000, 100),
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             efrMethod = efrMethod, accessibilityrule = accessibilityrule,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,
             irrigationsystem = irrigationsystem, cropmix = cropmix,
             landScen = "currCropland:NULL",
             comAg = TRUE, transDist = transDist, fossilGW = fossilGW,
             multicropping = multicropping, aggregate = FALSE,
             file = paste0("IrrigArea", "EconCUR.mz"))

  # Current cropland area
  calcOutput("CropareaAdjusted",
             iniyear = iniyear, dataset = "Toolbox",
             aggregate = FALSE,
             file = "croparea.mz")

  calcOutput("CropAreaShare", iniyear = iniyear, cropmix = "hist_rainf",
             aggregate = FALSE, file = "cropmix_rf.mz")
  calcOutput("CropAreaShare", iniyear = iniyear, cropmix = "hist_irrig",
             aggregate = FALSE, file = "cropmix_ir.mz")
  calcOutput("CropAreaShare", iniyear = iniyear, cropmix = "hist_total",
             aggregate = FALSE, file = "cropmix_tot.mz")

  # Crop yields (in USD/ha)
  calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib,
             priceAgg = str_split(rankmethod, pattern = ":")[[1]][2],
             multicropping = FALSE, aggregate = FALSE,
             file = "yields.mz")

  # Yield gain through irrigation (in )
  # Cellular yield improvement potential for all crops (in USD/ha)
  calcOutput("IrrigYieldImprovementPotential",
              selectyears = selectyears, iniyear = iniyear,
              lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
              unit = thresholdtype, yieldcalib = yieldcalib,
              comagyear = NULL, efrMethod = NULL, transDist = NULL,
              irrigationsystem = irrigationsystem,
              landScen = "currCropland:NA",
              multicropping = as.logical(stringr::str_split(multicropping, ":")[[1]][1]),
              aggregate = FALSE,
              file = "yieldgainCUR.mz")

  calcOutput("IrrigYieldImprovementPotential",
             selectyears = selectyears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
             unit = thresholdtype, yieldcalib = yieldcalib,
             comagyear = NULL, efrMethod = NULL, transDist = NULL,
             irrigationsystem = irrigationsystem,
             landScen = "potCropland:NA",
             multicropping = as.logical(stringr::str_split(multicropping, ":")[[1]][1]),
             aggregate = FALSE,
             file = "yieldgainPOT.mz")

  # The transformation elasticity between rainfed and irrigated area (tau_Li)
  # is calculated based on currently irrigated area and the maximum
  # potentially irrigated area
  # unit: Mha

  # Potentially irrigated area
  calcOutput("IrrigAreaPotential", gainthreshold = 0,
              cropAggregation = TRUE,
              selectyears = selectyears, iniyear = iniyear,
              climatetype = climatetype, lpjml = lpjml,
              accessibilityrule = accessibilityrule, efrMethod = efrMethod,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem,
              landScen = "currCropland:NA", cropmix = cropmix,
              comAg = TRUE, multicropping = multicropping,
              transDist = transDist, fossilGW = fossilGW,
              aggregate = FALSE,
              file = "PIA_CUR.mz")
  calcOutput("IrrigAreaPotential", gainthreshold = 0,
             cropAggregation = TRUE,
             selectyears = selectyears, iniyear = iniyear,
             climatetype = climatetype, lpjml = lpjml,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,
             irrigationsystem = irrigationsystem,
             landScen = "potCropland:NA", cropmix = cropmix,
             comAg = TRUE, multicropping = multicropping,
             transDist = transDist, fossilGW = fossilGW,
             aggregate = FALSE,
             file = "PIA_POT.mz")

  # Currently irrigated area
  calcOutput("IrrigAreaPotential", gainthreshold = 0,
             cropAggregation = TRUE,
             selectyears = selectyears, iniyear = iniyear,
             climatetype = climatetype, lpjml = lpjml,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,
             irrigationsystem = irrigationsystem,
             landScen = "currIrrig:NA", cropmix = cropmix,
             comAg = TRUE, multicropping = multicropping,
             transDist = transDist, fossilGW = fossilGW,
             aggregate = FALSE,
             file = "PIA_IRR.mz")
  # (just to check: should be the same as committed ag)
  calcOutput("IrrigAreaActuallyCommitted",
             fossilGW = fossilGW,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             efrMethod = efrMethod, transDist = transDist,
             multicropping = multicropping,
             aggregate = FALSE,
             file = "comAgArea.mz")

  # The price elasticity of surface water (epsilon_SW)
  # is calculated based on current irrigation water withdrawal and the maximum
  # potential irrigation water withdrawal
  # unit: mio m^3

  # Potential irrigation water withdrawal
  calcOutput("WaterUsePotential", gainthreshold = 0,
              selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
              accessibilityrule = accessibilityrule, efrMethod = efrMethod,
              rankmethod = rankmethod, yieldcalib = yieldcalib,
              allocationrule = allocationrule,
              irrigationsystem = irrigationsystem, iniyear = iniyear,
              landScen = "currCropland:NA", cropmix = cropmix,
              comAg = TRUE, multicropping = multicropping,
              transDist = transDist, fossilGW = fossilGW,
              aggregate = FALSE,
              file = "PIWW_CUR.mz")
  calcOutput("WaterUsePotential", gainthreshold = 0,
             selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = "potCropland:NA", cropmix = cropmix,
             comAg = TRUE, multicropping = multicropping,
             transDist = transDist, fossilGW = fossilGW,
             aggregate = FALSE,
             file = "PIWW_POT.mz")

  # Current irrigation water withdrawal
  calcOutput("WaterUsePotential", gainthreshold = 0,
             selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
             accessibilityrule = accessibilityrule, efrMethod = efrMethod,
             rankmethod = rankmethod, yieldcalib = yieldcalib,
             allocationrule = allocationrule,
             irrigationsystem = irrigationsystem, iniyear = iniyear,
             landScen = "currIrrig:NA", cropmix = cropmix,
             comAg = TRUE, multicropping = multicropping,
             transDist = transDist, fossilGW = fossilGW,
             aggregate = FALSE,
             file = "PIWW_IRR.mz")
  # (just to confirm. should be the same)
  calcOutput("WaterUseActuallyCommittedAg",
             lpjml = lpjml, climatetype = climatetype,
             selectyears = selectyears, iniyear = iniyear,
             multicropping = multicropping, efrMethod = efrMethod,
             fossilGW = fossilGW, transDist = transDist,
             aggregate = FALSE,
             file = "comAgWat.mz")
}
