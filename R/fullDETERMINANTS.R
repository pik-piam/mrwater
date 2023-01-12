#' @title fullDETERMINANTS
#' @description Function that returns potential determinants of irrigation for
#'              regression analysis
#'
#'
#' @author Felicitas Beier
#'
#' @export

fullDETERMINANTS <- function() {

  # Standard settings
  lpjml       <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                   crop = "ggcmi_phase3_nchecks_bft_e511ac58")
  climatetype <- "MRI-ESM2-0:historical"

  # Available years
  lpjyears    <- paste0("y", seq(1930, 2016, 1))
  iniyear     <- "y2010"

  ##################################
  ###  Biophysical Determinants  ###
  ##################################
  ### Grid-level data ###
  # Yearly runoff (smoothed, unit: mio. m^3)
  calcOutput("YearlyRunoff", selectyears = lpjyears,
             lpjml = lpjml, climatetype = climatetype,
             aggregate = FALSE,
             file = "runoff.mz")
  # Natural discharge (smoothed, unit: mio. m^3)
  calcOutput("RiverNaturalFlows", selectyears = lpjyears,
             lpjml = lpjml, climatetype = climatetype,
             aggregate = FALSE,
             file = "natFlow.mz")

  ###################################
  ###  Economic Irrigation Gain   ###
  ###################################
  ### Grid-level data ###
  # Yield gain through irrigation
  calcOutput("IrrigYieldImprovementPotential",
             lpjml = lpjml, climatetype = climatetype,
             unit = "USD_ha:GLO", cropmix = "hist_total",
             iniyear = iniyear, selectyears = lpjyears,
             comagyear = NULL, efrMethod = NULL, transDist = NULL,
             irrigationsystem = "initialization",
             landScen = paste0("currCropland:", "NULL"),
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", multicropping = TRUE,
             aggregate = FALSE,
             file = "yieldgain_multicropping_USDha.mz")
  calcOutput("IrrigYieldImprovementPotential",
             lpjml = lpjml, climatetype = climatetype,
             unit = "USD_ha:GLO", cropmix = "hist_total",
             iniyear = iniyear, selectyears = lpjyears,
             comagyear = NULL, efrMethod = NULL, transDist = NULL,
             irrigationsystem = "initialization",
             landScen = paste0("currCropland:", "NULL"),
             yieldcalib = "TRUE:FALSE", multicropping = FALSE,
             aggregate = FALSE,
             file = "yieldgain_singlecropping_USDha.mz")

  calcOutput("IrrigYieldImprovementPotential",
             lpjml = lpjml, climatetype = climatetype,
             unit = "USD_m3:GLO", cropmix = "hist_total",
             iniyear = iniyear, selectyears = lpjyears,
             comagyear = NULL, efrMethod = NULL, transDist = NULL,
             irrigationsystem = "initialization",
             landScen = paste0("currCropland:", "NULL"),
             yieldcalib = "TRUE:TRUE:actual:irrig_crop", multicropping = TRUE,
             aggregate = FALSE,
             file = "yieldgain_multicropping_USDm3.mz")
  calcOutput("IrrigYieldImprovementPotential",
             lpjml = lpjml, climatetype = climatetype,
             unit = "USD_m3:GLO", cropmix = "hist_total",
             iniyear = iniyear, selectyears = lpjyears,
             comagyear = NULL, efrMethod = NULL, transDist = NULL,
             irrigationsystem = "initialization",
             landScen = paste0("currCropland:", "NULL"),
             yieldcalib = "TRUE:FALSE", multicropping = FALSE,
             aggregate = FALSE,
             file = "yieldgain_singlecropping_USDm3.mz")

  # Cropland per crop (for crop diversity)
  calcOutput("CropareaAdjusted",
             iniyear = iniyear, dataset = "Toolbox",
             aggregate = FALSE,
             file = "cropareaToolbox.mz")


  ###################################
  ### Socio-economic Determinants ###
  ###################################

  ### Grid-level data ###
  calcOutput("GridPop", cells = "lpjcell", aggregate = FALSE,
             file = "griddedPop.mz")

  ### Country-level data ###
  # Governance (indicator raning from 0 to 1)
  calcOutput("GovernanceIndicator", aggregate = FALSE,
             file = "governance.mz")
  # GDP

  # Population

  # Gini


}
