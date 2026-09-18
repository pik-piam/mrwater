#' @title       calcIrrigationDataMAgPIE
#' @description returns irrigation data required for MAgPIE
#'              for different scenarios
#'              given available water and land:
#'              Potentially Irrigated Areas (PIA) and
#'              Potential Irrigation Water Withdrawals (PIWW)
#'
#' @param output        "PotIrrigAreas": potentially irrigated areas
#'                      "WaterAvlMAgPIE": potential irrigation water withdrawals
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years for which irrigatable area is calculated
#' @param iniyear       Initialization year for initial croparea
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigationDataMAgPIE", aggregate = FALSE)
#' }
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames add_dimension add_columns mbind
#'
#' @export

calcIrrigationDataMAgPIE <- function(output,
                                     lpjml, climatetype,
                                     selectyears, iniyear) {

  # Helper function: keep only those arguments that belong to respective calcFunction
  .filterArgs <- function(args, fun) {
    args[names(args) %in% names(formals(fun))]
  }

  calcFun <- get(paste0("calc", output))

  .runCalc <- function(scenArgs) {
    args <- c(commonArgs, scenArgs)
    args <- .filterArgs(args, calcFun)

    do.call(calcOutput,
            c(list(type = output, aggregate = FALSE), args))
  }

  # Scenario construction:
  commonArgs <- list(cropAggregation = TRUE,
                     usagetype = "withdrawal",
                     countryAggregation = FALSE,
                     lpjml = lpjml,
                     climatetype = climatetype,
                     selectyears = selectyears,
                     iniyear = iniyear,
                     comAg = TRUE,
                     cropmix = "hist_total", # c("maize", "rapeseed", "puls_pro"), (switch to proxy crops when ready)
                     yieldcalib = "TRUE:TRUE:actual:irrig_crop",
                     multicropping = "TRUE:actual:irrig_crop") # To Do: decide on default argument (or make scenario-dependent)

  ### Question (Jan, Benni): Should the scenarios fade in / start in 2020 or
  ###                        in initialization year. How to do the fade-over?
  ###                        In this function? Fading from ssp2off to the respective scenarios?
  ###                        How to ensure consistency with PIWW?
  ### What about "cropmix"? proxycrops or historical cropmix?
  ### Groundwater fade-out? Allow fossil-GW for 1995-2025 and then fade out after?
  # Idea for calcPotIrrWatMAgPIE: report fossil GW separately and feed to GAMS, so that it can be deactivated / faded out there
  # Which ranking to apply:   USD_ha (USD per hectare) for relative area return, or
  #                           USD_m3 (USD per cubic meter) for relative volumetric return;

  scenarioArgs <- list(
    # Taking the Green Road: sustainability
    ssp1 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                fossilGW = FALSE, # environment: no non-renewable groundwater use
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "drip", # technology
                transDist = 200, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Sustainable development: economy-driven innovation
    sdpEI = list(efrMethod = "Smakhtin:natural", # environment: very high EFP
                 fossilGW = FALSE, # environment: no non-renewable groundwater use
                 landScen = "potCropland:HalfEarth", # environment: no irrigation in certain areas
                 accessibilityrule = "CV:2", # technology
                 irrigationsystem = "drip", # technology: high efficiency
                 transDist = 200, # technology
                 allocationrule = "optimization", # economics
                 rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                 gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Sustainable development: resilient communities
    sdpRC = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                 fossilGW = FALSE, # environment: no non-renewable groundwater use
                 landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                 accessibilityrule = "CV:2", # technology
                 irrigationsystem = "sprinkler", # technology: medium efficiency
                 transDist = 200, # technology
                 allocationrule = "optimization", # economics
                 rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                 gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Sustainable development: managing the global commons
    sdpMC = list(efrMethod = "Smakhtin:natural", # environment: very high EFP
                 fossilGW = FALSE, # environment: no non-renewable groundwater use
                 landScen = "potCropland:HalfEarth", # environment: no irrigation in certain areas
                 accessibilityrule = "CV:2", # technology
                 irrigationsystem = "drip", # technology: high efficiency
                 transDist = 200, # technology
                 allocationrule = "optimization", # economics
                 rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                 gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Middle of the Road
    ssp2 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                fossilGW = FALSE, # environment: no non-renewable groundwater use
                landScen = "potCropland:WDPA", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "initialization", # technology
                transDist = 200, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:GLO:FALSE", # economics: global ranking by relative volumetric return, but with reduced potential (slightly less optimal)
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Rocky Road: Regional rivalry
    ssp3 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                fossilGW = FALSE, # environment: no non-renewable groundwater use
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "initialization", # technology
                transDist = 200, # technology
                allocationrule = "upstreamfirst", # economics
                rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # A Road Divided: Inequality
    ssp4 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                fossilGW = FALSE, # environment: no non-renewable groundwater use
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "initialization", # technology
                transDist = 200, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:ISO:TRUE", # economics: ranking by relative volumetric return and full potential based on country-level prices
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Taking the Highway: Fossil-fuel-driven development
    ssp5 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                fossilGW = FALSE, # environment: no non-renewable groundwater use
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "drip", # technology
                transDist = 200, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:GLO:FALSE", # economics: global ranking by relative volumetric return and reduced potential
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    )
  )

  ssp1    <- add_dimension(collapseNames(.runCalc(scenarioArgs$ssp1)[, , "on"][, , "ssp1"]),
                           dim = 3.1, add = "scen25", nm = "ssp1")
  ssp2    <- add_dimension(collapseNames(.runCalc(scenarioArgs$ssp2)[, , "off"][, , "ssp2"]),
                           dim = 3.1, add = "scen25", nm = "ssp2")
  ssp2efp <- add_dimension(collapseNames(.runCalc(scenarioArgs$ssp2)[, , "on"][, , "ssp2"]),
                           dim = 3.1, add = "scen25", nm = "ssp2efp") ### To Do: call calc only once and subset after for those with identical arguments.
  ssp3    <- add_dimension(collapseNames(.runCalc(scenarioArgs$ssp3)[, , "off"][, , "ssp3"]),
                           dim = 3.1, add = "scen25", nm = "ssp3")
  # To Do: implement full set of SSP/SDP scenarios
  ssp4 <- ssp5 <- sdpEI <- sdpRC <- sdpMC <- NULL
  #ssp4    <- add_dimension(collapseNames(.runCalc(scenarioArgs$ssp4)[, , "off"][, , "ssp3"]),
  #                         dim = 3.1, add = "scen25", nm = "ssp4")
  # To do (SSP4): implement EFP protection based on development status
  #ssp5    <- add_dimension(collapseNames(.runCalc(scenarioArgs$ssp5)[, , "on"][, , "ssp5"]),
  #                         dim = 3.1, add = "scen25", nm = "ssp5")
  #sdpEI   <- add_dimension(collapseNames(.runCalc(scenarioArgs$sdpEI)[, , "on"][, , "sdpEI"]),
  #                         dim = 3.1, add = "scen25", nm = "sdpEI")
  #sdpRC   <- add_dimension(collapseNames(.runCalc(scenarioArgs$sdpRC)[, , "on"][, , "sdpRC"]),
  #                         dim = 3.1, add = "scen25", nm = "sdpRC")
  #sdpMC   <- add_dimension(collapseNames(.runCalc(scenarioArgs$sdpMC)[, , "on"][, , "sdpMC"]),
  #                         dim = 3.1, add = "scen25", nm = "sdpMC")


  out <- mbind(ssp1 = ssp1, ssp2 = ssp2, ssp2efp = ssp2efp,
               ssp3 = ssp3, ssp4 = ssp4, ssp5 = ssp5,
               sdpEI = sdpEI, sdpRC = sdpRC, sdpMC = sdpMC)

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcIrrigationDataMAgPIE produced NA values")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcIrrigationDataMAgPIE produced negative values")
  }

  # Description
  if (output == "PotIrrigAreas") {
    description <- paste0("Potentially irrigated area (PIA) for different scenarios ",
                          "given available water and land")
    unit <- "Mha"
  } else if (output == "WaterAvlMAgPIE") {
    description <- paste0("Potential irrigation water withdrawals (PIWW) for different ",
                          "scenarios given available water and land")
    unit <- "mio. m^3"
  } else {
    stop("Invalid output argument.
         Please choose either 'PotIrrigAreas' or 'WaterAvlMAgPIE'.")
  }

  # Question (Jan): no weight required because areas are already in absolute values (Mha) and not in relative shares
  # and it's only relevant for aggregation, right?
  # Same for water: mio. m³ are absolute values, so no weight required for aggregation, right?
  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
