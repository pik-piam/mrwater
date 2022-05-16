#' @title       calcIrrigationPotentials
#' @description calculates irrigation potentials in terms of potentially irrigated
#'              areas and potential irrigation water for MAgPIE
#'
#' @param output           output to be returned:
#'                         "IrrigArea": irrigated area (in Mha/yr) or
#'                         "wat_ag_ww": available irrigation water for withdrawals (in km^3/yr) or
#'                         "wat_ag_wc": available consumptive irrigation water (in km^3/yr)
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param climatetype      Switch between different climate scenarios or
#'                         historical baseline "GSWP3-W5E5:historical"
#' @param selectyears      years for which potentially irrigated area is calculated
#' @param iniyear          initialization year
#'
#' @return magpie object
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigationPotentials", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames
#'
#' @export

calcIrrigationPotentials <- function(output, selectyears, iniyear, lpjml, climatetype) {

  ### STILL IN USE OR CAN BE DELETED? ###

  # Standard settings
  potential_wat     <- TRUE
  com_ag            <- TRUE
  multicropping     <- FALSE
  cropmix           <- c("maiz", "rapeseed", "puls_pro")
  landScen          <- "potCropland:HalfEarth"
  irrigationsystem  <- "initialization"

  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  yieldcalib        <- TRUE
  allocationrule    <- "optimization"

  gainthreshold     <- 0
  rankmethod        <- "USD_ha:TRUE"
  thresholdtype     <- "USD_ha"

  if (output == "IrrigArea") {

    x <- collapseNames(calcOutput("IrrigAreaPotential", gainthreshold = gainthreshold,
                                  selectyears = selectyears, iniyear = iniyear,
                                  climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, thresholdtype = thresholdtype,
                                  yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  irrigationsystem = irrigationsystem,
                                  landScen = landScen, cropmix = cropmix, potential_wat = potential_wat,
                                  com_ag = com_ag, multicropping = multicropping,
                                  aggregate = FALSE)[, , "irrigatable"])

    w <- NULL
    d <- "Potentially irrigated areas"
    u <- "Mha"

  } else {

    x <- collapseNames(calcOutput("WaterUsePotential", gainthreshold = gainthreshold,
                                  selectyears = selectyears, iniyear = iniyear,
                                  climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, thresholdtype = thresholdtype,
                                  yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  irrigationsystem = irrigationsystem,
                                  landScen = landScen, cropmix = cropmix, com_ag = com_ag,
                                  multicropping = multicropping, aggregate = FALSE)[, , output])
    # transform from mio. m^3 to km^3:
    # (1 km^3 = 1e+09 m^3)
    # (1 mio. = 1e+06)
    x <- x / 1000

    w       <- NULL
    d       <- paste0("Potential irrigation water",
                      ifelse(grepl("_ww", output), " (withdrawal)", " (consumption)"))
    u       <- "km^3"

  }

  x <- toolLPJcell2MAgPIEcell(x)

  return(list(x            = x,
              weight       = w,
              unit         = u,
              description  = d,
              isocountries = FALSE))
}
