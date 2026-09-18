#' @title       calcWatReqMAgPIE
#' @description This function returns irrigation water withdrawal requirements
#'              per crop, irrigation system, season and cell
#'              to be used in MAgPIE
#'
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param selectyears       Years to be returned
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear           Initialization year (for weight by cropland)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @seealso
#' \code{\link{calcIrrigSystemShr}}, \code{\link{calcIrrigWatRequirements}}
#'
#' @examples
#' \dontrun{
#' calcOutput("WatReqMAgPIE", aggregate = FALSE)
#' }
#'
#' @importFrom magclass dimSums setYears setNames mbind getItems
#' @importFrom madrat calcOutput

calcWatReqMAgPIE <- function(selectyears, iniyear,
                             lpjml, climatetype) {
  # Internal function: irrigation water requirements for MAgPIE
  .irrigReqMAG <- function(multicropping) {

    # Irrigation water requirement per crop per system
    # for different irrigation systems (in m^3 per ha per yr)
    irrigReq   <- calcOutput("IrrigWatRequirements",
                             selectyears = selectyears, iniyear = iniyear,
                             lpjml = lpjml,  climatetype = climatetype,
                             multicropping = multicropping,
                             aggregate = FALSE)
    cellorder  <- getItems(irrigReq, dim = 1)
    # Crop irrigation water requirements expressed in withdrawal for MAgPIE
    irrigReq <- collapseNames(irrigReq[, , "withdrawal"])

    # irrigation system mix in initialization time step
    # Irrigation system area share per crop
    irrigSystemShr <- setYears(calcOutput("IrrigSystemShr", iniyear = iniyear,
                                          aggregate = FALSE),
                               NULL)

    mix <- setNames(dimSums(irrigReq[, , getNames(irrigSystemShr)] * irrigSystemShr,
                            dim = "system"),
                    nm = "mixed")
    # combine
    irrigReq <- mbind(irrigReq, mix)
    irrigReq <- irrigReq[cellorder, , ]
  }

  # Irrigation water requirements in main growing season of crops
  main <- .irrigReqMAG(multicropping = FALSE)
  # Annual irrigation water requirements under multiple cropping in areas suitable
  # for multiple cropping
  annual <- .irrigReqMAG(multicropping = "potential:endogenous")
  # Off-season irrigation water requirements
  off <- pmax(annual - main, 0)

  out <- mbind(add_dimension(main, dim = 3.3, add = "season", nm = "main"),
               add_dimension(off, dim = 3.3, add = "season", nm = "off"))

  # Check for NAs and negative values
  if (any(is.na(out))) {
    stop("Problem in calcActualIrrigWatRequirements:
         produced NA irrigation water requirements")
  }
  if (any(out < 0)) {
    stop("Problem in calcActualIrrigWatRequirements:
         produced negative irrigation water requirements")
  }

  # Weight: potentially irrigated area in initizalization year (only used for aggregation)
  pia <- setYears(calcOutput("PotIrrigAreas", cropAggregation = TRUE,
                             lpjml = lpjml, climatetype = climatetype,
                             multicropping = FALSE,
                             # standard options (Question (Jan): How to hand them over more elegantly?)
                             # To Do: create new function (calcPotIrrigAreasMAgPIE)
                             #        that returns PIA as required by MAgPIE
                             #        and set standard settings there. Then call here and select
                             #        BAU and iniyear.
                             selectyears = seq(1995, 2100, by = 5), iniyear = 1995,
                             efrMethod = "VMF:fair", irrigationsystem = "initialization",
                             accessibilityrule = "CV:2", rankmethod = "USD_m3:GLO:TRUE",
                             gainthreshold = 10, allocationrule = "optimization",
                             yieldcalib = FALSE, comAg = TRUE,
                             fossilGW = TRUE, transDist = 100,
                             landScen = "potCropland:NULL", cropmix = "hist_total",
                             aggregate = FALSE)[, "y1995", "off"][, , "ssp2"],
                  NULL)

  return(list(x            = out,
              weight       = pia,
              unit         = "m^3 per ha per yr",
              description  = paste0("Irrigation water requirements ",
                                    "for different crop types, seasons, ",
                                    "and irrigation systems ",
                                    "per cell and crop"),
              isocountries = FALSE))
}
