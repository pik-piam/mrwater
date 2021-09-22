#' @title       calcYieldgainPotential
#' @description reports yield gain potential for irrigatable area under different
#'              scenarios
#'
#' @param scenario          Non-agricultural water use and EFP scenario, separated
#'                          by "." (e.g. "on.ssp2")
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which yield gain potential is calculated
#' @param iniyear           Initialization year
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod         EFR method used including selected strictness of EFRs
#'                          (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return;
#'                          and boolean indicating fullpotential (TRUE) or reduced potential (FALSE)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (same unit as thresholdtype)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen      Land availability scenario (currCropland, currIrrig, potIrrig)
#'                          combination of land availability scenario and initialization year separated by ":".
#'                          protection scenario separated by "_" (only relevant when potIrrig selected):
#'                          WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix           Cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#' @param unlimited         TRUE: no water limitation to potentially irrigated area
#'                          FALSE: irrigatable area limited by water availability
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldgainPotential", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getCells getNames setYears dimSums new.magpie
#' @importFrom mrcommons toolGetMappingCoord2Country
#'
#' @export

calcYieldgainPotential <- function(scenario, selectyears, iniyear, lpjml, climatetype,
                                   EFRmethod, yieldcalib, irrigationsystem,
                                   accessibilityrule, rankmethod,
                                   gainthreshold, allocationrule,
                                   avlland_scen, cropmix, multicropping, unlimited) {

  thresholdtype <- strsplit(rankmethod, ":")[[1]][1]

  # Cellular yield improvement potential for all crops (in USD/ha)
  yieldGain <- calcOutput("IrrigYieldImprovementPotential", selectyears = selectyears,
                          lpjml = lpjml, climatetype = climatetype, cropmix = NULL,
                          unit = thresholdtype, iniyear = iniyear, yieldcalib = yieldcalib,
                          multicropping = multicropping, aggregate = FALSE)

  # Total area that can potentially be irrigated (in Mha)
  if (unlimited) {

    # Area that can potentially be irrigated without water limitation
    area <- calcOutput("AreaPotIrrig", selectyears = selectyears,
                        avlland_scen = avlland_scen, comagyear = NULL,
                        aggregate = FALSE)
    d    <- "Potentially Irrigated Area only considering land constraint"

  } else {

    # Area that can potentially be irrigated given land and water constraints
    area <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = gainthreshold,
                                     selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                     accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                     rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                     thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                     avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = TRUE,
                                     com_ag = FALSE, multicropping = multicropping,
                                     aggregate = FALSE)[, , "irrigatable"][, , scenario])
    # sum over seasons (yearly irrigated area harvested)
    area <- dimSums(area, dim = "season")
    d    <- "Potentially Irrigated Area considering land and water constraints"

  }

  # Share of corp area by crop type for chosen cropmix
  if (length(cropmix) == 1 && grepl("hist", cropmix)) {

    # read in relevant croparea: total (irrigated + rainfed) or irrigated depending on chosen cropmix
    croparea <- setYears(calcOutput("CropareaAdjusted", years = iniyear,
                                    sectoral = "kcr", physical = TRUE,
                                    cells = "lpjcell", cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE), NULL)

    if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "irrig") {

      # irrigated croparea
      croparea <- collapseNames(croparea[, , "irrigated"])

    } else if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "total") {

      # total croparea (irrigated + rainfed)
      croparea <- dimSums(croparea, dim = "irrigation")

    } else {
      stop("Please select hist_irrig or hist_total when
           selecting historical cropmix")
    }

    # historical share of crop types in cropland per cell
    cropareaShr <- croparea / dimSums(croparea, dim = 3)
    # correct NAs: where no current cropland available,
    # representative crops (maize, rapeseed, pulses) assumed as proxy
    proxycrops   <- c("maiz", "rapeseed", "puls_pro")
    otherCrops   <- setdiff(getNames(croparea), proxycrops)
    cropareaShr[, , proxycrops][dimSums(croparea, dim = 3) == 0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
    cropareaShr[, , otherCrops][dimSums(croparea, dim = 3) == 0] <- 0

  } else {

    # equal crop area share for each proxycrop assumed
    cropareaShr              <- new.magpie(cells_and_regions = getCells(area),
                                            years = NULL,
                                            names = cropmix,
                                            sets = c("x.y.iso", "t", "data"))
    cropareaShr[, , cropmix] <- 1 / length(cropmix)
  }

  # Potential area by croptype (in Mha)
  area <- cropareaShr * area

  # Potential yield gain per cell (in mio. USD)
  x <- dimSums(yieldGain * area, dim = "MAG")
  u <- "mio. USD"

  out <- collapseNames(x)

  return(list(x            = out,
              weight       = NULL,
              unit         = u,
              description  = d,
              isocountries = FALSE))
}
