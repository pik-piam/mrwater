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
#' @param rankmethod        Method of calculating the rank:
#'                          "meancellrank": mean over cellrank of proxy crops,
#'                          "meancroprank": rank over mean of proxy crops (normalized),
#'                          "meanpricedcroprank": rank over mean of proxy crops (normalized using price),
#'                          "watervalue": rank over value of irrigation water;
#'                          and fullpotentail TRUE/FALSE separated by ":"
#'                          (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area).
#'                          FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param yieldcalib        FAO (LPJmL yields calibrated with current FAO yield) or
#'                          calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                          smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                          smoothed_calib (smoothed LPJmL yield potentials, not harmonized, calibrated for proxycrops)
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param thresholdtype     Thresholdtype of yield improvement:
#'                          TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
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
                                   thresholdtype, gainthreshold, allocationrule,
                                   avlland_scen, cropmix, multicropping, unlimited) {

  # Cellular yield improvement potential for all crops (in USD/ha)
  yield_gain <- calcOutput("IrrigYieldImprovementPotential", selectyears = selectyears,
                            lpjml = lpjml, climatetype = climatetype, cropmix = NULL,
                            monetary = thresholdtype, iniyear = iniyear, yieldcalib = yieldcalib,
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
    d    <- "Potentially Irrigated Area considering land and water constraints"

  }

  # Share of corp area by crop type for chosen cropmix
  if (length(cropmix) == 1 && grepl("hist", cropmix)) {

    # read in relevant croparea: total (irrigated + rainfed) or irrigated depending on chosen cropmix
    croparea <- setYears(calcOutput("Croparea", years = iniyear, sectoral = "kcr",
                                    cells = "lpjcell", physical = TRUE, cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE), NULL)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                          <- toolGetMappingCoord2Country()
    getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
    names(dimnames(croparea))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

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
    croparea_shr <- croparea / dimSums(croparea, dim = 3)
    # correct NAs: where no current cropland available,
    # representative crops (maize, rapeseed, pulses) assumed as proxy
    rep_crops    <- c("maiz", "rapeseed", "puls_pro")
    other_crops  <- setdiff(getNames(croparea), rep_crops)
    croparea_shr[, , rep_crops][dimSums(croparea, dim = 3) == 0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
    croparea_shr[, , other_crops][dimSums(croparea, dim = 3) == 0] <- 0

  } else {

    # equal crop area share for each proxycrop assumed
    croparea_shr              <- new.magpie(cells_and_regions = getCells(area),
                                            years = NULL,
                                            names = cropmix,
                                            sets = c("x.y.iso", "t", "data"))
    croparea_shr[, , cropmix] <- 1 / length(cropmix)
  }

  # Potential area by croptype (in Mha)
  area <- croparea_shr * area

  # Potential yield gain per cell (in mio. USD)
  x <- dimSums(yield_gain * area, dim = "MAG")
  u <- "mio. USD"

  out <- collapseNames(x)

  return(list(x            = out,
              weight       = NULL,
              unit         = u,
              description  = d,
              isocountries = FALSE))
}
