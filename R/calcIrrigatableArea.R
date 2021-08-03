#' @title       calcIrrigatableArea
#' @description Calculates area that can potentially be irrigated given
#'              available water and land
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which irrigatable area is calculated
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
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
#'                          smoothed_calibrated
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
#' @param potential_wat     If TRUE: potential available water and areas used,
#'                          if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag            If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigatableArea", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames add_dimension mbind

calcIrrigatableArea <- function(lpjml, selectyears, climatetype, EFRmethod,
                                accessibilityrule, rankmethod, yieldcalib, allocationrule,
                                thresholdtype, gainthreshold, irrigationsystem, avlland_scen,
                                cropmix, potential_wat, com_ag, multicropping) {

  # retrieve function arguments
  iniyear <- as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])

  ## Read in water available for irrigation
  if (potential_wat) {

    avlWat         <- calcOutput("WaterPotUse", selectyears = selectyears,
                                  lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
                                  accessibilityrule = accessibilityrule, rankmethod = rankmethod,
                                  yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, gainthreshold = gainthreshold,
                                  irrigationsystem = irrigationsystem, iniyear = iniyear,
                                  avlland_scen = avlland_scen, cropmix = cropmix,
                                  com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)
    avlWat_irrig_c <- collapseNames(avlWat[, , "wat_ag_wc"])
    avlWat_irrig_w <- collapseNames(avlWat[, , "wat_ag_ww"])

  } else {

    avlWat         <- calcOutput("RiverHumanUses", selectyears = selectyears,
                                  lpjml = lpjml, climatetype = climatetype, iniyear = iniyear,
                                  EFRmethod = EFRmethod, humanuse = "committed_agriculture", aggregate = FALSE)
    # adjust dimensions
    avlWat         <- add_dimension(avlWat, dim = 3.3, add = "season", nm = "single")
    avlWat         <- add_columns(avlWat, dim = 3.3, addnm = "double")
    avlWat         <- add_columns(avlWat, dim = 3.3, addnm = "triple")
    avlWat[, , c("double", "triple")] <- 0

    avlWat_irrig_c <- collapseNames(avlWat[, , "currHuman_wc"])
    avlWat_irrig_w <- collapseNames(avlWat[, , "currHuman_ww"])

  }

  # Irrigation water requirements for selected cropmix and irrigation system per cell (in mio. m^3)
  watReq   <- calcOutput("FullIrrigationRequirement", selectyears = selectyears,
                          lpjml = lpjml, climatetype = climatetype,
                          irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                          cropmix = cropmix, multicropping = multicropping, comagyear = NULL, aggregate = FALSE)
  watReqWW <- watReqWC <- new.magpie(cells_and_regions = getCells(avlWat_irrig_w),
                         years = getYears(avlWat_irrig_w),
                         names = getNames(avlWat_irrig_w),
                         fill = NA)

  watReqWW[, , ] <- collapseNames(watReq[, , "withdrawal"])
  watReqWC[, , ] <- collapseNames(watReq[, , "consumption"])

  # Read in area that can potentially be irrigated (in every season separately, i.e. multicropping = FALSE)
  # (including total potentially irrigatable area; defined by comagyear=NULL)
  areaPotIrrig <- calcOutput("AreaPotIrrig", selectyears = selectyears,
                              avlland_scen = avlland_scen, comagyear = NULL, aggregate = FALSE)

  # share of requirements that can be fulfilled given available water, when >1 whole area can be irrigated
  irrigareaWW <- pmin(avlWat_irrig_w / watReqWW, 1) * areaPotIrrig
  irrigareaWW[watReqWW == 0] <- 0      # cells with no water requirements also get no irrigated area assigned
  irrigareaWW <- add_dimension(irrigareaWW, dim = 3.4, add = "type", nm = "irrigatable_ww")

  irrigareaWC <- pmin(avlWat_irrig_c / watReqWC, 1) * areaPotIrrig
  irrigareaWC[watReqWC == 0] <- 0
  irrigareaWC <- add_dimension(irrigareaWC, dim = 3.4, add = "type", nm = "irrigatable_wc")

  irrigatableArea <- pmin(collapseNames(irrigareaWW), collapseNames(irrigareaWC))
  irrigatableArea <- add_dimension(irrigatableArea, dim = 3.4, add = "type", nm = "irrigatable")

  out <- mbind(irrigatableArea, irrigareaWW, irrigareaWC)

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("produced NA irrigatable area")
  }
  if (any(out < 0)) {
    stop("produced negative irrigatable area")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. ha",
              description  = "Area that can be irrigated given land and water constraints",
              isocountries = FALSE))
}
