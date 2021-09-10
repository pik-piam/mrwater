#' @title       calcWaterPotUse
#' @description This function returns the potential water quantity
#'              available for different uses
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
#' @param yieldcalib        Calibrated (LPJmL yield potentials smoothed and harmonized
#'                          to baseline and calibrated with global FAO calibration factor
#'                          for proxycrops where LPJmL crops mapped multiple times to MAgPIE crops) or
#'                          FAO (LPJmL yields calibrated with current FAO yield)
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param thresholdtype     Thresholdtype of yield improvement:
#'                          TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (same unit as thresholdtype)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param avlland_scen      Land availability scenario (currCropland, currIrrig, potIrrig)
#'                          combination of land availability scenario and initialization year separated by ":".
#'                          protection scenario separated by "_" (only relevant when potIrrig selected):
#'                          WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix           Cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
#' @param com_ag            If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears add_dimension new.magpie
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterPotUse", aggregate = FALSE)
#' }
#'
calcWaterPotUse <- function(lpjml, selectyears, climatetype, EFRmethod,
                            accessibilityrule, rankmethod, yieldcalib, allocationrule,
                            thresholdtype, gainthreshold, irrigationsystem, iniyear,
                            avlland_scen, cropmix, com_ag, multicropping) {

  # Check
  if (!is.na(as.list(strsplit(avlland_scen, split = ":"))[[1]][2]) &&
      iniyear != as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])) {
    stop("Initialization year in calcWaterPotUse does not match:
         iniyear and avlland_scen should have same initialization year")
  }

  # Water potentially available for irrigation (accounting for previously committed agricultural uses)
  watAvlAg    <- collapseNames(calcOutput("RiverSurplusDischargeAllocation",
                                        output = "potIrrigWat", selectyears = selectyears,
                                        lpjml = lpjml, climatetype = climatetype,
                                        EFRmethod = EFRmethod, accessibilityrule = accessibilityrule,
                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                        allocationrule = allocationrule, thresholdtype = thresholdtype,
                                        gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                        iniyear = iniyear, avlland_scen = avlland_scen,
                                        cropmix = cropmix, com_ag = com_ag,
                                        multicropping = multicropping, aggregate = FALSE))
  watAvlAgWW <- collapseNames(watAvlAg[, , "withdrawal"])
  watAvlAgWC <- collapseNames(watAvlAg[, , "consumption"])


  watNonAgWW <- watNonAgWC <- currHumanWW <- currHumanWC <- new.magpie(cells_and_regions = getCells(watAvlAgWW),
                         years = getYears(watAvlAgWW),
                         names = getNames(watAvlAgWW),
                         fill = 0)

  # Water use for non-agricultural purposes
  watNonAg <- calcOutput("RiverHumanUses", humanuse = "non_agriculture",
                                      lpjml = lpjml, climatetype = climatetype,
                                      EFRmethod = EFRmethod, selectyears = selectyears,
                                      iniyear = iniyear, aggregate = FALSE)
  watNonAgWW[, , "single"] <- collapseNames(watNonAg[, , "currHuman_ww"])
  watNonAgWC[, , "single"] <- collapseNames(watNonAg[, , "currHuman_wc"])

  if (com_ag == TRUE) {

    # Water already committed to irrigation
    currHuman <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
                             lpjml = lpjml, climatetype = climatetype,
                             EFRmethod = EFRmethod, selectyears = selectyears,
                             iniyear = iniyear, aggregate = FALSE)

  } else {

    # No water is previously committed
    currHuman       <- watNonAg
    currHuman[, , ] <- 0

  }

  currHumanWW[, , "single"] <- collapseNames(currHuman[, , "currHuman_ww"])
  currHumanWC[, , "single"] <- collapseNames(currHuman[, , "currHuman_wc"])

  # Function outputs
  watAgWW  <- watAvlAgWW + currHumanWW
  watAgWC  <- watAvlAgWC + currHumanWC
  watTotWW <- watNonAgWW + watAgWW
  watTotWC <- watNonAgWC + watAgWC

  watAgWW  <- add_dimension(watAgWW, dim = 3.4, add = "type", nm = "wat_ag_ww")
  watAgWC  <- add_dimension(watAgWC, dim = 3.4, add = "type", nm = "wat_ag_wc")
  watTotWW <- add_dimension(watTotWW, dim = 3.4, add = "type", nm = "wat_tot_ww")
  watTotWC <- add_dimension(watTotWC, dim = 3.4, add = "type", nm = "wat_tot_wc")

  out <- mbind(watAgWW, watAgWC, watTotWW, watTotWC)
  getSets(out) <- c("x", "y", "iso", "year", "EFP", "scen", "season", "type")

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "potential water availability for agricultural use or total human water use",
              isocountries = FALSE))
}
