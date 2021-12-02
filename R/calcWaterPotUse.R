#' @title       calcWaterPotUse
#' @description This function returns the potential water quantity
#'              available for different uses
#'
#' @param lpjml             LPJmL version used
#' @param selectyears       Years for which irrigatable area is calculated
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
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
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst", "equality")
#' @param thresholdtype     Thresholdtype of yield improvement:
#'                          TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold     Threshold of yield improvement potential required
#'                          (same unit as thresholdtype)
#' @param irrigationsystem  Irrigation system used
#'                          ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear           Initialization year of irrigation system
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
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
calcWaterPotUse <- function(lpjml, selectyears, climatetype, efrMethod,
                            accessibilityrule, rankmethod, yieldcalib, allocationrule,
                            thresholdtype, gainthreshold, irrigationsystem, iniyear,
                            landScen, cropmix, com_ag, multicropping) {

  # Check
  if (!is.numeric(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }

  # Water potentially available for irrigation (accounting for previously committed agricultural uses)
  watAvlAg    <- collapseNames(calcOutput("RiverSurplusDischargeAllocation",
                                        output = "potIrrigWat", selectyears = selectyears,
                                        lpjml = lpjml, climatetype = climatetype,
                                        efrMethod = efrMethod, accessibilityrule = accessibilityrule,
                                        rankmethod = rankmethod, yieldcalib = yieldcalib,
                                        allocationrule = allocationrule, thresholdtype = thresholdtype,
                                        gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                        iniyear = iniyear, landScen = landScen,
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
                                      efrMethod = efrMethod, selectyears = selectyears,
                                      iniyear = iniyear, aggregate = FALSE)
  watNonAgWW[, , "single"] <- collapseNames(watNonAg[, , "currHuman_ww"])
  watNonAgWC[, , "single"] <- collapseNames(watNonAg[, , "currHuman_wc"])

  if (com_ag == TRUE) {

    # Water already committed to irrigation
    currHuman <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
                             lpjml = lpjml, climatetype = climatetype,
                             efrMethod = efrMethod, selectyears = selectyears,
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
