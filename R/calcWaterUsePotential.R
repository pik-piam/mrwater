#' @title       calcWaterUsePotential
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
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated, consisting of:
#'                          Unit:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return; and
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices;
#'                          and boolean indicating fullpotential (TRUE, i.e. cell receives full
#'                                                                irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                                                receives at later stage in allocation algorithm);
#'                          separated by ":"
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
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param com_ag            If TRUE: currently already irrigated areas in
#'                                   initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells mbind add_dimension new.magpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterUsePotential", aggregate = FALSE)
#' }
#'
calcWaterUsePotential <- function(lpjml, selectyears, climatetype, efrMethod,
                            accessibilityrule, rankmethod, yieldcalib, allocationrule,
                            thresholdtype, gainthreshold, irrigationsystem, iniyear,
                            landScen, cropmix, com_ag, multicropping) {

  # Check
  if (!is.numeric(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }

  # Water potentially available for irrigation (accounting for previously committed agricultural uses)
  watAvlAg  <- collapseNames(calcOutput("RiverSurplusDischargeAllocation",
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
                         efrMethod = efrMethod, multicropping = multicropping,
                         selectyears = selectyears, iniyear = iniyear,
                         aggregate = FALSE)
  watNonAgWW <- collapseNames(watNonAg[, , "currHuman_ww"])
  watNonAgWC <- collapseNames(watNonAg[, , "currHuman_wc"])

  if (com_ag == TRUE) {

    # Water already committed to irrigation
    currHuman <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
                             lpjml = lpjml, climatetype = climatetype,
                             efrMethod = efrMethod, multicropping = multicropping,
                             selectyears = selectyears, iniyear = iniyear,
                             aggregate = FALSE)

  } else {

    # No water is previously committed
    currHuman       <- watNonAg
    currHuman[, , ] <- 0

  }

  currHumanWW <- collapseNames(currHuman[, , "currHuman_ww"])
  currHumanWC <- collapseNames(currHuman[, , "currHuman_wc"])

  # Function outputs
  watAgWW  <- watAvlAgWW + currHumanWW
  watAgWC  <- watAvlAgWC + currHumanWC
  watTotWW <- watNonAgWW + watAgWW
  watTotWC <- watNonAgWC + watAgWC

  watAgWW  <- add_dimension(watAgWW, dim = 3.4, add = "type", nm = "wat_ag_ww")
  watAgWC  <- add_dimension(watAgWC, dim = 3.4, add = "type", nm = "wat_ag_wc")
  watTotWW <- add_dimension(watTotWW, dim = 3.4, add = "type", nm = "wat_tot_ww")
  watTotWC <- add_dimension(watTotWC, dim = 3.4, add = "type", nm = "wat_tot_wc")

  out          <- mbind(watAgWW, watAgWC, watTotWW, watTotWC)
  getSets(out) <- c("x", "y", "iso", "year", "EFP", "scen", "type")

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "potential water availability for agricultural usage
                              or total human water usage",
              isocountries = FALSE))
}
