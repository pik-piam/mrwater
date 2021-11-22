#' @title       reportCountryResults
#' @description reports country-values (irrigated area or irrigation water availability)
#'
#' @param output            Output to be displayed: irrigated area "IrrigArea" or
#'                          available water volume "wat_ag_ww" "wat_ag_wc"
#' @param gainthreshold     gainthreshold chosen for reporting
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param climatetype       Switch between different climate scenarios or
#'                          historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod         EFR method used including selected strictness of EFRs
#'                          (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return;
#'                          and boolean indicating fullpotential
#'                          (TRUE, i.e. cell receives full irrigation requirements in total area)
#'                          or reduced potential
#'                          (FALSE, reduced potential of cell receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst")
#' @param thresholdtype     Unit of yield improvement potential used as threshold:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return
#' @param irrigationsystem  Irrigation system to be used for river basin discharge
#'                          allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param cropmix           cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @return data frame in country-resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' reportCountryResults()
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

reportCountryResults <- function(output, lpjml, climatetype, gainthreshold,
                                 efrMethod, accessibilityrule, rankmethod, yieldcalib,
                                 allocationrule, thresholdtype, irrigationsystem, cropmix,
                                 multicropping) {
  # Set arguments
  scenario    <- "ssp2"
  selectyears <- 2010
  iniyear     <- 2010
  com_ag      <- TRUE

  # LUH total irrigated area
  irrigAreaLUH <- setYears(collapseNames(calcOutput("Croparea", years = iniyear, sectoral = "kcr", physical = TRUE,
                           cells = "lpjcell", cellular = TRUE, irrigation = TRUE, aggregate = FALSE)[, , "irrigated"]),
                           iniyear)
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                              <- toolGetMappingCoord2Country()
  getCells(irrigAreaLUH)           <- paste(map$coords, map$iso, sep = ".")
  names(dimnames(irrigAreaLUH))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  if (output == "IrrigArea") {

    # Irrigated Area (LUH)
    currLUH           <- dimSums(irrigAreaLUH, dim = "MAG")
    getSets(currLUH)  <- c("x", "y", "iso", "year", "type")
    getNames(currLUH) <- "irrig_area_LUH"

    # Currently Irrigated Area [total, sustainable, unsustainable]
    currIrr <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml,
                                        gainthreshold = gainthreshold, selectyears = selectyears,
                                        climatetype = climatetype, accessibilityrule = accessibilityrule,
                                        efrMethod = efrMethod, rankmethod = rankmethod,
                                        yieldcalib = yieldcalib, allocationrule = allocationrule,
                                        thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                        landScen = "currCropland:2010",
                                        cropmix = cropmix, potential_wat = FALSE,
                                        com_ag = TRUE, multicropping = multicropping,
                                        aggregate = FALSE)[, , "irrigatable"][, , scenario])
    currIrr           <- dimSums(currIrr, dim = "season")
    getSets(currIrr)  <- c("x", "y", "iso", "year", "type")
    currIrrSus             <- currIrr[, , "on"]
    getNames(currIrrSus)   <- "curr_irrig_sus"
    currIrrUnsus           <- collapseNames(currIrr[, , "off"]) - collapseNames(currIrr[, , "on"])
    getNames(currIrrUnsus) <- "curr_irrig_unsus"
    currIrrTot             <- currIrr[, , "off"]
    getNames(currIrrTot)   <- "curr_irrig_tot"

    # Potentially Irrigated Area (on current Cropland) [total, sustainable, unsustainable]
    potIrrcurr <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml,
                                           gainthreshold = gainthreshold, selectyears = selectyears,
                                           climatetype = climatetype, accessibilityrule = accessibilityrule,
                                        efrMethod = efrMethod, rankmethod = rankmethod,
                                        yieldcalib = yieldcalib, allocationrule = allocationrule,
                                        thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                        landScen = "currCropland:2010",
                                        cropmix = cropmix, potential_wat = TRUE,
                                        com_ag = com_ag, multicropping = multicropping,
                                        aggregate = FALSE)[, , "irrigatable"][, , scenario])
    potIrrcurr           <- dimSums(potIrrcurr, dim = "season")
    getSets(potIrrcurr)  <- c("x", "y", "iso", "year", "type")
    potIrrcurrSus             <- potIrrcurr[, , "on"]
    getNames(potIrrcurrSus)   <- "curr_irrig_currCropland_sus"
    potIrrcurrUnsus           <- collapseNames(potIrrcurr[, , "off"]) - collapseNames(potIrrcurr[, , "on"])
    getNames(potIrrcurrUnsus) <- "curr_irrig_currCropland_unsus"
    potIrrcurrTot             <- potIrrcurr[, , "off"]
    getNames(potIrrcurrTot)   <- "curr_irrig_currCropland_tot"

    # Potentially Irrigated Area (on potential Cropland) [total, sustainable, unsustainable]
    potIrrpot <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml,
                                          gainthreshold = gainthreshold, selectyears = selectyears,
                                          climatetype = climatetype, accessibilityrule = accessibilityrule,
                                          efrMethod = efrMethod, rankmethod = rankmethod,
                                          yieldcalib = yieldcalib, allocationrule = allocationrule,
                                          thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                          landScen = "potCropland:HalfEarth",
                                          cropmix = cropmix, potential_wat = TRUE,
                                          com_ag = com_ag, multicropping = multicropping,
                                          aggregate = FALSE)[, , "irrigatable"][, , scenario])
    potIrrpot           <- dimSums(potIrrpot, dim = "season")
    getSets(potIrrpot)  <- c("x", "y", "iso", "year", "type")
    potIrrpotSus             <- potIrrpot[, , "on"]
    getNames(potIrrpotSus)   <- "curr_irrig_potCropland_sus"
    potIrrpotUnsus           <- collapseNames(potIrrpot[, , "off"]) - collapseNames(potIrrpot[, , "on"])
    getNames(potIrrpotUnsus) <- "curr_irrig_potCropland_unsus"
    potIrrpotTot             <- potIrrpot[, , "off"]
    getNames(potIrrpotTot)   <- "curr_irrig_potCropland_tot"

    # Combine to one MAgPIE object
    x <- mbind(currLUH, currIrrSus,    currIrrUnsus,    currIrrTot,
               potIrrcurrSus, potIrrcurrUnsus, potIrrcurrTot,
               potIrrpotSus,  potIrrpotUnsus,  potIrrpotTot)

    d <- "Irrigated Area"
    u <- "Mha"

  } else {

    if (output == "wat_ag_ww") {
      type <- "withdrawal"
    } else if (output == "wat_ag_wc") {
      type <- "consumption"
    } else {
      stop("Output type not available")
    }

    # Irrigated Water use (derived from LUH and crop irrigation requirements)
    watReq   <- collapseNames(calcOutput("ActualIrrigWatRequirements", lpjml = lpjml, climatetype = climatetype,
                           iniyear = iniyear, selectyears = selectyears, aggregate = FALSE)[, , type])
    currLUH           <- irrigAreaLUH * watReq
    currLUH           <- dimSums(currLUH, dim = "MAG")
    getSets(currLUH)  <- c("x", "y", "iso", "year", "type")
    getNames(currLUH) <- "irrig_wat_LUH"

    # Current Irrigation Water Use [total, sustainable, unsustainable]
    currIrr <- collapseNames(calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
                            lpjml = lpjml, climatetype = climatetype,
                            efrMethod = efrMethod, selectyears = selectyears,
                            iniyear = iniyear,
                            aggregate = FALSE)[, , scenario][, , paste0("currHuman_", gsub("(.*)_", "", output))])
    getSets(currIrr)  <- c("x", "y", "iso", "year", "type")
    currIrrSus             <- currIrr[, , "on"]
    getNames(currIrrSus)   <- "curr_irrig_sus"
    currIrrUnsus           <- collapseNames(currIrr[, , "off"]) - collapseNames(currIrr[, , "on"])
    getNames(currIrrUnsus) <- "curr_irrig_unsus"
    currIrrTot             <- currIrr[, , "off"]
    getNames(currIrrTot)   <- "curr_irrig_tot"

    # Potential Irrigation Water (on current Cropland) [total, sustainable, unsustainable]
    potIrrcurr <- collapseNames(calcOutput("WaterPotUse", lpjml = lpjml,
                                           gainthreshold = gainthreshold, selectyears = selectyears,
                                           climatetype = climatetype, accessibilityrule = accessibilityrule,
                                           efrMethod = efrMethod, rankmethod = rankmethod,
                                           yieldcalib = yieldcalib, allocationrule = allocationrule,
                                           thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                           iniyear = iniyear, landScen = "currCropland:2010",
                                           cropmix = cropmix, com_ag = com_ag, multicropping = multicropping,
                                           aggregate = FALSE)[, , output][, , scenario])
    potIrrcurr           <- dimSums(potIrrcurr, dim = "season")
    getSets(potIrrcurr)  <- c("x", "y", "iso", "year", "type")
    potIrrcurrSus             <- potIrrcurr[, , "on"]
    getNames(potIrrcurrSus)   <- "curr_irrig_currCropland_sus"
    potIrrcurrUnsus           <- collapseNames(potIrrcurr[, , "off"]) - collapseNames(potIrrcurr[, , "on"])
    getNames(potIrrcurrUnsus) <- "curr_irrig_currCropland_unsus"
    potIrrcurrTot             <- potIrrcurr[, , "off"]
    getNames(potIrrcurrTot)   <- "curr_irrig_currCropland_tot"

    # Potentially Irrigated Area (on potential Cropland) [total, sustainable, unsustainable]
    potIrrpot <- collapseNames(calcOutput("WaterPotUse", lpjml = lpjml,
                                          climatetype = climatetype, selectyears = selectyears,
                                          gainthreshold = gainthreshold, accessibilityrule = accessibilityrule,
                                          efrMethod = efrMethod, rankmethod = rankmethod, yieldcalib = yieldcalib,
                                          allocationrule = allocationrule, thresholdtype = thresholdtype,
                                          irrigationsystem = irrigationsystem, iniyear = iniyear,
                                          landScen = "potCropland:HalfEarth", cropmix = cropmix,
                                          com_ag = com_ag, multicropping = multicropping,
                                          aggregate = FALSE)[, , output][, , scenario])
    potIrrpot           <- dimSums(potIrrpot, dim = "season")
    getSets(potIrrpot)  <- c("x", "y", "iso", "year", "type")
    potIrrpotSus             <- potIrrpot[, , "on"]
    getNames(potIrrpotSus)   <- "curr_irrig_potCropland_sus"
    potIrrpotUnsus           <- collapseNames(potIrrpot[, , "off"]) - collapseNames(potIrrpot[, , "on"])
    getNames(potIrrpotUnsus) <- "curr_irrig_potCropland_unsus"
    potIrrpotTot             <- potIrrpot[, , "off"]
    getNames(potIrrpotTot)   <- "curr_irrig_potCropland_tot"

    # Combine to one MAgPIE object
    x <- mbind(currLUH, currIrrSus,    currIrrUnsus,    currIrrTot,
               potIrrcurrSus, potIrrcurrUnsus, potIrrcurrTot,
               potIrrpotSus,  potIrrpotUnsus,  potIrrpotTot)

    # Transform from mio. m^3 to km^3:
    # (1 km^3 = 1e+09 m^3)
    # (1 mio. = 1e+06)
    x <- x / 1000

    d <- "Potential Water Use"
    u <- "km^3"

  }

  # Aggregate to iso-country values
  mapping          <- toolGetMappingCoord2Country()
  mapping$coords   <- paste(mapping$coords, mapping$iso, sep = ".")

  x    <- toolAggregate(x, rel = mapping, from = "coords", to = "iso", dim = 1)
  xGLO <- dimSums(x, dim = 1)
  x    <- mbind(x, xGLO)

  # Transform to data frame
  out <- as.data.frame(x)
  out <- data.frame(Region = out$Region,
                    Variable = as.character(out$Data1),
                    Value = round(out$Value, digits = 2),
                    stringsAsFactors = FALSE)
  out <- reshape(out, idvar = "Region", timevar = "Variable", direction = "wide")
  names(out) <- c("Country",
                  "Irrigated Area (as reported by LUH)",
                  "Currently irrigated area (sustainable), our study",
                  "Currently irrigated area (unsustainable), our study",
                  "Currently irrigated area (total), our study",
                  "Potentially irrigated area (sustainable) on current cropland, our study",
                  "Potentially irrigated area (unsustainable) on current cropland, our study",
                  "Potentially irrigated area (total) on current cropland, our study",
                  "Potentially irrigated area (sustainable) on potential cropland, our study",
                  "Potentially irrigated area (unsustainable) on potential cropland, our study",
                  "Potentially irrigated area (total) on potential cropland, our study")

  out <- out[order(out$Country), ]

  return(list(data        = out,
              description = d,
              unit        = u))
}
