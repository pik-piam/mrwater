#' @title       reportCountryResults
#' @description reports country-values (irrigated area or irrigation water availability)
#'
#' @param output           output to be displayed: irrigated area "IrrigArea" or available water volume "wat_ag_ww" "wat_ag_wc"
#' @param gainthreshold    gainthreshold chosen for reporting
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param cropmix          cropmix for which irrigation yield improvement is calculated
#'                         can be selection of proxycrop(s) for calculation of average yield gain
#'                         or hist_irrig or hist_total for historical cropmix
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
#'
#' @return data frame in country-resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' reportCountryResults(GT_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

reportCountryResults <- function(output, lpjml, climatetype, gainthreshold,
                                 EFRmethod, accessibilityrule, rankmethod, yieldcalib,
                                 allocationrule, thresholdtype, irrigationsystem, cropmix,
                                 multicropping) {
  # Set arguments
  scenario    <- "ssp2"
  selectyears <- "y2010"
  iniyear     <- "y2010"
  com_ag      <- TRUE

  # LUH total irrigated area
  irrigAreaLUH <- setYears(collapseNames(calcOutput("Croparea", years = iniyear, sectoral = "kcr", physical = TRUE,
                           cells = "lpjcell", cellular = TRUE, irrigation = TRUE, aggregate = FALSE)[, , "irrigated"]),
                           iniyear)
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                              <- toolGetMappingCoord2Country()
  getCells(irrigAreaLUH)           <- paste(map$coords, map$iso, sep=".")
  names(dimnames(irrigAreaLUH))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  if (output == "IrrigArea") {

    # Irrigated Area (LUH)
    currLUH           <- dimSums(irrigAreaLUH, dim = "MAG")
    getSets(currLUH)  <- c("x", "y", "iso", "year", "type")
    getNames(currLUH) <- "irrig_area_LUH"

    # Currently Irrigated Area [total, sustainable, unsustainable]
    currIrr <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
                                        selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                        EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                        thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = "currCropland:2010",
                                        cropmix = cropmix, potential_wat = FALSE, com_ag = TRUE, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"][, , scenario])
    currIrr           <- dimSums(currIrr, dim = "season")
    getSets(currIrr)  <- c("x", "y", "iso", "year", "type")
    currIrrSus             <- currIrr[, , "on"]
    getNames(currIrrSus)   <- "curr_irrig_sus"
    currIrrUnsus           <- currIrr[, , "off"]
    getNames(currIrrUnsus) <- "curr_irrig_unsus"
    currIrrTot             <- dimSums(currIrr, dim = 3)
    getNames(currIrrTot)   <- "curr_irrig_tot"

    # Potentially Irrigated Area (on current Cropland) [total, sustainable, unsustainable]
    potIrrcurr <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
                                        selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                        EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                        thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = "currCropland:2010",
                                        cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"][, , scenario])
    potIrrcurr <- dimSums(potIrrcurr, dim = "season")

    potIrrcurr           <- dimSums(potIrrcurr, dim = "season")
    getSets(potIrrcurr)  <- c("x", "y", "iso", "year", "type")
    potIrrcurrSus             <- potIrrcurr[, , "on"]
    getNames(potIrrcurrSus)   <- "curr_irrig_currCropland_sus"
    potIrrcurrUnsus           <- potIrrcurr[, , "off"]
    getNames(potIrrcurrUnsus) <- "curr_irrig_currCropland_unsus"
    potIrrcurrTot             <- dimSums(potIrrcurr, dim = 3)
    getNames(potIrrcurrTot)   <- "curr_irrig_currCropland_tot"

    # Potentially Irrigated Area (on potential Cropland) [total, sustainable, unsustainable]
    potIrrpot <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
                                           selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                           EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                           thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = "potIrrig_HalfEarth:2010",
                                           cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"][, , scenario])
    potIrrpot <- dimSums(potIrrpot, dim = "season")

    potIrrpot           <- dimSums(potIrrpot, dim = "season")
    getSets(potIrrpot)  <- c("x", "y", "iso", "year", "type")
    potIrrpotSus             <- potIrrpot[, , "on"]
    getNames(potIrrpotSus)   <- "curr_irrig_potCropland_sus"
    potIrrpotUnsus           <- potIrrpot[, , "off"]
    getNames(potIrrpotUnsus) <- "curr_irrig_potCropland_unsus"
    potIrrpotTot             <- dimSums(potIrrpot, dim = 3)
    getNames(potIrrpotTot)   <- "curr_irrig_potCropland_tot"

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

    # # Current Irrigation Water Use [total, sustainable, unsustainable]
    # currIrr <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
    #                         lpjml = lpjml, climatetype = climatetype,
    #                         EFRmethod = EFRmethod, selectyears = selectyears,
    #                         iniyear = iniyear, aggregate = FALSE)
    #
    #
    # currIrr           <- dimSums(currIrr, dim = "season")
    # getSets(currIrr)  <- c("x", "y", "iso", "year", "type")
    # currIrrSus             <- currIrr[, , "on"]
    # getNames(currIrrSus)   <- "curr_irrig_sus"
    # currIrrUnsus           <- currIrr[, , "off"]
    # getNames(currIrrUnsus) <- "curr_irrig_unsus"
    # currIrrTot             <- dimSums(currIrr, dim = 3)
    # getNames(currIrrTot)   <- "curr_irrig_tot"
    #
    # # Potentially Irrigated Area (on current Cropland) [total, sustainable, unsustainable]
    # potIrrcurr <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
    #                                        selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
    #                                        EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
    #                                        thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = "currCropland:2010",
    #                                        cropmix = cropmix, potential_wat = TRUE, com_ag = TRUE, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"][, , scenario])
    # potIrrcurr <- dimSums(potIrrcurr, dim = "season")
    #
    # x <- collapseNames(calcOutput("WaterPotUse",     lpjml = lpjml, gainthreshold = gainthreshold,
    #                               selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
    #                               EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
    #                               thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear, avlland_scen = "currCropland:2010",
    #                               cropmix = cropmix, com_ag = TRUE, multicropping = multicropping, aggregate = FALSE)[, , output])
    #
    #
    # potIrrcurr           <- dimSums(potIrrcurr, dim = "season")
    # getSets(potIrrcurr)  <- c("x", "y", "iso", "year", "type")
    # potIrrcurrSus             <- potIrrcurr[, , "on"]
    # getNames(potIrrcurrSus)   <- "curr_irrig_currCropland_sus"
    # potIrrcurrUnsus           <- potIrrcurr[, , "off"]
    # getNames(potIrrcurrUnsus) <- "curr_irrig_currCropland_unsus"
    # potIrrcurrTot             <- dimSums(potIrrcurr, dim = 3)
    # getNames(potIrrcurrTot)   <- "curr_irrig_currCropland_tot"
    #
    # # Potentially Irrigated Area (on potential Cropland) [total, sustainable, unsustainable]
    # potIrrpot <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
    #                                       selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
    #                                       EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
    #                                       thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = "potIrrig_HalfEarth",
    #                                       cropmix = cropmix, potential_wat = TRUE, com_ag = TRUE, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"][, , scenario])
    # potIrrpot <- dimSums(potIrrpot, dim = "season")
    #
    # x <- collapseNames(calcOutput("WaterPotUse",     lpjml = lpjml, gainthreshold = gainthreshold,
    #                               selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
    #                               EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
    #                               thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear, avlland_scen = "potIrrig_HalfEarth",
    #                               cropmix = cropmix, com_ag = TRUE, multicropping = multicropping, aggregate = FALSE)[, , output])
    #
    #
    # potIrrpot           <- dimSums(potIrrpot, dim = "season")
    # getSets(potIrrpot)  <- c("x", "y", "iso", "year", "type")
    # potIrrpotSus             <- potIrrpot[, , "on"]
    # getNames(potIrrpotSus)   <- "curr_irrig_potCropland_sus"
    # potIrrpotUnsus           <- potIrrpot[, , "off"]
    # getNames(potIrrpotUnsus) <- "curr_irrig_potCropland_unsus"
    # potIrrpotTot             <- dimSums(potIrrpot, dim = 3)
    # getNames(potIrrpotTot)   <- "curr_irrig_potCropland_tot"
    #
    #
    #
    #
    #
    #
    # x <- collapseNames(calcOutput("WaterPotUse",     lpjml = lpjml, gainthreshold = 0,
    #                               selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
    #                               EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
    #                               thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear, avlland_scen = avlland_scen,
    #                               cropmix = cropmix, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , output])
    # x <- dimSums(x, dim = "season")
    #

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

  x <- toolAggregate(x, rel = mapping, from = "coords", to = "iso", dim = 1)
  x <- toolCountryFill(x, fill = NA)
  # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB" missing

  # Transform to data frame
  out <- as.data.frame(x)


  # # sum up over regional dimension and create data frame
  # df <- data.frame(EFP = x$Data1, Scen = x$Data2, GT0 = x$Value, stringsAsFactors = FALSE)
  #
  # if (GT_range[1] == 0) {
  #   GT_range <- GT_range[-1]
  # }
  #
  # for (gainthreshold in GT_range) {
  #
  #   if (output == "IrrigArea") {
  #
  #     x <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
  #                                   selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
  #                                   EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
  #                                   thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
  #                                   cropmix = cropmix, potential_wat = potential_wat, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])
  #     x <- dimSums(x, dim = "season")
  #
  #   } else {
  #
  #     x <- collapseNames(calcOutput("WaterPotUse",     lpjml = lpjml, gainthreshold = gainthreshold,
  #                                   selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
  #                                   EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
  #                                   thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear,
  #                                   avlland_scen = avlland_scen, cropmix = cropmix, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , output])
  #     x <- dimSums(x, dim = "season")
  #     x <- x / 1000
  #
  #   }
  #
  #   # sum up over regional dimension and create data frame
  #   x  <- as.data.frame(toolRegionSums(x = x, region = region))
  #
  #   tmp              <- data.frame(EFP = x$Data1, Scen = x$Data2, Value = x$Value)
  #   names(tmp)[3]    <- paste0("GT", gainthreshold)
  #   df               <- merge(df, tmp)
  # }
  #
  # df        <- data.frame(t(data.frame(Scen = paste(output, df$EFP, df$Scen, sep = "."), df[-c(1, 2)])), stringsAsFactors = FALSE)
  # names(df) <- as.character(unlist(df[1, ]))
  # df        <- df[-1, ]
  # df        <- data.frame(GT = as.numeric(gsub("GT", "", rownames(df))), df, stringsAsFactors = FALSE)
  # df        <- as.data.frame(lapply(df, as.numeric))

  return(list(data        = out,
              description = d,
              unit        = u))
}
