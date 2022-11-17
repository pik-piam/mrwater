#' @title       reportEconOfIrrig
#' @description reports potentially irrigated area depending on gainthreshold,
#'              land constraint and water constraint
#'
#' @param region           regional resolution (can be country iso-code,
#'                         region name and respective mapping "EUR:H12", "GLO" for global)
#' @param output           output to be displayed: irrigated area "IrrigArea" or
#'                         available water volume "wat_ag_ww" "wat_ag_wc"
#' @param gtRange          range of x-axis (gainthreshold) to be depicted on the curve
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param iniyear          initialization year
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod        EFR method used including selected strictness of EFRs
#'                         (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Method used: Quantile method (Q) or
#'                          Coefficient of Variation (CV)
#'                          combined with scalar value defining the strictness
#'                          of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year
#'                          (Qx, e.g. Q75: 0.25, Q50: 0.5)
#'                          or base value for exponential curve separated by : (CV:2)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param rankmethod        Rank and optimization method consisting of
#'                          Unit according to which rank is calculated:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return;
#'                          Unit of yield improvement potential to be returned;
#'                          Level of price aggregation used:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices;
#'                          and boolean indicating fullpotential (TRUE, i.e. cell
#'                          receives full irrigation requirements in total area)
#'                          or reduced potential (FALSE, reduced potential of cell
#'                          receives at later stage in allocation algorithm);
#'                          separated by ":"
#' @param allocationrule    Rule to be applied for river basin discharge allocation
#'                          across cells of river basin ("optimization", "upstreamfirst")
#' @param thresholdtype     Unit of yield improvement potential used as threshold,
#'                          consisting of two components:
#'                          Unit:
#'                          tDM (tons per dry matter),
#'                          USD_ha (USD per hectare) for area return, or
#'                          USD_m3 (USD per cubic meter) for volumetric return.
#'                          Price aggregation:
#'                          "GLO" for global average prices, or
#'                          "ISO" for country-level prices
#' @param irrigationsystem  Irrigation system used: system share as in initialization year,
#'                          or drip, surface, sprinkler for full irrigation by selected system
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param potentialWat     if TRUE: potential available water and areas used,
#'                          if FALSE: currently reserved water on current irrigated cropland used
#' @param comAg            if TRUE: the currently already irrigated areas
#'                                   in initialization year are reserved for irrigation,
#'                          if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                         by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                       GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#' @param transDist        Water transport distance allowed to fulfill locally
#'                         unfulfilled water demand by surrounding cell water availability
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' reportEconOfIrrig(gtRange = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

reportEconOfIrrig <- function(region = "GLO", output, gtRange, scenario, lpjml, iniyear,
                              selectyears, climatetype, efrMethod, accessibilityrule, rankmethod, yieldcalib,
                              allocationrule, thresholdtype, irrigationsystem, landScen, cropmix,
                              potentialWat = TRUE, comAg, multicropping, transDist) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  if (output == "IrrigArea") {

    x <- collapseNames(calcOutput("IrrigAreaPotential", gainthreshold = 0,
                                  lpjml = lpjml, climatetype = climatetype,
                                  selectyears = selectyears, iniyear = iniyear,
                                  accessibilityrule = accessibilityrule,
                                  efrMethod = efrMethod, rankmethod = rankmethod,
                                  yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, transDist = transDist,
                                  irrigationsystem = irrigationsystem, landScen = landScen,
                                  cropmix = cropmix, potentialWat = potentialWat,
                                  comAg = comAg, multicropping = multicropping,
                                  aggregate = FALSE)[, , "irrigatable"])
    d <- "Irrigatable Area"
    u <- "Irrigatable Area (Mha)"

  } else {

    x <- collapseNames(calcOutput("WaterUsePotential", gainthreshold = 0,
                                  lpjml = lpjml, climatetype = climatetype,
                                  selectyears = selectyears, iniyear = iniyear,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib,
                                  thresholdtype = thresholdtype, allocationrule = allocationrule,
                                  irrigationsystem = irrigationsystem, landScen = landScen,
                                  cropmix = cropmix, comAg = comAg, transDist = transDist,
                                  multicropping = multicropping, aggregate = FALSE)[, , output])
    # transform from mio. m^3 to km^3:
    # (1 km^3 = 1e+09 m^3)
    # (1 mio. = 1e+06)
    x <- x / 1000
    d <- paste0("Water Use Potential")
    u <- paste0("Potential water use", output, "(km^3)")

  }

  # sum up over regional dimension and create data frame
  x  <- as.data.frame(toolRegionSums(x = x, region = region))
  df <- data.frame(EFP = x$Data1, Scen = x$Data2, GT0 = x$Value, stringsAsFactors = FALSE)

  if (gtRange[1] == 0) {
    gtRange <- gtRange[-1]
  }

  for (gainthreshold in gtRange) {

    if (output == "IrrigArea") {

      x <- collapseNames(calcOutput("IrrigAreaPotential",
                                    iniyear = iniyear, selectyears = selectyears,
                                    lpjml = lpjml, climatetype = climatetype,
                                    yieldcalib = yieldcalib, cropmix = cropmix,
                                    gainthreshold = gainthreshold, rankmethod = rankmethod,
                                    allocationrule = allocationrule, thresholdtype = thresholdtype,
                                    accessibilityrule = accessibilityrule,
                                    efrMethod = efrMethod,
                                    irrigationsystem = irrigationsystem,
                                    landScen = landScen, transDist = transDist,
                                    potentialWat = potentialWat, comAg = comAg,
                                    multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])

    } else {

      x <- collapseNames(calcOutput("WaterUsePotential",
                                    iniyear = iniyear, selectyears = selectyears,
                                    lpjml = lpjml, climatetype = climatetype,
                                    yieldcalib = yieldcalib, cropmix = cropmix,
                                    gainthreshold = gainthreshold, rankmethod = rankmethod,
                                    allocationrule = allocationrule, thresholdtype = thresholdtype,
                                    accessibilityrule = accessibilityrule,
                                    efrMethod = efrMethod, transDist = transDist,
                                    irrigationsystem = irrigationsystem,
                                    landScen = landScen, comAg = comAg,
                                    multicropping = multicropping, aggregate = FALSE)[, , output])
      x <- x / 1000

    }

    # sum up over regional dimension and create data frame
    x  <- as.data.frame(toolRegionSums(x = x, region = region))

    tmp              <- data.frame(EFP = x$Data1, Scen = x$Data2, Value = x$Value)
    names(tmp)[3]    <- paste0("GT", gainthreshold)
    df               <- merge(df, tmp)
  }

  df        <- data.frame(t(data.frame(Scen = paste(output, df$EFP, df$Scen, sep = "."),
                                       df[-c(1, 2)])), stringsAsFactors = FALSE)
  names(df) <- as.character(unlist(df[1, ]))
  df        <- df[-1, ]
  df        <- data.frame(GT = as.numeric(gsub("GT", "", rownames(df))), df, stringsAsFactors = FALSE)
  df        <- as.data.frame(lapply(df, as.numeric))

  return(list(data        = df,
              description = d,
              unit        = u))
}
