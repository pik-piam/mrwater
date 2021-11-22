#' @title       reportEconOfIrrig
#' @description reports potentially irrigated area depending on gainthreshold,
#'              land constraint and water constraint
#'
#' @param region           regional resolution (can be country iso-code, region name and respective mapping "EUR:H12", "GLO" for global)
#' @param output           output to be displayed: irrigated area "IrrigArea" or available water volume "wat_ag_ww" "wat_ag_wc"
#' @param GT_range         range of x-axis (gainthreshold) to be depicted on the curve
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param rankmethod        method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule    Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype     Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem  Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. landScen (currCropland, currIrrig, potCropland)
#'                          2. for curr-scenarios: initialization year;
#'                          for pot-scenarios: protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                          For case of pot-scenario without land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
#' @param potential_wat     if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag            if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' reportEconOfIrrig(GT_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom stringr str_split
#'
#' @export

reportEconOfIrrig <- function(region = "GLO", output, GT_range, scenario, lpjml,
                              selectyears, climatetype, efrMethod, accessibilityrule, rankmethod, yieldcalib,
                              allocationrule, thresholdtype, irrigationsystem, landScen, cropmix,
                              potential_wat = TRUE, com_ag, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  iniyear <- as.numeric(as.list(strsplit(landScen, split = ":"))[[1]][2])

  if (output == "IrrigArea") {

    x <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                  efrMethod = efrMethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
                                  cropmix = cropmix, potential_wat = potential_wat, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])
    x <- dimSums(x, dim = "season")
    d <- "Irrigatable Area"
    u <- "Irrigatable Area (Mha)"

  } else {

    x <- collapseNames(calcOutput("WaterPotUse",     lpjml = lpjml, gainthreshold = 0,
                                  selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                  efrMethod = efrMethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear, landScen = landScen,
                                  cropmix = cropmix, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , output])
    x <- dimSums(x, dim = "season")
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

  if (GT_range[1] == 0) {
    GT_range <- GT_range[-1]
  }

  for (gainthreshold in GT_range) {

    if (output == "IrrigArea") {

      x <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
                                    selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                    efrMethod = efrMethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                    thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
                                    cropmix = cropmix, potential_wat = potential_wat, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])
      x <- dimSums(x, dim = "season")

    } else {

      x <- collapseNames(calcOutput("WaterPotUse",     lpjml = lpjml, gainthreshold = gainthreshold,
                                    selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                    efrMethod = efrMethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                    thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, iniyear = iniyear,
                                    landScen = landScen, cropmix = cropmix, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , output])
      x <- dimSums(x, dim = "season")
      x <- x / 1000

    }

    # sum up over regional dimension and create data frame
    x  <- as.data.frame(toolRegionSums(x = x, region = region))

    tmp              <- data.frame(EFP = x$Data1, Scen = x$Data2, Value = x$Value)
    names(tmp)[3]    <- paste0("GT", gainthreshold)
    df               <- merge(df, tmp)
  }

  df        <- data.frame(t(data.frame(Scen = paste(output, df$EFP, df$Scen, sep = "."), df[-c(1, 2)])), stringsAsFactors = FALSE)
  names(df) <- as.character(unlist(df[1, ]))
  df        <- df[-1, ]
  df        <- data.frame(GT = as.numeric(gsub("GT", "", rownames(df))), df, stringsAsFactors = FALSE)
  df        <- as.data.frame(lapply(df, as.numeric))

  return(list(data        = df,
              description = d,
              unit        = u))
}
