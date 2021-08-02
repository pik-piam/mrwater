#' @title       plotReturnToIrrigation
#' @description plot monetary yield gain achieved on irrigated area (USD) or
#'              irrigatable area (Mha) dependent on water accessibility
#'
#' @param y_axis_range     range of y-axis (monetary irrigation gain) to be depicted on the curve
#' @param x_axis           x_axis type to be displayed: irrigated area "IrrigArea" or available water volume "wat_ag_ww" "wat_ag_wc" "wat_tot_ww" "wat_tot_wc"
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotReturnToIrrigation(y_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom ggplot2 ggplot geom_line geom_point aes_string ggtitle xlab ylab theme_bw
#'
#' @export

plotReturnToIrrigation <- function(y_axis_range, x_axis, scenario, lpjml, selectyears, climatetype, EFRmethod, rankmethod, yieldcalib, allocationrule, thresholdtype, gainthreshold, irrigationsystem, avlland_scen, cropmix, com_ag, multicropping, potential_wat = TRUE) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for Potential Irrigatable Area Supply Curve")
  }

  iniyear <- as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])

  x     <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
                                    selectyears = selectyears, climatetype = climatetype,
                                    accessibilityrule = "Q0", EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib,
                                    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                    avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = potential_wat, com_ag = com_ag,
                                    multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])
  x     <- dimSums(x, dim = "season")
  x     <- as.data.frame(dimSums(x, dim = 1))
  tmp1A <- data.frame(EFP = x$Data1, Scen = x$Data2, Irrigarea_GT0 = x$Value, stringsAsFactors = F)

  x     <- collapseNames(calcOutput("WaterPotUse",      lpjml = lpjml, gainthreshold = gainthreshold,
                                    selectyears = selectyears, climatetype = climatetype, accessibilityrule = "Q0",
                                    EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib,
                                    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                    iniyear = iniyear, avlland_scen = avlland_scen, cropmix = cropmix, com_ag = com_ag,
                                    multicropping = multicropping, aggregate = FALSE)[, , x_axis])
  x     <- dimSums(x, dim = "season")
  # transform from mio. m^3 to km^3:
  x     <- x / 1000
  x     <- as.data.frame(dimSums(x, dim = 1))
  tmp2A <- data.frame(EFP = x$Data1, Scen = x$Data2, Potwat_GT0 = x$Value, stringsAsFactors = F)

  if (y_axis_range[1] == 0) {
    y_axis_range <- y_axis_range[-1]
  }

  for (accessibilityrule in y_axis_range) {

    x <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, selectyears = selectyears,
                                  climatetype = climatetype, accessibilityrule = paste("Q", accessibilityrule, sep = ":"),
                                  EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                  avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = potential_wat, com_ag = com_ag,
                                  multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])
    x <- dimSums(x, dim = "season")
    x <- as.data.frame(dimSums(x, dim = 1))
    tmp1B              <- data.frame(EFP = x$Data1, Scen = x$Data2, Value = x$Value)
    names(tmp1B)[3]    <- paste0("Q", accessibilityrule)
    tmp1A              <- merge(tmp1A, tmp1B)

    x <- collapseNames(calcOutput("WaterPotUse",     lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                  accessibilityrule = paste("Q", accessibilityrule, sep = ":"), EFRmethod = EFRmethod,
                                  rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  thresholdtype = thresholdtype, gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                  iniyear = iniyear, avlland_scen = avlland_scen, cropmix = cropmix, com_ag = com_ag,
                                  multicropping = multicropping, aggregate = FALSE)[, , x_axis])
    x <- dimSums(x, dim = "season")
    x <- x / 1000
    x <- as.data.frame(dimSums(x, dim = 1))
    tmp2B              <- data.frame(EFP = x$Data1, Scen = x$Data2, Value = x$Value)
    names(tmp2B)[3]    <- paste0("Q", accessibilityrule)
    tmp2A              <- merge(tmp2A, tmp2B)
  }

  tmp1A        <- data.frame(t(data.frame(Scen = paste("IrrigArea", tmp1A$EFP, tmp1A$Scen, sep = "."), tmp1A[-c(1, 2)])), stringsAsFactors = F)
  names(tmp1A) <- as.character(unlist(tmp1A[1, ]))
  tmp1A        <- tmp1A[-1, ]
  tmp1A        <- data.frame(GT = as.numeric(gsub("GT", "", rownames(tmp1A))), tmp1A, stringsAsFactors = F)
  tmp1A        <- as.data.frame(lapply(tmp1A, as.numeric))

  tmp2A        <- data.frame(t(data.frame(Scen = paste("PotWat", tmp2A$EFP, tmp2A$Scen, sep = "."), tmp2A[-c(1, 2)])), stringsAsFactors = F)
  names(tmp2A) <- as.character(unlist(tmp2A[1, ]))
  tmp2A        <- tmp2A[-1, ]
  tmp2A        <- data.frame(GT = as.numeric(gsub("GT", "", rownames(tmp2A))), tmp2A, stringsAsFactors = F)
  tmp2A        <- as.data.frame(lapply(tmp2A, as.numeric))

  df <- merge(tmp1A, tmp2A)

  out <- ggplot(data = df, aes_string(y = paste("IrrigArea", "on", scenario, sep = "."))) +
    geom_line(aes_string(x = paste("PotWat", "on", scenario, sep = ".")),  color = "darkblue")                    + geom_point(aes_string(x = paste("PotWat", "on", scenario, sep = "."))) +
    geom_line(aes_string(x = paste("PotWat", "off", scenario, sep = ".")), color = "darkred", linetype = "twodash") + geom_point(aes_string(x = paste("PotWat", "off", scenario, sep = "."))) +
    theme_bw() +
    ggtitle(paste0("Return to to Irrigation with FAO-calib=", yieldcalib, " on ", avlland_scen)) + ylab("Irrigatable Area (Mha)") + xlab("Accessible Water (km^3)")

  return(out)
}
