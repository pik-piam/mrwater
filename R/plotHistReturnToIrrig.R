#' @title       plotHistReturnToIrrig
#' @description plot map of share of current irrigation that can be fulfilled given surface water availability of the algorithm
#'
#' @param scenario    EFP and non-agricultural water use scenario separated by "."
#'                    (e.g. "on.ssp2")
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
#' @param selectyears Year of plot (Note: one single year must be selected)
#' @param iniyear     Initialization year for committed agricultural uses
#' @param climatetype Switch between different climate models or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod   EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param cropmix     cropmix for which irrigation yield improvement is calculated
#'                    can be selection of proxycrop(s) for calculation of average yield gain
#'                    or hist_irrig or hist_total for historical cropmix
#' @param thresholdtype Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield)
#'                      or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops)
#'                      or smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#'                      or smoothed_calibrated
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotHistReturnToIrrig()
#' }
#'
#' @export

plotHistReturnToIrrig <- function(scenario, iniyear, lpjml, selectyears, climatetype, EFRmethod, cropmix, thresholdtype, multicropping, yieldcalib) {

  if (!requireNamespace("plotrix", quietly = TRUE)) stop("The package plotrix is required for plotting plotHistShrCurrIrrigFulfilled!")

  if (length(selectyears) > 1) {
    stop("Please select one year only for Map depicting the share of current irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  # Croplist
  croplist <- collapseNames(calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
                                     iniyear = iniyear, selectyears = selectyears, yieldcalib = yieldcalib, aggregate = FALSE)[, , "rainfed"])
  croplist <- getNames(croplist)
  croplist <- croplist[croplist != "rice_pro"]

  # Return to Irrigation
  return    <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype, selectyears = selectyears,
                            cropmix = croplist, unit = thresholdtype, iniyear = iniyear, yieldcalib = yieldcalib,
                            multicropping = multicropping, aggregate = FALSE) #### ALLES AUSSER REIS (evtl. stacked oder besser transparent)
  return    <- dimSums(return, dim = 3)

  # rice return
  ricereturn <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype, selectyears = selectyears,
                           cropmix = "rice_pro", unit = "USD_ha", iniyear = iniyear, yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)
  ricereturn <- dimSums(ricereturn, dim = 3)

  # cap return to irrigation
  return[return > 5000] <- 5000
  ricereturn[ricereturn > 5000] <- 5000

  ## Read in cropland area (by crop) from crop area initialization (in mio. ha)
  area     <- calcOutput("IrrigAreaCommitted", selectyears = selectyears, iniyear = iniyear, aggregate = FALSE)
  ricearea <- area[, , "rice_pro"]
  area     <- dimSums(area, dim = 3)

  plotrix::weighted.hist(x = return, w = area, breaks = 100, col = "#5ab4ac", ylab = "Irrigated Area (Mha)", xlab = "Return to Irrigation (USD/ha)")
  out <- plotrix::weighted.hist(x = ricereturn, w = ricearea, breaks = 100, col = "#372E1A80", ylab = "Irrigated Area (Mha)", xlab = "Return to Irrigation (USD/ha)", add = T)

  return(out)
}
