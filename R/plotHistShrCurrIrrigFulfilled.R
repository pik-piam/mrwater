#' @title       plotHistShrCurrIrrigFulfilled
#' @description plots histogram of share of current irrigation that can be
#'              fulfilled given water availability of the algorithm
#'              weighted with irrigated area
#'
#' @param scenario    EFP and non-agricultural water use scenario separated by "."
#'                    (e.g. "on.ssp2")
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
#' @param selectyears Year of plot (Note: one single year must be selected)
#' @param iniyear     Initialization year for committed agricultural uses
#' @param climatetype Switch between different climate models or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod   EFR method used including selected strictness of EFRs (e.g. Smakhtin:fair)
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotHistShrCurrIrrigFulfilled() }
#'
#' @export

plotHistShrCurrIrrigFulfilled <- function(scenario, iniyear, lpjml, selectyears, climatetype, EFRmethod) {

  if (!requireNamespace("plotrix", quietly = TRUE)) stop("The package plotrix is required for plotting plotHistShrCurrIrrigFulfilled!")

  if (length(selectyears) > 1) {
    stop("Please select one year only for Map depicting the share of current irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  # Committed Agricultural Water (in mio. m^3)
  CAU_magpie <- calcOutput("WaterUseCommittedAg", lpjml = lpjml, selectyears = selectyears, climatetype = climatetype, iniyear = iniyear, aggregate = FALSE)
  act_ww     <- collapseNames(dimSums(CAU_magpie[, , "withdrawal"], dim = 3))
  act_wc     <- collapseNames(dimSums(CAU_magpie[, , "consumption"], dim = 3))

  # Water Committed to Agriculture after Routing (in mio. m^3)
  ComAg_wat <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture", lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod, selectyears = selectyears, iniyear = iniyear, aggregate = FALSE)
  com_ww    <- collapseNames(ComAg_wat[, , "currHuman_ww"])
  com_wc    <- collapseNames(ComAg_wat[, , "currHuman_wc"])

  ## Read in cropland area (by crop) from crop area initialization (in mio. ha)
  area <- calcOutput("IrrigAreaCommitted", selectyears = selectyears, iniyear = iniyear, aggregate = FALSE)
  area <- dimSums(area, dim = 3)

  ww_shr <- com_ww / act_ww
  ww_shr[act_ww == 0 & com_ww == 0] <- 0
  ww_shr[act_ww == 0]               <- NA
  wc_shr <- com_wc / act_wc
  wc_shr[act_wc == 0 & com_wc == 0] <- 0
  wc_shr[act_wc == 0]               <- NA

  area[is.na(ww_shr[, , scenario])] <- 0
  ww_shr[is.na(ww_shr)]             <- 0

  out <- plotrix::weighted.hist(x = ww_shr[, , scenario], w = area, breaks = 50)

  return(out)
}
