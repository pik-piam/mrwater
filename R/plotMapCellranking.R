#' @title       plotMapCellranking
#' @description map of cellranking
#'
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      year to be displayed
#' @param iniyear          Initialization year of irrigation system
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or
#'                         calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                         smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                         smoothed_calib (smoothed LPJmL yield potentials, not harmonized, calibrated for proxycrops)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops,
#'                         "meancroprank": rank over mean of proxy crops (normalized),
#'                         "meanpricedcroprank": rank over mean of proxy crops (normalized using price),
#'                         "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE
#'                         separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area).
#'                                           FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotMapCellranking() }
#'
#' @importFrom luplot plotmap2
#'
#' @export

plotMapCellranking <- function(lpjml, climatetype, selectyears, rankmethod, cropmix, iniyear, yieldcalib, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for the map")
  }

  glocellrank  <- calcOutput("IrrigCellranking", lpjml = lpjml, climatetype = climatetype, cellrankyear = selectyears, method = rankmethod,
                             cropmix = cropmix, iniyear = iniyear, yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)

  if (length(glocellrank[, 1, 1]) > 67420) {
    glocellrank <- glocellrank[1:67420, , ]
    getCells(glocellrank) <- gsub("A_", "", getCells(glocellrank))
  }

  out <- plotmap2(toolLPJcell2MAgPIEcell(glocellrank), lowcol = "darkred", highcol = "white")

  return(out)
}
