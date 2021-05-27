#' @title       plotMapCellranking
#' @description map of cellranking
#'
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      year to be displayed
#' @param iniyear          Initialization year of irrigation system
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
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

plotMapCellranking <- function(lpjml, climatetype, selectyears, rankmethod, proxycrop, iniyear, yieldcalib) {

  if (length(selectyears)>1) {
    stop("Please select one year only for the map")
  }

  glocellrank  <- calcOutput("IrrigCellranking", lpjml=lpjml, climatetype=climatetype, cellrankyear=selectyears, method=rankmethod, proxycrop=proxycrop, iniyear=iniyear, yieldcalib=yieldcalib, aggregate=FALSE)

  if (length(glocellrank[,1,1])>67420) {
    glocellrank <- glocellrank[1:67420,,]
    getCells(glocellrank) <- gsub("A_", "", getCells(glocellrank))
  }

  # transform to 59199 for plotting
  glocellrank <- glocellrank[magclassdata$cellbelongings$LPJ_input.Index,,]

  out <- plotmap2(glocellrank, lowcol="darkred", highcol="white")

  return(out)
}
