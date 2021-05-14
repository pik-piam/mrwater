#' @title       plotMapCellranking
#' @description map of cellranking
#'
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      year to be displayed
#' @param iniyear          Initialization year of irrigation system
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param FAOyieldcalib    TRUE (LPJmL yields scaled with current FAO yield) or FALSE (LPJmL yield potentials)
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

plotMapCellranking <- function(lpjml, climatetype, selectyears, rankmethod, proxycrop, iniyear, FAOyieldcalib) {

  if (length(selectyears)>1) {
    stop("Please select one year only for the map")
  }

  glocellrank  <- calcOutput("IrrigCellranking", lpjml=lpjml, climatetype=climatetype, cellrankyear=selectyears, method=rankmethod, proxycrop=proxycrop, iniyear=iniyear, FAOyieldcalib=FAOyieldcalib, aggregate=FALSE)

  out <- plotmap2(toolLPJarrayToMAgPIEmap(glocellrank))

  return(out)
}
