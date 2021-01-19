#' @title plotIrrigCellranking
#' @description This function plots the cellranking based on yield improvement potential through irrigation on a global map
#'
#' @param climatetype switch between different climate scenarios for yields
#' @param time            time smoothing: average, spline (default) or raw
#' @param averaging_range just specify for time=="average": number of time steps to average
#' @param dof             just specify for time=="spline": degrees of freedom
#' @param harmonize_baseline FALSE (default) no harmonization, harmonization: if a baseline is specified here data is harmonized to that baseline (from ref_year onwards)
#' @param ref_year           just specify for harmonize_baseline != FALSE : Reference year
#' @param cellrankyear year(s) for which cell rank is calculated
#' @param crops       switch between "magpie" and "lpjml" (default) crops
#' @param cells       switch between "lpjcell" (67420) and "magpiecell" (59199)
#' @param method      method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price)
#' @param proxycrop   proxycrop(s) selected for rank calculation
#' @param iniyear     initialization year for price in price-weighted normalization of meanpricedcroprank
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @importFrom luplot plotmap2
#'
#' @examples
#' \dontrun{ plotIrrigCellranking() }

plotmapIrrigCellranking <- function(climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
                                 cellrankyear="y1995", cells="lpjcell", crops="magpie", method="meancellrank", proxycrop=c("maiz", "rapeseed", "puls_pro"), iniyear=1995){

  # read in cell ranking
  x <- calcOutput("IrrigCellranking", version="LPJmL5", aggregate=FALSE, cells=cells,
                  climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                  cellrankyear=cellrankyear,  crops=crops, method=method, proxycrop=proxycrop, iniyear=iniyear)

  # Correct number of cells
  if (length(getCells(x))==67420){
    x <- x[magclassdata$cellbelongings$LPJ_input.Index,,]
    x <- toolCell2isoCell(x)
  } else if (length(getCells(x))==59199){
    x <- x
  } else {
    stop("Please provide object in cellular resolution for map")
  }

  # Plot
  out <- plotmap2(x, lowcol="#8B0000", highcol="white")

  return(out)
}
