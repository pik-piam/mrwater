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
#' @param method      method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price)
#' @param proxycrop   proxycrop(s) selected for rank calculation
#' @param iniyear     initialization year for price in price-weighted normalization of meanpricedcroprank
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotIrrigCellranking() }

plotIrrigCellranking <- function(climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
                                 cellrankyear="y1995", cells="lpjcell", crops="magpie", method="meancellrank", proxycrop=c("maiz", "rapeseed", "puls_pro"), iniyear=1995){

  # read in cell ranking
  x <- calcOutput("IrrigCellranking", version="LPJmL5", cells="lpjcell", aggregate=FALSE,
                  climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year,
                  cellrankyear=cellrankyear,  crops=crops, method=method, proxycrop=proxycrop, iniyear=iniyear)


  toolLPJarrayToMAgPIEmap


  ### Read in potential yield gain per cell (tons per ha)
  yield_gain <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=cellrankyear,
                           harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, averaging_range=averaging_range, dof=dof,
                           cells=cells, crops=crops, proxycrop="all", monetary=FALSE, aggregate=FALSE)
  # select proxy crops
  yield_gain <- yield_gain[,,proxycrop]

  ### Calculate global cell rank
  # Def. "meancellrank": ranking of cells or proxy crops, then: average over ranks
  if (method=="meancellrank"){

    # cell ranking for crop (from highest yield gain (rank=1) to lowest yield gain (rank=1+x))
    cropcellrank <- apply(-yield_gain,c(2,3),rank)

    # calculate mean over cropcellranks
    glocellrank  <- dimSums(cropcellrank,dim=3)/length(getNames(cropcellrank))
    # ties are solved by first occurrence
    glocellrank <- apply(glocellrank,2,rank,ties.method="first")

    # Def. "meancroprank": average over yield gain of proxycrops, then: ranking of resulting average yield gain
  } else if (method=="meancroprank"){

    # normalize yield gains of proxy crops (unity-based normalization)
    min_yield  <- as.magpie(apply(yield_gain,c(2,3),min))
    max_yield  <- as.magpie(apply(yield_gain,c(2,3),max))
    yield_gain <- (yield_gain-min_yield)/(max_yield-min_yield)

    # calculate average yield gain over normalized proxy crops
    yield_gain <- dimSums(yield_gain,dim=3)/length(getNames(yield_gain))

    # calculate rank (ties are solved by first occurence)
    glocellrank <- apply(-yield_gain,c(2,3),rank,ties.method="first")

  } else if (method=="meanpricedcroprank"){

    # price in initialization (USD05/tDM)
    p <- calcOutput("IniFoodPrice", datasource="FAO", years=NULL, aggregate=FALSE, year=iniyear)[,,proxycrop]

    # monetary yield gain (in USD05/ha)
    yield_gain <- yield_gain*p

    # normalize yield gains of proxy crops (unity-based normalization)
    min_yield  <- as.magpie(apply(yield_gain,c(2,3),min))
    max_yield  <- as.magpie(apply(yield_gain,c(2,3),max))
    yield_gain <- (yield_gain-min_yield)/(max_yield-min_yield)

    # calculate average yield gain over normalized proxy crops
    yield_gain <- dimSums(yield_gain,dim=3)/length(getNames(yield_gain))

    # calculate rank (ties are solved by first occurence)
    glocellrank <- apply(-yield_gain,c(2,3),rank,ties.method="first")

  } else {
    stop("Please select a method for rank calculation")
  }

  glocellrank <- as.magpie(glocellrank,spatial=1)

  # Check for NAs
  if(any(is.na(glocellrank))){
    stop("Function YieldImprovementPotential produced NAs")
  }

  return(list(
    x=glocellrank,
    weight=NULL,
    unit="1",
    description="Rank of cell according to yield gain potential by irrigation",
    isocountries=FALSE))
}
