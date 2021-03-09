#' @title       calcIrrigCellranking
#' @description This function calculates a cellranking for the river basin discharge allocation based on yield improvement potential through irrigation
#'
#' @param climatetype     Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical" for yields
#' @param cellrankyear year(s) for which cell rank is calculated
#' @param method      method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water
#' @param proxycrop   proxycrop(s) selected for crop mix specific calculations: list of proxycrops or "historical"
#' @param iniyear     initialization year for price in price-weighted normalization of meanpricedcroprank and case of monetary=TRUE year of prices
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigCellranking", aggregate=FALSE) }

calcIrrigCellranking <- function(climatetype, cellrankyear, method, proxycrop, iniyear) {

  if (method=="meancellrank") {
    # Def. "meancellrank": ranking of cells or proxy crops, then: average over ranks

    ## Read in potential yield gain per cell (tons per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=cellrankyear, proxycrop=NULL, monetary=FALSE, iniyear=NULL, aggregate=FALSE)
    # select proxy crops
    yield_gain <- yield_gain[,,proxycrop]

    # cell ranking for crop (from highest yield gain (rank=1) to lowest yield gain (rank=1+x))
    cropcellrank <- apply(-yield_gain, c(2,3), rank)

    # calculate mean over cropcellranks
    glocellrank  <- dimSums(cropcellrank, dim=3) / length(getNames(cropcellrank))
    # ties are solved by first occurrence
    glocellrank <- apply(glocellrank, 2, rank, ties.method="first")

  } else if (method=="meancroprank") {
    # Def. "meancroprank": average over yield gain of proxycrops, then: ranking of resulting average yield gain

    ## Read in potential yield gain per cell (tons per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=cellrankyear, proxycrop=NULL, monetary=FALSE, iniyear=NULL, aggregate=FALSE)
    # select proxy crops
    yield_gain <- yield_gain[,,proxycrop]

    # normalize yield gains of proxy crops (unity-based normalization)
    min_yield  <- as.magpie(apply(yield_gain, c(2,3), min))
    max_yield  <- as.magpie(apply(yield_gain, c(2,3), max))
    yield_gain <- (yield_gain - min_yield) / (max_yield - min_yield)

    # calculate average yield gain over normalized proxy crops
    yield_gain <- dimSums(yield_gain,dim=3) / length(getNames(yield_gain))

    # calculate rank (ties are solved by first occurence)
    glocellrank <- apply(-yield_gain, c(2,3), rank, ties.method="first")

  } else if (method=="meanpricedcroprank") {
    # Def.: average potential yield gain in USD05 (yield gain * food price)

    ## Read in average potential yield gain per cell (USD05 per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=cellrankyear, proxycrop=proxycrop, monetary=TRUE, iniyear=iniyear, aggregate=FALSE)

    # calculate rank (ties are solved by first occurrence)
    glocellrank <- apply(-yield_gain, c(2,3), rank, ties.method="first")

  } else if (method=="watervalue") {
    # Def.: water value following D'Odorico et al. (2020) = yield gain / irrigation water requirements

    # Read in average water value per cell (USD05 per m^3)
    watvalue <- calcOutput("IrrigWatValue", selectyears=cellrankyear, climatetype=climatetype, iniyear=iniyear, proxycrop=proxycrop, aggregate=FALSE)

    # calculate rank (ties are solved by first occurence)
    glocellrank <- apply(-watvalue,c(2,3),rank,ties.method="first")

  } else if (method=="mostprofitable") {

    #### NOT FULLY FUNCTIONAL YET!!!!!!!!

    # Read in average potential yield gain per cell for all crops (USD05 per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=cellrankyear, proxycrop=NULL, monetary=TRUE, iniyear=iniyear, aggregate=FALSE)

    # Maximum monetary yield gain in the location (across all crops)
    yield_gain_max <- pmax(yield_gain[,,"tece"], yield_gain[,,"maiz"], yield_gain[,,"trce"], yield_gain[,,"soybean"],
                           yield_gain[,,"rapeseed"], yield_gain[,,"groundnut"], yield_gain[,,"rice_pro"],
                           yield_gain[,,"sunflower"], yield_gain[,,"puls_pro"], yield_gain[,,"potato"], yield_gain[,,"others"],
                           yield_gain[,,"cassav_sp"], yield_gain[,,"sugr_cane"], yield_gain[,,"sugr_beet"])
    getNames(yield_gain_max) <- NULL


    #####!!!!!????? QUESTION: How to ensure that most profitable is then also selected in FullIrrigationRequirements etc.

    # Calculate rank (ties are solved by first occurrence)
    glocellrank <- apply(-yield_gain_max, c(2,3), rank, ties.method="first")

  } else {
    stop("Please select a method for rank calculation")
  }

  glocellrank <- as.magpie(glocellrank, spatial=1)

  # Check for NAs
  if (any(is.na(glocellrank))) {
    stop("Function IrrigCellranking produced NAs")
  }

  return(list(
    x=glocellrank,
    weight=NULL,
    unit="1",
    description="Rank of cell according to yield gain potential by irrigation",
    isocountries=FALSE))
}
