#' @title       calcIrrigCellranking
#' @description This function calculates a cellranking for the river basin discharge allocation based on yield improvement potential through irrigation
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical" for yields
#' @param cellrankyear  year(s) for which cell rank is calculated
#' @param method        method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotential TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param iniyear       initialization year for price in price-weighted normalization of meanpricedcroprank and case of monetary=TRUE year of prices
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigCellranking", aggregate = FALSE)
#' }
#'
calcIrrigCellranking <- function(lpjml, climatetype, cellrankyear, method, cropmix, iniyear, yieldcalib, multicropping) {

  fullpotential <- as.logical(strsplit(method, ":")[[1]][2])
  method        <- strsplit(method, ":")[[1]][1]

  if (method == "meancellrank") {
    # Def. "meancellrank": ranking of cells or proxy crops, then: average over ranks

    ## Read in potential yield gain per cell (tons per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype, selectyears = cellrankyear, cropmix = NULL, monetary = FALSE, iniyear = NULL, yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)
    # select proxy crop(s)
    yield_gain <- yield_gain[,,cropmix]

    if (!fullpotential) {
      yield_gain_reduced <- 0.75 * yield_gain
      getCells(yield_gain) <- paste0("A_", getCells(yield_gain))
      getCells(yield_gain_reduced) <- paste0("B_", getCells(yield_gain_reduced))
      yield_gain <- mbind(yield_gain, yield_gain_reduced)
    }

    # cell ranking for crop (from highest yield gain (rank=1) to lowest yield gain (rank=1+x))
    cropcellrank <- apply(-yield_gain, c(2, 3), rank)

    # calculate mean over cropcellranks
    glocellrank  <- dimSums(cropcellrank, dim = 3) / length(getNames(cropcellrank))
    # ties are solved by first occurrence
    glocellrank <- apply(glocellrank, 2, rank, ties.method = "first")

  } else if (method == "meancroprank") {
    # Def. "meancroprank": average over yield gain of proxycrops, then: ranking of resulting average yield gain

    ## Read in potential yield gain per cell (tons per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype, selectyears = cellrankyear, yieldcalib = yieldcalib, cropmix = NULL, monetary = FALSE, iniyear = NULL, multicropping = multicropping, aggregate = FALSE)
    # select proxy crop(s)
    yield_gain <- yield_gain[,,cropmix]

    if (!fullpotential) {
      yield_gain_reduced <- 0.75 * yield_gain
      getCells(yield_gain) <- paste0("A_", getCells(yield_gain))
      getCells(yield_gain_reduced) <- paste0("B_", getCells(yield_gain_reduced))
      yield_gain <- mbind(yield_gain, yield_gain_reduced)
    }

    # normalize yield gains of proxy crops (unity-based normalization)
    min_yield  <- as.magpie(apply(yield_gain, c(2, 3), min))
    max_yield  <- as.magpie(apply(yield_gain, c(2, 3), max))
    yield_gain <- (yield_gain - min_yield) / (max_yield - min_yield)

    # calculate average yield gain over normalized proxy crops
    yield_gain <- dimSums(yield_gain, dim = 3) / length(getNames(yield_gain))

    # calculate rank (ties are solved by first occurrence)
    glocellrank <- apply(-yield_gain, c(2, 3), rank, ties.method = "first")

  } else if (method == "meanpricedcroprank") {
    # Def.: average potential yield gain in USD05 (yield gain * food price)

    ## Read in average potential yield gain per cell (USD05 per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype, selectyears = cellrankyear,
                             cropmix = cropmix, yieldcalib = yieldcalib, monetary = TRUE, iniyear = iniyear, multicropping = multicropping, aggregate = FALSE)

    if (!fullpotential) {
      yield_gain_reduced <- 0.75 * yield_gain
      getCells(yield_gain) <- paste0("A_", getCells(yield_gain))
      getCells(yield_gain_reduced) <- paste0("B_", getCells(yield_gain_reduced))
      yield_gain <- mbind(yield_gain, yield_gain_reduced)
    }

    # calculate rank (ties are solved by first occurrence)
    glocellrank <- apply(-yield_gain, c(2, 3), rank, ties.method = "first")

  } else if (method == "watervalue") {
    # Def.: water value following D'Odorico et al. (2020) = yield gain / irrigation water requirements

    # Read in average water value per cell (USD05 per m^3)
    watvalue <- calcOutput("IrrigWatValue", selectyears = cellrankyear, lpjml = lpjml, climatetype = climatetype, iniyear = iniyear,
                           cropmix = cropmix, yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)

    if (!fullpotential) {
      watvalue_reduced <- 0.75 * watvalue
      getCells(watvalue) <- paste0("A_", getCells(watvalue))
      getCells(watvalue_reduced) <- paste0("B_", getCells(watvalue_reduced))
      watvalue <- mbind(watvalue, watvalue_reduced)
    }

    # calculate rank (ties are solved by first occurrence)
    glocellrank <- apply(-watvalue, c(2, 3), rank, ties.method = "first")

  } else if (method == "mostprofitable") {

    #### NOT FULLY FUNCTIONAL YET!!!!!!!!

    # Read in average potential yield gain per cell for all crops (USD05 per ha)
    yield_gain <- calcOutput("IrrigYieldImprovementPotential", lpjml = lpjml, climatetype = climatetype, selectyears = cellrankyear, cropmix = NULL, monetary = TRUE, iniyear = iniyear, yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)

    # Maximum monetary yield gain in the location (across all crops)
    yield_gain_max <- setNames(pmax(yield_gain[,,"tece"], yield_gain[,,"maiz"], yield_gain[,,"trce"], yield_gain[,,"soybean"], yield_gain[,,"cottn_pro"],
      yield_gain[,,"rapeseed"], yield_gain[,,"groundnut"], yield_gain[,,"rice_pro"], yield_gain[,,"oilpalm"], yield_gain[,,"betr"],
      yield_gain[,,"sunflower"], yield_gain[,,"puls_pro"], yield_gain[,,"potato"], yield_gain[,,"others"], yield_gain[, , "begr"],
      yield_gain[,,"cassav_sp"], yield_gain[,,"sugr_cane"], yield_gain[,,"sugr_beet"], yield_gain[,,"foddr"]), NULL) #### INCLUDE foddr, begr, betr or not?

    if (!fullpotential) {
      yield_gain_reduced <- 0.75 * yield_gain_max
      getCells(yield_gain_max) <- paste0("A_", getCells(yield_gain_max))
      getCells(yield_gain_reduced) <- paste0("B_", getCells(yield_gain_reduced))
      yield_gain_max <- mbind(yield_gain_max, yield_gain_reduced)
    }

    ##### !!!!!????? QUESTION: How to ensure that most profitable is then also selected in FullIrrigationRequirements etc.

    # Calculate rank (ties are solved by first occurrence)
    glocellrank <- apply(-yield_gain_max, c(2, 3), rank, ties.method = "first")

  } else {
    stop("Please select a method for rank calculation")
  }

  glocellrank <- as.magpie(glocellrank, spatial = 1)

  # Check for NAs
  if (any(is.na(glocellrank))) {
    stop("Function IrrigCellranking produced NAs")
  }

  return(list(
    x = glocellrank,
    weight = NULL,
    unit = "1",
    description = "Rank of cell according to yield gain potential by irrigation",
    isocountries = FALSE))
}
