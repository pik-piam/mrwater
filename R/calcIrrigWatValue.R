#' @title       calcIrrigWatValue
#' @description This function calculates the value of irrigation water (added value of water to the production process) based on yield gain, crop prices and irrigation water requirements
#'
#' @param selectyears years to be returned
#' @param version     switch between LPJmL version for yields and irrigation water requirements
#' @param climatetype switch between different climate scenarios for yields and irrigation water requirements
#' @param time            time smoothing: average, spline (default) or raw
#' @param averaging_range just specify for time=="average": number of time steps to average
#' @param dof             just specify for time=="spline": degrees of freedom
#' @param harmonize_baseline FALSE (default) no harmonization, harmonization: if a baseline is specified here data is harmonized to that baseline (from ref_year onwards)
#' @param ref_year           just specify for harmonize_baseline != FALSE : Reference year
#' @param cells       switch between "lpjcell" (67420) and "magpiecell" (59199)
#' @param iniyear     initialization year for price in price-weighted normalization of meanpricedcroprank
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigWatValue", aggregate=FALSE) }

calcIrrigWatValue <- function(selectyears=c(1995,2100,5), version="LPJmL5", climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year="y2015",
                              cells="lpjcell", iniyear=1995) {
  ## Note: Methodology for calculating value of water following D'Odorico et al. (2020)

  # Read in potential yield gain per cell (USD05 per ha)
  yield_gain <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears,
                           harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, averaging_range=averaging_range, dof=dof,
                           cells=cells, proxycrop=NULL, monetary=TRUE, aggregate=FALSE)

  # Set negative yield_gains to 0 (Negative yield gains (i.e. fewer yields through irrigation) would result in water value of 0)
  yield_gain[yield_gain<0] <- 0

  # Read in irrigation water requirement (withdrawals) (in m^3 per hectar per year) [smoothed and harmonized]
  # Note: Following D'Odorico et al. (2020), results refer to water withdrawals (because that's what one would pay for rather than for consumption)
  irrig_withdrawal <- calcOutput("IrrigWatRequirements", aggregate=FALSE, version=version, selectyears=iniyear, climatetype=climatetype, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof)
  irrig_withdrawal <- collapseNames(irrig_withdrawal[,,"withdrawal"])
  irrig_withdrawal <- irrig_withdrawal[,,intersect(gsub("[.].*","",getNames(irrig_withdrawal)), getNames(yield_gain))]

  # Read in irrigation system area initialization
  irrigation_system <- calcOutput("IrrigationSystem", source="Jaegermeyr_lpjcell", aggregate=FALSE)

  # Calculate irrigation water requirements
  IWR        <- dimSums((irrigation_system[,,]*irrig_withdrawal[,,]), dim=3.1)
  # Note: Correction where IWR is small (close to 0)
  IWR[IWR<1] <- NA             ### CHOOSE APPROPRIATE THRESHOLD!

  # Calculate value of water
  watvalue <- new.magpie(getCells(yield_gain), getYears(yield_gain), getNames(yield_gain), fill=0)
  watvalue <- yield_gain / IWR

  # Check for NAs
  if (any(is.na(watvalue))) {
    stop("Function YieldImprovementPotential produced NAs")
  }

  return(list(
    x=watvalue,
    weight=NULL,
    unit="USD05 per m^3",
    description="Value of irrigation water",
    isocountries=FALSE))
}
