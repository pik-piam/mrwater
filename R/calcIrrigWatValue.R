#' @title       calcIrrigWatValue
#' @description This function calculates the value of irrigation water (added value of water to the production process) based on yield gain, crop prices and irrigation water requirements
#'
#' @param selectyears years to be returned
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear     initialization year for price in price-weighted normalization of meanpricedcroprank and yield improvement potential prices
#' @param proxycrop        historical crop mix pattern ("historical") or list of proxycrop(s) or NULL for all crops
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames dimSums getYears getCells
#' @importFrom mrcommons toolGetMappingCoord2Country
#'
#' @examples
#' \dontrun{ calcOutput("IrrigWatValue", aggregate=FALSE) }

calcIrrigWatValue <- function(selectyears, climatetype, iniyear, proxycrop) {
  ## Note: Methodology for calculating value of water following D'Odorico et al. (2020)

  # Read in potential yield gain per cell (USD05 per ha)
  yield_gain <- calcOutput("IrrigYieldImprovementPotential", climatetype=climatetype, selectyears=selectyears, proxycrop=NULL, monetary=TRUE, iniyear=iniyear, aggregate=FALSE)

  # Set negative yield_gains to 0 (Negative yield gains (i.e. fewer yields through irrigation) would result in water value of 0)
  yield_gain[yield_gain<0] <- 0

  # Read in irrigation water requirement (withdrawals) (in m^3 per hectar per year) [smoothed and harmonized]
  # Note: Following D'Odorico et al. (2020), results refer to water withdrawals (because that's what one would pay for rather than for consumption)
  irrig_withdrawal <- calcOutput("IrrigWatRequirements", aggregate=FALSE, selectyears=iniyear, climatetype=climatetype)
  irrig_withdrawal <- collapseNames(irrig_withdrawal[,,"withdrawal"])
  irrig_withdrawal <- irrig_withdrawal[,,intersect(gsub("[.].*","",getNames(irrig_withdrawal)), getNames(yield_gain))]

  # Read in irrigation system area initialization
  irrigation_system <- calcOutput("IrrigationSystem", source="Jaegermeyr", aggregate=FALSE)

  # Calculate irrigation water requirements
  IWR        <- dimSums((irrigation_system[,,]*irrig_withdrawal[,,]), dim=3.1)
  # Note: Correction where IWR is small (close to 0)
  IWR[IWR<1] <- NA             ### CHOOSE APPROPRIATE THRESHOLD!

  # Calculate value of water
  watvalue <- yield_gain / IWR

  # Selected crops
  if (!is.null(proxycrop)) {

    # share of corp area by crop type
    if (proxycrop=="historical") {
      # historical crop mix
      # read in total (irrigated + rainfed) croparea
      croparea <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=FALSE, aggregate=FALSE)

      ### Note: Yield gain only given for 14 crops (begr, betr, cottn_pro, foddr, oilpalm missing)
      # Reduce crops in croparea_shr
      croparea <- croparea[,,getNames(watvalue)]

      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
      map                          <- toolGetMappingCoord2Country()
      getCells(croparea)           <- paste(map$coords, map$iso, sep=".")
      names(dimnames(croparea))[1] <- "x.y.iso"
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

      # historical share of crop types in total cropland per cell
      croparea_shr <- croparea / dimSums(croparea, dim=3)
      # correct NAs: where no land available -> crop share 0
      croparea_shr[dimSums(croparea, dim=3)==0] <- 0

      # average water value over histrical crops weighted with their croparea share
      watvalue <- dimSums(croparea_shr * watvalue, dim=3)

      description <- "Average yield improvement potential for crop types weighted with historical croparea share"

    } else {
      # equal crop area share for each proxycrop assumed
      watvalue    <- watvalue[,,proxycrop]
      # calculate average water value over proxy crops
      watvalue    <- dimSums(watvalue, dim=3) / length(getNames(watvalue))

      description <- "Value of irrigation water for selection of crop types"
    }

  } else {
    description <- "Value of irrigation water for all different crop types"
  }

  # Check for NAs
  if (any(is.na(watvalue))) {
    stop("Function IrrigWatValue produced NAs")
  }

  return(list(
    x=watvalue,
    weight=NULL,
    unit="USD05 per m^3",
    description=description,
    isocountries=FALSE))
}
