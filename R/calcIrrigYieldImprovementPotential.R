#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential of irrigation for different crops
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears   years to be returned by the function
#' @param monetary      yield improvement potential in tDM (FALSE, default) or priced yield improvement potential in USD05 (TRUE)
#' @param iniyear       year to be used when monetary activated
#' @param proxycrop     proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigYieldImprovementPotential", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells getSets dimSums
#' @importFrom mrcommons toolGetMappingCoord2Country

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, monetary, iniyear, selectyears, proxycrop, yieldcalib) {

  # read in cellular lpjml yields [in tons/ha]
  yields     <- calcOutput("YieldsAdjusted", lpjml=lpjml, climatetype=climatetype, iniyear=iniyear, selectyears=selectyears, yieldcalib=yieldcalib, aggregate=FALSE)

  # yield gap (irrigated vs. rainfed) [in tons/ha]
  yield_gain <- collapseNames(yields[,,"irrigated"]) - collapseNames(yields[,,"rainfed"])
  # (Note: irrigation may lead to shift in growing period -> tmp can have negative values; also: under N-stress, irrigation may lead to lower yields, the latter is only relevant for limited-N-LPJmL version, default: unlimited N)

  # magpie crops
  croplist   <- getNames(yield_gain)

  if (monetary) {
    # Read in crop output price in initialization (USD05/tDM)
    p           <- calcOutput("IniFoodPrice", datasource="FAO", products="kcr", aggregate=FALSE, years=NULL, year=iniyear)
    croplist    <- intersect(croplist, getNames(p))

    # Calculate monetary yield gain (in USD05/ha)
    yield_gain  <- yield_gain[,,croplist] * p[,,croplist]
    unit        <- "USD05 per ha"
  } else {
    unit        <- "tons per ha"
  }

  # Selected crops
  if (!is.null(proxycrop)) {

    # share of corp area by crop type
    if (length(proxycrop)==1 && proxycrop=="historical") {

      # read in total (irrigated + rainfed) croparea
      croparea <- setYears(calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE), NULL)
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
      map                          <- toolGetMappingCoord2Country()
      getCells(croparea)           <- paste(map$coords, map$iso, sep=".")
      names(dimnames(croparea))[1] <- "x.y.iso"
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

      # total croparea (irrigated + rainfed)
      croparea     <- dimSums(croparea, dim="irrigation")
      # historical share of crop types in total cropland per cell
      croparea_shr <- croparea / dimSums(croparea, dim=3)
      # correct NAs: where no current cropland available -> representative crops (maize, rapeseed, pulses) assumed as proxy
      rep_crops    <- c("maiz", "rapeseed", "puls_pro")
      other_crops  <- setdiff(getNames(croparea), rep_crops)
      croparea_shr[,,rep_crops][dimSums(croparea, dim=3)==0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
      croparea_shr[,,other_crops][dimSums(croparea, dim=3)==0] <- 0

      # average yield gain over hisotrical crops weighted with their croparea share
      croplist    <- intersect(croplist, getNames(croparea_shr))
      yield_gain  <- dimSums(croparea_shr[,,croplist] * yield_gain[,,croplist], dim=3)

      description <- "Average yield improvement potential for crop types weighted with historical croparea share"

    } else {
      # equal crop area share for each proxycrop assumed
      # select proxy crops
      yield_gain  <- yield_gain[,,proxycrop]
      # average over proxy crops
      yield_gain  <- dimSums(yield_gain, dim=3) / length(getNames(yield_gain))
      description <- "Average yield improvement potential for selection of crop types"
    }

  } else {
    description <- "Yield improvement potential by irrigation for all different crop types"
  }

  # Check for NAs
  if (any(is.na(yield_gain))) {
    stop("Function YieldImprovementPotential produced NAs")
  }

  return(list(
    x=yield_gain,
    weight=NULL,
    unit=unit,
    description=description,
    isocountries=FALSE))
}
