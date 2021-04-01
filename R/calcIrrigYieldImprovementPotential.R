#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential of irrigation for different crops
#'
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears years to be returned by the function
#' @param monetary        yield improvement potential in tDM (FALSE, default) or priced yield improvement potential in USD05 (TRUE)
#' @param iniyear         year to be used when monetary activated
#' @param proxycrop   proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigYieldImprovementPotential", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass collapseNames new.magpie getYears getNames collapseDim
#' @importFrom magpiesets addLocation
#' @importFrom mrcommons toolGetMappingCoord2Country

calcIrrigYieldImprovementPotential <- function(climatetype, monetary, iniyear, selectyears, proxycrop) {

  # read in yields [in tons/ha]
  yields     <- calcOutput("Yields", lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"), cells="lpjcell", climatetype=climatetype, years=selectyears, aggregate=FALSE)

  # yield gap (irrigated vs. rainfed) [in tons/ha]
  yield_gain <- collapseNames(yields[,,"irrigated"]) - collapseNames(yields[,,"rainfed"])
  # (Note: irrigation may lead to shift in growing period -> tmp can have negative values; also: under N-stress, irrigation may lead to lower yields, the latter is only relevant for limited-N-LPJmL version, default: unlimited N)

  if (monetary) {
    # Read in crop output price in initialization (USD05/tDM)
    p <- calcOutput("IniFoodPrice", datasource="FAO", products="kcr", aggregate=FALSE, years=NULL, year=iniyear)

    # Calculate monetary yield gain (in USD05/ha)
    yield_gain  <- yield_gain[,,intersect(getNames(yield_gain), getNames(p))] * p[,,intersect(getNames(yield_gain), getNames(p))]
    unit        <- "USD05 per ha"
  } else {
    unit        <- "tons per ha"
  }

  # Selected crops
  if (!is.null(proxycrop)) {

    # share of corp area by crop type
    if (length(proxycrop)==1 && proxycrop=="historical") {
      # historical crop mix
      # read in total (irrigated + rainfed) croparea
      croparea <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=FALSE, aggregate=FALSE)

      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
      map                          <- toolGetMappingCoord2Country()
      getCells(croparea)           <- paste(map$coords, map$iso, sep=".")
      names(dimnames(croparea))[1] <- "x.y.iso"
      #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

      # historical share of crop types in total cropland per cell
      croparea_shr <- croparea / dimSums(croparea, dim=3)
      # correct NAs: where no current cropland available -> representative crops (maize, rapeseed, pulses) assumed as proxy
      rep_crops   <- c("maiz", "rapeseed", "puls_pro")
      other_crops <- setdiff(getNames(croparea), rep_crops)
      croparea_shr[,,rep_crops][dimSums(croparea, dim=3)==0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
      croparea_shr[,,other_crops][dimSums(croparea, dim=3)==0] <- 0

      # average yield gain over histrical crops weighted with their croparea share
      yield_gain  <- dimSums(croparea_shr * yield_gain, dim=3)

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
