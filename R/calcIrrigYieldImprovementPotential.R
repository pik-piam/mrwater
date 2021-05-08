#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential of irrigation for different crops
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears   years to be returned by the function
#' @param monetary      yield improvement potential in tDM (FALSE, default) or priced yield improvement potential in USD05 (TRUE)
#' @param iniyear       year to be used when monetary activated
#' @param proxycrop     proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param FAOyieldcalib TRUE (LPJmL yields scaled with current FAO yield) or FALSE (LPJmL yield potentials)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigYieldImprovementPotential", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass collapseNames new.magpie getYears getNames collapseDim add_columns getCells getSets
#' @importFrom magpiesets addLocation
#' @importFrom mrcommons toolGetMappingCoord2Country

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, monetary, iniyear, selectyears, proxycrop, FAOyieldcalib) {

  # read in cellular lpjml yields [in tons/ha]
  yields     <- setYears(calcOutput("Yields", lpjml=lpjml, cells="lpjcell", climatetype=climatetype, years=selectyears, aggregate=FALSE), selectyears)
  # only crops (pasture is not irrigated)
  yields     <- yields[,,"pasture",invert=T]
  # set correct dimension names
  getSets(yields)[c("d3.1", "d3.2")] <- c("MAG", "irrigation")
  #*#*#*# @KRISTINE: Glaubst du, es kann potenziell ein Problem sein, dass yields MAG.irrigation ist und croparea irrigation.MAG oder sind die MAgPIE-Objekte dafür ausreichend fool-proof? #*#*#*ä
  #*#*#*# @KRISTINE: Ist das mit dem getSets() ok so, oder hätte ich da potenziell das Problem, dass sich die Funktion ändert und das mit d3.1 d3.2 nicht mehr zusammen passt bzw. sich die Anzahl der Dimenstionen in calcYields ändert o.Ä.?

  # yield gap (irrigated vs. rainfed) [in tons/ha]
  yield_gain <- collapseNames(yields[,,"irrigated"]) - collapseNames(yields[,,"rainfed"])
  # (Note: irrigation may lead to shift in growing period -> tmp can have negative values; also: under N-stress, irrigation may lead to lower yields, the latter is only relevant for limited-N-LPJmL version, default: unlimited N)

  # magpie crops
  croplist   <- getNames(yield_gain)

  if (FAOyieldcalib | !is.null(proxycrop) && length(proxycrop)==1 && proxycrop=="historical") {
    # read in total (irrigated + rainfed) croparea
    croparea <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="lpjcell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                          <- toolGetMappingCoord2Country()
    getCells(croparea)           <- paste(map$coords, map$iso, sep=".")
    names(dimnames(croparea))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  }

  if (FAOyieldcalib) {

    # LPJmL cellular production
    LPJmL_production <- croparea * yields
    # LPJmL total production (irrigated + rainfed)
    LPJmL_production <- dimSums(LPJmL_production, dim="irrigation")
    # LPJmL iso-country production
    ##### Aggregate to iso-countries
    ##### NOTE: ideally this should be handled by dimSums(x,dim=c("x","y")) as soon as this functionality is available #####
    out <- NULL
    for (i in unique(gsub(".*\\.", "", getCells(LPJmL_production)))) {
      tmp           <- dimSums(LPJmL_production[i,,], dim=1)
      getCells(tmp) <- i
      out <- mbind(out, tmp)
    }
    LPJmL_production <- out
    rm(out, tmp)
    ##### NOTE: ideally this should be handled by dimSums(x,dim=c("x","y")) as soon as this functionality is available #####
    LPJmL_production <- toolCountryFill(LPJmL_production)
    LPJmL_production[is.na(LPJmL_production)] <- 0
    # Note: Following countries not part of 67k cells: "ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB"

    # FAO iso-country production
    FAO_production   <- setYears(collapseNames(calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,,"production"][,iniyear,"dm"]), NULL)
    # list of commons crops
    common_crops     <- intersect(getNames(FAO_production), getNames(LPJmL_production))
    # adjust dimensions
    tmp              <- LPJmL_production[,,common_crops]
    tmp[,,]          <- 1
    FAO_production   <- FAO_production[,,common_crops] * tmp[,,common_crops]

    # Calibration Factor:
    Calib            <- FAO_production[,,common_crops] / LPJmL_production[,,common_crops]
    Calib[LPJmL_production[,,common_crops]==0] <- 0
    ##### CORRECT!!! Seems very high...

    # add crops that are missing in FAO
    missingcrops  <- setdiff(croplist, common_crops)
    Calib         <- add_columns(Calib, addnm=missingcrops, dim=3.1)
    Calib[,,missingcrops]     <- 0
    names(dimnames(Calib))[1] <- "iso"

    yield_gain    <- yield_gain[,,croplist] * Calib[intersect(unique(gsub(".*\\.", "", getCells(yield_gain))), getCells(Calib)),,croplist]
  }

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
