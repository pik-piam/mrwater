#' @title       calcYieldsPotential
#' @description This function returns irrigated and rainfed yield potential from LPJmL for magpie crops.
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical" for yields
#' @param selectyears   Years to be returned by the function
#' @param proxycalib    TRUE (smoothed yields with LPJmL proxy yield calibration to FAO proxycrops where LPJmL crop not available) or FALSE (only smoothed yields)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("YieldsPotential", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput toolAggregate toolGetMapping toolFillYears
#' @importFrom magclass collapseNames getNames dimSums add_columns mbind
#' @importFrom magpiesets findset

calcYieldsPotential <- function(lpjml, climatetype, selectyears, proxycalib) {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+12)
  on.exit(options(magclass_sizeLimit=sizelimit))

  stage        <- "smoothed"
  LPJ2MAG      <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")
  lpjml_crops  <- unique(LPJ2MAG$LPJmL)
  irrig_types  <- c("irrigated", "rainfed")
  yields       <- NULL

  for(crop in lpjml_crops){
    subdata <- as.vector(outer(crop, irrig_types, paste, sep="."))
    tmp     <- calcOutput("LPJmL_new", version=lpjml[["crop"]], climatetype=climatetype, subtype="harvest", subdata=subdata, stage=stage, years=selectyears, aggregate=FALSE)
    yields  <- mbind(yields, tmp)
  }

  # LPJmL to MAgPIE crops
  yields    <- toolAggregate(yields, LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel=TRUE)
  yields    <- setYears(yields, selectyears)

  # Check for NAs
  if (any(is.na(yields))) {
    stop("produced NA yields")
  }

  if (proxycalib) {
     FAOproduction     <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,,"production"][,,"dm"])
     MAGarea           <- calcOutput("Croparea", sectoral="kcr", physical=TRUE, aggregate=FALSE)

     MAGcroptypes  <- findset("kcr")
     missing       <- c("betr", "begr")
     MAGcroptypes  <- setdiff(MAGcroptypes, missing)
     FAOproduction <- add_columns(FAOproduction[,,MAGcroptypes], addnm = missing, dim = 3.1)
     FAOproduction[,,missing] <- 0

     FAOYields         <- dimSums(FAOproduction,dim=1)/dimSums(MAGarea, dim=1)

     matchingFAOyears <- intersect(getYears(yields), getYears(FAOYields))
     FAOYields        <- FAOYields[,matchingFAOyears,]
     Calib            <- new.magpie("GLO", matchingFAOyears, c(getNames(FAOYields), "pasture"), fill=1, sets=c("iso","year","data"))
     Calib[,matchingFAOyears,"oilpalm"]   <- FAOYields[,,"oilpalm"] / FAOYields[,,"groundnut"]      # LPJmL proxy for oil palm is groundnut
     Calib[,matchingFAOyears,"cottn_pro"] <- FAOYields[,,"cottn_pro"] / FAOYields[,,"groundnut"]    # LPJmL proxy for cotton is groundnut
     Calib[,matchingFAOyears,"foddr"]     <- FAOYields[,,"foddr"] / FAOYields[,,"maiz"]             # LPJmL proxy for fodder is maize
     Calib[,matchingFAOyears,"others"]    <- FAOYields[,,"others"] / FAOYields[,,"maiz"]            # LPJmL proxy for others is maize
     Calib[,matchingFAOyears,"potato"]    <- FAOYields[,,"potato"] / FAOYields[,,"sugr_beet"]       # LPJmL proxy for potato is sugar beet

     # interpolate between FAO years
     Calib <- toolFillYears(Calib, getYears(yields))

     # recalibrate yields for proxys
     yields <- yields * Calib[,,getNames(yields, dim=1)]
  }

  description <- "LPJmL yield potential (unharmonized) for all different (MAgPIE) crop types"
  unit        <- "tDM per ha"

  # Check for NAs
  if (any(is.na(yields))) {
    stop("Function calcYieldsPotential produced NAs")
  }

  return(list(
    x=yields,
    weight=NULL,
    unit=unit,
    description=description,
    isocountries=FALSE))
}
