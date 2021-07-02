#' @title       calcFullIrrigationRequirement
#' @description This function calculates the water requirements for full
#'              irrigation per cell per crop given potentially available land
#'
#' @param lpjml            LPJmL version used
#' @param selectyears      Years to be returned
#' @param climatetype      Climate model or historical baseline "GSWP3-W5E5:historical"
#' @param comagyear        if !NULL: already irrigated area is subtracted;
#'                         if NULL: total potential land area is used;
#'                         year specified here is the year of the initialization
#'                         used for cropland area initialization in calcIrrigatedArea
#' @param irrigationsystem irrigation system used: system share as in initialization year,
#'                         or drip, surface, sprinkler for full irrigation by selected system
#' @param avlland_scen     Land availability scenario (currCropland, currIrrig, potIrrig)
#'                         combination of land availability scenario and initialization year separated by ":".
#'                         protection scenario separated by "_" (only relevant when potIrrig selected):
#'                         WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix          cropmix for which irrigation yield improvement is calculated
#'                         can be selection of proxycrop(s) for calculation of average yield gain
#'                         or hist_irrig or hist_total for historical cropmix
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("FullIrrigationRequirement", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass collapseNames getCells getSets getYears getNames new.magpie dimSums
#' @importFrom mrcommons toolCell2isoCell toolGetMappingCoord2Country

calcFullIrrigationRequirement <- function(lpjml, climatetype, selectyears, comagyear, irrigationsystem, avlland_scen, cropmix) {

  # retrieve function arguments
  iniyear <- as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])

  # read in irrigation water requirements for each irrigation system [in m^3 per hectare per year] (smoothed & harmonized)
  irrig_wat <- calcOutput("IrrigWatRequirements", selectyears = selectyears,
                          lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
  # pasture is not irrigated in MAgPIE
  irrig_wat <- irrig_wat[, , "pasture", invert = T]

  # land area that can potentially be used for irrigated agriculture given assumptions set in the arguments [in Mha]
  land <- calcOutput("AreaPotIrrig", selectyears = selectyears,
                     comagyear = comagyear, avlland_scen = avlland_scen, aggregate = FALSE)

  # share of corp area by crop type
  if (length(cropmix) == 1 && grepl("hist", cropmix)) {

    # read in relevant croparea: total (irrigated + rainfed) or irrigated depending on chosen cropmix
    croparea <- setYears(calcOutput("Croparea", years = iniyear, sectoral = "kcr",
                                    cells = "lpjcell", physical = TRUE, cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE), NULL)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                          <- toolGetMappingCoord2Country()
    getCells(croparea)           <- paste(map$coords, map$iso, sep = ".")
    names(dimnames(croparea))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

    if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "irrig") {
      # irrigated croparea
      croparea <- collapseNames(croparea[, , "irrigated"])
    } else if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "total") {
      # total croparea (irrigated + rainfed)
      croparea <- dimSums(croparea, dim = "irrigation")
    } else {
      stop("Please select hist_irrig or hist_total when selecting the historical cropmix")
    }

    # historical share of crop types in cropland per cell
    croparea_shr <- croparea / dimSums(croparea, dim = 3)
    # correct NAs: where no current cropland available -> representative crops (maize, rapeseed, pulses) assumed as proxy
    rep_crops    <- c("maiz", "rapeseed", "puls_pro")
    other_crops  <- setdiff(getNames(croparea), rep_crops)
    croparea_shr[, , rep_crops][dimSums(croparea, dim = 3) == 0]   <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
    croparea_shr[, , other_crops][dimSums(croparea, dim = 3) == 0] <- 0

  } else {
    # equal crop area share for each proxycrop assumed
    croparea_shr            <- new.magpie(cells_and_regions = getCells(land),
                                          years = NULL,
                                          names = cropmix, sets = c("x.y.iso", "t", "data"))
    croparea_shr[, , cropmix] <- 1 / length(cropmix)
  }

  # Check
  if (any(round(dimSums(croparea_shr, dim = 3)) != 1)) {
    stop("Croparea share does not sum up to 1 in calcFullIrrigationRequirement")
  }

  # land area per crop
  land <- land * croparea_shr

  # water requirements for full irrigation in cell per crop accounting for cropshare (in mio. m^3)
  # Note on unit transformation:
  # land (mio ha -> ha): multiply with 1e6,
  # irrigation water requirements (m^3 per ha -> mio. m^3 per ha): divide by 1e6
  # --> cancels out -> water requirements for full irrigation (mio. m^3)
  irrig_wat <- irrig_wat[, , getNames(croparea_shr)] * land

  # sum over crops
  irrig_wat <- dimSums(irrig_wat, dim = "crop")

  # calculate irrigation water requirements per crop [in mio. m^3 per year] given irrigation system share in use
  if (irrigationsystem == "initialization") {

    # read in irrigation system area initialization [share of AEI by system] and expand to all years
    tmp                   <- calcOutput("IrrigationSystem", source = "Jaegermeyr", aggregate = FALSE)
    irrigation_system     <- new.magpie(cells_and_regions = getCells(irrig_wat),
                                        years = getYears(irrig_wat),
                                        names = getNames(tmp),
                                        sets = c("x.y.iso", "year", "system"))
    irrigation_system[, , ] <- tmp

    # every crop irrigated by same share of initialization irrigation system
    irrig_wat <- dimSums(irrigation_system * irrig_wat, dim = "system")

  } else {

    # whole area irrigated by one system as selected in argument "irrigationsystem"
    irrig_wat <- collapseNames(irrig_wat[, , irrigationsystem])

  }

  # Adjust dimension names
  getSets(irrig_wat, fulldim = FALSE)[1] <- "x.y.iso"

  # Checks
  if (any(is.na(irrig_wat))) {
    stop("produced NA full irrigation requirements")
  }

  return(list(x            = irrig_wat,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Full irrigation requirements per cell for selected cropmix
                              and irrigation system",
              isocountries = FALSE))
}
