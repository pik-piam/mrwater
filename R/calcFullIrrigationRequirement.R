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
#' @param irrigationsystem Irrigation system used: system share as in initialization year,
#'                         or drip, surface, sprinkler for full irrigation by selected system
#' @param landScen         Land availability scenario (currCropland, currIrrig, potCropland)
#'                         combination of land availability scenario and initialization year separated by ":".
#'                         Initialization year only relevant for curr scenarios.
#'                         protection scenario separated by "_" (only relevant when potCropland selected):
#'                         WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix          Cropmix for which irrigation yield improvement is calculated
#'                         can be selection of proxycrop(s) for calculation of average yield gain
#'                         or hist_irrig or hist_total for historical cropmix
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
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

calcFullIrrigationRequirement <- function(lpjml, climatetype, selectyears, comagyear,
                                          irrigationsystem, landScen, cropmix, multicropping) {

  # retrieve function arguments
  iniyear  <- as.numeric(as.list(strsplit(landScen, split = ":"))[[1]][2])

  # read in irrigation water requirements for each irrigation system
  # [in m^3 per hectare per year] (smoothed & harmonized)
  irrigWat <- calcOutput("IrrigWatRequirements", selectyears = selectyears,
                          lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
  # pasture is not irrigated in MAgPIE
  irrigWat <- irrigWat[, , "pasture", invert = TRUE]

  # land area that can potentially be used for irrigated agriculture given assumptions set in the arguments [in Mha]
  land <- calcOutput("AreaPotIrrig", selectyears = selectyears,
                     comagyear = comagyear, landScen = landScen, aggregate = FALSE)

  # share of corp area by crop type
  if (length(cropmix) == 1 && grepl("hist", cropmix)) {

    # read in relevant croparea: total (irrigated + rainfed) or irrigated depending on chosen cropmix
    croparea <- setYears(calcOutput("CropareaAdjusted", years = iniyear,
                                    sectoral = "kcr", physical = TRUE,
                                    cells = "lpjcell", cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE), NULL)

    if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "irrig") {

      # irrigated croparea
      croparea <- collapseNames(croparea[, , "irrigated"])

    } else if (as.list(strsplit(cropmix, split = "_"))[[1]][2] == "total") {

      # total croparea (irrigated + rainfed)
      croparea <- dimSums(croparea, dim = "irrigation")

    } else {
      stop("Please select hist_irrig or hist_total when
           selecting historical cropmix")
    }

    # historical share of crop types in cropland per cell
    cropareaShr <- croparea / dimSums(croparea, dim = 3)
    # correct NAs: where no current cropland available,
    # representative crops (maize, rapeseed, pulses) assumed as proxy
    proxyCrops  <- c("maiz", "rapeseed", "puls_pro")
    otherCrops  <- setdiff(getNames(croparea), proxyCrops)
    cropareaShr[, , proxyCrops][dimSums(croparea, dim = 3) == 0] <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
    cropareaShr[, , otherCrops][dimSums(croparea, dim = 3) == 0] <- 0

  } else {

    # equal crop area share for each proxycrop assumed
    cropareaShr              <- new.magpie(cells_and_regions = getCells(land),
                                           years = NULL,
                                           names = cropmix,
                                           sets = c("x.y.iso", "t", "data"))
    cropareaShr[, , cropmix] <- 1 / length(cropmix)
  }

  # Check
  if (any(round(dimSums(cropareaShr, dim = 3)) != 1)) {
    stop("Croparea share does not sum up to 1 in calcFullIrrigationRequirement")
  }

  # land area per crop
  land <- land * cropareaShr

  # water requirements for full irrigation in cell per crop accounting for cropshare (in mio. m^3)
  # Note on unit transformation:
  # land (mio ha -> ha): multiply with 1e6,
  # irrigation water requirements (m^3 per ha -> mio. m^3 per ha): divide by 1e6
  # --> cancels out -> water requirements for full irrigation (mio. m^3)
  irrigWat <- irrigWat[, , getNames(cropareaShr)] * land

  # add seasonality dimension
  irrigWat <- add_dimension(irrigWat, dim = 3.1, add = "season", nm = "single")
  irrigWat <- add_columns(irrigWat,   dim = 3.1, addnm = c("double", "triple"))

  # Increase of water requirements where multicropping takes place
  if (multicropping) {

    # water requirement reduction parameters (share of water requirement necessary in second / third season):
    watShr2 <- 0.9    #-#-# FIND LITERATURE VALUES FOR THIS!!!! #-#-#
    watShr3 <- 0.8    #-#-# FIND LITERATURE VALUES FOR THIS!!!! #-#-#

    # read in multiple cropping zones [3 layers: single, double, triple cropping]
    mc          <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
    mc          <- collapseNames(mc[, , "irrigated"])

    irrigWat[, , "double"] <- irrigWat[, , "single"] * watShr2
    irrigWat[, , "triple"] <- irrigWat[, , "single"] * watShr3

  } else {

    # when multicropping is deactivated: only first season is considered
    irrigWat[, , c("double", "triple")] <- 0

  }

  # sum over crops
  irrigWat <- dimSums(irrigWat, dim = "crop")

  # calculate irrigation water requirements per crop [in mio. m^3 per year] given irrigation system share in use
  if (irrigationsystem == "initialization") {

    # read in irrigation system area initialization [share of AEI by system] and expand to all years
    tmp               <- calcOutput("IrrigationSystem", source = "Jaegermeyr", aggregate = FALSE)
    irrigSystem       <- new.magpie(cells_and_regions = getCells(irrigWat),
                                    years = getYears(irrigWat),
                                    names = getNames(tmp),
                                    sets = c("x.y.iso", "year", "system"))
    irrigSystem[, , ] <- tmp

    # every crop irrigated by same share of initialization irrigation system
    irrigWat <- dimSums(irrigSystem * irrigWat, dim = "system")

  } else {

    # whole area irrigated by one system as selected in argument "irrigationsystem"
    irrigWat <- collapseNames(irrigWat[, , irrigationsystem])

  }

  # Adjust dimension names and ordering of cells
  getSets(irrigWat, fulldim = FALSE) <- c("x.y.iso", "year", "season.irrig_type")
  order    <- paste(c(rep("single", 2), rep("double", 2), rep("triple", 2)),
                    c("consumption", "withdrawal"),
                    sep = ".")
  irrigWat <- irrigWat[, , order]

  # Checks
  if (any(is.na(irrigWat))) {
    stop("produced NA full irrigation requirements")
  }

  return(list(x            = irrigWat,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "Full irrigation requirements per cell for selected cropmix
                              and irrigation system",
              isocountries = FALSE))
}
