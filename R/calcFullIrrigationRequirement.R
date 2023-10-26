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
#' @param iniyear          Croparea initialization year
#' @param irrigationsystem Irrigation system used: system share as in initialization year,
#'                         or drip, surface, sprinkler for full irrigation by selected system
#' @param landScen         Land availability scenario consisting of two parts separated by ":":
#'                         1. available land scenario (currCropland, currIrrig, potCropland)
#'                         2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                         For case of no land protection select "NA"
#'                         or do not specify second part of the argument
#' @param cropmix          Selected cropmix (options:
#'                         "hist_irrig" for historical cropmix on currently irrigated area,
#'                         "hist_total" for historical cropmix on total cropland,
#'                         or selection of proxycrops)
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE) and
#'                         Multiple Cropping Suitability mask selected
#'                         (mask can be:
#'                         "none": no mask applied (only for development purposes)
#'                         "actual:total": currently multicropped areas calculated from total harvested areas
#'                                         and total physical areas per cell from readLanduseToolbox
#'                         "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                         "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                         "potential:endogenous": potentially multicropped areas given
#'                                                 temperature and productivity limits
#'                         "potential:exogenous": potentially multicropped areas given
#'                                                GAEZ suitability classification)
#'                         (e.g. TRUE:actual:total; TRUE:none; FALSE)
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
#' @importFrom magclass collapseNames getItems new.magpie dimSums
#' @importFrom mrcommons toolCell2isoCell toolGetMappingCoord2Country

calcFullIrrigationRequirement <- function(lpjml, climatetype,
                                          selectyears, iniyear, comagyear,
                                          irrigationsystem, landScen, cropmix,
                                          multicropping) {

  # cropland area per crop (in Mha)
  croparea <- calcOutput("CropAreaPotIrrig",
                         selectyears = selectyears, iniyear = iniyear,
                         comagyear = comagyear,
                         cropmix = cropmix, landScen = landScen,
                         aggregate = FALSE)

  croplist <- getItems(croparea, dim = "crop")

  # read in irrigation water requirements for given irrigation system
  # per crop (in m^3 per hectare per year)
  irrigWat <- calcOutput("ActualIrrigWatRequirements",
                         selectyears = selectyears, iniyear = iniyear,
                         lpjml = lpjml, climatetype = climatetype,
                         irrigationsystem = irrigationsystem, multicropping = multicropping,
                         aggregate = FALSE)[, , croplist]

  # # correct irrigation water requirements where irrigation would lead to 0 yield gains
  # tmp <- calcOutput("IrrigCropYieldGain", priceAgg = "GLO",
  #                   lpjml = lpjml, climatetype = climatetype,
  #                   iniyear = iniyear, selectyears = selectyears,
  #                   yieldcalib = yieldcalib,
  #                   multicropping = as.logical(stringr::str_split(multicropping, ":")[[1]][1]),
  #                   aggregate = FALSE)[, , croplist]
  # tmp[tmp > 0] <- 1
  # tmp[tmp < 0] <- 0
  #
  # irrigWat <- irrigWat * tmp
  ### JENS: Should this be included or not? Would need to be addressed in calcIrrigAreaPotential
  ###       accordingly. It has an effect on calcRevenue. MAgPIE only gets aggregated across crops.

  # water requirements for full irrigation in cell accounting for cropshare (in mio. m^3)
  # Note on unit transformation:
  # croparea (mio ha -> ha): multiply with 1e6,
  # irrigation water requirements (m^3 per ha -> mio. m^3 per ha): divide by 1e6
  # --> cancels out -> water requirements for full irrigation (mio. m^3)
  irrigWat <- dimSums(irrigWat * croparea, dim = "crop")

  # Checks
  if (any(is.na(irrigWat))) {
    stop("mrwater::calcFullIrrigationRequirements:
         produced NA full irrigation requirements")
  }
  if (any(round(irrigWat, digits = 6) < 0)) {
    stop("mrwater::calcFullIrrigationRequirements:
         produced negative full irrigation requirements")
  }

  return(list(x            = irrigWat,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("Full irrigation requirements ",
                                    "per cell for selected cropmix ",
                                    "and irrigation system"),
              isocountries = FALSE))
}
