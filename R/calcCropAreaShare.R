#' @title       calcCropAreaShare
#' @description This function calculates the crop area share for a chosen cropmix
#'
#' @param iniyear Croparea initialization year
#' @param cropmix Cropmix for which croparea share is calculated
#'                (options:
#'                "hist_irrig" for historical cropmix on currently irrigated area,
#'                "hist_total" for historical cropmix on total cropland,
#'                or selection of proxycrops)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Benjamin L. Bodirsky
#'
#' @examples
#' \dontrun{
#' calcOutput("CropAreaShare", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass collapseNames getCells getSets getYears getNames new.magpie dimSums
#' @importFrom mrcommons toolCell2isoCell toolGetMappingCoord2Country

calcCropAreaShare <- function(iniyear, cropmix) {

  # read in relevant physical croparea: total (irrigated + rainfed) or
  # irrigated depending on chosen cropmix
  croparea <- calcOutput("CropareaAdjusted", iniyear = iniyear,
                                       aggregate = FALSE)

  # share of crop area by crop type
  if (length(cropmix) == 1 && grepl("hist", cropmix)) {

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
    cropareaShr <- croparea / dimSums(croparea, dim = "crop")

    # correct NAs: where no current cropland available,
    # representative crops (maize, rapeseed, pulses) assumed as proxy
    proxyCrops  <- c("maiz", "rapeseed", "puls_pro")
    otherCrops  <- setdiff(getNames(croparea), proxyCrops)
    cropareaShr[, , proxyCrops][dimSums(croparea, dim = 3) == 0] <- 1 / length(c("maiz", "rapeseed", "puls_pro"))
    cropareaShr[, , otherCrops][dimSums(croparea, dim = 3) == 0] <- 0

  } else {

    # equal crop area share for each proxycrop assumed
    cropareaShr              <- new.magpie(cells_and_regions = getCells(croparea),
                                           years = NULL,
                                           names = getNames(collapseNames(croparea[, , "irrigated"])),
                                           sets = c("x.y.iso", "t", "crop"),
                                           fill = 0)
    cropareaShr[, , cropmix] <- 1 / length(cropmix)
  }

  # Checks
  if (any(round(dimSums(cropareaShr, dim = 3)) != 1)) {
    stop("Croparea share does not sum up to 1.
         Please check: calcCropAreaShare!")
  }

  if (any(is.na(cropareaShr))) {
    stop("produced NA crop area share.
         Please check: calcCropAreaShare!")
  }

  return(list(x            = cropareaShr,
              weight       = NULL,
              unit         = "share",
              description  = "Share of crop per cell for selected cropmix",
              isocountries = FALSE))
}
