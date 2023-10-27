#' @title       calcYieldsValued
#' @description This function calculates yields per crop
#'              valued at FAO prices (in USD per hectare)
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned by the function
#' @param priceAgg      Price aggregation:
#'                      "GLO" for global average prices, or
#'                      "ISO" for country-level prices, or
#'                      "CONST" for same price for all crops
#' @param iniyear       initialization year for food price and cropmix area
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                      If FALSE: uncalibrated LPJmL yields are used
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from LandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldsValued", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells dimSums time_interpolate

calcYieldsValued <- function(lpjml, climatetype, priceAgg,
                             iniyear, selectyears,
                             yieldcalib, multicropping) {

  # prices are reported in current US$MER
  priceUnit <- "current US$MER"

  # read in cellular lpjml yields [in tDM/ha]
  yields    <- calcOutput("YieldsAdjusted", lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib,
                          multicropping = multicropping, aggregate = FALSE)
  # extract magpie crops
  croplist  <- getNames(collapseNames(yields[, , "irrigated"]))

  # historical FAO producer prices
  pGLO <- collapseDim(calcOutput("PriceAgriculture", unit = priceUnit,
                                 datasource = "FAO", aggregate = "GLO"))

  if (priceAgg == "GLO") {
    # global average agricultural prices
    p <- pGLO

  } else if (priceAgg == "ISO") {

    # country-level agricultural prices
    p <- collapseNames(calcOutput("PriceAgriculture", unit = priceUnit,
                                  datasource = "FAO", aggregate = FALSE))

    # fill with region averages where possible
    pricesRegional <- collapseDim(calcOutput("PriceAgriculture", unit = priceUnit,
                                             datasource = "FAO",
                                             aggregate = TRUE, regionmapping = "regionmappingH12.csv"))
    pricesRegional <- toolAggregate(pricesRegional, rel = toolGetMapping("regionmappingH12.csv",
                                                                         where = "mappingfolder"),
                                    from = "RegionCode", to = "CountryCode")
    p[p == 0] <- pricesRegional[p == 0]

    # fill countries that have zeros over whole time period with GLO values
    for (i in getItems(p, dim = 1)) {
      p[i, , where(dimSums(p[i, , ],
                           dim = 2) == 0)$true$data] <- pGLO[, , where(dimSums(p[i, , ],
                                                                               dim = 2) == 0)$true$data]
    }

    # fill missing time gaps
    p[p == 0] <- NA

    for (c in getItems(p, dim = 3)) {
      for (i in getItems(p, dim = 1)) {
        if (length(where(is.na(p[i, , c]))$true$years) > 0) {

          tmp <- p[i, , c]
          tmp <- tmp[, where(is.na(p[i, , c]))$true$years, invert = TRUE]

          p[i, , c] <- time_interpolate(tmp, where(is.na(p[i, , c]))$true$years,
                                        integrate_interpolated_years = TRUE,
                                        extrapolation_type = "constant")
        }
      }
    }

    p <- p[getItems(yields, dim = "iso"), , ]

  } else if (priceAgg == "CONST") {

    p       <- pGLO
    p[, , ] <- 571.5455 # 691.9 (median price) # 571.5455 (mean yield value gain) # 117.93 (mean yield value gain)

  } else {
    stop("Problem in calcYieldsValued:
       Please select price aggregation level to GLO or ISO or CONST via
       the unit argument.")
  }

  # missing crop types get proxy values of 2005 and average price development over time
  proxyP      <- calcOutput("IniFoodPrice", datasource = "FAO", products = "kcr",
                            years = NULL, year = "y2005", aggregate = FALSE)
  pNormalized <- pGLO / setYears(pGLO[, 2005, ], NULL)
  weight      <- dimSums(collapseDim(calcOutput("Production", products = "kcr", attributes = "dm",
                                                aggregate = FALSE))[, getYears(pNormalized),
                                                                    intersect(croplist, getNames(p))],
                         dim = 1)
  averagePriceDevelopment <- dimSums(pNormalized[, , intersect(croplist, getNames(p))] *
                                       (weight / dimSums(weight, dim = "ItemCodeItem")),
                                     dim = "ItemCodeItem")
  proxyP <- proxyP * averagePriceDevelopment

  missingCrops <- setdiff(croplist, getNames(p))
  p <- add_columns(p, addnm = missingCrops, dim = 3, fill = NA)[, , croplist]

  p[, , missingCrops] <- proxyP[, , missingCrops]

  # average price around initialization year to balance price fluctuations
  if (is.character(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }
  averageYears <- seq(iniyear - 2, iniyear + 2, 1)

  p <- setYears(dimSums(p[, averageYears, ], dim = "Year") / length(averageYears),
                NULL)

  # Yields valued at USD per hectare
  yields <- yields * p

  # Check for NAs
  if (any(is.na(yields))) {
    stop("Function calcYieldsValued produced NAs")
  }

  # Check for negatives
  if (any(round(yields, digits = 3) < 0)) {
    stop("Function calcYieldsValued produced negative values")
  }

  return(list(x            = yields,
              weight       = NULL,
              unit         = "USD per hectare",
              description  = "Yields for all different crop types",
              isocountries = FALSE))
}
