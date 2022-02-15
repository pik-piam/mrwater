#' @title       readIrrigationSystem
#' @description Read in irrigation system type for initialization
#'
#' @param subtype Data source to be used:
#'                Jaegermeyr (irrigation system share based on FAO 2014, ICID 2012 and Rohwer et al. 2007) or
#'                LPJmL (dominant irrigation system per country)
#'
#' @return MAgPIE object of at country-level
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("IrrigationSystem", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolCountry2isocode toolCountryFill toolConditionalReplace
#' @importFrom magclass getCells
#' @importFrom mrcommons toolGetMappingCoord2Country

readIrrigationSystem <- function(subtype = "Jaegermeyr") {

  # Mapping
  map <- toolGetMappingCoord2Country()

  # Irrigation system share by country
  if (subtype == "Jaegermeyr") {

    # Read in and transform data
    x         <- read.csv("Jaegermeyr-2015-supplement_shr.csv")
    x$Country <- toolCountry2isocode(x$Country, mapping = c("bahamas, the" = "BHS",
                                                            "congo-brazzaville" = "COG",
                                                            "dr congo, former zaire" = "COD",
                                                            "gambia, the" = "GMB",
                                                            "myanmar (burma)" = "MMR",
                                                            "west bank" = "PSE"))
    # Transform to MAgPIE object
    x <- as.magpie(x)
    x <- toolCountryFill(x)

    # TWN and HKG get values of China
    x["TWN", , ] <- x["CHN", , ]
    x["HKG", , ] <- x["CHN", , ]

    # Where system share is not given, it is assumed that 100% of irrigated land is surface irrigation
    x[, , "shr_AEI_surface"][is.na(x[, , "shr_AEI_surface"])]     <- 1
    x[, , "shr_AEI_sprinkler"][is.na(x[, , "shr_AEI_sprinkler"])] <- 0
    x[, , "shr_AEI_drip"][is.na(x[, , "shr_AEI_drip"])]           <- 0

    # Expand to cellular level
    x <- x[map$iso, , ]

  } else if (subtype == "LPJmL") {

    # Read in and transform data
    x         <- read.csv("LPJmL_manage_irrig_IFT.csv")
    x         <- subset(x, x$country != "No_Land")
    x$country <- toolCountry2isocode(x$country, mapping = c("bahamas, the" = "BHS",
                                                            "congo-brazzaville" = "COG",
                                                            "dr congo, former zaire" = "COD",
                                                            "falkland islands (islas malvinas)" = "FLK",
                                                            "western samoa" = "WSM",
                                                            "french southern and antarctica lands" = "ATF",
                                                            "gambia, the" = "GMB",
                                                            "myanmar (burma)" = "MMR",
                                                            "west bank" = "PSE",
                                                            "cocos keeling islands" = "CCK",
                                                            "pitcairn islands" = "PCN",
                                                            "saint helena ascension and tristan da cunha" = "SHN",
                                                            "st. vincent and the grenadines" = "VCT",
                                                            "virgin islands" = "VIR"))
    x         <- subset(x, !duplicated(x))

    # Transform to MAgPIE object
    x <- as.magpie(x)
    x <- toolCountryFill(x)

    # Expand to cellular level
    x <- x[map$iso, , ]

    # Replace NAs with 1 (surface irrigation)
    x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 1)

  }

  # Object dimensions
  getCells(x)           <- paste(map$coords, map$iso, sep = ".")
  names(dimnames(x))[1] <- "x.y.iso"

  return(x)
}
