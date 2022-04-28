#' @title       readJaegermeyr2015
#' @description Read in irrigation system suitability per crop type 
#'              and country-level irrigation system shares from Jaegermeyr (2015)
#' 
#' @param subtype Data to be read in:
#'                "systemShare": irrigation system share 
#'                               as provided in SI of Jägermeyr et al. (2015), 
#'                               based on FAO 2014, ICID 2012 and Rohwer et al. 2007); 
#'                "systemSuitability": biophysical and technical irrigation system suitability 
#'                                     by crop type (CFT) as provided in Table 2 of Jägermeyr et al. (2015)
#'                                     based on Sauer et al. (2010) and Fischer et al. (2012).
#'
#' @return MAgPIE object
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("Jaegerymeyr2015", convert = FALSE)
#' }
#'
#' @importFrom magclass getSets as.magpie getNames
#' @importFrom madrat toolCountryFill
#' @importFrom utils read.csv

readJaegermeyr2015 <- function(subtype) {

  if (subtype == "systemShare") {

    # Irrigation system share by country
    x <- read.csv("Jaegermeyr-2015-supplement_shr.csv")
    x$Country <- toolCountry2isocode(x$Country, 
                                     mapping = c("bahamas, the" = "BHS",
                                                "congo-brazzaville" = "COG",
                                                "dr congo, former zaire" = "COD",
                                                "gambia, the" = "GMB",
                                                "myanmar (burma)" = "MMR",
                                                "west bank" = "PSE"))
    # Transform to MAgPIE object
    x <- as.magpie(x)
    x <- suppressWarnings(toolCountryFill(x))

    # TWN and HKG get values of China
    x["TWN", , ] <- x["CHN", , ]
    x["HKG", , ] <- x["CHN", , ]

    # Where system share is not given, it is assumed that 100% of irrigated land is surface irrigation
    x[, , "shr_AEI_surface"][is.na(x[, , "shr_AEI_surface"])] <- 1
    x[, , "shr_AEI_sprinkler"][is.na(x[, , "shr_AEI_sprinkler"])] <- 0
    x[, , "shr_AEI_drip"][is.na(x[, , "shr_AEI_drip"])] <- 0

    # When all three shares are 0, it is assumed that 100% of irrigated land (if any exists) is surface irrigation
    x[, , "shr_AEI_surface"][which(x[, , "shr_AEI_surface"] == 0 & x[, , "shr_AEI_sprinkler"] == 0 & x[, , "shr_AEI_drip"] == 0)] <- 1

    # Dimension and set names
    getNames(x) <- gsub("shr_AEI_", "", getNames(x))
    getSets(x)  <- c("iso", "year", "system")

  } else if (subtype == "systemSuitability") {

    # Irrigation system suitability by LPJmL crop type
    x <- read.csv("Jaegermeyr2015_Table2.csv")
    x <- as.magpie(x)
    getSets(x) <- c("region", "year", "crop", "system")

  }

  return(x)
}
