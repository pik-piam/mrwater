#' @title        toolRegionSums
#' @description  sum over regional aggregation chosen in region argument
#'
#' @param x      magpie object to be summed up over first dimension
#' @param region regional resolution (can be country iso-code, "GLO" for global,
#'               or region name and respective mapping "EUR:H12")
#'
#' @return       magpie object with regional sum
#' @author       Felicitas Beier
#'
#' @importFrom madrat toolGetMapping toolAggregate
#' @importFrom magclass dimSums
#' @importFrom stringr str_split
#'
#' @export

toolRegionSums <- function(x, region) {

  if (region == "GLO") {

    out <- dimSums(x, dim = 1)

  } else {

    map    <- str_split(region, ":")[[1]][2]
    region <- str_split(region, ":")[[1]][1]

    # aggregate to iso-countries
    x <- dimSums(x, dim = c(1.1, 1.2))
    x <- toolCountryFill(x, fill = 0) # Note: "ABW" "AND" "ATA" "BES" "BLM" "BVT"
                                      # "GIB" "LIE" "MAC" "MAF" "MCO" "SMR" "SXM"
                                      # "VAT" "VGB" missing in LPJmL cells
    # aggregate to regions
    if (!is.na(map) && map == "H12") {
      regmap        <- toolGetMapping("regionmappingH12.csv")
      names(regmap) <- c("Country", "iso", "reg")
      x             <- toolAggregate(x, rel = regmap, from = "iso", to = "reg", dim = 1)
    } else if (!is.na(map) && map != "H12") {
      stop("Selected regionmapping is not yet available. Please select region
           and respective mapping via region argument: e.g. EUR:H12")
    }

    out <- dimSums(x[region, , ], dim = 1)
  }

  return(out)
}
