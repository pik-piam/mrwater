#' @title readISIMIP
#' @description Read in non-agricultural water demand data from ISIMIP2b input data
#' @param subtype Data source to be read from
#' @return MAgPIE object of non-agricultural water demand at 0.5 cellular level in mio. m^3
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ readSource("ISIMIP", convert="onlycorrect") }
#'
#' @import madrat
#' @import magclass

readISIMIP <- function(subtype="water_abstraction"){

  if(subtype=="water_abstraction"){

    # List of input files
    input <- list()
    input <- c("domww_histsoc_annual_1901-2005.nc","indwc_histsoc_annual_1901-2005.nc","indww_histsoc_annual_1901-2005.nc")

    # Read in data
    x     <- read.magpie(paste0(subtype,"/histsoc/domwc_histsoc_annual_1901-2005.nc"))
    for (i in (1:length(input))){
      tmp <- read.magpie(paste0(subtype,"/histsoc/",input[i]))
      x   <- mbind(x,tmp)
    }

    ### Correct years dimension:
    years       <- round(as.numeric(gsub("y","",getYears(x))),0)
    # Provided ISIMIP data starts in year 1901:
    start_year  <- 1901
    years       <- years + start_year
    years       <- paste0("y",years)
    getYears(x) <- years

    # Unit transformation (from m3/yr to mio. m3/yr):
    x <- x/1000000

    ### Sum up over all non-agricultural water uses (domestic, industry)
    # water withdrawal:
    ww <- dimSums(mbind(x[,,"domww"],x[,,"indww"]),dim=3)
    getNames(ww) <- "withdrawal"
    # water consumption:
    wc <- dimSums(mbind(x[,,"domwc"],x[,,"indwc"]),dim=3)
    getNames(wc) <- "consumption"
    x <- mbind(ww,wc)
  }

  return(x)
}
