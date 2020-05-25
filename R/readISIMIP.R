#' @title readISIMIP
#' @description Read in non-agricultural water demand data from ISIMIP3b input data
#' @param subtype Data source to be read from and time frame (past, future) to be read in, separated by "."
#' @return MAgPIE object of non-agricultural water demand at 0.5 cellular level in mio. m^3
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ readSource("ISIMIP", subtype="water_abstraction.past", convert="onlycorrect") }
#'
#' @import madrat
#' @import magclass

readISIMIP <- function(subtype="water_abstraction.past"){

  subtype  <- strsplit(subtype, split="\\.")
  time     <- unlist(subtype)[2]
  subtype  <- unlist(subtype)[1]

  if(subtype=="water_abstraction"){

    # Time frame to be read in
    if(time=="past"){
      folder <- "/histsoc/"
      readyears  <- "1901_2014"
   } else if(time=="future"){
      folder <- "/2015soc/"
      readyears  <- "2015_2100"
   }

    # List of input files
    input <- list()
    input <- c(paste0("domww_",gsub("\\/","",folder),"_annual_",readyears,".nc"),
               paste0("indwc_",gsub("\\/","",folder),"_annual_",readyears,".nc"),
               paste0("indww_",gsub("\\/","",folder),"_annual_",readyears,".nc"))

    # Read in data
    x <- read.magpie(paste0(subtype,folder,"domwc_", gsub("\\/","",folder),"_annual_",readyears,".nc"))
    for (i in (1:length(input))){
      tmp <- read.magpie(paste0(subtype,folder,input[i]))
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
    ww           <- dimSums(mbind(x[,,"domww"],x[,,"indww"]),dim=3)
    getNames(ww) <- "withdrawal"
    # water consumption:
    wc           <- dimSums(mbind(x[,,"domwc"],x[,,"indwc"]),dim=3)
    getNames(wc) <- "consumption"
    x            <- mbind(ww,wc)
  }

  return(x)
}
