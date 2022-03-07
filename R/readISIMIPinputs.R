#' @title readISIMIPinputs
#' @description Read in non-agricultural water demand data from ISIMIP inputs
#' @param subtype Data source to be read from including path separated by ":", subtype separated by "."
#' @return MAgPIE object of non-agricultural water demand at 0.5 cellular level in mio. m^3
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("ISIMIPinputs",
#' subtype = "ISIMIP3b:water:histsoc.waterabstraction",
#' convert = "onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readISIMIPinputs <- function(subtype = "ISIMIP3b:water:histsoc.waterabstraction") {

  if (grepl("\\.", subtype)) {
    subtype     <- strsplit(gsub(":", "/", subtype), split = "\\.")
    folder      <- unlist(subtype)[1]
    time        <- unlist(strsplit(folder, split = "\\/"))[3]
    subtype     <- unlist(subtype)[2]
  }

  ### ISIMIP non-agricultural water withdrawals
  if (subtype == "waterabstraction") {

    # Time frame to be read in
    if (time == "histsoc") {
      readyears  <- paste0(time, "_annual_1901_2014")
   } else if (time == "2015soc") {
      readyears  <- paste0(time, "_annual_2015_2100")
   }

    # List of input files
    input <- list()
    input <- c(paste0(folder, "/domww_", readyears, ".nc"),
               paste0(folder, "/indwc_", readyears, ".nc"),
               paste0(folder, "/indww_", readyears, ".nc"))

    ## Read in data
    # read in raster brick
    brick        <- brick(paste0(folder, "/domwc_", readyears, ".nc"))
    # start year (with name X0) is 1901:
    names(brick) <- paste0("y", round(as.numeric(gsub("X", "", names(brick))), 0) + 1901)
    # transform to magpie object with coordinate data
    x            <- as.magpie(brick)
    getNames(x)  <- brick@title

    # append data
    for (i in (1:length(input))) {

      brick         <- brick(input[i])
      # start year (with name X0) is 1901:
      names(brick)  <- paste0("y", round(as.numeric(gsub("X", "", names(brick))), 0) + 1901)
      # transform to magpie object with coordinate data
      tmp           <- as.magpie(brick)
      getNames(tmp) <- brick@title

      x   <- mbind(x, tmp)
    }

    # Unit transformation (from m3/yr to mio. m3/yr):
    x <- x / 1000000

    # Rename dimensions and sets
    getNames(x) <- gsub("industrial water ", "industry.", getNames(x))
    getNames(x) <- gsub("domestic water ", "domestic.", getNames(x))
    getSets(x)["d3.1"] <- "use"
    getSets(x)["d3.2"] <- "type"

  }

  return(x)
}
