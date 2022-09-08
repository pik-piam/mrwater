#' @title readWATERGAP
#' @description Read in non-agricultural water demand data from WATERGAP model
#' @param subtype Data source to be read from
#' @return MAgPIE object of non-agricultural water demand at 0.5 cellular level in mio. m^3
#' @author Felicitas Beier, Abhijeet Mishra
#'
#' @examples
#' \dontrun{
#' readSource("WATERGAP", convert = "onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom raster brick

readWATERGAP <- function(subtype = "WATCH_ISIMIP_WATERGAP") {

  # Note to "WATCH_IMAGE_WATERGAP": old non-agricultural waterdemand data (will be deleted soon!)
  if (subtype == "WATCH_ISIMIP_WATERGAP") {
    # Non-agricultural water demands (withdrawals) from WATCH, ISIMIP and WATERGAP
      # A2: WATERGAP WATCH project; B1: WATERGAP WATCH project; SSP2: WATERGAP ISIMIP project
    x <- read.magpie(paste0(subtype, "/watdem_nonagr_0.5.mz"))

  } else if (subtype == "WATERGAP2020") {

    ### List of input files
    input <- list()
    input[["ssp1"]] <- list()
    input[["ssp1"]][["wc"]] <- list()
    input[["ssp1"]][["wc"]] <- c("watergap_ssp1_rcp4p5_elec_wc_annual_2005_2100.nc",
                                 "watergap_ssp1_rcp4p5_man_wc_annual_2005_2100.nc",
                                 "watergap_ssp1_rcp4p5_dom_wc_annual_2005_2100.nc")
    input[["ssp1"]][["ww"]] <- gsub("wc", "ww", input[["ssp1"]][["wc"]])
    input[["ssp2"]][["wc"]] <- gsub("ssp1_rcp4p5", "ssp2_rcp6p0", input[["ssp1"]][["wc"]])
    input[["ssp2"]][["ww"]] <- gsub("ssp1_rcp4p5", "ssp2_rcp6p0", input[["ssp1"]][["ww"]])
    input[["ssp3"]][["wc"]] <- gsub("ssp1_rcp4p5", "ssp3_rcp6p0", input[["ssp1"]][["wc"]])
    input[["ssp3"]][["ww"]] <- gsub("ssp1_rcp4p5", "ssp3_rcp6p0", input[["ssp1"]][["ww"]])

    ### Reading in files and combining to one magpie object:
    # read in raster brick
    brick        <- suppressWarnings(brick(paste0(subtype, "/", input[[1]][["wc"]][1])))
    # start year (with name X0) is 2005:
    names(brick) <- paste0("y", as.numeric(gsub("X", "", names(brick))) + 2005)
    # transform to magpie object with coordinate data
    x            <- as.magpie(brick)
    getNames(x)  <- brick@title
    getNames(x)  <- paste0("sspX.", getNames(x))

    # Different SSPs:
    for (i in seq_len(input)) {

      # Different water use types (withdrawal, consumption)
      for (j in seq_len(input[["ssp1"]])) {

        # Different industries (manufacturing, electricity, domestic)
        for (k in seq_len(input[["ssp1"]][["wc"]])) {

          brick        <- suppressWarnings(brick(paste0(subtype, "/", input[[i]][[j]][k])))
          # start year (with name X0) is 2005:
          names(brick) <- paste0("y", as.numeric(gsub("X", "", names(brick))) + 2005)
          # transform to magpie object with coordinate data
          tmp            <- as.magpie(brick)
          getNames(tmp)  <- brick@title

          getNames(tmp) <- paste0("ssp", i, ".", getNames(tmp))
          x <- mbind(x, tmp)
        }
      }
    }
    # Remove redundant scenario (was for temporary use in loop only)
    x <- x[, , "sspX", invert = TRUE]

    # Unit transformation (from m3/yr to mio. m3/yr):
    x <- x / 1000000

    # Rename dimensions and sets
    getNames(x) <- gsub("electricity production water ", "electricity.", getNames(x))
    getNames(x) <- gsub("manufacturing water ", "manufacturing.", getNames(x))
    getNames(x) <- gsub("Domestic water ", "domestic.", getNames(x))
    getSets(x)["d3.1"] <- "scenario"
    getSets(x)["d3.2"] <- "use"
    getSets(x)["d3.3"] <- "type"

  }

  return(x)
}
