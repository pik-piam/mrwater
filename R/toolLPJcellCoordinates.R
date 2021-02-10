#' @title toolLPJcellCoordinates
#' @description maps LPJ cells and coordinates
#'
#' @param type direction of transformation (coord2lpj or lpj2coord)
#'
#' @return magpie object with coordinates in LPJ cell ordering
#' @author Felicitas Beier
#'
#' @import magclass
#' @importFrom madrat toolGetMapping


toolLPJcellCoordinates <- function(type){

  # read in LPJmL mapping
  LPJcells <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell", where="mappingfolder")
  # transform to coordinate style data with p separation
  LPJcells <- data.frame(LPJcells, data.frame("coordinates"=paste(gsub("\\.", "p", LPJcells$lon), gsub("\\.", "p", LPJcells$lat), sep="."),stringsAsFactors=F))
  # add cell number (LPJmL cell code)
  LPJcells <- data.frame(LPJcells, data.frame("cell"=paste(LPJcells$ISO,c(1:67420),sep="."),stringsAsFactors=F))
  # subset data frame
  LPJcells <- data.frame("cell"=LPJcells$cell,"coordinates"=LPJcells$coordinates,stringsAsFactors=F)


  if (type=="coord2lpj") {

    # EXAMPLE DATA:
    #x <- calcOutput("WaterUseNonAg", source="WATERGAP2020", selectyears=selectyears, time=time, dof=dof, averaging_range=averaging_range, waterusetype="withdrawal", seasonality="total", finalcells="lpjcell", aggregate=FALSE)

    out <- x[LPJcells$coordinates,,]
    getCells(out) <- LPJcells$cell

  } else if (type=="lpj2coord") {


    # EXAMPLE DATA
    #x <- calcOutput("LPJmL", version="LPJmL4", selectyears=selectyears, climatetype=climatetype, subtype="runoff_lpjcell", aggregate=FALSE, harmonize_baseline=harmonize_baseline, ref_year=ref_year, time=time, dof=dof, averaging_range=averaging_range)

    # transform lpjcells to coordinates
    getCells(x) <- LPJcells$coordinates
    out <- x

  } else {
    stop("Please select direction of transformation via type argument: coord2lpj or lpj2coord")
  }

  return(out)
}
