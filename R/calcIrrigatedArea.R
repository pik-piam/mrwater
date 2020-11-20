#' @title       calcIrrigatedArea
#' @description calculates area reserved for irrigation based on area irrigated in initialization year and depreciation parameter
#'
#' @param iniyear      initialization year
#' @param selectyears  select years
#' @param depreciation parameter defining rate at which previously irrigated cropland becomes "unreserved" for irrigation
#' @param cells        cells to be returned by the function (lpjcell or magpiecell)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigatedArea", aggregate=FALSE) }
#'
#' @import magclass
#' @import magpiesets

calcIrrigatedArea <- function(selectyears=seq(1995,2100,by=5), iniyear=1995, depreciation=0.1, cells="lpjcell"){

  # Read in data: crop- and water supply type specific crop area (in Mha) in initialization year:
  tmp <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="magpiecell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
  # Retrieve irrigated area (per crop)
  tmp <- collapseNames(tmp[,,"irrigated"])

  # Empty object to be filled with area reserved for irrigation in current and future time steps
  irrig_area <- new.magpie(getCells(tmp),selectyears,getNames(tmp))

  # Each year certain share (parameter: "depreciation") of irrigated cropland is lost
  for (y in selectyears){
    irrig_area[,y,] <- tmp
    tmp             <- tmp*(1-depreciation)
  }

  # Corrections
  # years
  if(selectyears!="all"){
    years       <- sort(findset(selectyears,noset="original"))
    irrig_area  <- irrig_area[,years,]
  }

  # number of cells
  if (cells=="magpiecell") {
    out <- irrig_area
  } else if (cells=="lpjcell") {
    lpj_cells_map  <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")
    getCells(irrig_area)  <- paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep=".")
    out     <- new.magpie(1:67420,getYears(irrig_area),getNames(irrig_area))
    out[,,] <- 0
    out[paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep="."),,] <- irrig_area[,,]
    getCells(out) <- paste(lpj_cells_map$ISO,1:67420,sep=".")
  } else {
    stop("Cells argument not supported. Please select lpjcell for 67420 cells or magpiecell for 59199 cells")
  }

  # check for NAs and negative values
  if(any(is.na(out))){
    stop("produced NA irrigation water requirements")
  }
  if(any(out<0)){
    stop("produced negative irrigation water requirements")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. ha",
    description="Cropland area reserved for irrigation per crop",
    isocountries=FALSE))
}
