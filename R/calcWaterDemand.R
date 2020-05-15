#' @title calcWaterDemand
#' @description This function extracts non-agricultural water demand
#' @param selectyears years to be returned
#' @param source data source to be used
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("WaterDemand", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcWaterDemand <- function(selectyears=seq(1995,2095,by=5),seasonality="grper",source="WATERGAP2013"){

  # Read in nonagricultural water demand:
  watdem_nonagr   <- readSource("WATERGAP",convert="onlycorrect",subtype=source)

  if (seasonality=="grper") {
    # get growing period
    grow_days     <- calcOutput("GrowingPeriod", aggregate=FALSE)[,paste("y",years,sep=""),] ####### grow_days doesn't work yet!!!!!!!!
    # calculate non-agricultural water demand in growing period
    out <- watdem_nonagr[,paste("y",years,sep=""),]*(rowSums(grow_days,dim=2)/365) ######### ADJUST TO MAGPIE OBJECT FORMAT!!! grow_days is magpie object no longer array!!
    description <- "Non-agricultural water demand (industry, electiricty) in growing period"
  }

  if (seasonality=="total") {
    out <- watdem_nonagr[,paste("y",years,sep=""),]
    description <- "Total non-agricultural water demand (industry, electiricty)"
  }

  # Check for NAs
  if (any(is.na(watdem_nonagr))) {
    stop("produced NA watdem_nonagr")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
