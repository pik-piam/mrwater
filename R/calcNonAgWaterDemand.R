#' @title calcNonAgWaterDemand
#' @description This function extracts non-agricultural water demand
#' @param selectyears years to be returned
#' @param source data source to be used
#' @param seasonality grper (default): non-agricultural water demand in growing period per year; total: non-agricultural water demand throughout the year
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("NonAgWaterDemand", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcNonAgWaterDemand <- function(selectyears="all",seasonality="grper",source="WATCH_ISIMIP_WATERGAP"){

  # Read in nonagricultural water demand:
  watdem_nonagr   <- readSource("WATERGAP",convert="onlycorrect", subtype=source)

  if(selectyears!="all"){
    years   <- sort(findset(selectyears, noset="original"))
    watdem_nonagr     <- watdem_nonagr[,years,]
  }

  if (seasonality=="grper") {
    # Get growing days per month
    grow_days     <- calcOutput("GrowingPeriod", aggregate=FALSE) ####### grow_days doesn't work yet!!!!!!!!

    # Adjust years
    years_watdem <- getYears(watdem_nonagr)
    years_grper <- getYears(grow_days)
    if(length(years_watdem)>=length(years_grper)){
      years <- years_grper
    } else {
      years <- years_watdem
    }
    rm(years_grper, years_watdem)

    # Calculate non-agricultural water demand in growing period
    out         <- watdem_nonagr[,years,]*(dimSums(grow_days[,years,],dim=3)/365)
    description <- "Non-agricultural water demand (industry, electiricty, domestic) in growing period"
  }

  if (seasonality=="total") {
    out         <- watdem_nonagr[,,]
    description <- "Total non-agricultural water demand (industry, electiricty, domestic)"
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
