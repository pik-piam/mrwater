#' @title calcNonAgWaterDemand
#' @description This function extracts non-agricultural water demand
#' @param selectyears years to be returned
#' @param source data source to be used
#' @param seasonality grper (default): non-agricultural water demand in growing period per year; total: non-agricultural water demand throughout the year
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param harmonize_baseline FALSE (default) nothing happens, if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year just specify for harmonize_baseline != FALSE : Reference year
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("NonAgWaterDemand", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcNonAgWaterDemand <- function(selectyears="all",seasonality="grper",source="WATCH_ISIMIP_WATERGAP",
                                 climatetype="CRU_4", harmonize_baseline=FALSE, ref_year="y2015"){

  ########################################
  ############ Calculations  #############
  #######################################

  # Old Non-Agricultural Waterdemand data (current default, will be deleted soon):
  if(source=="WATCH_ISIMIP_WATERGAP"){
    # Read in nonagricultural water demand:
    watdem_nonagr      <- readSource("WATERGAP", convert="onlycorrect", subtype=source)
  }

  # New Non-Agricultural Waterdemand data (will be new default)
  if(source=="WATERGAP2020"){
    # Read in nonagricultural water demand:
    watdem_nonagr_WATERGAP      <- readSource("WATERGAP", convert="onlycorrect", subtype="WATERGAP2020")
    watdem_nonagr_ISIMIP_hist   <- readSource("ISIMIP", convert="onlycorrect", subtype="water_abstraction.past")
    watdem_nonagr_ISIMIP_future <- readSource("ISIMIP", convert="onlycorrect", subtype="water_abstraction.future")

    ### Combine datasets from different sources:
    # historical and future ISIMIP data:
    watdem_ISIMIP <- mbind(watdem_nonagr_ISIMIP_hist, watdem_nonagr_ISIMIP_future)

    # empty magpie object
    cells <- getCells(watdem_nonagr_WATERGAP)
    years <- getYears(watdem_ISIMIP)
    names <- c(getNames(watdem_nonagr_WATERGAP),paste0("ISIMIP.",getNames(watdem_ISIMIP)))
    watdem_nonagr <- new.magpie(cells,years,names)

    # historical and future ISIMIP data
    watdem_nonagr[,getYears(watdem_ISIMIP),getNames(watdem_ISIMIP)] <- watdem_ISIMIP[,getYears(watdem_ISIMIP),getNames(watdem_ISIMIP)]
    # future WATERGAP scenarios
    watdem_nonagr[,getYears(watdem_nonagr_WATERGAP),getNames(watdem_nonagr_WATERGAP)] <- watdem_nonagr_WATERGAP[,getYears(watdem_nonagr_WATERGAP),getNames(watdem_nonagr_WATERGAP)]
    # historical data provided by ISIMIP (same for all scenarios)

    #This throws an error during lucode2::buildLibrary(), so this line was added here
    watdem_nonagr_hist <- NULL
    watdem_nonagr[,getYears(watdem_nonagr_ISIMIP_hist),] <- watdem_nonagr_hist[,getYears(watdem_nonagr_ISIMIP_hist),]
  }



  ###########################################
  ############ Function Output  #############
  ###########################################
  if(selectyears!="all"){
    years         <- sort(findset(selectyears, noset="original"))
    watdem_nonagr <- watdem_nonagr[,years,]
  }

  ### Non-agricultural water demands in Growing Period
  if(seasonality=="grper"){
    # Get growing days per month
    grow_days <- calcOutput("GrowingPeriod", version="LPJmL5", climatetype=climatetype, time="spline", dof=4,
                            harmonize_baseline=harmonize_baseline, ref_year=ref_year, yield_ratio=0.1, aggregate=FALSE)

    # Adjust years
    years_watdem <- getYears(watdem_nonagr)
    years_grper  <- getYears(grow_days)
    if(length(years_watdem)>=length(years_grper)){
      years <- years_grper
    } else {
      years <- years_watdem
    }
    rm(years_grper, years_watdem)

    # Calculate non-agricultural water demand in growing period
    out         <- ((watdem_nonagr[,years,])/365)*dimSums(grow_days[,years,],dim=3)
    description <- "Non-agricultural water demand (industry, electiricty, domestic) in growing period"
  }

  ### Total non-agricultural water demands per year
  if(seasonality=="total"){
    out         <- watdem_nonagr[,,]
    description <- "Total non-agricultural water demand (industry, electiricty, domestic)"
  }

  # Check for NAs
  if(any(is.na(watdem_nonagr))){
    stop("produced NA watdem_nonagr")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
