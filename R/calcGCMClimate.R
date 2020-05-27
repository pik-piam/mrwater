#' @title calcGCMClimate
#' @description Disaggregate CO2 global atmospheric concentration to cellular level
#' @param rcp specify the RCP (rcp85, rcp60, rcp45 or rcp26)
#' @param GCMModel !!!! add description
#' @param ClimateVariable !!!! add description
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("GCMClimate", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset
#'

calcGCMClimate <-function(GCMModel = "HadGEM2", ClimateVariable = "temperature", rcp = "rcp85"){

  .subtype = paste0(rcp,":",GCMModel,".",ClimateVariable)

  x <- readSource("GCMClimate", subtype=.subtype, convert="onlycorrect")

  .unit = switch (ClimateVariable,
                  "temperature"          = "Degree Celcius",
                  "precipitation"        = "mm3 per year",
                  "longwave_radiation"   = "watt per m2",
                  "shortwave_radiation"  = "watt per m2",
                  "wetdays"              = "day")

  .description = switch (ClimateVariable,
                         "temperature"          = "Average annual air temperature (Mitchell & Jones 2005)",
                         "precipitation"        = "Average precipitation (Becker et al. 2013)",
                         "longwave_radiation"   = "?",
                         "shortwave_radiation"  = "?",
                         "wetdays"              = "?")

  return(list(
    x=x,
    weight=NULL,
    unit=.unit,
    description=.description,
    isocountries=FALSE))
}
