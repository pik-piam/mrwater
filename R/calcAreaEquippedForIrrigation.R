#' @title calcAreaEquippedForIrrigation
#' @description Calculates the area equipped for irrigation based on LU2v2 dataset. It assumes, that all cropland irrigated in the last 20 years at least once is equipped for irrigation.
#'
#' @param source switch between different data sources
#' @param cellular if true, dataset is returned on 0.5 degree resolution
#'
#' @return List of magpie objects with results on country/cellular level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#'
#' \dontrun{
#' calcOutput("AreaEquippedForIrrigation")
#' }
#' @importFrom magclass as.magpie getRegionList<- ncells
#'

calcAreaEquippedForIrrigation<-function(source="LUH2v2", cellular=FALSE){

  if(source=="LUH2v2"){

    LUHcrops <- subset(toolGetMapping("LUH2v2.csv", type = "sectoral"), land=="crop", select="luh2v2", drop=TRUE)

    #calcLUH2v2 needed here due to corrections, but year selection not working ike this
    x        <- calcOutput("LUH2v2", landuse_types="LUH2v2", irrigation=TRUE, cellular=TRUE, selectyears="past", aggregate = FALSE)
    x        <- dimSums(x[,,"irrigated"][,,LUHcrops],dim=3)
    past     <- as.numeric(substring(findset("past"),2))
    out      <- NULL

    for (year_x in past){
      span <- (year_x-20):year_x
      tmp  <- setYears(as.magpie(apply(X = x[,span,], FUN = max, MARGIN = 1)),paste0("y",year_x))
      out  <- mbind(out,tmp)
    }

  } else if (source=="Siebert"){

     out   <- readSource("Siebert", convert="onlycorrect")

  } else stop("Unknown source for calcArea")


  if (!cellular){
    mapping <- toolMappingFile(type="cell", name="CountryToCellMapping.csv",readcsv=TRUE)
    out     <- toolAggregate(data=out, rel=mapping, from="celliso", to="iso",dim=1)
    out     <- toolCountryFill(out,fill=0)
  }

  return(list(
    x=out,
    weight=NULL,
    unit="Million ha",
    description="Million hectare land area for different land use types.",
    isocountries=!cellular))
}
