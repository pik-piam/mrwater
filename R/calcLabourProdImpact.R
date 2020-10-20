#' @title calcLabourProdImpact
#' @description Labour productivity impacts
#' @param timestep 5year or yearly
#' @param subtype  data source comes from
#' @param cellular cellular is true
#' @return List of magpie objects with results on 0.5deg grid level, weights based on production value, unit (ratio) and description.
#' @author David Chen
#' @importFrom magpiesets findset
#' @importFrom mstools toolHoldConstant
#' @importFrom magclass collapseNames


calcLabourProdImpact <-function(timestep = "5year", subtype="Orlov", cellular=TRUE){

  if (subtype=="Orlov"){


    out <- readSource("LabourProdImpactOrlov", subtype="IPSL-CM5A-LR_rcp85_wbgtod_hothaps_400W.nc", convert=FALSE)
    out <- toolTimeSpline(out)
    tran <- readSource("LabourProdImpactOrlov", subtype="IPSL-CM5A-LR_rcp60_wbgtod_hothaps_400W.nc", convert=FALSE)
    tran <- toolTimeSpline(tran)
    out[,c(2006:2020),] <- tran[,c(2006:2020),]


    if(timestep == "5year"){
      #add future
      out <- out[,seq(1985,2095,5),]

      past <- new.magpie(cells_and_regions = getCells(out), years=seq(1965,1980,5), names=getNames(out),fill=1)
      past[,seq(1965,1980,5),] <- setYears(out[,1985,],NULL)
      out <- mbind(past,out)

      out <- toolHoldConstantBeyondEnd(out)
      out <- collapseNames(out)

    }

    else if(timestep=="yearly"){

      past <- new.magpie(cells_and_regions = getCells(out), years=c(1965:1980), names=getNames(out),fill=1)
      past[,c(1965:1980),] <- setYears(out[,1981,],NULL)
      out <- mbind(past,out)
      out <- toolHoldConstantBeyondEnd(out)

      out <- collapseNames(out)

    }



  }
  #weight <- calcOutput("ValueProduction",aggregate=F)[,2010,]
  #no weight yet because doesn't work in old preprocessing

  if(subtype!="Orlov") {
    stop("Not a Valid Subtype")}


    return(list(
    x=out,
    weight=NULL,
    unit="Percentage of total labour productivity",
    isocountries=(!cellular & (nregions(out)!=1)),
    description="Labour productivity impacts as percentage of full labour prod 1"))
}
