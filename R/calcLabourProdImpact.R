#' @title calcLabourProdImpact
#' @description Labour productivity impacts
#' @param timestep 5year or yearly
#' @param subtype  data source comes from
#' @param cellular cellular is true
#' @return List of magpie objects with results on 0.5deg grid level, weights based on production value, unit (ratio) and description.
#' @author David Chen
#' @importFrom magpiesets findset
#' @importFrom mstools toolHoldConstant

calcLabourProdImpact <-function(timestep = "5year", subtype="Orlov", cellular=TRUE){

  if (subtype=="Orlov"){


    out <- readSource("LabourProdImpactOrlov",convert=FALSE)

    if(timestep == "5year"){
      out <- out[,seq(1995,2095,5),]
      out <- toolHoldConstant(out, seq(2100,2150, 5))
    }

    else if(timestep=="yearly"){
      out <- out[,(1995:2099),]
      out <- toolHoldConstant(out, c(2100:2150))
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
