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


    out <- readSource("LabourProdImpactOrlov",convert=FALSE)



    if(timestep == "5year"){
      #add future
      out <- out[,seq(1985,2095,5),]
      out <- toolHoldConstant(out, seq(2100,2150, 5))

      out <- collapseNames(out)
      getNames(out) <- "factor"

      #add dummy 1 fill for no cliamte impacts
      normal <- new.magpie(cells_and_regions = getCells(out), years = getYears(out), names="normal", fill=1)
      out <- mbind(out,normal)


      ## harmonize the years until 2020 so they don't give different initial results

      out[,seq(1985,2020,5),"normal"] <- out[,seq(1985,2020,5),"factor"]
      out[,seq(2025,2150,5),"normal"] <-out[,2020,"factor"]

      #add past
      past <- new.magpie(cells_and_regions = getCells(out), years=seq(1965,1980, 5), names=getNames(out),fill=1)
      out <- mbind(past,out)

    }

    else if(timestep=="yearly"){
      out <- out[,(1995:2099),]
      out <- toolHoldConstant(out, c(2100:2150))

      out <- collapseNames(out)
      getNames(out) <- "factor"

      normal <- new.magpie(cells_and_regions = getCells(out), years = getYears(out), names="normal", fill=1)
      out <- mbind(out,normal)

      out[,c(1995:2020),"normal"] <- out[,c(1995:2020),"factor"]
      out[,c(2021:2150),"normal"] <-out[,2020,"factor"]

      past <- new.magpie(cells_and_regions = getCells(out), years=c(1965:1980), names=getNames(out),fill=1)
      out <- mbind(past,out)

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
