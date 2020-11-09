#' @title calcISIMIPYields
#' @description reads and cleans up ISIMIP3b crop yield data
#' @param subtype subtype of yield based on readISIMIPoutputs, for crop yields
#' @param time spline or average, if spline specify dof, if average specify averaging range
#' @param dof degrees of freedom for spline
#'  @return magpie object in cellular resolution
#' @author David Meng-Chuen Chen
#' @examples
#' \dontrun{ calcOutput("ISIMIPYields", aggregate = FALSE) }
#'

calcISIMIPYields <-function(subtype="ISIMIP3b:yields.EPIC-IIASA_ukesm1-0-ll_ssp585_default",
                            time="spline", dof=4){

  x <- readSource("ISIMIPoutputs", subtype=subtype, convert=FALSE)
  x[is.na(x)] <- 0

  if (time=="spline"){
  x <- toolTimeSpline(x, dof=4)
  if("y2099" %in%getYears(x)) x <- toolFillYears(x, c(getYears(x, as.integer=TRUE)[1]:2100))
}
  if (time=="average"){
    x <- toolTimeAverage(x)
  }

# toolTimeSpline creates very small values, set these to 0
  x[x<=0.01] <- 0

#weights
  crop_area_weight     <- x
  crop_area_weight[,,] <- 1

  return(list(
    x=x,
    weight=crop_area_weight,
    unit="none",
    description="ISIMIP3b GGCMI yields for soy rice wheat maize",
    isocountries=FALSE))
}
