#' @title       calcLandAvl
#' @description calculates area that is available for (irrigated) cropland under different assumptions
#'
#' @param iniyear      initialization year used for irrigated area
#' @param selectyears  years to be returned by this function
#' @param depreciation parameter defining yearly depreciation rate at which previously irrigated cropland becomes "unreserved" for irrigation
#' @param cells        cells to be returned by the function (lpjcell or magpiecell)
#'
#'
#' @param landtype     current cropland area (currentcropland) or potential cropland area (potentialcropland) or nonprotected (restriction of withdrawals in protected areas)
#' @param protectscen  protection scenario for protected areas ("WDPA", "HalfEarth", )
#' @param climatetype        Switch between different climate scenarios (default: "CRU_4")
#' @param time               Time smoothing: average, spline or raw (default)
#' @param averaging_range    only specify if time=="average": number of time steps to average
#' @param dof                only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param allocationrule     Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param allocationshare    Share of water to be allocated to cell (only needs to be selected in case of allocationrule=="equality")
#' @param gainthreshold      Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem   Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param irrigini           When "initialization" selected for irrigation system: choose initialization data set for irrigation system initialization ("Jaegermeyr_lpjcell", "LPJmL_lpjcell")
#' @param proxycrops         Proxycrops for water requirements
#' @param output             Type of area to be returned: irrigatable_area (default, considers both water and land availability), irrig_area_ww or irrig_area_wc (area that can be irrigated given water available for withdrawal or consumption), avl_land, protect_area (restricts water withdrawal in protected areas)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("LandAvl", aggregate=FALSE) }
#'
#' @import magclass
#' @import magpiesets


calcLandAvl <- function(selectyears=1995, cells="lpjcell", output="irrigatable_area",
                                climatetype="HadGEM2_ES:rcp2p6:co2", time="spline", averaging_range=NULL, dof=4, harmonize_baseline="CRU_4", ref_year="y2015",
                                allocationrule="optimization", allocationshare=NULL, gainthreshold=1, irrigationsystem="initialization", irrigini="Jaegermeyr_lpjcell", iniyear=1995,
                                landtype="potentialcropland", protectscen="WDPA", proxycrops="maiz"){

#
  # w001001 <- brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/w001001.adf")



#
#    summary(as.data.frame(w001001,na.omit=T))
#    #w001001 <- raster("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/w001001.adf")
#
#    test <- as.magpie(w001001)
# #
#
#    plot(brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/w001001.adf"))
#    plot(brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/w001001x.adf"))
#    plot(brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/dblbnd.adf"))
#    plot(brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/sta.adf"))
#    plot(brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/hdr.adf"))
#
#
#    brick1 <- brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/w001001x.adf")
#    plot(brick1)
#
#    brick2 <- brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/dblbnd.adf")
#    plot(brick2)
#
#    test1 <- as.magpie(brick1)
#
#   brick2 <- brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/dblbnd.adf")
#   test2 <- as.magpie(brick2)
#
#   brick3 <- brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/sta.adf")
#   test3 <- as.magpie(brick3)
#
#   info <- read.table("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/info/arc0002.dat", header=T)
#
#   info <- read.delim("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/info/arc0002.dat", sep=" ")
#   info <- readLines("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/info/arc0003.dat", warn=F)
#
#   info <- readLines("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/info/arc0002.dat")


  ## Area available and suitable for cropland
  # read in land available for agricultural use (in mio. ha)
  land <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE)[,,"si0"])

  if (iniarea) {
    # subtract area already reserved for irrigation by committed agricultural uses (in mio. ha)
    crops_grown    <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, cells="magpiecell", aggregate=FALSE)
    crops_grown    <- collapseNames(dimSums(crops_grown,dim=3))
    land           <- land - crops_grown
  }
  # negative values may occur because AvlLandSi is based on Ramankutty data and Cropara based on LUH -> set to 0
  land[land<0] <- 0

  # water requirements for full irrigation in cell per crop (in mio. m^3)
  # Note on unit transformation:
  # land (mio ha -> ha): multiply with 1e6,
  # irrigation water requirements (m^3 per ha -> mio. m^3 per ha): devide by 1e6
  # --> cancels out -> water requirements for full irrigation (mio. m^3)
  tmp <- irrig_wat*land




  if (landtype=="potentialcropland") {
    # Potential cropland: area suitable for crop production (Mha)
    tmp <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE)[,,"si0"])

    # correct LUH and Ramankutty mismatch
    # rainfed and irrigated cropland in LUH (in Mha)
    # tmp_correction <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="magpiecell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
    # tmp_correction <- dimSums(tmp_correction,dim=3)
    # test <- tmp - tmp_correction
    #
    # tmp_correction <- calcOutput("Croparea", years=iniyear, sectoral="kcr", cells="magpiecell", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
    # tmp_correction <- dimSums(tmp_correction[,,"irrigated"],dim=3)
    # test <- tmp - tmp_correction
    #
    # tmp[tmp_correction<0] <-

    # # read in land available for agricultural use (in mio. ha)
    # land <- collapseNames(calcOutput("AvlLandSi", aggregate=FALSE)[,,"si0"])
    # if (iniarea) {
    #   # subtract area already reserved for irrigation by committed agricultural uses (in mio. ha)
    #   crops_grown    <- calcOutput("IrrigatedArea", selectyears=selectyears, iniyear=iniyear, cells="magpiecell", aggregate=FALSE)
    #   crops_grown    <- collapseNames(dimSums(crops_grown,dim=3))
    #   land           <- land - crops_grown
    # }
    # # negative values may occur because AvlLandSi is based on Ramankutty data and Cropara based on LUH -> set to 0
    # land[land<0] <- 0

    # convert from Mha to ha
    tmp <- tmp*1e6

    # correct cellular dimension of land
    if (cells=="magpiecell") {
      land <- tmp
    } else if (cells=="lpjcell") {
      lpj_cells_map  <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")
      getCells(tmp)  <- paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep=".")
      land           <- new.magpie(1:67420,getYears(tmp),getNames(tmp))
      land[,,]       <- 0
      land[paste("GLO",magclassdata$cellbelongings$LPJ_input.Index,sep="."),,] <- tmp[,,]
      getCells(land) <- paste(lpj_cells_map$ISO,1:67420,sep=".")
    } else {
      stop("Cells argument not supported. Please select lpjcell for 67420 cells or magpiecell for 59199 cells")
    }
  } else if (landtype=="currentcropland") {
    # Current cropland: area currently under crop production (sum over both rainfed and irrigated, sum over all crops)
    land <- calcOutput("Croparea", years=selectyears, sectoral="kcr", cells=cells, physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
    land <- dimSums(land, dim=3)

 # } else if (landtype=="nonprotected") {
    # Read in protected area


  #  protect_area <- calcOutput("ProtectArea", aggregate = FALSE)


    # land <- calcOutput("Croparea", years=selectyears, sectoral="kcr", cells=cells, physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate=FALSE)
    # land <- dimSums(land, dim=3)

  } else {
    stop("Choose potentialcropland or currentcropland or nonprotected as available landtype")
  }

  avl_land[,,] <- land

  # irrigatable area (area that can be irrigated): enough local water available & enough local land available
  irrigatable_area <- pmin(avl_land, irrig_area_wc, irrig_area_ww)

  if (output=="irrigatable_area") {
    out <- irrigatable_area
  } else if (output=="irrig_area_ww") {
    out <- irrig_area_ww
  } else if (output=="irrig_area_wc") {
    out <- irrig_area_wc
  } else if (output=="avl_land") {
    out <- avl_land
  } else {
    stop("Please select which area type should be returned: irrigatable_area includes both the water and the land constraint.")
  }

  # convert to mio. ha
  out <- out*1e-6

  # Corrections
  # years
  if(selectyears!="all"){
    years <- sort(findset(selectyears,noset="original"))
    out   <- out[,years,]
  }

  # check for NAs and negative values
  if(any(is.na(out))){
    stop("produced NA available land")
  }
  if(any(out<0)){
    stop("produced negative available land")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. ha",
    description="Area potentially available for irrigated cropland",
    isocountries=FALSE))
}
