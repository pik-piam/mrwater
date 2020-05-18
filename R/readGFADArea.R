#' @title readGFADArea
#' @description Read GLobal Forest Age Dataset derived from MODIS and COPENICUS satellite data
#'
#' @return magpie object in cellular resolution
#' @author Abhijeet Mishra, Felicitas Beier
#' @examples
#'
#' \dontrun{
#'   readSource("GFADArea", convert="onlycorrect")
#' }
#' @importFrom raster brick area subset
#' @importFrom ncdf4 nc_open ncvar_get
#' @export

readGFADArea <- function(){
  #poulter_data <- "C:/Users/mishra/Desktop/Current Tasks/GFAD_V1-1/GFAD_V1-1.nc"
  poulter_data <- "GFAD_V1-1.nc"

  gfad <- nc_open(poulter_data)

  lon <- ncvar_get(gfad,"lon")
  nlon <- dim(lon)

  lat <- ncvar_get(gfad,"lat")
  nlat <- dim(lat)

  mapping   <- toolGetMapping(type="cell",name="CountryToCellMapping.csv")
#  load("C:/PIK/data_processing/libraries/moinput/R/sysdata.rda")

  age_class <- paste0("X",1:15)
  age_class_conversion <- paste0("ac",1:15*10)
  forest_poulter <- c("NEEV","NEDE","BREV","BRDC")

  b <- brick(poulter_data,level=1)
  b <- subset(b,1)
  b <- area(b)

  # lon       <- seq(-179.75,179.75,by=0.5)
  # lat       <- rev(seq(-89.75,89.75,by=0.5))

  cellNames <- mapping$celliso


  #Change longitude and latitude
  matrix <- t(as.matrix(b))

  #Create array for 59199 isocells, 1 year and 1 data dimension
  mag   <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,"y2010",1))

  #Fill array with data from raster object (note magpie_coord are

  for (j in 1:59199) {
    mag[j,,] <- matrix[which(magpie_coord[j, 1]==lon),
                       which(magpie_coord[j,2]==lat)]
  }

  #Convert array to magpie object and rename set
  x          <- clean_magpie(as.magpie(mag))
  getSets(x) <- c("cell","t","ac")
  x <- add_dimension(x = x,dim = 3.1,add = "type",nm = "area")

  x <- collapseNames(x)

  out <- setYears(x,NULL)


  return(out)
}
