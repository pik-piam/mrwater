#' @title calcClusterBase
#' @description Reads a series of MAgPIE files and combines them to a matrix
#' which is then used for calculating a clustering.
#'
#' @param years2use A vector with years with should be taken into account for
#' the clustering
#' @param spatial_header A vector of the form c("REG.1","REG.2") (region name,
#' cell number) with entries for each spatial entity of the MAgPIE input files
#' which should be used to replace the names given in the inputs (required for
#' flexible region aggregation as here region names might change.). If set to
#' NULL the original information is used.
#' @return A matrix containing the data
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcCluster}}
#' @importFrom magclass wrap
#' @importFrom madrat toolMappingFile
calcClusterBase <- function(years2use=1995) {

  d <- list()
  # read in data which should be used to determine cluster
  #d$yld    <- calcOutput("MAgPIEYields",aggregate=FALSE)[,years2use,]
  #d$airrig <- calcOutput("MAgPIEAirrig",aggregate=FALSE)[,years2use,]
  #d$td     <- calcOutput("TransportDistance", aggregate=FALSE)[,,rep(1,16)]
  d$yld    <- read.magpie("/home/dietrich/Modelling/tmp/test/isimip_rcp-IPSL_CM5A_LR-rcp2p6-co2_rev43_0.5/lpj_yields_0.5.mz")[,years2use,]
  d$airrig <- read.magpie("/home/dietrich/Modelling/tmp/test/isimip_rcp-IPSL_CM5A_LR-rcp2p6-co2_rev43_0.5/lpj_airrig_0.5.mz")[,years2use,]
  d$td     <- read.magpie("/home/dietrich/Modelling/tmp/test/isimip_rcp-IPSL_CM5A_LR-rcp2p6-co2_rev43_0.5/transport_distance_0.5.mz")[,,rep(1,16)]

  cdata <- do.call(cbind,lapply(d,wrap,list(1,c(2,3))))
  cdata <- scale(cdata)
  colnames(cdata) <- paste0("i",1:ncol(cdata))

  #make sure that spatial dimension contains country information
  iso <- toolMappingFile("cell", "CountryToCellMapping.csv", readcsv = TRUE)$iso
  dimnames(cdata)[[1]] <- paste(iso, 1:length(iso), sep = ".")

  return(list(
    x = as.magpie(cdata),
    weight = NULL,
    unit = "1",
    description = "Similarity matrix as basis for clustering",
    isocountries = FALSE))
}
