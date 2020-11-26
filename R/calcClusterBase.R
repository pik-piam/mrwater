#' @title calcClusterBase
#' @description Reads a series of MAgPIE files and combines them to a matrix
#' which is then used for calculating a clustering.
#' @param clusterdata similarity data to be used to determine clusters: yield_airrig (current default) or yield_increment
#' @return A matrix containing the data
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @seealso \code{\link{calcCluster}}
#' @importFrom magclass wrap read.magpie
#' @importFrom madrat toolMappingFile
calcClusterBase <- function(clusterdata="yield_airrig") {

  d <- list()
  # read in data which should be used to determine cluster
  if (clusterdata=="yield_airrig") {
    d$yld    <- calcOutput("Yields",     years=1995, aggregate=FALSE)
    d$irrig  <- calcOutput("Irrigation", years=1995, aggregate=FALSE)
    d$td     <- calcOutput("TransportDistance",      aggregate=FALSE)[,,rep(1,floor(ndata(d$yld)/2))]
  } else if (clusterdata=="yield_increment") {
    yield    <- calcOutput("Yields", years=1995, aggregate=FALSE)
    d$yld    <- collapseNames(yield[,,"rainfed"])
    d$irrig  <- collapseNames(yield[,,"irrigated"][,,"pasture",invert=T])-collapseNames(yield[,,"rainfed"][,,"pasture",invert=T])
    d$td     <- calcOutput("TransportDistance", aggregate=FALSE)[,,rep(1,ndata(d$yld))]
  } else {
    stop("Specify clusterdata argument: yield_airrig (old clustering data) or yield_increment (new clustering data)")
  }

  cdata <- do.call(cbind,lapply(d,wrap,list(1,c(2,3))))
  cdata <- cdata[,dimSums(cdata,dim=1)!=0]
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
