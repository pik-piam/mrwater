#' Apply region names
#'
#' This tool function replaces country names with region names
#' in the spatial dimension of the object. To avoid mixing up
#' of cache files with different regional aggregation the
#' regioncode needs to supplied and checked as well.
#' Only if the supplied regions code agrees with the
#' region mapping currently chosen the function will return
#' the data.
#'
#' @param cdata a cluster data file as produced by cluster_base
#' @param regionscode regionscode of the regional mapping to be used.
#' Must agree with the regionscode of the mapping mentioned in the madrat
#' config! Can be retrieved via \code{regionscode()}.
#' @return the cluster data file with region names in spatial dimension
#' rather than country names
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcClusterKMeans}}, \code{\link{calcClusterBase}}
#' @importFrom madrat toolMappingFile regionscode
#' @importFrom mrland spatial_header

toolApplyRegionNames <- function(cdata,regionscode){
  ### APPLY REGIONS HERE ON SPATIAL NAMING OF CDATA INSTEAD OF COUNTRIES ###
  ### regionscode needs to be checked and provided as argument to ensure
  ### that caching is not mixing up aggregations with different regional
  ### mapping.
  map <- toolMappingFile("regional",getConfig("regionmapping"),readcsv = TRUE)
  if(regionscode!=regionscode(map)) stop("Provided regionscode does not match regionscode of regional mapping!")
  getCells(cdata) <- paste(sub("\\..*$", "", dimnames(cdata)[[1]]), spatial_header(map), sep=".")
  getSets(cdata,fulldim=FALSE)[1] <- "country.region.cell"
  return(cdata)
}

