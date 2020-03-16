#' MAgPIE Kmeans clustering
#'
#' Performs MAgPIE kmeans clustering and calculates corresponding spam relation
#' matrix
#'
#' @param regionscode regionscode of the regional mapping to be used. Must agree with the regionscode of the mapping
#' mentioned in the madrat config! Can be retrieved via \code{regionscode()}.
#' @param ncluster The desired total number of clusters.
#' @param weight named vector with weighting factors for each region for the cluster distribution
#' ,e.g. weight=c(AFR=3,EUR=0.5). weight > 1 will grant more cluster to a region and
#' weight < 1 less cluster than by default.
#' @param cpr cells-per-region information as returned by cluster_per_region. Weight and ncluster are
#' ignored in case that cpr is provided!
#' @param seed a single value, interpreted as an integer, or NULL, to define seed for random calculations
#' @return A mapping between regions and clusters
#' @author Jan Philipp Dietrich
#' @importFrom stats kmeans
#' @seealso \code{\link{toolClusterPerRegion}}, \code{\link{calcClusterHierarchical}}
#' @export

calcClusterKMeans <- function(regionscode, ncluster, weight=NULL, cpr=NULL, seed=42) {

  cdata <- toolApplyRegionNames(calcOutput("ClusterBase", aggregate=FALSE),regionscode)

  if(is.null(cpr)) cpr <- toolClusterPerRegion(sub("^[^\\.]*\\.","",getCells(cdata)),ncluster,weight)

  cdata <- as.array(cdata)[,,]

  out <- new.magpie(dimnames(cdata)[[1]],fill=1)
  getSets(out,fulldim = FALSE)[1] <- "country.region.cell.cluster"
  ccount <- 0
  set.seed(seed)
  for(r in dimnames(cpr)[[1]]) {
    cells <- grep(r,dimnames(cdata)[[1]])
    fit <- kmeans(cdata[cells,],cpr[r,"clusters"],iter.max=10000)
    getCells(out)[cells] <- paste(getCells(out)[cells],fit$cluster+ccount, sep=".")
    ccount <- ccount + cpr[r,"clusters"]
  }
  set.seed(NULL)
  return(list(
    x = out,
    weight = NULL,
    unit = "1",
    description = "KMeans cluster mapping between cells and clusters",
    isocountries = FALSE))
}
