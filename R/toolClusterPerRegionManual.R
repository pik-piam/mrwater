#' Cluster per Region set manually
#'
#' This function translates weights into number of clusters per region as it
#' is needed for ClusterKMeans. Weights have to sum up to total number of clusters.
#'
#' @param cells spatial names as returned by \code{getCells}
#' @param ncluster The desired total number of clusters.
#' @param ncluster2reg named vector with numbers per region
#'
#' @return A matrix with regions in rows and number of cells and clusters in
#' columns
#' @author Kristine Karstens
#' @seealso \code{\link{calcClusterKMeans}}, \code{\link{calcClusterBase}}

toolClusterPerRegionManual <- function(cells,ncluster,ncluster2reg){

  regions <- unique(sub("\\..*$","",cells))

  if(!setequal(regions, names(ncluster2reg))) stop("regions in 'weight' and regions in regionmapping are not equal.")
  if(sum(ncluster2reg)!=ncluster)             stop("sum of clusters in 'weight' and total cluster number are not equal.")
  if(length(regions) > ncluster)              stop("More regions than cluster. Clustering stopped!")
  if(any(ncluster2reg==0))                    stop("All regions have to have at least one cluster.")

  cpr <- rep(NA,length(regions))
  names(cpr) <- regions
  for(r in regions) cpr[r] <- length(grep(r,cells))
  cpr <- cbind(cpr,ncluster2reg[regions], rep(1,length(regions)))
  dimnames(cpr)[[2]] <- c("cells","clusters","weight")

  return(cpr)
}

