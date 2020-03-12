#' Cluster per Region
#'
#' This function calculates an appropriate number of clusters per region as it
#' is needed for ClusterKMeans
#'
#' @param cells spatial names as returned by \code{getCells}
#' @param ncluster The desired total number of clusters.
#' @param weight named vector with weighting factors for each region for the cluster distribution
#' ,e.g. weight=c(AFR=3,EUR=0.5). weight > 1 will grant more cluster to a region and
#' weight < 1 less cluster than by default.
#' @return A matrix with regions in rows and number of cells and clusters in
#' columns
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcClusterKMeans}}, \code{\link{calcClusterBase}}

toolClusterPerRegion <- function(cells,ncluster,weight=NULL){
  regions <- unique(sub("\\..*$","",cells))
  if(length(regions) > ncluster) stop("More regions than cluster. Clustering stopped!")
  calcw <- function(weight, regions) {
    w <- rep(1,length(regions))
    names(w) <- regions
    if(!is.null(weight)) w[names(weight)] <- weight
    return(w)
  }
  cpr <- rep(NA,length(regions))
  names(cpr) <- regions
  for(r in regions) cpr[r] <- length(grep(r,cells))
  cpr <- cbind(cpr,cpr,calcw(weight,regions))
  dimnames(cpr)[[2]] <- c("cells","clusters","weight")
  cpr[,"clusters"] <- round(cpr[,"cells"]*cpr[,"weight"]/sum(cpr[,"cells"]*cpr[,"weight"])*ncluster)
  cpr[cpr[,"clusters"]==0,"clusters"] <- 1
  while(sum(cpr[,"clusters"])!=ncluster) {
    correct <- ncluster-sum(cpr[,"clusters"])
    m <- which(cpr[,"clusters"]==max(cpr[,"clusters"]))[1]
    cpr[m,"clusters"] <- cpr[m,"clusters"] + sign(correct)
  }
  return(cpr)
}

