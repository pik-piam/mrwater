#' @title calcClusterMapping
#' @description This function calculates the aggregation mapping for a given cluster methodology
#' @param type aggregation type, which is a combination of a single letter, indicating the cluster methodology, and a number,
#' indicating the number of resulting clusters. Available methodologies are hierarchical clustering (h), normalized k-means clustering
#' (n) and combined hierarchical/normalized k-means clustering (c). In the latter hierarchical clustering is used to determine the
#' cluster distribution among regions whereas normalized k-means is used for the clustering within a region.
#' @param seed Seed for Random Number Generation. If set to NULL it is chosen automatically, if set to an integer it will
#' always return the same pseudo-random numbers (useful to get identical clusters under identical inputs for n and c
#' clustering)
#' @param weight Should specific regions be resolved with more or less detail? Values > 1 mean higher share, < 1 lower share
#' e.g. cfg$cluster_weight <- c(LAM=2) means that a higher level of detail for region LAM if set to NULL all weights will be
#' assumed to be 1 (examples: c(LAM=1.5,SSA=1.5,OAS=1.5), c(LAM=2,SSA=2,OAS=2))
#' @return magpie object in cellular resolution
#' @author Jan Philipp Dietrich
#'
#' @examples
#' \dontrun{ calcOutput("ClusterMapping", type="c200", aggregate = FALSE) }
#'
#' @importFrom luscale clusterspam

calcClusterMapping <- function(type="c200", seed=42, weight=NULL){

  # read in data which should be used to determine cluster
  yld    <- calcOutput("MAgPIEYields",aggregate=FALSE)
  airrig <- calcOutput("MAgPIEAirrig",aggregate=FALSE)
  td     <- calcOutput("TransportDistance", aggregate=FALSE)



  spam <- clusterspam(lr=0.5,hr=type,ifolder=finput,ofolder=foutput,cfiles=c("lpj_yields_0.5", "lpj_airrig", rep("transport_distance",16)),spatial_header=spatial_header, weight=weight, seed=seed)


  return(list(
    x=spam,
    weight=NULL,
    unit="1",
    description="Mapping between cells and cluster",
    isocountries=FALSE))
}
