#' @title calcClusterMapping
#' @description This function calculates the aggregation mapping for a given cluster methodology
#' @param type aggregation type, which is a combination of a single letter, indicating the cluster methodology, and a number,
#' indicating the number of resulting clusters. Available methodologies are hierarchical clustering (h), normalized k-means clustering
#' (n) and combined hierarchical/normalized k-means clustering (c). In the latter hierarchical clustering is used to determine the
#' cluster distribution among regions whereas normalized k-means is used for the clustering within a region.
#' @param regionscode regionscode of the regional mapping to be used. Must agree with the regionscode of the mapping
#' mentioned in the madrat config! Can be retrieved via \code{regionscode()}.
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
#' @importFrom luscale mag_kmeans mag_hierarchical
#' @importFrom madrat toolMappingFile regionscode
#' @importFrom moinput spatial_header

calcClusterMapping <- function(type, regionscode=madrat::regionscode(), seed=42, weight=NULL){

  mode <- substr(lr,0,1)
  ncluster <- as.integer(substring(lr,2))

  cdata <- calcOutput("ClusterBase", aggregate=FALSE)

  ### APPLY REGIONS HERE ON SPATIAL NAMING OF CDATA INSTEAD OF COUNTRIES ###
  ### regionscode needs to be checked and provided as argument to ensure
  ### that caching is not mixing up aggregations with different regional
  ### mapping.
  map <- toolMappingFile("regional",getConfig("regionmapping"),readcsv = TRUE)
  if(regionscode!=regionscode(map)) stop("Provided regionscode does not match regionscode of regional mapping!")
  getCells(cdata) <- spatial_header(map)


  if(mode=="n") {
    spam <- mag_kmeans(cdata,ncluster,weight,seed=seed)
  } else if(mode=="h" | mode=="w" | mode=="s") {
    # NEED TO FIND A WAY TO GET RID OF IFOLDER AND TO STORE CLUSTER TREE VIA MADRAT INSTEAD
    spam <- mag_hierarchical(cdata,ncluster,ifolder,mode,weight)
  } else if(mode=="c"){
    calcCPR <- function(spam, cell2reg) {
      reg <- unique(cell2reg)
      cluster2reg <- as.factor(spam%*%as.numeric(cell2reg)/rowSums(spam))
      levels(cluster2reg) <- levels(cell2reg)
      cpr <- t(rbind(table(cell2reg),table(cluster2reg)))
      dimnames(cpr)[[2]] <- c("cells","clusters")
      return(cpr)
    }
    tmpspam <- mag_hierarchical(cdata,ncluster,ifolder,mode="h",weight)
    cell2reg <- as.factor(sub("\\..*$","",dimnames(cdata)[[1]]))
    spam <- mag_kmeans(cdata,cpr=calcCPR(tmpspam,cell2reg),seed=seed)
  } else {
    stop("Unkown clustering mode ",mode,"!")
  }
  wkey <- ifelse(is.null(weight), "", gsub(".","",paste0("_",names(weight),weight,collapse=""),fixed=TRUE))

  # !!! HOW TO FORWARD NAME INFORMATION? !!! #
  #write.spam(spam,path(ofolder,paste(hr,"-to-",lr,wkey,"_sum.spam",sep="")))
  #saveRDS(spam2mapping(spam,rownames(cdata)), path(ofolder,paste(hr,"-to-",lr,wkey,"_mapping.rds",sep="")))

  # !!! IN WHICH FORMAT SHOULD THE DATA BE RETURNED? !!! #

  return(list(
    x=spam,
    weight=NULL,
    unit="1",
    description="Mapping between cells and cluster",
    isocountries=FALSE))
}
