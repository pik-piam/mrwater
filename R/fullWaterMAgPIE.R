#' @title fullWaterMAgPIE
#' @description Function that produces the complete water-related cellular data required for running the MAgPIE model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @param ctype aggregation clustering type, which is a combination of a single letter, indicating the cluster methodology, and a number,
#' indicating the number of resulting clusters. Available methodologies are hierarchical clustering (h), normalized k-means clustering
#' (n) and combined hierarchical/normalized k-means clustering (c). In the latter hierarchical clustering is used to determine the
#' cluster distribution among regions whereas normalized k-means is used for the clustering within a region.
#' @param dev development suffix to distinguish development versions for the same data revision. This can be useful to distinguish
#' parallel lines of development.
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param clusterweight Should specific regions be resolved with more or less detail? Values > 1 mean higher share, < 1 lower share
#' e.g. cfg$clusterweight <- c(LAM=2) means that a higher level of detail for region LAM if set to NULL all weights will be assumed to be 1.
#' examples:
#' c(LAM=1.5,SSA=1.5,OAS=1.5)
#' c(LAM=2,SSA=2,OAS=2)
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Felicitas Beier, Jan Philipp Dietrich
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#'
#' \dontrun{
#' retrieveData("CELLULARMAGPIE", revision=12, mainfolder="pathtowhereallfilesarestored")
#' }
#' @importFrom madrat setConfig getConfig
#' @importFrom magpiesets findset

fullWaterMAgPIE <- function(rev=0.1, dev="", ctype="c200", climatetype="HadGEM2_ES:rcp2p6:co2", clusterweight=NULL) {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+12)
  on.exit(options(magclass_sizeLimit=sizelimit))

  setConfig(debug=TRUE)

  # mag_years_past_short <- c("y1995","y2000","y2005","y2010")
  # mag_years_past_long  <- c("y1995","y2000","y2005","y2010","y2015")
  # mag_years <- findset("time")
  # short_years <- findset("t_all")
  lpj_years <- seq(1995, 2100,by=5)
  #
  # ### test settings (will be loaded from config in fina version)
  # climatetype=climatetype
  harmonize_baseline="CRU_4"
  ref_year="y2015"
  iniyear=1995

  #### THIS MIGHT BE NECESSARY IF I WANT MY OWN AGGREGATION (E.G. TO RIVER BASINS)
 # map <- calcOutput("Cluster", ctype=ctype, weight=clusterweight, clusterdata=clusterdata, aggregate=FALSE)
  #weightID <- ifelse(is.null(clusterweight),"",paste0("_",names(clusterweight),clusterweight,collapse=""))
  #clustermapname <- sub("\\.[^.]*$",".rds",paste0("clustermap_rev",rev,dev,"_",ctype,weightID,"_",getConfig("regionmapping")))
  #toolStoreMapping(map,clustermapname,type="regional",where=c("mappingfolder","outputfolder"),error.existing = FALSE)
  #setConfig(extramappings = clustermapname)


  #41 area equipped for irrigation
  #calcOutput("AreaEquippedForIrrigation", aggregate="cluster", cellular=TRUE, source="Siebert", round=6, file=paste0("avl_irrig_", ctype, ".mz"))
  #calcOutput("AreaEquippedForIrrigation", aggregate="cluster", cellular=TRUE, source="LUH2v2",  selectyears=mag_years_past_long, round=6, file=paste0("avl_irrig_luh_t_", ctype, ".mz"))

  # KEEP IN MRMAGPIE OR MOVE TO MRWATER?????

  ##### CREATE FILES FOR FIRST TEST RUN ######
  setConfig(extramappings="clustermap_rev4.54+mrmagpie10_riverrouting_allocation_c200_h12.rds")

  #42 water demand
  wat_req_crops_c <- calcOutput("ActualIrrigWatRequirements", selectyears=lpj_years, climatetype="GSWP3-W5E5:historical", iniyear=iniyear, aggregate="cluster", round=6)
  wat_req_crops_c <- collapseNames(wat_req_crops_c)
  write.magpie(wat_req_crops_c, file_name = "C:/Users/beier/Documents/Tasks/MAgPIE tasks/Sim4Nexus/wat_req_crops_c.cs2")

  #43 water availability
  avl_wat_agr_c <- calcOutput("WaterAllocation", selectyears=lpj_years, output="consumption", climatetype="GSWP3-W5E5:historical", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=FALSE, ref_year=NULL, finalcells="magpiecell",
             allocationrule="optimization", allocationshare=NULL, thresholdtype=TRUE, gainthreshold=1, irrigationsystem="initialization", iniyear=1995, aggregate="cluster", round=6)
  avl_wat_agr_c <- collapseNames(avl_wat_agr_c)
  write.magpie(avl_wat_agr_c, file_name="C:/Users/beier/Documents/Tasks/MAgPIE tasks/Sim4Nexus/avl_wat_agr_c.cs3")


  #### -- only temporary -- ####

  #42 water demand
  calcOutput("ActualIrrigWatRequirements", selectyears=lpj_years, climatetype="GSWP3-W5E5:historical", iniyear=iniyear, aggregate="cluster", round=6, file=paste0("wat_req_crops_c_",ctype,".mz"))

  #43 water availability
  calcOutput("WaterAllocation", selectyears=lpj_years, output="consumption", climatetype="GSWP3-W5E5:historical", time="spline", averaging_range=NULL, dof=4, harmonize_baseline=harmonize_baseline, ref_year=ref_year, finalcells="magpiecell",
               allocationrule="optimization", allocationshare=NULL, thresholdtype=TRUE, gainthreshold=1, irrigationsystem="initialization", iniyear=1995, aggregate="cluster", round=6, file=paste0("avl_wat_agr_c_",ctype,".mz"))

  ##### AGGREGATION ######

  # create info file
  writeInfo <- function(file,lpjml_data, res_high, res_out, rev) {
    functioncall <- paste(deparse(sys.call(-2)), collapse = "")

    map <- toolMappingFile("regional", getConfig("regionmapping"),
                           readcsv = TRUE)
    regionscode <- regionscode(map)

    info <- c('lpj2magpie settings:',
              paste('* LPJmL data:',lpjml_data),
              paste('* Revision:', rev),
              '','aggregation settings:',
              paste('* Input resolution:',res_high),
              paste('* Output resolution:',res_out),
              paste('* Regionscode:',regionscode),
              paste('* Call:', functioncall))
    base::cat(info,file=file,sep='\n')
  }
  writeInfo(file=paste0(getConfig("outputfolder"),'/info.txt'), lpjml_data=climatetype, res_high="0.5", res_out=ctype, rev=rev)

}
