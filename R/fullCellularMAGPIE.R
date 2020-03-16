#' fullCellularMAGPIE
#'
#' Function that produces the complete cellular data set required for running the
#' MAgPIE model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#'
#' \dontrun{
#' fullMAgPIE(revision=12, mainfolder="pathtowhereallfilesarestored")
#' }

fullCellularMAGPIE <- function(rev=0.1) {

    res_out <- "c200"

    mag_years_past_short <- c("y1995","y2000","y2005","y2010")
    mag_years_past_long  <- c("y1995","y2000","y2005","y2010","y2015")
    mag_years <- findset("time")
    short_years <- findset("t_all")


    map <- calcOutput("Cluster", ctype=res_out, weight=NULL, aggregate=FALSE)
    toolStoreMapping(map,"clustermapping.csv",type="regional",error.existing = FALSE)
    setConfig(regionmapping = "clustermapping.csv")

    # 09 drivers
    ### gridded pop?

    # 14 yields
    # calcOutput("Yields")

    #10 land
    calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, land="fao", input_magpie=TRUE, years=mag_years_past_long, round=6, file="avl_land_t_0.5.mz")
    calcOutput("SeaLevelRise", aggregate=FALSE, round=6, file="f10_SeaLevelRise_0.5.mz")

    #30 crop
    calcOutput("Croparea", aggregate=FALSE, physical=TRUE, cellular=TRUE, irrigation=TRUE, round=6, file="f30_croparea_w_initialisation_0.5.mz")

    #34
    calcOutput("UrbanLandFuture", aggregate=FALSE, round=6, file="f34_UrbanLand_0.5.mz")

    #40
    calcOutput("TransportDistance", aggregate=FALSE, round=6, file="transport_distance_0.5.mz" )
    #41 water
    calcOutput("AreaEquippedForIrrigation", aggregate=FALSE, cellular=TRUE, years=mag_years_past_short, round=6, file="avl_irrig_luh_t_0.5.mz")
    # WATER!

    ## this one needed?
    calcOutput("Avl_irrig", aggregate=FALSE, cellular=TRUE, round=6, file="avl_irrig_0.5.mz")

    #50 nitrogen
    calcOutput("AtmosphericDepositionRates", cellular=TRUE, aggregate=FALSE, round=6, file="f50_AtmosphericDepositionRates_0.5.mz")
    calcOutput("NitrogenFixationRateNatural", aggregate=FALSE, round=6, file="f50_NitrogenFixationRateNatural_0.5.mz")

    #52 carbon
    #CARBON!

    #59 som
    calcOutput("SOMinitialsiationPools", aggregate = FALSE, round=6, file="f59_som_initialisation_pools_0.5.mz")

    ## OTHER ##

    calcOutput("CalibratedArea", aggregate=FALSE, round=6, file="calibrated_area_0.5.mz" )
    calcOutput("ProtectArea", aggregate=FALSE, round=6, file="protect_area_0.5.mz" )



    ##### AGGREGATION ######

    # create info file
    writeInfo <- function(file,lpjml_data, res_high, res_out, rev) {
      functioncall <- paste(deparse(sys.call(-1)), collapse = "")

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
      cat(info,file=file,sep='\n')
    }
    writeInfo(file='info.txt', lpjml_data="default", res_high="0.5", res_out=res_out, rev=rev)






}
