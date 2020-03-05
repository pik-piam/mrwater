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


    mag_years_past_short <- c("y1995","y2000","y2005","y2010")
    mag_years_past_long  <- c("y1995","y2000","y2005","y2010","y2015")
    mag_years <- findset("time")
    short_years <- findset("t_all")

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

    #41 water
    calcOutput("AreaEquippedForIrrigation", aggregate=FALSE, cellular=TRUE, years=mag_years_past_short, round=6, file="avl_irrig_luh_t_0.5.mz")
    # WATER!


    #50 nitrogen
    calcOutput("AtmosphericDepositionRates", cellular=TRUE, aggregate=FALSE, round=6, file="f50_AtmosphericDepositionRates_0.5.mz")
    calcOutput("NitrogenFixationRateNatural", aggregate=FALSE, round=6, file="f50_NitrogenFixationRateNatural_0.5.mz")

    #52 carbon
    #CARBON!

    #59 som
    calcOutput("SOMinitialsiationPools", aggregate = FALSE, round=6, file="f59_som_initialisation_pools_0.5.mz")

}
