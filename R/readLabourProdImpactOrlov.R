#' @title readLabourProdImpactOrlov
#' @description read in labour productivity impacts from climate change from Orlov (see Orlov et al. 2019. Economic Losses of Heat-Induced Reductions in Outdoor Worker Productivity: a Case Study of Europe. Economics of Disasters and Climate Change, 3(3), 191-211.)
#' @return magpie object of gridded productivity as share of 1 (full productivity)
#' @param subtype subtype of choice between indoor outdoor work, GCM, work intesnsity (300W medium, 400W high, rcp)
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @importFrom magclass as.magpie
#' @importFrom ncdf4 nc_open ncvar_get

readLabourProdImpactOrlov <- function(subtype="IPSL-CM5A-LR_rcp85_wbgtod_hothaps_400W.nc") {

  files <- vector()

  for (od in (c("od","id"))){
            for (int in c("300W", "400W")){
              for (gcm in c("IPSL-CM5A-LR", "GFDL-ESM2M", "HadGEM2-ES")){
                for (rcp in c("rcp26", "rcp60", "rcp85")){
    files <- append(files, paste0(gcm,"_",rcp,"_","wbgt",od,"_","hothaps","_",int,".nc" ))
        }}}}

    names(files) <- files

    file <- toolSubtypeSelect(subtype, files)

  nc <- nc_open(file ,verbose=F)
  years <- ncvar_get(nc, "years")
  buf <-ncvar_get(nc,names(nc$var)[1])
  #define dims
  mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)
  cellNames <- mapping$celliso
  lon <- seq(-179.75,179.75,by=0.5)
  lat <- rev(seq(-89.75,89.75,by=0.5))

  mag <- array(NA,dim=c(59199,length(years),1),dimnames=list(cellNames,paste0("y",years),paste("Productivity",gsub("_",".",file),sep=".")))
  mag <- collapseNames(mag, collapsedim=7)
  for (t in 1:length(years)){
    bif <- buf[,,t]
    for (j in 1:59199) {
        mag[j,t,1] <- bif[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
      }
    }

  x <- as.magpie(mag,spatial=1,temporal=2)

  return(x)

}
