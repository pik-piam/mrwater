#' @title readLabourProdImpactOrlov
#' @description read in labour productivity impacts from climate change from Orlov (see Orlov et al. 2019. Economic Losses of Heat-Induced Reductions in Outdoor Worker Productivity: a Case Study of Europe. Economics of Disasters and Climate Change, 3(3), 191-211.)
#' @return magpie object of gridded productivity as share of 1 (full productivity)
#' @param gcm option of 3 gcms IPSL-CM5A-LR, HadGEM2-ES, GFDL-ESM2M
#' @param rcp option of rcp26,rcp60, rcp85
#' @param outdoor outdoor or indoor (shady) work
#' @param intensity high or low - 400W is high intensity 300W moderate industrial intensity
#' @param hothaps method of translating heat into labour prod. hothaps (epidemiological) or iso (standards and working time based)
#' @author David Chen
#' @seealso \code{\link{readSource}}
#' @importFrom magclass as.magpie
#' @importFrom ncdf4 nc_open ncvar_get

readLabourProdImpactOrlov <- function(outdoor= TRUE, intensity = "high", hothaps=TRUE, gcm="IPSL-CM5A-LR", rcp="rcp85") {

  path <- paste0("./ISIMIP/",gcm, "_", rcp, "/")

    if (outdoor==TRUE){
    od <- "od" } else {
    od <- "id"}
  if(intensity == "high") {
    int <- "400W" } else if (intensity=="low"){
    int <- "300W"}
  if(hothaps == TRUE) {
    hothaps <- "hothaps"} else {
      hothaps ="iso"}

  nc <- nc_open(paste0(path,gcm,"_",rcp,"_","wbgt",od,"_",hothaps,"_",int,".nc" ) ,verbose=F)
  years <- ncvar_get(nc, "years")
  buf <-ncvar_get(nc,names(nc$var)[1])
  #define dims
  mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)
  cellNames <- mapping$celliso
  lon <- seq(-179.75,179.75,by=0.5)
  lat <- rev(seq(-89.75,89.75,by=0.5))

  mag <- array(NA,dim=c(59199,length(years),1),dimnames=list(cellNames,paste0("y",years),paste("Productivity",gcm,rcp,od,int,hothaps,sep=".")))

  for (t in 1:length(years)){
    bif <- buf[,,t]
    for (j in 1:59199) {
        mag[j,t,1] <- bif[which(magpie_coord[j, 1]==lon), which(magpie_coord[j,2]==lat)]
      }
    }

  x <- as.magpie(mag,spatial=1,temporal=2)

  return(x)

}
