#' @importFrom madrat vcat toolCodeLabels
#' @importFrom digest digest

.onLoad <- function(libname, pkgname){
  madrat::setConfig(packages=c(madrat::getConfig("packages"),pkgname), .cfgchecks=FALSE, .verbose=FALSE)

  # add labels for common ctype selections
  labels <- NULL
  for(t in c("c","n","h")) {
    ncells <- c(seq(10,90,10),seq(100,900,100),seq(1000,10000,1000))
    for(n in ncells) {
      tmp <- paste0(t,n)
      labels[tmp] <- digest::digest(list(ctype=tmp),"md5")
    }
  }
  toolCodeLabels(add=labels)
}

#create an own warning function which redirects calls to vcat (package internal)
warning <- function(...) vcat(0,...)

# create a own stop function which redirects calls to stop (package internal)
stop <- function(...) vcat(-1,...)

# create an own cat function which redirects calls to cat (package internal)
cat <- function(...) vcat(1,...)
