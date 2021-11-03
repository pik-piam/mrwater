## ----Setup, echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, echo = TRUE, eval = FALSE, comment = "#>")

library(mrwater)

## ----RiverStructure, eval = TRUE, echo = TRUE-------------------------------------------------------------------------------------------------------------------------------------
rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))
str(rs)

## ----Basins, eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------------------------------------------
mississippi <- toolSelectRiverBasin(basinname = "Mississippi") 
str(mississippi)

