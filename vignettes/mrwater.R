## ----Setup, echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, echo = TRUE, eval = FALSE, comment = "#>")

## ----Libraries-----------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(madrat)
#  library(magclass)
#  library(mrcommons)
#  library(mrland)
#  library(mrmagpie)

## ----NatDischarge--------------------------------------------------------------------------------------------------------------------------------------------------------
#  natQ <- calcOutput("RiverNaturalFlows", selectyears = 2010, climatetype = "GFDL-ESM4:ssp126",
#                     lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_9ca735cb"), aggregate = FALSE)[, , "discharge_nat"]

## ----EFRs----------------------------------------------------------------------------------------------------------------------------------------------------------------
#  selectyears <- 2010
#  lpjml       <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_bft_6277d36e")
#  climatetype <- "GFDL-ESM4:ssp126"
#
#  efr <- calcOutput("EnvmtlFlowRequirements", efrMethod = "VMF:fair",
#                    selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
#                    aggregate = FALSE)[, , "EFR"]
#
#  inaccessibleQ <- calcOutput("DischargeInaccessible", accessibilityrule = "CV:2",
#                              selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
#                              aggregate = FALSE)
#
#  efrMethod         <- "VMF:fair"
#  accessibilityrule <- "CV:2"

## ----HumanDischarge------------------------------------------------------------------------------------------------------------------------------------------------------
#  humanQ <- calcOutput("RiverDischargeNatAndHuman", iniyear = 2010, com_ag = TRUE,
#                       selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
#                       efrMethod = efrMethod, aggregate = FALSE)
#
#  iniyear <- 2010
#  com_ag  <- TRUE

## ----Landuse-------------------------------------------------------------------------------------------------------------------------------------------------------------
#  croparea <- calcOutput("Croparea", years = iniyear, sectoral = "kcr", physical = TRUE, cells = "lpjcell", cellular = TRUE, irrigation = TRUE, aggregate = FALSE)

## ----AllocationAlgorithm-------------------------------------------------------------------------------------------------------------------------------------------------
#  iwp <- calcOutput("RiverSurplusDischargeAllocation", output = "potIrrigWat",
#                     selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
#                     efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                     iniyear = iniyear, com_ag = com_ag,
#                     allocationrule = "optimization",
#                     rankmethod = "USD_ha:TRUE", yieldcalib = TRUE, cropmix = "hist_total",
#                     thresholdtype = "USD_ha", gainthreshold = 500,
#                     landScen = "potCropland:HalfEarth",
#                     irrigationsystem = "initialization", multicropping = FALSE, aggregate = FALSE)

## ----YieldValueGain------------------------------------------------------------------------------------------------------------------------------------------------------
#  yieldGain <- calcOutput("IrrigYieldImprovementPotential", unit = "USD_ha",
#                          selectyears = selectyears, climatetype = climatetype, lpjml = lpjml, iniyear = iniyear,
#                          cropmix = "hist_total", yieldcalib = TRUE,
#                          multicropping = FALSE, aggregate = FALSE)

## ----IAP-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#  allocationrule   <- "optimization"
#  rankmethod       <- "USD_ha:TRUE"
#  yieldcalib       <- TRUE
#  cropmix          <- "hist_total"
#  thresholdtype    <- "USD_ha"
#  gainthreshold    <- 500
#  landScen         <- "potCropland:HalfEarth"
#  irrigationsystem <- "initialization"
#  multicropping    <- FALSE
#
#  iap <- calcOutput("IrrigatableArea", selectyears = selectyears, iniyear = iniyear,
#                     lpjml = lpjml, climatetype = climatetype,
#                     efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#                     rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
#                     thresholdtype = thresholdtype, gainthreshold = gainthreshold,
#                     irrigationsystem = irrigationsystem, landScen = landScen,
#                     cropmix = cropmix, com_ag = com_ag,
#                     potential_wat = TRUE, multicropping = FALSE, aggregate = FALSE)

## ----EconOfIrrig---------------------------------------------------------------------------------------------------------------------------------------------------------
#  iad <- calcOutput("EconOfIrrig", scenario = "ssp2", output = "IrrigArea",
#               GT_range = c(0, 250, 500, 1000, 2000, 3000), selectyears = 2010, iniyear = 2010,
#               lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, accessibilityrule = accessibilityrule,
#               rankmethod = rankmethod, yieldcalib = yieldcalib,
#               allocationrule = allocationrule, thresholdtype = thresholdtype,
#               irrigationsystem = irrigationsystem, landScen = landScen, cropmix = cropmix,
#               potential_wat = TRUE, com_ag = FALSE, multicropping = FALSE, aggregate = FALSE)
