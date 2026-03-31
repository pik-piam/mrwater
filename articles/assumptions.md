# mrwater Assumptions

## Overview

The following document provides an overview of assumptions that the
mrwater library is based on.

### Treatment of data mismatches

## Zero Irrigation Water Requirements

The allocation of potentially irrigated areas (PIA) is based on the
total volume of water requirements (sum of water requirements over all
crops grown under the selected cropmix) that is compared to the
available water (see `calcRiverDischargeAllocation`). For the case that
crop-specific irrigation water requirements are reported to be 0 in the
input data, no irrigation area is assigned to these crops in the
algorithm that derives PIA because the assumption is that irrigation is
not required there and therefore rainfed production takes place (see
`calcPotIrrigAreas`).

``` r
print(paste0("Total rainfed area is, ",
              round(sum(rfArea), digits = 2),
              " Mha."))
print(paste0("Of these, ",
             round(sum(rfArea[cropIrrigReq == 0]), digits = 2),
             " Mha have 0 water requirements."))
print(paste0("That is, ",
             round(sum(rfArea[cropIrrigReq == 0]) / sum(rfArea) * 100, digits = 2),
             " % of rainfed land."))
```

However, for the scenario of ‘committed agricultural uses’ where areas
that are already irrigated are reserved for irrigation before the
allocation of additional potentials, areas that are reported to be
irrigated but show 0 irrigation water requirements (ca. 0.43 Mha,
i.e. 0.18% of irrigated area), areas are assigned to be irrigated (see
`calcIrrigAreaActuallyCommitted`) to avoid data mismatches in the area
accounting.

``` r
print(paste0("Total irrigated area is, ",
             round(sum(irArea), digits = 2),
             " Mha."))
print(paste0("Of these, ",
             round(sum(irArea[cropIrrigReq == 0]), digits = 2),
             " Mha have 0 water requirements."))
print(paste0("That is, ",
             round(sum(irArea[cropIrrigReq == 0]) / sum(irArea) * 100, digits = 2),
             " % of irrigated land."))
```
