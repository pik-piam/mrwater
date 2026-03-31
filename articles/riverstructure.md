# mrwater River Structure

## Overview

The `mrwater` library is based on the STN river structure (Vörösmarty et
al. 2000, 2011) and the CRU gridded land mask (Harris et al. 2014,
2021).

## Data

### River structure

The river structure is stored in the form of a sorted list of objects in
the folder `inst/extdata` and can be accessed with the function call
`rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))`.
For each of the 67420 grid cells, `rs$nextcell` contains the cell that
comes after the respective grid cell (i.e. cell to which discharge of
current cell flows (exactly 1 cell)). Values of -1 indicate that there
is no downstream cell (i.e. the respective cell is the last of the river
basin). `rs$downstreamcells` (`rs$upstreamcells`) contains the list of
downstreamcells (upstreamcells) of the respective grid cell (i.e. all
cells that are downstream (upstream) of current cell (list of cells)).
Values of 0 indicate that there is no downstream (upstream) cell.
`rs$endcell` provides the last downstream cell to the respective cell
(i.e. estuary cell of current cell, i.e. last cell of the river of which
current cell is part of (exactly 1 cell)). It indicates which cells
belong to one river basin. `rs$calcorder` indicates the ordering of
cells for calculation from upstream to downstream. Grid cells with the
same calcorder can be calculated in parallel. `rs$coordinates` provides
the coordinates (lon.lat) to the respective grid cell.

``` r
rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))
str(rs)
#> List of 7
#>  $ nextcell       : num [1:67420] -1 -1 -1 13 6 14 15 9 -1 -1 ...
#>  $ downstreamcells:List of 67420
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:4(1d)] 13 26 38 39
#>   ..$ : num [1:5(1d)] 6 14 26 38 39
#>   ..$ : num [1:4(1d)] 14 26 38 39
#>   ..$ : num [1:4(1d)] 15 27 38 39
#>   ..$ : num 9
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 10
#>   ..$ : num(0) 
#>   ..$ : num [1:3(1d)] 26 38 39
#>   ..$ : num [1:3(1d)] 26 38 39
#>   ..$ : num [1:3(1d)] 27 38 39
#>   ..$ : num [1:3(1d)] 28 38 39
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:2(1d)] 31 30
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:3(1d)] 26 38 39
#>   ..$ : num [1:2(1d)] 38 39
#>   ..$ : num [1:2(1d)] 38 39
#>   ..$ : num [1:2(1d)] 38 39
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 30
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 33
#>   ..$ : num 24
#>   ..$ : num [1:3(1d)] 26 38 39
#>   ..$ : num [1:2(1d)] 38 39
#>   ..$ : num 39
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 40
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:2(1d)] 34 33
#>   ..$ : num [1:2(1d)] 35 24
#>   ..$ : num [1:3(1d)] 37 38 39
#>   ..$ : num [1:2(1d)] 38 39
#>   ..$ : num 39
#>   ..$ : num(0) 
#>   ..$ : num [1:2(1d)] 41 40
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 54
#>   ..$ : num [1:3(1d)] 45 34 33
#>   ..$ : num [1:3(1d)] 58 67 76
#>   ..$ : num [1:2(1d)] 67 76
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 64
#>   ..$ : num [1:2(1d)] 67 76
#>   ..$ : num 76
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 64
#>   ..$ : num [1:2(1d)] 65 64
#>   ..$ : num [1:2(1d)] 75 76
#>   ..$ : num 76
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:3(1d)] 88 89 90
#>   ..$ : num [1:3(1d)] 88 89 90
#>   ..$ : num [1:2(1d)] 89 90
#>   ..$ : num 83
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 85
#>   ..$ : num [1:3(1d)] 88 89 90
#>   ..$ : num [1:2(1d)] 89 90
#>   ..$ : num 90
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 93
#>   ..$ : num [1:3(1d)] 96 97 98
#>   ..$ : num [1:2(1d)] 97 98
#>   ..$ : num 98
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   .. [list output truncated]
#>  $ upstreamcells  :List of 67420
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 5
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 8
#>   ..$ : num 11
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 4
#>   ..$ : num [1:2] 5 6
#>   ..$ : num 7
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:2] 35 46
#>   ..$ : num(0) 
#>   ..$ : num [1:7] 4 5 6 13 14 25 36
#>   ..$ : num [1:2] 7 15
#>   ..$ : num 16
#>   ..$ : num(0) 
#>   ..$ : num [1:2] 19 31
#>   ..$ : num 19
#>   ..$ : num(0) 
#>   ..$ : num [1:3] 34 45 56
#>   ..$ : num [1:2] 45 56
#>   ..$ : num 46
#>   ..$ : num(0) 
#>   ..$ : num 47
#>   ..$ : num [1:16] 4 5 6 7 13 14 15 16 25 26 ...
#>   ..$ : num [1:18] 4 5 6 7 13 14 15 16 25 26 ...
#>   ..$ : num [1:2] 41 51
#>   ..$ : num 51
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 56
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 55
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 57
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:3] 65 72 73
#>   ..$ : num 73
#>   ..$ : num(0) 
#>   ..$ : num [1:3] 57 58 66
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 74
#>   ..$ : num [1:6] 57 58 66 67 74 75
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 82
#>   ..$ : num(0) 
#>   ..$ : num 86
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num [1:3] 79 80 87
#>   ..$ : num [1:5] 79 80 81 87 88
#>   ..$ : num [1:6] 79 80 81 87 88 89
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 94
#>   ..$ : num(0) 
#>   ..$ : num(0) 
#>   ..$ : num 95
#>   ..$ : num [1:2] 95 96
#>   ..$ : num [1:9] 95 96 97 104 105 106 107 116 117
#>   ..$ : num(0) 
#>   .. [list output truncated]
#>  $ endcell        : num [1:67420(1d)] 1 2 3 39 39 39 39 9 9 10 ...
#>  $ calcorder      : num [1:67420(1d)] 1 1 1 2 1 2 2 1 2 2 ...
#>  $ cells          : chr [1:67420] "FJI.1" "RUS.2" "RUS.3" "RUS.4" ...
#>  $ coordinates    : chr [1:67420] "-179p75.-16p25" "-179p75.65p25" "-179p75.65p75" "-179p75.66p25" ...
```

### Basin attribution

While `rs$endcell` can be used to uniquely identify which grid cells
belong to one common basin, it does not provide the name of the river
basin. To identify specific river basins, the `toolSelectRiverBasin`
function can be used to select all grid cells that belong to one
specific river basin that is listed in the `RiverBasinMapping.csv`
stored in `inst/extdata`. It returns a list of grid cells including
their coordinates and country attribution.

``` r
mississippi <- toolSelectRiverBasin(basinname = "Mississippi") 
str(mississippi)
#>  chr [1:1372] "-113p75.44p75.USA" "-113p25.45p25.USA" ...
```
