<!-- README.md is generated from README.Rmd. Please edit that file -->
# agronomymap
<!-- badges: start -->
<!-- badges: end -->

The goal of the agronomymap package is to generate an interpolated spatial heatmap of agronomic field trial data using row/column coordinates 
and a chosen measurement variable (e.g., SOC, yield, biomass). Points are plotted on top of a raster surface, and axes may optionally be labeled 
or hidden to produce clean field maps for publication.

□ Code detailed:  </br>
□ Website:  </br>
□ Contact: kimjk@agronomy4future.com </br>

## Installation
You can install agronomymap() like so:
Before installing, please download Rtools (https://cran.r-project.org/bin/windows/Rtools)

``` r
if(!require(remotes)) install.packages("remotes")
if (!requireNamespace("agronomymap", quietly = TRUE)) {
 remotes::install_github("agronomy4future/agronomymap", force= TRUE)
}
library(remotes)
library(agronomymap)
```
## Code practice
``` r
# data upload
if(!require(remotes)) install.packages("readr")
library (readr)
github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/refs/heads/main/SOC.csv"
df=data.frame(read_csv(url(github),show_col_types= FALSE))

print(head(df,3))
  Column Row  SOC
1      1   1 1.45
2      2   1 1.60
3      3   1 2.34
.
.
.

## Basic field heatmap
agronomymap(df,
            map= c("Column","Row"),
            variable= "SOC")

# With styled format
agronomymap(
 data= df,
 map= c("Column","Row"),
 variable= "SOC",
 # fill scale
 grid_res = 100,
 palette= "Blues",
 fill_limits= c(0, 4),
 fill_breaks= seq(0, 4, 1),
 # label
 label_title= "Yield (kg/plant)",
 label_size= 12,
 label_key= 12,
 label_position= "right",
 label_key_height= 1.2,
 label_key_width= 0.7,
 xlab= "Row",
 ylab= "Column",
 # point aesthetics
 point_size= 1,
 point_shape= 21,
 point_color= "black",
 point_fill= "grey25",
 #border and axis unit
 add_border= TRUE,
 axis_units= TRUE
)



