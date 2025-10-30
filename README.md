
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ssdsims

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/poissonconsulting/ssdsims/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/poissonconsulting/ssdsims/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ssdsims is to facilitate simulation studies with species
sensitivity distribution data.

## Installation

``` r
library(ssdsims)

withr::with_seed(42, {
  ssd_simulate_data(ssddata::ccme_boron, nsim =10) |>
    ssd_fit_dists_sims() |>
    ssd_hc_sims()
})
#> # A tibble: 10 × 5
#>      sim stream data             fits       hc               
#>    <int>  <int> <list>           <list>     <list>           
#>  1     1      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  2     2      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  3     3      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  4     4      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  5     5      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  6     6      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  7     7      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  8     8      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#>  9     9      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
#> 10    10      1 <tibble [6 × 5]> <fitdists> <tibble [1 × 15]>
```
