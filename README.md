
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

withr::with_seed(42, {
  ssd_run_scenario(ssddata::ccme_boron, nsim = 10)
})
#> # A tibble: 20 × 7
#>      sim stream replace  nrow data              fits       hc               
#>    <int>  <int> <lgl>   <int> <list>            <list>     <list>           
#>  1     1      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#>  2     1      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#>  3     2      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#>  4     2      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#>  5     3      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#>  6     3      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#>  7     4      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#>  8     4      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#>  9     5      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#> 10     5      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#> 11     6      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#> 12     6      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#> 13     7      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#> 14     7      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#> 15     8      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#> 16     8      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#> 17     9      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#> 18     9      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
#> 19    10      1 FALSE       6 <tibble [6 × 5]>  <fitdists> <tibble [1 × 15]>
#> 20    10      1 FALSE      10 <tibble [10 × 5]> <fitdists> <tibble [1 × 15]>
```
