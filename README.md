
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
  ssd_sim_data(ssddata::ccme_boron, nrow = c(6, 10), nsim = 2) |>
    print() |>
    ssd_fit_dists_sims() |>
    print() |>
    ssd_hc_sims(ci_method = c("multi_fixed", "weighted_samples"))
})
#> # A tibble: 4 × 5
#>     sim stream replace  nrow data             
#>   <int>  <int> <lgl>   <dbl> <list>           
#> 1     1      1 FALSE       6 <tibble [6 × 5]> 
#> 2     1      1 FALSE      10 <tibble [10 × 5]>
#> 3     2      1 FALSE       6 <tibble [6 × 5]> 
#> 4     2      1 FALSE      10 <tibble [10 × 5]>
#> # A tibble: 4 × 6
#>     sim stream replace  nrow data              fits      
#>   <int>  <int> <lgl>   <dbl> <list>            <list>    
#> 1     1      1 FALSE       6 <tibble [6 × 5]>  <fitdists>
#> 2     1      1 FALSE      10 <tibble [10 × 5]> <fitdists>
#> 3     2      1 FALSE       6 <tibble [6 × 5]>  <fitdists>
#> 4     2      1 FALSE      10 <tibble [10 × 5]> <fitdists>
#> # A tibble: 8 × 8
#>     sim stream replace  nrow data              fits       ci_method     hc      
#>   <int>  <int> <lgl>   <dbl> <list>            <list>     <chr>         <list>  
#> 1     1      1 FALSE       6 <tibble [6 × 5]>  <fitdists> multi_fixed   <tibble>
#> 2     1      1 FALSE       6 <tibble [6 × 5]>  <fitdists> weighted_sam… <tibble>
#> 3     1      1 FALSE      10 <tibble [10 × 5]> <fitdists> multi_fixed   <tibble>
#> 4     1      1 FALSE      10 <tibble [10 × 5]> <fitdists> weighted_sam… <tibble>
#> 5     2      1 FALSE       6 <tibble [6 × 5]>  <fitdists> multi_fixed   <tibble>
#> 6     2      1 FALSE       6 <tibble [6 × 5]>  <fitdists> weighted_sam… <tibble>
#> 7     2      1 FALSE      10 <tibble [10 × 5]> <fitdists> multi_fixed   <tibble>
#> 8     2      1 FALSE      10 <tibble [10 × 5]> <fitdists> weighted_sam… <tibble>

withr::with_seed(42, {
  ssd_run_scenario(ssddata::ccme_boron, nrow = c(6, 10), ci_method = c("multi_fixed", "weighted_samples"), nsim = 2)
})
#> # A tibble: 8 × 8
#>     sim stream replace  nrow data              fits       ci_method     hc      
#>   <int>  <int> <lgl>   <dbl> <list>            <list>     <chr>         <list>  
#> 1     1      1 FALSE       6 <tibble [6 × 5]>  <fitdists> multi_fixed   <tibble>
#> 2     1      1 FALSE       6 <tibble [6 × 5]>  <fitdists> weighted_sam… <tibble>
#> 3     1      1 FALSE      10 <tibble [10 × 5]> <fitdists> multi_fixed   <tibble>
#> 4     1      1 FALSE      10 <tibble [10 × 5]> <fitdists> weighted_sam… <tibble>
#> 5     2      1 FALSE       6 <tibble [6 × 5]>  <fitdists> multi_fixed   <tibble>
#> 6     2      1 FALSE       6 <tibble [6 × 5]>  <fitdists> weighted_sam… <tibble>
#> 7     2      1 FALSE      10 <tibble [10 × 5]> <fitdists> multi_fixed   <tibble>
#> 8     2      1 FALSE      10 <tibble [10 × 5]> <fitdists> weighted_sam… <tibble>
```
