# hc_sims 1 sim ci bootstrap interval (dev withr only)

    Code
      ci
    Output
            se      lcl      ucl 
      0.219423 0.289166 0.583962 

# hazard-concentrations: a vector ci is rejected

    Code
      ssd_hc_sims(sims, ci = c(FALSE, TRUE))
    Condition
      Error in `ssd_hc_sims()`:
      ! `ci` must be a flag (TRUE or FALSE).

