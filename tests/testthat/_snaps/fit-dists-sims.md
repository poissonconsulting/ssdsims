# fit_dists_sims edge cases

    Code
      ssd_fit_dists_sims(data)
    Output
      # A tibble: 1 x 9
          sim stream  nrow args       data     rescale computable at_boundary_ok
        <int>  <int> <int> <list>     <list>   <lgl>   <lgl>      <lgl>         
      1     1      1     6 <list [0]> <tibble> FALSE   FALSE      TRUE          
      # i 1 more variable: fits <list>

# fit_dists_sims no seed

    Code
      ssd_fit_dists_sims(data)
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      # A tibble: 1 x 9
          sim stream  nrow args       data     rescale computable at_boundary_ok
        <int>  <int> <int> <list>     <list>   <lgl>   <lgl>      <lgl>         
      1     1      1     6 <list [0]> <tibble> FALSE   FALSE      TRUE          
      # i 1 more variable: fits <list>

# fit_dists_sims 1 sim

    Code
      fits
    Output
      # A tibble: 1 x 9
          sim stream  nrow args       data     rescale computable at_boundary_ok
        <int>  <int> <int> <list>     <list>   <lgl>   <lgl>      <lgl>         
      1     1      1     6 <list [0]> <tibble> FALSE   FALSE      TRUE          
      # i 1 more variable: fits <list>

