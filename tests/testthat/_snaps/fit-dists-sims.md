# fit_dists_sims edge cases

    Code
      ssd_fit_dists_sims(data)
    Output
      # A tibble: 1 x 12
          sim stream  nrow args   data     rescale computable at_boundary_ok min_pmix
        <int>  <int> <int> <list> <list>   <lgl>   <lgl>      <lgl>          <list>  
      1     1      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
      # i 3 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>

# fit_dists_sims no seed

    Code
      ssd_fit_dists_sims(data)
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      # A tibble: 1 x 12
          sim stream  nrow args   data     rescale computable at_boundary_ok min_pmix
        <int>  <int> <int> <list> <list>   <lgl>   <lgl>      <lgl>          <list>  
      1     1      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
      # i 3 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>

# fit_dists_sims 1 sim

    Code
      fits
    Output
      # A tibble: 1 x 12
          sim stream  nrow args   data     rescale computable at_boundary_ok min_pmix
        <int>  <int> <int> <list> <list>   <lgl>   <lgl>      <lgl>          <list>  
      1     1      1     6 <list> <tibble> FALSE   FALSE      TRUE           <fn>    
      # i 3 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>

