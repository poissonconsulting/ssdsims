# fit_dists_sims edge cases

    Code
      ssd_fit_dists_sims(data)
    Output
      # A tibble: 1 x 12
          sim stream  nrow args       data     rescale compu~1 at_bo~2 min_p~3 range~4
        <int>  <int> <int> <list>     <list>   <lgl>   <lgl>   <lgl>   <list>  <list> 
      1     1      1     6 <list [0]> <tibble> FALSE   FALSE   TRUE    <fn>    <dbl>  
      # i abbreviated names: 1: computable, 2: at_boundary_ok, 3: min_pmix,
      #   4: range_shape1
      # i 2 more variables: range_shape2 <list>, fits <list>

# fit_dists_sims no seed

    Code
      ssd_fit_dists_sims(data)
    Output
      # A tibble: 1 x 12
          sim stream  nrow args       data     rescale compu~1 at_bo~2 min_p~3 range~4
        <int>  <int> <int> <list>     <list>   <lgl>   <lgl>   <lgl>   <list>  <list> 
      1     1      1     6 <list [0]> <tibble> FALSE   FALSE   TRUE    <fn>    <dbl>  
      # i abbreviated names: 1: computable, 2: at_boundary_ok, 3: min_pmix,
      #   4: range_shape1
      # i 2 more variables: range_shape2 <list>, fits <list>

# fit_dists_sims 1 sim

    Code
      fits
    Output
      # A tibble: 1 x 12
          sim stream  nrow args       data     rescale compu~1 at_bo~2 min_p~3 range~4
        <int>  <int> <int> <list>     <list>   <lgl>   <lgl>   <lgl>   <list>  <list> 
      1     1      1     6 <list [0]> <tibble> FALSE   FALSE   TRUE    <fn>    <dbl>  
      # i abbreviated names: 1: computable, 2: at_boundary_ok, 3: min_pmix,
      #   4: range_shape1
      # i 2 more variables: range_shape2 <list>, fits <list>

