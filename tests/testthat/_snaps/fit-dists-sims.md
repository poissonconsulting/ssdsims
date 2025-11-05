# fit_dists_sims edge cases

    Code
      ssd_fit_dists_sims(data)
    Output
      # A tibble: 1 x 6
          sim stream args        nrow data             fits      
        <int>  <int> <list>     <int> <list>           <list>    
      1     1      1 <list [0]>     6 <tibble [6 x 1]> <fitdists>

# fit_dists_sims no seed

    Code
      ssd_fit_dists_sims(data)
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      # A tibble: 1 x 6
          sim stream args        nrow data             fits      
        <int>  <int> <list>     <int> <list>           <list>    
      1     1      1 <list [0]>     6 <tibble [6 x 1]> <fitdists>

# fit_dists_sims 1 sim

    Code
      fits
    Output
      # A tibble: 1 x 6
          sim stream args        nrow data             fits      
        <int>  <int> <list>     <int> <list>           <list>    
      1     1      1 <list [0]>     6 <tibble [6 x 1]> <fitdists>

