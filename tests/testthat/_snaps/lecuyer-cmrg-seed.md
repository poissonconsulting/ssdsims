# get_lecuyer_cmrg_seed_stream repeatable

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seed_stream())
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      

# get_lecuyer_cmrg_seed_stream repeatable multiple seeds

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seed_stream(nseed = 2L))
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      
      [[2]]
      [1]       10407   125608557 -1794459080 -1494058990  -993642217 -1807090181
      [7]   734238052
      

# get_lecuyer_cmrg_seed_stream repeatable other starts

    Code
      withr::with_seed(42, get_lecuyer_cmrg_seed_stream())
    Output
      [[1]]
      [1]       10407  -922606465   891908805  2065540205  1561987267  1325379220
      [7] -1522797297
      

# get_lecuyer_cmrg_seed_stream seeds repeatable with other seed types

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_seed_stream())
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      

