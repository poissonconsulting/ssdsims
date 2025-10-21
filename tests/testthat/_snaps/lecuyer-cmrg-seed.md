# get_lecuyer_cmrg_seed_stream repeatable

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seed_stream())
    Output
      [[1]]
      [1]       10407 -1852613690  1473416771  -413238268    -5614164   596817356
      [7] -2081937358
      

# get_lecuyer_cmrg_seed_stream repeatable multiple seeds

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seed_stream(nseed = 2L))
    Output
      [[1]]
      [1]       10407 -1852613690  1473416771  -413238268    -5614164   596817356
      [7] -2081937358
      
      [[2]]
      [1]      10407 -523009245 2009368735  379397622   26024764 -450628420 -210045713
      

# get_lecuyer_cmrg_seed_stream repeatable other starts

    Code
      withr::with_seed(42, get_lecuyer_cmrg_seed_stream())
    Output
      [[1]]
      [1]       10407  1812818762   957998414 -1062960512   738666160 -1560237390
      [7]  1852499593
      

# get_lecuyer_cmrg_seed_stream seeds repeatable with other seed types

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_seed_stream())
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      

