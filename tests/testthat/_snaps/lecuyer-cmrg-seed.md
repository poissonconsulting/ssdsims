# get_lecuyer_cmrg_seeds_stream repeatable

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seeds_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      [[1]]
      [1]       10407 -1852613690  1473416771  -413238268    -5614164   596817356
      [7] -2081937358
      

# get_lecuyer_cmrg_seeds_stream seed fast enough

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seeds_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 10^5))
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      [[1]]
      [1]       10407  1518849669  1286614382 -2054563069 -1947811657  -510186063
      [7]   -76299841
      

# get_lecuyer_cmrg_seeds_stream stream fast enough

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seeds_stream(seed = NULL, nsim = 1L,
        start_sim = 1L, stream = 10^5))
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      [[1]]
      [1]       10407 -1057094955  -361067631   -29809164 -1935507648 -1558905348
      [7]   568944148
      

# get_lecuyer_cmrg_seeds_stream seed stream fast enough

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seeds_stream(seed = NULL, nsim = 1L,
        start_sim = 10^5, stream = 10^5))
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      [[1]]
      [1]       10407  -897834042  1537537592 -1934295747  -803525422     2072687
      [7]  1248657956
      

# get_lecuyer_cmrg_seeds_stream seeds stream fast enough

    Code
      seeds[[10^5]]
    Output
      [1]      10407 -817513537  351062398 2023775657 -874340457 -296760353 -794966047

# get_lecuyer_cmrg_seeds_stream repeatable multiple seeds

    Code
      withr::with_seed(10, get_lecuyer_cmrg_seeds_stream(seed = NULL, nsim = 2L,
        stream = 1L, start_sim = 1L))
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      [[1]]
      [1]       10407 -1852613690  1473416771  -413238268    -5614164   596817356
      [7] -2081937358
      
      [[2]]
      [1]      10407 -523009245 2009368735  379397622   26024764 -450628420 -210045713
      

# get_lecuyer_cmrg_seeds_stream repeatable other starts

    Code
      withr::with_seed(42, get_lecuyer_cmrg_seeds_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      [[1]]
      [1]       10407  1812818762   957998414 -1062960512   738666160 -1560237390
      [7]  1852499593
      

# get_lecuyer_cmrg_seeds_stream seeds repeatable with other seed types

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_seeds_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Condition
      Warning in `RNGkind()`:
      '.Random.seed' is not an integer vector but of type 'NULL', so ignored
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      

