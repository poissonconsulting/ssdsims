# get_lecuyer_cmrg_states_stream repeatable

    Code
      withr::with_seed(10, get_lecuyer_cmrg_states_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      

# get_lecuyer_cmrg_states_stream seed fast enough

    Code
      withr::with_seed(10, get_lecuyer_cmrg_states_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 10^5))
    Output
      [[1]]
      [1]       10407    96217785   989876424   891017701   949736748 -1097908781
      [7] -1427362856
      

# get_lecuyer_cmrg_states_stream stream fast enough

    Code
      withr::with_seed(10, get_lecuyer_cmrg_states_stream(seed = NULL, nsim = 1L,
        start_sim = 1L, stream = 10^5))
    Output
      [[1]]
      [1]       10407   432652119  -575981330 -1961021926 -1445990401   245217999
      [7]  1725202986
      

# get_lecuyer_cmrg_states_stream seed stream fast enough

    Code
      withr::with_seed(10, get_lecuyer_cmrg_states_stream(seed = NULL, nsim = 1L,
        start_sim = 10^5, stream = 10^5))
    Output
      [[1]]
      [1]       10407  -624102422   647544610  -953698482 -1630547158 -1602554452
      [7]   248690873
      

# get_lecuyer_cmrg_states_stream seeds stream fast enough

    Code
      seeds[[10^5]]
    Output
      [1]       10407  1222159184  2127954179   -48790419    17570307 -1110399007
      [7] -1488892956

# get_lecuyer_cmrg_states_stream repeatable multiple seeds

    Code
      withr::with_seed(10, get_lecuyer_cmrg_states_stream(seed = NULL, nsim = 2L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      
      [[2]]
      [1]       10407   125608557 -1794459080 -1494058990  -993642217 -1807090181
      [7]   734238052
      

# get_lecuyer_cmrg_states_stream repeatable other starts

    Code
      withr::with_seed(42, get_lecuyer_cmrg_states_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407  -922606465   891908805  2065540205  1561987267  1325379220
      [7] -1522797297
      

# get_lecuyer_cmrg_states_stream seeds repeatable with other seed types

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_states_stream(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407   115614581  1315917716   768866738 -1769155862  1955826369
      [7]   645839282
      

# local_lecuyer_cmrg_seed different seeds produce different outcomes (snapshot)

    Code
      gen_lecuyer_runif(10)
    Output
      [1] 0.3276690 0.7040534 0.3022566

---

    Code
      gen_lecuyer_runif(42)
    Output
      [1] 0.1738456 0.5547401 0.4833771

# with_lecuyer_cmrg_seed different seeds produce different outcomes (snapshot)

    Code
      with_lecuyer_cmrg_seed(10, runif(3))
    Output
      [1] 0.3276690 0.7040534 0.3022566

---

    Code
      with_lecuyer_cmrg_seed(42, runif(3))
    Output
      [1] 0.1738456 0.5547401 0.4833771

# get_lecuyer_cmrg_states_stream return values state different sub-streams (snapshot)

    Code
      with_lecuyer_cmrg_state(states[[1]], runif(3))
    Output
      [1] 0.4075339 0.6022828 0.9188258

---

    Code
      with_lecuyer_cmrg_state(states[[2]], runif(3))
    Output
      [1] 0.0006085795 0.0701991342 0.7472101830

