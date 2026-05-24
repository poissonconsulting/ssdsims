# get_lecuyer_cmrg_stream_states repeatable

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_stream_states(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407 -2038793495  -246736810   305049957  1634315539  1750612736
      [7]   808732210
      

# get_lecuyer_cmrg_stream_states seed fast enough

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_stream_states(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 10^5))
    Output
      [[1]]
      [1]       10407  1721741772  1925791868 -1147332526 -1539986976 -1374629412
      [7]   590684957
      

# get_lecuyer_cmrg_stream_states stream fast enough

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_stream_states(seed = NULL, nsim = 1L,
        start_sim = 1L, stream = 10^5))
    Output
      [[1]]
      [1]       10407   669110052 -1948224521   -47549820   -69140895   817690492
      [7]  -178654449
      

# get_lecuyer_cmrg_stream_states seed stream fast enough

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_stream_states(seed = NULL, nsim = 1L,
        start_sim = 10^5, stream = 10^5))
    Output
      [[1]]
      [1]      10407 -151786288 1694320913 1804794388 1014966609 2117470929 2002498857
      

# get_lecuyer_cmrg_stream_states seeds stream fast enough

    Code
      seeds[[10^5]]
    Output
      [1]       10407   -40460579   675317274 -1715603968  -718619875    17520228
      [7]   -27441657

# get_lecuyer_cmrg_stream_states repeatable multiple seeds

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_stream_states(seed = NULL, nsim = 2L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407 -2038793495  -246736810   305049957  1634315539  1750612736
      [7]   808732210
      
      [[2]]
      [1]       10407   729983441  1157127356  -806736850 -2123917413   938682574
      [7]   569701025
      

# get_lecuyer_cmrg_stream_states repeatable other starts

    Code
      with_lecuyer_cmrg_seed(42, get_lecuyer_cmrg_stream_states(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407  1607056201  -514628575  2135767272 -1346969587  1573711359
      [7] -1178291560
      

# get_lecuyer_cmrg_stream_states seeds repeatable with other seed types

    Code
      with_lecuyer_cmrg_seed(10, get_lecuyer_cmrg_stream_states(seed = NULL, nsim = 1L,
        stream = 1L, start_sim = 1L))
    Output
      [[1]]
      [1]       10407 -2038793495  -246736810   305049957  1634315539  1750612736
      [7]   808732210
      

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

# get_lecuyer_cmrg_stream_states return values state different sub-streams (snapshot)

    Code
      with_lecuyer_cmrg_state(states[[1]], runif(3))
    Output
      [1] 0.6773952 0.2574178 0.9373020

---

    Code
      with_lecuyer_cmrg_state(states[[2]], runif(3))
    Output
      [1] 0.4024546 0.7585162 0.6445623

# local_lecuyer_cmrg_seed rejects non-scalar seed

    Code
      local_lecuyer_cmrg_seed(state)
    Condition
      Error in `local_lecuyer_cmrg_seed()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      local_lecuyer_cmrg_seed(integer())
    Condition
      Error in `local_lecuyer_cmrg_seed()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      local_lecuyer_cmrg_seed(NA_integer_)
    Condition
      Error in `local_lecuyer_cmrg_seed()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      local_lecuyer_cmrg_seed("10")
    Condition
      Error in `local_lecuyer_cmrg_seed()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

# local_lecuyer_cmrg_seed rejects non-environment .local_envir

    Code
      local_lecuyer_cmrg_seed(10, .local_envir = "env")
    Condition
      Error in `local_lecuyer_cmrg_seed()`:
      ! `.local_envir` must be an environment.

# local_lecuyer_cmrg_state rejects non-integer state

    Code
      local_lecuyer_cmrg_state(10)
    Condition
      Error in `local_lecuyer_cmrg_state()`:
      ! `state` must be integer.

---

    Code
      local_lecuyer_cmrg_state(1:6)
    Condition
      Error in `local_lecuyer_cmrg_state()`:
      ! `state` must be length 7 not 6.

---

    Code
      local_lecuyer_cmrg_state(c(1L, 2L, 3L, 4L, 5L, 6L, NA_integer_))
    Condition
      Error in `local_lecuyer_cmrg_state()`:
      ! `state` must not have any missing values.

---

    Code
      local_lecuyer_cmrg_state("a")
    Condition
      Error in `local_lecuyer_cmrg_state()`:
      ! `state` must be integer.

# local_lecuyer_cmrg_state rejects non-environment .local_envir

    Code
      local_lecuyer_cmrg_state(state, .local_envir = "env")
    Condition
      Error in `local_lecuyer_cmrg_state()`:
      ! `.local_envir` must be an environment.

