# dqrng-backend: dqrng_usable is TRUE only when dqrng is loaded at >= 0.4.1

    Code
      local_dqrng_backend()
    Condition
      Error in `local_dqrng_backend()`:
      ! The dqrng RNG backend requires the `dqrng` package (>= 0.4.1) to be loaded. ssdsims never loads it implicitly (registering a user-supplied RNG provider is a process-global side effect); run `library(dqrng)` first.

# dqrng-backend: the witness aborts when the backend is torn down

    Code
      chk_dqrng_backend_intact()
    Condition
      Error:
      ! The dqrng backend is no longer intact: it was reset during the task (base R RNG is now "Mersenne-Twister"), so the task's draws did not all come from dqrng and are not reproducible.

