# dqrng-backend: chk_dqrng_backend_intact aborts when the backend is torn down

    Code
      chk_dqrng_backend_intact()
    Condition
      Error:
      ! The dqrng backend is not intact: it was reset mid-task. Base R's RNG is now `Mersenne-Twister`, not dqrng's pcg64, so the task's draws did not come from dqrng.

