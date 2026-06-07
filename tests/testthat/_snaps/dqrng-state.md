# parallel-safe-seeding: local_dqrng_state aborts on entry when the backend is not intact

    Code
      local_dqrng_state(42L, c(1L, 2L))
    Condition
      Error in `local_dqrng_state()`:
      ! The dqrng backend is not intact: it was reset mid-task. Base R's RNG is now `Mersenne-Twister`, not dqrng's pcg64, so the task's draws did not come from dqrng.

# parallel-safe-seeding: local_dqrng_state validation

    Code
      local_dqrng_state(1.5, c(1L, 2L))
    Condition
      Error in `local_dqrng_state()`:
      ! `seed` must be a whole number (non-missing integer scalar or double equivalent).

---

    Code
      local_dqrng_state(42L, c(1L, 2L, 3L))
    Condition
      Error in `local_dqrng_state()`:
      ! `primer` must be length 2 not 3.

