# parallel-safe-seeding: local_dqrng_state aborts outside an active backend

    Code
      local_dqrng_state(42L, c(1L, 2L))
    Condition
      Error in `local_dqrng_state()`:
      ! The dqrng backend is not active. Open a `local_dqrng_backend()` scope before calling `local_dqrng_state()`.

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
      ! `state` must be length 2 not 3.

