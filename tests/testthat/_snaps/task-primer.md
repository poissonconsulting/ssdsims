# parallel-safe-seeding: task_primer rejects invalid params

    Code
      task_primer(tibble::tibble(sim = 1:2))
    Condition
      Error in `task_primer()`:
      ! `params` must be a single-row data frame, not one with 2 rows.

---

    Code
      task_primer(1L)
    Condition
      Error in `task_primer()`:
      ! `params` must be a plain list or a single-row data frame.

