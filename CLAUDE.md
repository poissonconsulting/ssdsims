# ssdsims

## Pull request review threads

- When addressing a review comment, **reply on the thread** explaining what
  changed (or answering the question) and **leave the thread open** — do not
  resolve it. The reviewer resolves their own threads.

## Tests

- Keep `targets` pipeline test fixtures (the `_targets.R` files a test sources)
  as plain text templates under `tests/testthat/fixtures/`, copied into a temp
  dir by the test — not built inline with `writeLines()`.
