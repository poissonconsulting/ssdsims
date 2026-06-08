# Reprex: `ssd_gen()` + `ssd_scenario_data()` — generation split from assembly

Follow-up to `reprexes-and-api.md`. This sketches the **two-helper** API:

- **`ssd_scenario_data(...)`** — the assembly/validation collection constructor
  (today's `ssd_data()`, **renamed** to avoid the `ssdtools::ssd_data(x)` clash;
  rename is in scope for this change). Accepts data frames and the result of
  `ssd_gen()`.
- **`ssd_gen(..., .n, .seed)`** — a new helper accepting **only** generator-style
  inputs (function / function-name string / `fitdists` / `tmbfit`), materialising
  each to one reproducible `Conc` tibble of `.n` rows. `.n` and `.seed` are
  **required**, dot-prefixed formals.

Intended use (both forms supported):

```r
ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 42))
ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 42))
```

Everything below was **run and validated** (`devtools::load_all()` + `ssdtools
2.6.0.9002`); the assertion block at the end prints all-`TRUE`.

## Prototype `ssd_gen()`

```r
# Resolve any generator-style input to a single-argument draw fn(n) -> numeric.
# Dispatch most-specific-first (tmbfit before fitdists), mirroring ssd_sim_data.
resolve_draw <- function(value, expr, env) {
  if (inherits(value, "tmbfit")) {
    pars <- ssdtools::estimates(value)
    rfun <- get(paste0("ssd_r", value$dist), envir = asNamespace("ssdtools"))
    return(list(name = NULL, fn = function(n) do.call(rfun, c(list(n), pars))))
  }
  if (inherits(value, "fitdists")) {                       # pick top-weighted dist
    top <- value[[which.max(ssdtools::glance(value, wt = TRUE)$wt)]]
    return(resolve_draw(top, expr, env))                   # → tmbfit path
  }
  if (is.character(value)) {                               # function-name string
    fn <- get0(value, envir = env, mode = "function", inherits = TRUE) %||%
          get0(value, envir = asNamespace("ssdtools"), mode = "function")
    if (is.null(fn)) stop(sprintf("`%s` does not resolve to a function.", value))
    return(list(name = value, fn = function(n) fn(n)))     # string IS the name
  }
  if (is.function(value)) return(list(name = NULL, fn = function(n) value(n)))
  stop("ssd_gen() accepts only generators (function, name, fitdists, tmbfit).")
}

# Per-generator: seed dqrng with (.seed, name-as-stream), draw, restore on exit.
draw_one <- function(fn, n, seed, primer) {
  local_dqrng_state(seed, primer)                          # restores on return
  fn(n)
}

ssd_gen <- function(..., .n, .seed) {
  if (missing(.seed)) stop("`.seed` is required: ssd_gen() generates data.")
  if (missing(.n)) stop("`.n` is required: the number of rows to generate.")
  chk::chk_whole_number(.seed)
  chk::chk_whole_number(.n)
  inputs <- rlang::list2(...); exprs <- rlang::enexprs(...)
  env    <- rlang::caller_env()
  argnms <- names(inputs) %||% rep("", length(inputs))
  local_dqrng_backend()                                    # scoped; reset on exit
  out <- vector("list", length(inputs)); nms <- character(length(inputs))
  for (i in seq_along(inputs)) {
    r       <- resolve_draw(inputs[[i]], exprs[[i]], env)
    nms[i]  <- if (nzchar(argnms[i])) argnms[i]
               else r$name %||% expr_to_name(exprs[[i]]) %||%
                    stop("Unable to derive a generator name; supply one.")
    primer  <- task_primer(list(dataset = nms[i]))         # name = dqrng stream
    out[[i]] <- tibble::tibble(Conc = draw_one(r$fn, .n, .seed, primer))
  }
  if (anyDuplicated(nms)) stop("Generator names must be unique.")
  structure(rlang::set_names(out, nms), class = "ssdsims_gen")
}
```

**The "little magic" in `ssd_scenario_data()`** — an unnamed argument that
`inherits("ssdsims_gen")` is flattened in (its named tibbles become collection
members); everything else is a data frame named by arg/symbol. The `!!!` splice
form needs no magic (rlang splices the named list directly), so both work.

## Covers every existing `ssd_sim_data()` method

```r
fit <- ssdtools::ssd_fit_dists(ssddata::ccme_boron, dists = c("lnorm", "gamma"))

ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 8, .seed = 42)   # function
ssd_gen(synth = "ssd_rlnorm",         .n = 8, .seed = 42)   # function-name string
ssd_gen(refit = fit[[1]],             .n = 8, .seed = 42)   # tmbfit
ssd_gen(refit = fit,                  .n = 8, .seed = 42)   # fitdists (top dist)
ssd_gen(a = ssdtools::ssd_rlnorm, b = ssdtools::ssd_rlnorm, .n = 8, .seed = 42) # fan-out
```

Validated assertions (all print `TRUE`):

```
character == function (same resolution)            TRUE
reproducible: same .seed re-run is byte-identical  TRUE
different .seed yields different data               TRUE
name-as-stream: a != b under one .seed             TRUE
.seed required (omitting it errors)                TRUE
data frame rejected by ssd_gen()                   TRUE
ssd_scenario_data(.., ssd_gen(..))  ==  ssd_scenario_data(.., !!!ssd_gen(..))  TRUE
```

A `data.frame` is **not** an `ssd_gen()` input — it goes straight into
`ssd_scenario_data()`:

```r
ssd_scenario_data(boron = ssddata::ccme_boron)                  # method #1, unchanged
```

## How this scores against the three goals

- **No surprises** — generation is its own named step returning plain tibbles;
  the scenario never runs RNG or user code. A generator is *not* mistaken for the
  legacy per-sim design: you visibly create a fixture, then a scenario resamples
  it (same as any data frame).
- **Built-in reproducibility** — `.seed` is a **required** formal of `ssd_gen()`,
  so the "forgot `with_seed`" and "inert `seed=`" footguns from the prior note are
  *impossible*. One `.seed` fans out across generators on independent
  name-keyed dqrng streams (`task_primer(list(dataset = name))`), reusing the
  package's existing primitives.
- **Ergonomics** — one `ssd_scenario_data()` call mixes data frames and generators;
  `.seed` sits with the generators it seeds (resolving the old "one `.seed` for
  the whole call" coupling); the gnarly `fitdists`/`tmbfit` → `ssd_r<dist>`
  dispatch (§4–5 of the prior note) is handled for you.

## Scope changes this implies (for the proposal)

1. **Rename** `ssd_data()` → `ssd_scenario_data()` (the `ssdtools::ssd_data(x)` clash;
   in scope per the task). `ssd_scenario_data` and `ssd_gen` are both free of
   `ssdtools` exports (checked).
2. **Drop `name=` from `ssd_define_scenario()`** and have its `data` argument
   accept **only an `ssd_scenario_data()` collection** — naming lives entirely in
   `ssd_scenario_data()`/`ssd_gen()`. This deletes the bare-data-frame / named-list /
   unnamed-list routing and the `data_expr` capture in `scenario_datasets()`,
   shrinking the constructor. Migration: `ssd_define_scenario(ccme_boron, …)`
   becomes `ssd_define_scenario(ssd_scenario_data(ccme_boron), …)`.
3. **`ssd_gen()` is the only place** that touches dqrng/`task_primer` for
   generation — the scenario constructor keeps the dependency edge
   `ssd-define-scenario` avoided. (`ssd_gen()` opens a scoped
   `local_dqrng_backend()`, draws, and restores global RNG on return.)

## Resolved (folded into `proposal.md` / `design.md`)

- **`.n` granularity** — one `.n` per `ssd_gen()` call; differing sizes use
  multiple calls
  (`ssd_scenario_data(!!!ssd_gen(a=…, .n=30, .seed=1), !!!ssd_gen(b=…, .n=200, .seed=2))`).
- **dqrng-only check** — reuse `task-rng-postcheck` (#117): gate via
  `dqrng_usable()` and witness each draw via `chk_dqrng_backend_intact()` rather
  than a bespoke base-R-RNG state check.
- **`ssd_gen()` return type** — a classed `ssdsims_gen` list, so the inline
  (no-`!!!`) form and the `!!!` splice both work.
- **`.n` / `.seed`** — both **required**, dot-prefixed formals (no defaults): the
  reproducibility and non-degenerate-sizing win over one-arg convenience.
