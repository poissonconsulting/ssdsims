# Reprexes: every `ssd_sim_data()` method via the *current* `ssd_data()`

Goal of this note: show, for each of the five `ssd_sim_data()` S3 methods, how to
get the *same effect* — one validated `Conc` dataset in a scenario — using the
**current, data-frame-only** `ssd_data()` plus whatever up-front work the
generator needs. Then use the friction these reprexes expose to reason about a
better API.

All snippets below were run against `devtools::load_all()` +
`ssdtools 2.6.0.9002` and pass. `ssd_data()` is called unqualified (note:
`ssdtools` also exports an unrelated `ssd_data(x)`, so in a session where
`ssdtools` is attached you must call `ssdsims::ssd_data()`).

```r
library(tibble)
# Each example produces a single tibble with a numeric `Conc` column, then
# hands it to the existing collection constructor.
```

## 1. `data.frame` — already a one-liner, no generation

```r
ssd_data(boron = ssddata::ccme_boron)
#> <ssdsims_data> names: boron
```

## 2. `function` — generate one dataset up-front; **you** must seed it

```r
synth <- withr::with_seed(42L, tibble(Conc = ssdtools::ssd_rlnorm(30)))
ssd_data(synth = synth)
```

Friction: you must remember (a) to wrap in a seed, (b) to build the `Conc`
tibble, and (c) to pick `n`. Forget (a) and you get **irreproducible data with
no error**.

## 3. `character` — resolve the name yourself, then as a function

```r
gen <- get("ssd_rlnorm", envir = asNamespace("ssdtools"))
ssd_data(synth = withr::with_seed(42L, tibble(Conc = gen(30))))
```

Friction: name resolution is now the caller's job (`get(..., asNamespace(...))`).

## 4. `tmbfit` (`fit[[1]]`) — pull estimates, draw from the matching `ssd_r<dist>`

```r
fit  <- ssdtools::ssd_fit_dists(ssddata::ccme_boron, dists = c("lnorm", "gamma"))
tf   <- fit[[1]]
pars <- ssdtools::estimates(tf)                                   # meanlog, sdlog
rfun <- get(paste0("ssd_r", tf$dist), envir = asNamespace("ssdtools"))
refit <- withr::with_seed(42L, tibble(Conc = do.call(rfun, c(list(30), pars))))
ssd_data(refit = refit)
```

Friction: the caller must reproduce `ssd_sim_data.tmbfit()`'s internals —
`estimates()`, the `dist → ssd_r<dist>` name mapping, and a `do.call`.

## 5. `fitdists` (`fit`) — pick the top-weighted dist, then as `tmbfit`

```r
wt    <- ssdtools::glance(fit, wt = TRUE)$wt
top   <- fit[[which.max(wt)]]
pars2 <- ssdtools::estimates(top)
rfun2 <- get(paste0("ssd_r", top$dist), envir = asNamespace("ssdtools"))
ssd_data(refit = withr::with_seed(42L, tibble(Conc = do.call(rfun2, c(list(30), pars2)))))
```

Friction: on top of the `tmbfit` work, the caller must reproduce
`ssd_sim_data.fitdists()`'s model-selection (`glance(wt = TRUE)` → `which.max`).

## And the whole point — a scenario from a generated dataset

```r
synth <- withr::with_seed(42L, tibble(Conc = ssdtools::ssd_rlnorm(30)))
ssd_define_scenario(synth, nsim = 3L, nrow = c(6L, 8L), seed = 1L, name = "synth")
#> datasets: synth
```

---

# What the reprexes teach us

**The friction scales with input type, and none of it lives in `ssd_data()`.**

```
 data.frame   ▏ trivial         (pass it)
 function     ▏▏ easy           (seed + tibble(Conc=) + pick n)
 character    ▏▏▏ + name resolution
 tmbfit       ▏▏▏▏▏ + estimates() + dist→ssd_r<dist> + do.call
 fitdists     ▏▏▏▏▏▏▏ + glance(wt) + which.max model selection
```

`ssd_data()` already does its job (assemble + validate a named collection of
tibbles). The hard, error-prone part is **reproducibly turning a generator into
one dataset** — and for `fitdists`/`tmbfit` that means hand-reimplementing
`ssd_sim_data`'s dispatch.

**Two reproducibility footguns surfaced:**

1. **Silent irreproducibility.** Forget `withr::with_seed()` and you get random
   data with no warning.
2. **The existing `seed=` is *inert* for generators.** Verified empirically:
   `ssd_sim_data(ssd_rlnorm, nsim = 1, seed = 42)$data[[1]]` is byte-identical to
   `... seed = 999 ...` — the generator methods seed off the *sim index*, not the
   `seed` argument (only the `data.frame` method honours `seed`). So the obvious
   "reproducible" path (`ssd_sim_data(..., seed = s)`) **looks** seed-controlled
   but is not, and also returns a nested `nsim`-row tibble you must dig into.

---

# Reasoning toward a better API

Design targets: **(N) no surprises · (R) built-in reproducibility (hard to do
wrong) · (E) good ergonomics.**

## The decisive observation

A materialised generator is, by this change's own design, **indistinguishable
downstream from a data frame** (no descriptor stored, provenance deferred to
`dataset-provenance`). And the scenario **resamples** whatever tibble it holds
(`slice_sample(data, n = max(nrow), replace)`), so a generator materialised
*once* is a *fixture to bootstrap*, **not** the legacy "fresh data per sim"
design. So the only real job is: *produce one reproducible fixture tibble.* That
is a **generation** concern, cleanly separable from **scenario definition**.

## Candidate APIs

### C1 — generators inside `ssd_data(..., .seed=)` (the current proposal)

```r
ssd_define_scenario(ssdtools::ssd_rlnorm, .seed = 42, nsim = 100, nrow = 6)
```

- **N ✗** the constructor runs arbitrary user code + RNG; `.seed` is legal only
  when a generator is present (else aborts); a base-R generator aborts via the
  dqrng-only check; "materialise once then resample" silently differs from the
  legacy per-sim semantics of the same-looking `ssd_run_scenario(ssd_rlnorm,…)`.
- **R ~** `.seed` is *optional* (`NULL`); pure generators run unseeded, dqrng
  generators abort. Reproducibility is conditional, not guaranteed.
- **E ✓✓** single terse call.

### C2 — a dedicated single-dataset generator; `ssd_data()` stays data-only

A `ssd_simulate(x, n, seed)` that dispatches on the **same five types** but
returns **one** validated `Conc` tibble and **requires** `seed`:

```r
synth <- ssd_simulate(ssdtools::ssd_rlnorm, n = 30, seed = 42)  # function
refit <- ssd_simulate(fit,                 n = 30, seed = 42)  # fitdists → top dist
ssd_define_scenario(synth, nsim = 100, nrow = 6, seed = 1)
```

- **N ✓** generation and scenario definition are separate, each does one thing;
  the result is a plain inspectable tibble; no RNG in the constructor.
- **R ✓✓** `seed` is a **required** argument — you *cannot* generate
  irreproducibly. Fixes both footguns above (inert `seed=`, silent randomness).
- **E ✓** two steps, but each is clear and reusable, and the gnarly
  `fitdists`/`tmbfit` dispatch is done for you (the real pain in §4–5).

### C3 — per-generator builder consumed by `ssd_data()` (C2 + co-location)

```r
ssd_data(
  boron = ssddata::ccme_boron,
  synth = ssd_gen(ssdtools::ssd_rlnorm, n = 30, seed = 42),  # seed required
  refit = ssd_gen(fit,                 n = 30, seed = 7)
)
```

- **N ✓ · R ✓✓ · E ✓✓** single `ssd_data()` call, mixed data + generators, and
  the seed is **co-located with each generator** — which also resolves the
  deferred design question Q2 (one `.seed` for all generators) by giving each its
  own, the obvious place for it.

## Recommendation

**C3 (or C2 as its minimal core).** Both beat C1 on *no surprises* and
*reproducibility* — the two goals C1 is weakest on — at a small ergonomics cost
that the reprexes show is *already* borne by users today (only, unsafely and by
hand). Key wins:

- **Reproducible by construction**: a required `seed` makes the inert-`seed=` and
  forgot-`with_seed` footguns impossible.
- **One honest mental model**: "generate a fixture, then define a scenario over
  it" — matching what the scenario actually does (resample a stored tibble),
  instead of borrowing `ssd_run_scenario`'s single-call shape while quietly
  changing its per-sim semantics.
- **No new constructor dependency**: generation seeded with base-R `with_seed`
  (the `ssd_r*` family uses base-R RNG) keeps `dqrng`/`task_primer` *out* of the
  constructor — preserving the edge `ssd-define-scenario` deliberately avoided.
  The dqrng world stays where it belongs: the per-task `sample`/`fit`/`hc` RNG.
- **`n` is explicit**, killing the unanswered row-count question — and letting a
  user size the fixture so `replace = FALSE` resampling is non-degenerate.

The cost vs C1 is one extra call and no `ssd_define_scenario(gen, …)` sugar. If
that sugar is wanted later, it can be a thin wrapper over C2/C3 once
`dataset-provenance` makes recording the generator+seed actually load-bearing.
