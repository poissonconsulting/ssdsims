# Two packages registering a user-defined RNG, loaded simultaneously

Reproducible examples for the question: *what happens when two R packages
that each register a user-supplied uniform RNG (here `dqrng` and
`randtoolbox`) are loaded at the same time?*

## TL;DR

Base R has exactly **one** process-global user-supplied RNG slot. R resolves
it by the C symbol name `user_unif_rand` (and friends `user_unif_init`,
`user_unif_nseed`, `user_unif_seedloc`) via `R_FindSymbol`, searching **all
loaded DLLs**. Both `dqrng.so` and `randtoolbox.so` export a global
`user_unif_rand` symbol:

```
$ nm -D dqrng.so       | grep user_unif      $ nm -D randtoolbox.so | grep user_unif
  T user_unif_init                             T user_unif_init
  T user_unif_rand                             T user_unif_rand
                                               T user_unif_set_generator  ...
```

When both are loaded the symbol **collides**. The binding goes to the
**most-recently-loaded DLL**, regardless of which package's R-level setup
function (`dqrng::register_methods()` / `randtoolbox::set.generator()`) you
called. Two failure modes follow:

| Most-recent DLL | State of that package | Result |
|---|---|---|
| dqrng | auto-initialised on load | works, but ignores randtoolbox config (**silently wrong RNG**) |
| randtoolbox | **not** initialised (no `set.generator`) | NULL deref → **segfault** |

Reproducibility is silently broken in the first case; the process crashes in
the second.

## Running the reprexes

Each case needs a fresh R session (load order matters and segfaults abort the
process):

```sh
Rscript case1-baselines.R                  # each package fine in isolation
Rscript case2-silent-wrong-numbers.R       # wrong numbers, no error
Rscript case3-segfault.R                   # crashes (expected)
Rscript case4-resolution-probe.R dqrng         # binds dqrng, works
Rscript case4-resolution-probe.R randtoolbox   # binds randtoolbox, segfault
```

Tested with R 4.5.3, dqrng 0.4.1, randtoolbox 2.0.5 (rngWELL 0.10-10).

## Distinctive sequences (seed 42)

| Generator | `runif(3)` |
|---|---|
| dqrng (default) | `0.908370 0.330612 0.950935` |
| randtoolbox WELL512a | `0.825669 0.341247 0.848482` |

These let you tell from the *numbers* which backend actually served the draw.

## Why this matters for `ssdsims`

`ssdsims` deliberately does **not** register dqrng at package load
(`R/dqrng-backend.R` is "intentionally inert"); it activates the backend only
for the duration of a scenario run via `local_dqrng_backend()`, and its
`dqrng_backend_active()` probe refuses to re-register if a user-supplied RNG
is already in effect. These reprexes show the failure modes that scoped,
load-time-inert design is avoiding: a second user-RNG package in the same
session can hijack base R's `runif()` or crash it, depending purely on load
order.

## Mitigations

- There is no way for two packages to *both* be base R's user-supplied RNG at
  once — the slot is singular and keyed on one symbol name.
- Activate a user-RNG backend in a **scoped** way and restore on exit
  (`withr`/`on.exit`), as `ssdsims` does, rather than globally at load.
- `dqrng` draws taken through its **own** API (`dqrunif()`, `dqrnorm()`,
  `dqsample()`) do not go through the base `user_unif_rand` slot and so do not
  collide; only routing base R's `runif()`/`rnorm()` through dqrng
  (`register_methods()`) does.
- Avoid loading two such packages in the same session; if unavoidable, never
  rely on load order and re-assert your backend immediately before use.
