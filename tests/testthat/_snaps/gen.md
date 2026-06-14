# scenario-definition: an anonymous generator needs an explicit name

    Code
      ssd_gen(function(n) rep(1, n), .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Unable to derive a name for generator 1; supply a name (e.g. `ssd_gen(synth = ...)`).

# scenario-definition: ssd_gen rejects duplicate names

    Code
      ssd_gen(x = ssdtools::ssd_rlnorm, x = ssdtools::ssd_rlnorm, .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Generator names must be unique.

# scenario-definition: ssd_gen rejects a data frame

    Code
      ssd_gen(d = ssddata::ccme_boron, .n = 30, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Generator `d` is a data frame, which is not a generator; pass it directly to `ssd_scenario_data()`.

# scenario-definition: ssd_gen rejects a non-generator input

    Code
      ssd_gen(d = 1:5, .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Generator `d` must be a generator: a function, a function-name string, a `fitdists` object, or a `tmbfit` object.

# scenario-definition: an unresolvable function-name string aborts

    Code
      ssd_gen("no_such_generator", .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Generator name "no_such_generator" does not resolve to a function (searched the calling environment and the ssdtools namespace).

# scenario-definition: a name resolving to a non-function aborts

    Code
      ssd_gen("not_a_function", .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Generator name "not_a_function" does not resolve to a function (searched the calling environment and the ssdtools namespace).

# scenario-definition: ssd_gen requires at least one generator

    Code
      ssd_gen(.n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! `ssd_gen()` requires at least one generator.

# scenario-definition: a generator must draw .n numeric values

    Code
      ssd_gen(short = function(n) 1, .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Generator `short` must draw a numeric vector of length `.n` (5); it returned a numeric vector of length 1.

---

    Code
      ssd_gen(chr = function(n) letters[seq_len(n)], .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! Generator `chr` must draw a numeric vector of length `.n` (5); it returned an object of type "character".

# scenario-definition: .n and .seed are required

    Code
      ssd_gen(synth = ssdtools::ssd_rlnorm, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! `.n` must be supplied (a scalar whole number): the number of rows each generator materialises.

---

    Code
      ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30)
    Condition
      Error in `ssd_gen()`:
      ! `.seed` must be supplied (a scalar whole number): `ssd_gen()` generates data, so generation must be seeded.

# scenario-definition: a generator escaping dqrng aborts

    Code
      ssd_gen(escape = escape, .n = 5, .seed = 1L)
    Condition
      Error in `ssd_gen()`:
      ! The dqrng backend is not intact: it was reset mid-task. Base R's RNG is now `Mersenne-Twister`, not dqrng's pcg64, so the task's draws did not come from dqrng.

