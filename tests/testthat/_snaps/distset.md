# scenario-definition: ssd_distset prints stably

    Code
      ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz(), lnorm = "lnorm")
    Output
      <ssdsims_distset>
        BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
        lnorm: lnorm

# scenario-definition: ssd_distset rejects invalid input naming the offending set

    Code
      ssd_distset()
    Condition
      Error in `ssd_distset()`:
      ! `ssd_distset()` must be given at least one distribution set.

---

    Code
      ssd_distset(ssdtools::ssd_dists_bcanz())
    Condition
      Error in `ssd_distset()`:
      ! Every distribution set must be named (e.g. `ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())`).

---

    Code
      ssd_distset(a = "lnorm", a = "gamma")
    Condition
      Error in `ssd_distset()`:
      ! Distribution-set names must be unique.

---

    Code
      ssd_distset(bad = "not_a_distribution")
    Condition
      Error in `ssd_distset()`:
      ! Distribution set "bad" names unknown distribution '"not_a_distribution"'; members must be a subset of `ssdtools::ssd_dists_all()`.

---

    Code
      ssd_distset(empty = character(0))
    Condition
      Error in `ssd_distset()`:
      ! Distribution set "empty" must be a non-empty character vector of distribution names.

---

    Code
      ssd_distset(dup = c("lnorm", "lnorm"))
    Condition
      Error in `ssd_distset()`:
      ! Distribution set "dup" must not contain duplicate distribution names.

---

    Code
      ssd_distset(`bad/name` = "lnorm")
    Condition
      Error in `ssd_distset()`:
      ! Distribution-set name '"bad/name"' must be filesystem-safe (letters, digits, `.`, `_`, or `-` only); each becomes a `distset=<name>` path segment.

# scenario-definition: ssd_define_scenario() accepts only an ssd_distset() collection

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, dists = ssdtools::ssd_dists_bcanz())
    Condition
      Error in `ssd_define_scenario()`:
      ! `dists` must be an `ssd_distset()` collection; assemble distribution sets with `ssd_distset()` (e.g. `ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())`).

---

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, dists = list(BCANZ = ssdtools::ssd_dists_bcanz()))
    Condition
      Error in `ssd_define_scenario()`:
      ! `dists` must be an `ssd_distset()` collection; assemble distribution sets with `ssd_distset()` (e.g. `ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())`).

---

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, dists = "lnorm")
    Condition
      Error in `ssd_define_scenario()`:
      ! `dists` must be an `ssd_distset()` collection; assemble distribution sets with `ssd_distset()` (e.g. `ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())`).

---

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, dists = NULL)
    Condition
      Error in `ssd_define_scenario()`:
      ! `dists` must be an `ssd_distset()` collection; assemble distribution sets with `ssd_distset()` (e.g. `ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())`).

---

    Code
      ssd_define_scenario(data, nsim = 2L, seed = 1L, dists = 1L)
    Condition
      Error in `ssd_define_scenario()`:
      ! `dists` must be an `ssd_distset()` collection; assemble distribution sets with `ssd_distset()` (e.g. `ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz())`).

