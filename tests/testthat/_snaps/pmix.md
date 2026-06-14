# ssd_pmix: prints a successful collection

    Code
      ssd_pmix(ssd_min_pmix = ssdtools::ssd_min_pmix, strict = function(n) 0.1)
    Output
      <ssdsims_pmix>
        ssd_min_pmix: <fn>
        strict: <fn>

# ssd_pmix: rejects a non-function entry

    Code
      ssd_pmix(1)
    Condition
      Error in `ssd_pmix()`:
      ! Each `ssd_pmix()` entry must be a single-argument function; entry 1 is not a function (a name-string is not accepted - pass the function itself).

# ssd_pmix: rejects a name-string entry (no string resolution)

    Code
      ssd_pmix("ssd_min_pmix")
    Condition
      Error in `ssd_pmix()`:
      ! Each `ssd_pmix()` entry must be a single-argument function; entry 1 is not a function (a name-string is not accepted - pass the function itself).

# ssd_pmix: rejects a multi-argument function

    Code
      ssd_pmix(bad = function(a, b) 0.05)
    Condition
      Error in `ssd_pmix()`:
      ! Each `ssd_pmix()` entry must take a single argument (the number of rows); entry 1 does not.

# ssd_pmix: rejects duplicate names

    Code
      ssd_pmix(a = f, a = f)
    Condition
      Error in `ssd_pmix()`:
      ! `ssd_pmix()` names must be unique.

# ssd_pmix: requires at least one function

    Code
      ssd_pmix()
    Condition
      Error in `ssd_pmix()`:
      ! `ssd_pmix()` requires at least one `min_pmix` function.

# ssd_pmix: an unnamed bare literal cannot derive a name

    Code
      ssd_pmix(function(n) 0.05)
    Condition
      Error in `ssd_pmix()`:
      ! Unable to derive a name for `ssd_pmix()` entry 1; supply a name (e.g. `ssd_pmix(strict = ...)`).

