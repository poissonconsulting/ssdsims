## Case 7 -- Naming the culprit, for the abort message.
##
## Run with: Rscript case7-who-owns-rng.R
##
## When the postcondition fails, the error should say *who* took base R's RNG.
## Two facts are available at R level:
##
##   owner     = getNativeSymbolInfo("user_unif_rand")$dll[["name"]]
##               -- the package whose user_unif_rand R currently resolves
##               (the same R_FindSymbol all-DLL search R's RNG_Init uses).
##   providers = the loaded DLLs that export user_unif_rand at all
##               -- every package that *could* take the single global slot.
##
## GOTCHA: a `DLLInfo` overloads `$` to look up a *native symbol* by that name,
## so `info$dll$name` tries to resolve a symbol called `name` and errors. Read
## the DLL's name with `[["name"]]`, which does not intercept.
##
## SUBTLETY: `owner` is only meaningful when RNGkind()[1] == "user-supplied".
## If the backend was torn down (e.g. RNGkind("Mersenne-Twister")), the symbol
## still resolves to some loaded DLL that is NOT serving RNG -- so the message
## must branch on RNGkind() first (see the torn-down row below).

rng_slot_owner <- function() {
  si <- tryCatch(getNativeSymbolInfo("user_unif_rand"), error = function(e) {
    NULL
  })
  if (is.null(si)) NA_character_ else si$dll[["name"]]
}
user_rng_providers <- function() {
  out <- character(0)
  for (d in getLoadedDLLs()) {
    ok <- tryCatch(
      {
        getNativeSymbolInfo("user_unif_rand", PACKAGE = d)
        TRUE
      },
      error = function(e) FALSE
    )
    if (isTRUE(ok)) out <- c(out, d[["name"]])
  }
  out
}

cat(sprintf(
  "RNGkind=%-15s owner=%-12s providers={%s}\n",
  RNGkind()[1],
  rng_slot_owner(),
  paste(user_rng_providers(), collapse = ", ")
))

## Try it under different loads, each in a fresh session, e.g.:
##   # foreign hijack (owner = randtoolbox, RNGkind still user-supplied):
##   library(dqrng); library(randtoolbox)
##   dqrng::dqset.seed(42); set.generator("WELL", 512, "a", seed = 42)
##   # -> owner=randtoolbox  providers={dqrng, randtoolbox}
