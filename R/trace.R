# Tracing helpers
#
# All tracing is gated by `getOption("ssdsims.trace", FALSE)`. When the
# option is FALSE (the default) `trace_msg()` is a no-op. When TRUE,
# each call emits a single `message()` to stderr.
#
# A length-7 L'Ecuyer-CMRG state is fingerprinted with the first 8 hex
# digits of `rlang::hash()` over its mutable tail (positions 2..7), so
# state identity across stages is visible at a glance.

trace_enabled <- function() {
  isTRUE(getOption("ssdsims.trace", FALSE))
}

# Walk back through sys.calls() and return "basename(file):line" for the
# most recent call site whose source is *outside* this trace.R file.
# Returns "" if no usable srcref is available.
trace_caller_loc <- function() {
  calls <- sys.calls()
  for (i in seq.int(length(calls), 1L)) {
    src <- attr(calls[[i]], "srcref")
    if (is.null(src)) {
      next
    }
    sf <- attr(src, "srcfile")
    if (is.null(sf)) {
      next
    }
    file <- sf$filename
    if (is.null(file) || !nzchar(file)) {
      next
    }
    if (grepl("(^|/)trace\\.R$", file)) {
      next
    }
    return(sprintf("%s:%d", basename(file), src[1L]))
  }
  ""
}

trace_state_hash <- function(state) {
  if (is.null(state)) {
    return("NULL")
  }
  if (!is.integer(state) || length(state) != 7L) {
    return(format_trace_value(state))
  }
  substr(rlang::hash(state[2:7]), 1L, 8L)
}

format_trace_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.integer(x) && length(x) == 7L) {
    return(trace_state_hash(x))
  }
  if (is.atomic(x) && length(x) == 1L) {
    return(format(x))
  }
  if (is.atomic(x) && length(x) <= 4L) {
    return(paste(format(x), collapse = ","))
  }
  if (is.atomic(x)) {
    return(sprintf("<%s[%d]>", typeof(x), length(x)))
  }
  if (is.data.frame(x)) {
    return(sprintf("<df[%dx%d]>", nrow(x), ncol(x)))
  }
  sprintf("<%s>", class(x)[1])
}

trace_msg <- function(tag, ...) {
  if (!trace_enabled()) {
    return(invisible(NULL))
  }
  args <- list(...)
  loc <- trace_caller_loc()
  prefix <- if (nzchar(loc)) {
    sprintf("[ssdsims %s]", loc)
  } else {
    "[ssdsims]"
  }
  if (length(args)) {
    nms <- names(args)
    if (is.null(nms)) {
      nms <- rep("", length(args))
    }
    parts <- vapply(
      seq_along(args),
      function(i) {
        v <- format_trace_value(args[[i]])
        if (nzchar(nms[i])) sprintf("%s=%s", nms[i], v) else v
      },
      character(1)
    )
    message(sprintf("%s %-20s %s", prefix, tag, paste(parts, collapse = " ")))
  } else {
    message(sprintf("%s %s", prefix, tag))
  }
  invisible(NULL)
}

trace_rng_kind <- function() {
  if (!trace_enabled()) {
    return(invisible(NULL))
  }
  has <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  trace_msg(
    ".Random.seed",
    exists = has,
    kind = paste(RNGkind(), collapse = "/")
  )
}
