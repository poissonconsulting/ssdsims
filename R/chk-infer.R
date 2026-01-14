# Helper functions for extracting chk validation calls from function bodies

# Check if expression contains a specific symbol
contains_symbol <- function(expr, symbol_name) {
  if (is.symbol(expr) && identical(expr, as.symbol(symbol_name))) {
    return(TRUE)
  }
  if (is.call(expr) || is.list(expr)) {
    for (i in seq_along(expr)) {
      if (contains_symbol(expr[[i]], symbol_name)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

# Normalize a chk call to namespaced form
# Returns the normalized expression if it's a chk call, NULL otherwise
normalize_chk_call <- function(expr) {
  if (!is.call(expr)) {
    return(NULL)
  }

  # Check for chk::function_name pattern (namespace call)
  if (
    is.call(expr[[1]]) &&
      length(expr[[1]]) == 3 &&
      identical(expr[[1]][[1]], quote(`::`)) &&
      identical(expr[[1]][[2]], quote(chk))
  ) {
    return(expr)
  }

  # Check for direct function call where function is exported by chk
  if (is.symbol(expr[[1]])) {
    fun_name <- as.character(expr[[1]])
    if (fun_name %in% get_chk_exports()) {
      # Create a namespaced call: chk::function_name
      namespaced_fun <- call("::", quote(chk), as.symbol(fun_name))
      # Replace the function part of the call
      expr[[1]] <- namespaced_fun
      return(expr)
    }
  }

  # Return NULL if it's not a chk call
  NULL
}

# Walk the function body to find chk:: calls
find_chk_calls_in_body <- function(expr, fun_args, arg_results) {
  normalized_expr <- normalize_chk_call(expr)

  if (!is.null(normalized_expr)) {
    # Check which arguments this call references
    for (arg_name in fun_args) {
      if (contains_symbol(expr, arg_name)) {
        # Add this call to the results for this argument
        arg_results[[arg_name]][[
          length(arg_results[[arg_name]]) + 1
        ]] <- normalized_expr
      }
    }
  } else if (is.call(expr) || is.list(expr)) {
    # Recursively walk the expression tree
    for (i in seq_along(expr)) {
      arg_results <- find_chk_calls_in_body(expr[[i]], fun_args, arg_results)
    }
  }
  arg_results
}

# Get the function object from a call expression
get_function_from_call <- function(call_expr, fun_env) {
  fun_called <- call_expr[[1]]

  # Extract function name for display
  if (is.call(fun_called) && identical(fun_called[[1]], quote(`::`))) {
    # It's a namespace call like pkg::fun
    display_name <- paste0(
      as.character(fun_called[[2]]),
      "::",
      as.character(fun_called[[3]])
    )
  } else if (is.symbol(fun_called)) {
    display_name <- as.character(fun_called)
  } else {
    display_name <- "<complex call>"
  }

  # Extract the function object
  fun_obj <- NULL
  tryCatch(
    {
      if (is.call(fun_called) && identical(fun_called[[1]], quote(`::`))) {
        # It's a namespace call like pkg::fun
        pkg_name <- as.character(fun_called[[2]])
        fun_name_in_pkg <- as.character(fun_called[[3]])
        fun_obj <- getExportedValue(pkg_name, fun_name_in_pkg)
      } else if (is.symbol(fun_called)) {
        # It's a plain function name, try to find it
        fun_name_str <- as.character(fun_called)
        # First check if it's in the current environment
        fun_obj <- tryCatch(
          get(fun_name_str, envir = fun_env),
          error = function(e) NULL
        )
      }
    },
    error = function(e) {
      fun_obj <- NULL
    }
  )

  list(fun_obj = fun_obj, display_name = display_name)
}

# Find the first usage of an argument in the function body
# Returns the innermost call that contains the argument
find_first_usage <- function(expr, arg_name, depth = 0) {
  if (is.symbol(expr) && identical(expr, as.symbol(arg_name))) {
    # Found the argument itself
    return(NULL)
  }

  if (is.call(expr)) {
    # First, recursively search in nested calls to find innermost usage
    innermost_result <- NULL
    for (i in seq_along(expr)) {
      result <- find_first_usage(expr[[i]], arg_name, depth + 1)
      if (!is.null(result)) {
        # Found something deeper, return it (it's more innermost)
        return(result)
      }
    }

    # If we didn't find anything deeper, check if this call contains the argument
    for (i in seq_along(expr)) {
      if (i == 1) {
        next
      } # Skip the function name itself

      # Check if this element contains our argument directly
      if (contains_symbol(expr[[i]], arg_name)) {
        # This call contains the argument at this level
        return(list(call = expr, arg_position = i))
      }
    }
  } else if (is.list(expr)) {
    # Handle lists (like function bodies)
    for (i in seq_along(expr)) {
      result <- find_first_usage(expr[[i]], arg_name, depth + 1)
      if (!is.null(result)) {
        return(result)
      }
    }
  }

  NULL
}

# Determine which parameter in the called function receives our argument
determine_called_arg_name <- function(call_expr, arg_name, fun_obj, indent) {
  called_arg_name <- NULL
  tryCatch(
    {
      matched_call <- rlang::call_match(call_expr, fun_obj)

      # Find which formal parameter our argument was matched to
      for (i in seq_along(matched_call)) {
        if (i == 1) {
          next
        } # Skip function name
        if (contains_symbol(matched_call[[i]], arg_name)) {
          # Found it - get the parameter name
          param_names <- names(matched_call)
          if (!is.null(param_names) && param_names[i] != "") {
            called_arg_name <- param_names[i]
            break
          }
        }
      }
    },
    error = function(e) {
      # If call_match fails, fall back to manual matching
      cli::cli_alert_warning("{indent}    Could not match call: {e$message}")
    }
  )

  called_arg_name
}

# Process arguments without direct chk calls by tracing their usage
trace_indirect_chk_calls <- function(
  args_without_calls,
  fun_body,
  arg_results,
  fun,
  indent,
  .depth,
  extract_chk_calls
) {
  for (arg_name in args_without_calls) {
    cli::cli_alert("{indent}  Tracing {.arg {arg_name}}...")

    # Find the first usage of this argument
    first_usage <- find_first_usage(fun_body, arg_name)

    if (!is.null(first_usage)) {
      call_expr <- first_usage$call

      # Get the function object and display name
      fun_info <- get_function_from_call(call_expr, environment(fun))

      cli::cli_alert_info(
        "{indent}    First usage found in call to {.fn {fun_info$display_name}}"
      )

      if (!is.null(fun_info$fun_obj) && is.function(fun_info$fun_obj)) {
        # Check if function is from base package or is primitive
        is_primitive <- is.primitive(fun_info$fun_obj)
        fun_env <- environment(fun_info$fun_obj)
        is_base <- !is.null(fun_env) && identical(fun_env, baseenv())

        if (is_primitive) {
          cli::cli_alert_info("{indent}    Function is a primitive (base)")
        } else if (is_base) {
          cli::cli_alert_info("{indent}    Function is from base package")
        } else {
          fun_formals <- formals(fun_info$fun_obj)
          formal_names <- names(fun_formals)
          formal_names <- formal_names[formal_names != "..."]
          cli::cli_alert_info(
            "{indent}    Function has {length(formal_names)} formal{?s}: {.arg {formal_names}}"
          )

          # Determine which argument in the called function receives our argument
          called_arg_name <- determine_called_arg_name(
            call_expr,
            arg_name,
            fun_info$fun_obj,
            indent
          )

          if (!is.null(called_arg_name)) {
            cli::cli_alert_info(
              "{indent}    Argument {.arg {arg_name}} maps to parameter {.arg {called_arg_name}}"
            )

            # Recursively analyze the called function for this specific argument
            cli::cli_alert(
              "{indent}    Recursively analyzing {.fn {fun_info$display_name}} for {.arg {called_arg_name}}..."
            )
            traced_results <- extract_chk_calls(
              fun_info$fun_obj,
              arg = called_arg_name,
              fun_name = fun_info$display_name,
              .depth = .depth + 1
            )

            # Extract results for the specific argument
            if (!is.null(traced_results[[called_arg_name]])) {
              arg_results[[arg_name]] <- traced_results[[called_arg_name]]
              cli::cli_alert_success(
                "{indent}    Found {length(traced_results[[called_arg_name]])} indirect chk call{?s} for {.arg {arg_name}}"
              )
            } else {
              cli::cli_alert_info(
                "{indent}    No indirect chk calls found for {.arg {arg_name}}"
              )
            }
          } else {
            cli::cli_alert_warning(
              "{indent}    Could not determine parameter mapping for {.arg {arg_name}}"
            )
          }
        }
      } else {
        cli::cli_alert_warning("{indent}    Could not resolve function object")
      }
    } else {
      cli::cli_alert_warning("{indent}    No usage found for {.arg {arg_name}}")
    }
  }

  arg_results
}

# Main function to extract chk function calls by argument
extract_chk_calls <- function(fun, arg = NULL, fun_name = NULL, .depth = 0) {
  # Get the function body and arguments
  fun_body <- body(fun)

  # Capture the function name for verbose messaging
  if (is.null(fun_name)) {
    # Fall back to abbreviated form for complex expressions
    fun_name <- "<function>"
  }

  indent <- paste(rep("  ", .depth), collapse = "")

  # Step 1: If function is an S3 generic, get all methods and recursively analyze them.
  # Combine the results from all methods, stripping duplicates.

  if (is_s3_generic(fun)) {
    return(analyze_s3_generic(
      fun,
      arg,
      fun_name,
      indent,
      .depth,
      extract_chk_calls
    ))
  }

  # For non-generic functions, get arguments from the function itself
  if (is.null(arg)) {
    fun_args <- names(formals(fun))
    fun_args <- fun_args[fun_args != "..."]
    cli::cli_alert_info(
      "{indent}Non-generic function {.fn {fun_name}} with {length(fun_args)} argument{?s}: {.arg {fun_args}}"
    )
  } else {
    stopifnot(arg %in% names(formals(fun)))
    fun_args <- arg
    cli::cli_alert_info(
      "{indent}Non-generic function {.fn {fun_name}}, analyzing argument: {.arg {arg}}"
    )
  }

  # Step 2: Find all top-level chk:: calls in the function body for the function's arguments.
  cli::cli_alert("{indent}Step 2: Searching for chk:: calls in function body")

  # Initialize results for each argument
  arg_results <- stats::setNames(lapply(fun_args, function(x) list()), fun_args)

  # Find all chk:: calls in the function body
  arg_results <- find_chk_calls_in_body(fun_body, fun_args, arg_results)

  # Report findings
  args_with_calls <- names(arg_results)[lengths(arg_results) > 0]
  args_without_calls <- names(arg_results)[lengths(arg_results) == 0]
  n_calls <- sum(lengths(arg_results))

  if (length(args_with_calls) > 0) {
    cli::cli_alert_success(
      "{indent}Found {n_calls} chk call{?s} across {length(args_with_calls)} argument{?s}: {.arg {args_with_calls}}"
    )
  } else {
    cli::cli_alert_warning("{indent}No chk calls found")
  }

  # Step 3: For arguments without direct chk:: calls, find the first usage in another expression and
  # recursively analyze that expression for chk:: calls. Use the result for only that argument.

  if (length(args_without_calls) > 0) {
    cli::cli_alert_info(
      "{indent}No chk calls found for {length(args_without_calls)} argument{?s}: {.arg {args_without_calls}}"
    )
    cli::cli_alert(
      "{indent}Step 3: Tracing indirect argument usage for {length(args_without_calls)} argument{?s}"
    )

    arg_results <- trace_indirect_chk_calls(
      args_without_calls,
      fun_body,
      arg_results,
      fun,
      indent,
      .depth,
      extract_chk_calls
    )
  } else {
    cli::cli_alert_info("{indent}All arguments have chk calls")
  }

  # Step 4: Return a named list of lists of quoted expressions for each argument analyzed.
  cli::cli_alert(
    "{indent}Returning results for {length(fun_args)} argument{?s}"
  )
  arg_results
}

combine_chk_calls <- function(fun) {
  combined <- extract_chk_calls(fun)
  empty <- (lengths(combined) == 0)
  combined[empty] <- purrr::imap(
    names(empty)[empty],
    ~ rlang::expr(quote(FIXME_ADD_CHECK_CALL(!!rlang::sym(.x))))
  )

  rlang::call2("{", !!!unlist(unname(combined)))
}
