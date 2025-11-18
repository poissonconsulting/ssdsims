# Helper function to check if expression contains UseMethod call
contains_use_method <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], quote(UseMethod))) {
    return(TRUE)
  }
  if (is.call(expr) || is.list(expr)) {
    for (i in seq_along(expr)) {
      if (contains_use_method(expr[[i]])) {
        return(TRUE)
      }
    }
  }
  FALSE
}

# Helper function to find UseMethod call in function body and extract generic name
find_use_method <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], quote(UseMethod))) {
    # UseMethod("name") - extract the name
    if (length(expr) >= 2 && is.character(expr[[2]])) {
      return(expr[[2]])
    }
  }
  if (is.call(expr) || is.list(expr)) {
    for (i in seq_along(expr)) {
      result <- find_use_method(expr[[i]])
      if (!is.null(result)) {
        return(result)
      }
    }
  }
  NULL
}

# Helper function to check if a function is an S3 generic
is_s3_generic <- function(fun) {
  if (!is.null(attr(fun, "generic"))) {
    return(TRUE)
  }

  fun_body <- body(fun)
  if (is.null(fun_body)) {
    return(FALSE)
  }

  contains_use_method(fun_body)
}

# Helper function to get S3 methods for a generic function
get_s3_methods <- function(fun) {
  # Try to get the generic name from the function's attributes first
  if (!is.null(attr(fun, "generic"))) {
    fun_name <- attr(fun, "generic")
  } else {
    # Extract from UseMethod call in function body
    fun_body <- body(fun)
    fun_name <- find_use_method(fun_body)
    if (is.null(fun_name)) {
      stop("Cannot determine generic name from function")
    }
  }

  # Get method names
  method_names <- utils::.S3methods(fun_name, envir = environment(fun))

  # Retrieve actual method functions as a named list
  method_funs <- list()
  for (method in method_names) {
    tryCatch(
      {
        # Extract class name from method name
        class_name <- sub(paste0("^", fun_name, "\\."), "", method)
        method_fun <- getS3method(fun_name, class_name, envir = environment(fun))
        # Use the full method name as the list name
        method_funs[[as.character(method)]] <- method_fun
      },
      error = function(e) {
        # Skip methods we can't access
        NULL
      }
    )
  }

  method_funs
}

# Helper function to combine and deduplicate results from multiple method analyses
combine_method_results <- function(results_list, fun_args) {
  combined <- list()

  for (arg_name in fun_args) {
    # Collect all expressions for this argument across all methods
    all_exprs <- list()
    for (method_result in results_list) {
      if (!is.null(method_result[[arg_name]])) {
        all_exprs <- c(all_exprs, method_result[[arg_name]])
      }
    }

    # Remove duplicates by comparing language objects directly
    if (length(all_exprs) > 0) {
      unique_exprs <- list()
      for (expr in all_exprs) {
        # Check if this expression is already in unique_exprs
        is_duplicate <- FALSE
        for (unique_expr in unique_exprs) {
          if (identical(expr, unique_expr)) {
            is_duplicate <- TRUE
            break
          }
        }
        if (!is_duplicate) {
          unique_exprs[[length(unique_exprs) + 1]] <- expr
        }
      }
      combined[[arg_name]] <- unique_exprs
    } else {
      combined[[arg_name]] <- list()
    }
  }

  combined
}

# Helper function to collect unique argument names from all methods
collect_method_args <- function(method_funs) {
  method_funs |>
    purrr::map(formals) |>
    purrr::map(names) |>
    purrr::map(~ .x[.x != "..."]) |>
    purrr::reduce(c) |>
    unique()
}

# Helper function to handle S3 generic function analysis
analyze_s3_generic <- function(
  fun,
  arg,
  fun_name,
  indent,
  .depth,
  extract_chk_calls
) {
  # Get all S3 methods for this generic (returns named list)
  method_funs <- get_s3_methods(fun)

  cli::cli_alert_info("{indent}Function {.fn {fun_name}} is an S3 generic")
  cli::cli_alert_info(
    "{indent}Found {length(method_funs)} method{?s}: {.fn {names(method_funs)}}"
  )

  # Collect all unique argument names from all methods
  all_args <- collect_method_args(method_funs)
  cli::cli_alert_info(
    "{indent}Collected {length(all_args)} unique argument{?s} across all methods: {.arg {all_args}}"
  )

  # If arg is specified, use only that argument
  if (!is.null(arg)) {
    if (!(arg %in% all_args)) {
      stop("Argument '", arg, "' not found in any method")
    }
    fun_args <- arg
    cli::cli_alert_info("{indent}Analyzing specific argument: {.arg {arg}}")
  } else {
    fun_args <- all_args
    cli::cli_alert_info("{indent}Analyzing all arguments")
  }

  # Recursively analyze each method
  cli::cli_alert("{indent}Recursively analyzing each method...")
  results_list <- purrr::imap(method_funs, function(method_fun, method_name) {
    # If a specific argument is requested, check if this method has it
    if (!is.null(arg)) {
      method_formals <- names(formals(method_fun))
      if (!(arg %in% method_formals)) {
        cli::cli_alert_info(
          "{indent}  Skipping {.fn {method_name}} - does not have argument {.arg {arg}}"
        )
        # Return empty list for this argument
        result <- list()
        result[[arg]] <- list()
        return(result)
      }
    }

    extract_chk_calls(
      method_fun,
      arg = arg,
      fun_name = method_name,
      .depth = .depth + 1
    )
  })

  # Combine and return results from all methods
  cli::cli_alert("{indent}Combining results from all methods")
  combine_method_results(results_list, fun_args)
}
