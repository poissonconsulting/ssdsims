test_that("contains_use_method detects UseMethod call", {
  expr1 <- quote(UseMethod("foo"))
  expect_true(contains_use_method(expr1))

  expr2 <- quote({
    x <- 1
    UseMethod("foo")
  })
  expect_true(contains_use_method(expr2))

  expr3 <- quote(print(x))
  expect_false(contains_use_method(expr3))
})

test_that("find_use_method extracts generic name", {
  expr1 <- quote(UseMethod("foo"))
  expect_equal(find_use_method(expr1), "foo")

  expr2 <- quote({
    x <- 1
    UseMethod("bar")
  })
  expect_equal(find_use_method(expr2), "bar")

  expr3 <- quote(print(x))
  expect_null(find_use_method(expr3))
})

test_that("is_s3_generic identifies S3 generics", {
  # Create a simple S3 generic
  test_generic <- function(x, ...) {
    UseMethod("test_generic")
  }

  expect_true(is_s3_generic(test_generic))

  # Regular function
  regular_fun <- function(x) {
    x + 1
  }

  expect_false(is_s3_generic(regular_fun))
})

test_that("collect_method_args collects unique arguments", {
  method1 <- function(x, y, z) NULL
  method2 <- function(x, a, b) NULL
  method3 <- function(x, y, c) NULL

  method_funs <- list(
    method1 = method1,
    method2 = method2,
    method3 = method3
  )

  args <- collect_method_args(method_funs)
  expect_setequal(args, c("x", "y", "z", "a", "b", "c"))
})

test_that("combine_method_results deduplicates expressions", {
  # Create mock results with duplicate expressions
  expr1 <- quote(chk_not_null(x))
  expr2 <- quote(chk_not_null(y))
  expr3 <- quote(chk_not_null(x)) # Duplicate of expr1

  results_list <- list(
    list(x = list(expr1), y = list(expr2)),
    list(x = list(expr3), y = list())
  )

  fun_args <- c("x", "y")

  combined <- combine_method_results(results_list, fun_args)

  expect_length(combined$x, 1) # Should deduplicate
  expect_length(combined$y, 1)
  expect_identical(combined$x[[1]], expr1)
  expect_identical(combined$y[[1]], expr2)
})

test_that("analyze_s3_generic works with mocked extract_chk_calls", {
  # Create a simple S3 generic with methods
  # Need to assign to global environment for methods() to find them
  local({
    test_fun <<- function(x, y, ...) {
      UseMethod("test_fun")
    }

    test_fun.default <<- function(x, y, z = 1) {
      x + y + z
    }

    test_fun.numeric <<- function(x, y, a = 2) {
      x * y + a
    }
  })

  # Clean up after test
  on.exit(
    {
      rm(test_fun, test_fun.default, test_fun.numeric, envir = .GlobalEnv)
    },
    add = TRUE
  ) # Mock extract_chk_calls to return simple chk_not_null calls
  mock_extract_chk_calls <- function(
    fun,
    arg = NULL,
    fun_name = NULL,
    .depth = 0
  ) {
    fun_args <- names(formals(fun))
    fun_args <- fun_args[fun_args != "..."]

    if (!is.null(arg)) {
      fun_args <- arg
    }

    result <- stats::setNames(
      lapply(fun_args, function(arg_name) {
        list(call("chk_not_null", as.symbol(arg_name)))
      }),
      fun_args
    )

    result
  }

  # Test analyze_s3_generic with the mock
  result <- analyze_s3_generic(
    fun = test_fun,
    arg = NULL,
    fun_name = "test_fun",
    indent = "",
    .depth = 0,
    extract_chk_calls = mock_extract_chk_calls
  )

  # Should have collected arguments from both methods
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true("z" %in% names(result))
  expect_true("a" %in% names(result))

  # Each argument should have a chk_not_null call
  expect_length(result$x, 1)
  expect_equal(result$x[[1]], quote(chk_not_null(x)))

  expect_length(result$y, 1)
  expect_equal(result$y[[1]], quote(chk_not_null(y)))

  expect_length(result$z, 1)
  expect_equal(result$z[[1]], quote(chk_not_null(z)))

  expect_length(result$a, 1)
  expect_equal(result$a[[1]], quote(chk_not_null(a)))
})

test_that("analyze_s3_generic with specific argument", {
  # Create a simple S3 generic with methods
  local({
    test_fun <<- function(x, y, ...) {
      UseMethod("test_fun")
    }

    test_fun.default <<- function(x, y, z = 1) {
      x + y + z
    }

    test_fun.numeric <<- function(x, y, a = 2) {
      x * y + a
    }
  })

  # Clean up after test
  on.exit(
    {
      rm(test_fun, test_fun.default, test_fun.numeric, envir = .GlobalEnv)
    },
    add = TRUE
  )

  # Mock extract_chk_calls
  mock_extract_chk_calls <- function(
    fun,
    arg = NULL,
    fun_name = NULL,
    .depth = 0
  ) {
    fun_args <- names(formals(fun))
    fun_args <- fun_args[fun_args != "..."]

    if (!is.null(arg)) {
      fun_args <- arg
    }

    result <- stats::setNames(
      lapply(fun_args, function(arg_name) {
        list(call("chk_not_null", as.symbol(arg_name)))
      }),
      fun_args
    )

    result
  }

  rlang::local_options(cli.default_handler = invisible)

  # Test with specific argument "x"
  result <- analyze_s3_generic(
    fun = test_fun,
    arg = "x",
    fun_name = "test_fun",
    indent = "",
    .depth = 0,
    extract_chk_calls = mock_extract_chk_calls
  )

  # Should only have results for "x"
  expect_equal(names(result), "x")
  expect_length(result$x, 1)
  expect_equal(result$x[[1]], quote(chk_not_null(x)))
})

test_that("analyze_s3_generic skips methods without requested argument", {
  # Create a simple S3 generic with methods
  local({
    test_fun <<- function(x, ...) {
      UseMethod("test_fun")
    }

    test_fun.default <<- function(x, y) {
      x + y
    }

    test_fun.numeric <<- function(x, z) {
      # Has 'z' instead of 'y'
      x * z
    }
  })

  # Clean up after test
  on.exit(
    {
      rm(test_fun, test_fun.default, test_fun.numeric, envir = .GlobalEnv)
    },
    add = TRUE
  )

  # Mock extract_chk_calls
  mock_extract_chk_calls <- function(
    fun,
    arg = NULL,
    fun_name = NULL,
    .depth = 0
  ) {
    fun_args <- names(formals(fun))

    if (!is.null(arg)) {
      if (!(arg %in% fun_args)) {
        # This method doesn't have the requested argument
        result <- list()
        result[[arg]] <- list()
        return(result)
      }
      fun_args <- arg
    }

    result <- stats::setNames(
      lapply(fun_args, function(arg_name) {
        list(call("chk_not_null", as.symbol(arg_name)))
      }),
      fun_args
    )

    result
  }

  rlang::local_options(cli.default_handler = invisible)

  # Test with argument "y" which only exists in default method
  result <- analyze_s3_generic(
    fun = test_fun,
    arg = "y",
    fun_name = "test_fun",
    indent = "",
    .depth = 0,
    extract_chk_calls = mock_extract_chk_calls
  )

  # Should have results for "y" only from the default method
  expect_equal(names(result), "y")
  expect_length(result$y, 1)
  expect_equal(result$y[[1]], quote(chk_not_null(y)))
})

test_that("get_s3_methods retrieves methods correctly", {
  # Create a simple S3 generic with methods
  local({
    test_gen <<- function(x, ...) {
      UseMethod("test_gen")
    }

    test_gen.default <<- function(x, y) {
      x + y
    }

    test_gen.numeric <<- function(x, z) {
      x * z
    }
  })

  # Clean up after test
  on.exit(
    {
      rm(test_gen, test_gen.default, test_gen.numeric, envir = .GlobalEnv)
    },
    add = TRUE
  )

  methods_list <- get_s3_methods(test_gen)

  expect_type(methods_list, "list")
  expect_true("test_gen.default" %in% names(methods_list))
  expect_true("test_gen.numeric" %in% names(methods_list))
  expect_true(is.function(methods_list$test_gen.default))
  expect_true(is.function(methods_list$test_gen.numeric))
})

test_that("extract_chk_calls snapshot: S3 generic with multiple methods", {
  # Create S3 generic with methods
  local({
    validate_obj <<- function(x, ...) {
      UseMethod("validate_obj")
    }

    validate_obj.default <<- function(x, strict = FALSE) {
      chk::chk_not_null(x)
      if (strict) {
        chk::chk_not_empty(x)
      }
      x
    }

    validate_obj.numeric <<- function(x, min_val = 0, max_val = 100) {
      chk::chk_number(x)
      chk::chk_gte(x, min_val)
      chk::chk_lte(x, max_val)
      x
    }

    validate_obj.character <<- function(x, pattern = NULL) {
      chk::chk_string(x)
      if (!is.null(pattern)) {
        chk::chk_match(x, pattern)
      }
      x
    }
  })

  on.exit(
    {
      rm(
        validate_obj,
        validate_obj.default,
        validate_obj.numeric,
        validate_obj.character,
        envir = .GlobalEnv
      )
    },
    add = TRUE
  )

  expect_snapshot({
    extract_chk_calls(validate_obj, fun_name = "validate_obj")
  })
})

test_that("extract_chk_calls snapshot: S3 generic analyzing specific argument", {
  # Create S3 generic with methods
  local({
    process_data <<- function(x, ...) {
      UseMethod("process_data")
    }

    process_data.default <<- function(x, verbose = FALSE, timeout = 60) {
      chk::chk_flag(verbose)
      chk::chk_whole_number(timeout)
      x
    }

    process_data.data.frame <<- function(x, verbose = FALSE, nrows = NULL) {
      chk::chk_flag(verbose)
      if (!is.null(nrows)) {
        chk::chk_whole_number(nrows)
      }
      x
    }
  })

  on.exit(
    {
      rm(
        process_data,
        process_data.default,
        process_data.data.frame,
        envir = .GlobalEnv
      )
    },
    add = TRUE
  )

  expect_snapshot({
    # Analyze only the 'verbose' argument
    extract_chk_calls(process_data, arg = "verbose", fun_name = "process_data")
  })
})

test_that("extract_chk_calls snapshot: S3 generic with indirect validation", {
  # Helper function
  local({
    check_positive <<- function(val) {
      chk::chk_number(val)
      chk::chk_gt(val, 0)
      val
    }

    compute <<- function(x, ...) {
      UseMethod("compute")
    }

    compute.default <<- function(x, scale = 1) {
      chk::chk_not_null(x)
      check_positive(scale)
      x * scale
    }

    compute.numeric <<- function(x, scale = 1, offset = 0) {
      chk::chk_number(x)
      check_positive(scale)
      chk::chk_number(offset)
      x * scale + offset
    }
  })

  on.exit(
    {
      rm(
        check_positive,
        compute,
        compute.default,
        compute.numeric,
        envir = .GlobalEnv
      )
    },
    add = TRUE
  )

  expect_snapshot({
    extract_chk_calls(compute, fun_name = "compute")
  })
})

test_that("combine_method_results snapshot: deduplicates across methods", {
  # Create expressions that appear in multiple methods
  expr1 <- quote(chk::chk_flag(verbose))
  expr2 <- quote(chk::chk_number(x))
  expr3 <- quote(chk::chk_string(name))

  results_list <- list(
    method1 = list(
      verbose = list(expr1),
      x = list(expr2),
      name = list(expr3)
    ),
    method2 = list(
      verbose = list(expr1), # Duplicate
      x = list(expr2), # Duplicate
      name = list()
    ),
    method3 = list(
      verbose = list(expr1), # Duplicate
      x = list(),
      name = list(expr3) # Duplicate
    )
  )

  fun_args <- c("verbose", "x", "name")
  expect_snapshot({
    combine_method_results(results_list, fun_args)
  })
})
