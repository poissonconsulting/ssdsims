# Tests for chk-infer.R helper functions

test_that("contains_symbol finds symbols in expressions", {
  expr1 <- quote(x + y)
  expect_true(contains_symbol(expr1, "x"))
  expect_true(contains_symbol(expr1, "y"))
  expect_false(contains_symbol(expr1, "z"))

  expr2 <- quote(foo(x, bar(y)))
  expect_true(contains_symbol(expr2, "x"))
  expect_true(contains_symbol(expr2, "y"))
  expect_false(contains_symbol(expr2, "z"))
})

test_that("normalize_chk_call detects and normalizes chk calls", {
  # Namespaced call - should return as-is
  expr1 <- quote(chk::chk_string(x))
  result1 <- normalize_chk_call(expr1)
  expect_false(is.null(result1))
  expect_equal(result1, expr1)

  # Another namespaced call
  expr2 <- quote(chk::chk_flag(y))
  result2 <- normalize_chk_call(expr2)
  expect_false(is.null(result2))
  expect_equal(result2, expr2)

  # Non-namespaced chk call - should be converted to namespaced
  expr3 <- quote(chk_string(x))
  result3 <- normalize_chk_call(expr3)
  expect_false(is.null(result3))
  expect_equal(result3, quote(chk::chk_string(x)))

  # Non-chk call - should return NULL
  expr4 <- quote(print(x))
  result4 <- normalize_chk_call(expr4)
  expect_null(result4)

  # Different namespace - should return NULL
  expr5 <- quote(other::fun(x))
  result5 <- normalize_chk_call(expr5)
  expect_null(result5)
})

test_that("find_chk_calls_in_body extracts chk calls", {
  fun_args <- c("x", "y")
  arg_results <- stats::setNames(lapply(fun_args, function(x) list()), fun_args)

  # Body with chk calls
  body_expr <- quote({
    chk::chk_string(x)
    chk::chk_flag(y)
    print(x + y)
  })

  result <- find_chk_calls_in_body(body_expr, fun_args, arg_results)

  expect_length(result$x, 1)
  expect_length(result$y, 1)
  expect_equal(result$x[[1]], quote(chk::chk_string(x)))
  expect_equal(result$y[[1]], quote(chk::chk_flag(y)))
})

test_that("get_function_from_call extracts function objects", {
  # Test with namespace call
  call_expr1 <- quote(base::mean(x))
  result1 <- get_function_from_call(call_expr1, .GlobalEnv)
  expect_equal(result1$display_name, "base::mean")
  expect_true(is.function(result1$fun_obj))

  # Test with plain symbol
  call_expr2 <- quote(sum(x))
  result2 <- get_function_from_call(call_expr2, .GlobalEnv)
  expect_equal(result2$display_name, "sum")
  expect_true(is.function(result2$fun_obj))
})

test_that("find_first_usage finds argument usage in expressions", {
  body_expr <- quote({
    y <- foo(x)
    z <- bar(w)
  })

  # Find first usage of x
  result_x <- find_first_usage(body_expr, "x")
  expect_false(is.null(result_x))
  expect_equal(result_x$call, quote(foo(x)))
  expect_equal(result_x$arg_position, 2)

  # Find first usage of y
  result_w <- find_first_usage(body_expr, "w")
  expect_false(is.null(result_w))
  expect_equal(result_w$call, quote(bar(w)))
})

test_that("find_first_usage finds innermost usage", {
  body_expr <- quote({
    y <- outer(inner(x))
  })

  result <- find_first_usage(body_expr, "x")
  expect_false(is.null(result))
  # Should find the innermost call containing x
  expect_equal(result$call, quote(inner(x)))
})

test_that("determine_called_arg_name matches arguments", {
  test_fun <- function(a, b, c) {
    a + b + c
  }

  call_expr <- quote(test_fun(a = my_x, b = my_y, c = my_z))

  result <- determine_called_arg_name(
    call_expr,
    "my_x",
    test_fun,
    ""
  )

  expect_equal(result, "a")
})

test_that("extract_chk_calls works with non-generic function", {
  test_fun <- function(x, y) {
    chk::chk_string(x)
    chk::chk_flag(y)
    x
  }

  rlang::local_options(cli.default_handler = invisible)

  result <- extract_chk_calls(test_fun, fun_name = "test_fun")

  expect_equal(names(result), c("x", "y"))
  expect_length(result$x, 1)
  expect_length(result$y, 1)
  expect_equal(result$x[[1]], quote(chk::chk_string(x)))
  expect_equal(result$y[[1]], quote(chk::chk_flag(y)))
})

test_that("extract_chk_calls with specific argument", {
  test_fun <- function(x, y) {
    chk::chk_string(x)
    chk::chk_flag(y)
    x
  }

  rlang::local_options(cli.default_handler = invisible)

  result <- extract_chk_calls(test_fun, arg = "x", fun_name = "test_fun")

  expect_equal(names(result), "x")
  expect_length(result$x, 1)
  expect_equal(result$x[[1]], quote(chk::chk_string(x)))
})

test_that("extract_chk_calls traces indirect usage", {
  helper_fun <- function(val) {
    chk::chk_string(val)
    val
  }

  test_fun <- function(x) {
    helper_fun(x)
  }

  # Add helper_fun to global env so it can be found
  assign("helper_fun", helper_fun, envir = .GlobalEnv)
  on.exit(rm(helper_fun, envir = .GlobalEnv), add = TRUE)

  rlang::local_options(cli.default_handler = invisible)

  result <- extract_chk_calls(test_fun, fun_name = "test_fun")

  expect_equal(names(result), "x")
  expect_length(result$x, 1)
  # Should have traced through to find chk_string on the mapped parameter
  expect_equal(result$x[[1]], quote(chk::chk_string(val)))
})

test_that("extract_chk_calls snapshot: simple function with direct non-namespaced chk calls", {
  test_fun <- function(x, y, z) {
    chk_string(x)
    chk_flag(y)
    chk_number(z)
    paste(x, y, z)
  }

  expect_snapshot({
    extract_chk_calls(test_fun, fun_name = "test_fun")
  })
})

test_that("extract_chk_calls snapshot: simple function with direct chk calls", {
  test_fun <- function(x, y, z) {
    chk::chk_string(x)
    chk::chk_flag(y)
    chk::chk_number(z)
    paste(x, y, z)
  }

  expect_snapshot({
    extract_chk_calls(test_fun, fun_name = "test_fun")
  })
})

test_that("extract_chk_calls snapshot: function with indirect chk calls", {
  validate_input <- function(val) {
    chk::chk_string(val)
    chk::chk_not_empty(val)
    val
  }

  test_fun <- function(x, y) {
    chk::chk_flag(y)
    validate_input(x)
  }

  # Add helper to global env
  assign("validate_input", validate_input, envir = .GlobalEnv)
  on.exit(rm(validate_input, envir = .GlobalEnv), add = TRUE)

  expect_snapshot({
    extract_chk_calls(test_fun, fun_name = "test_fun")
  })
})

test_that("extract_chk_calls snapshot: function with mixed validation", {
  helper1 <- function(a) {
    chk::chk_whole_number(a)
    a
  }

  helper2 <- function(b) {
    chk::chk_range(b, c(0, 1))
    b
  }

  test_fun <- function(x, y, z) {
    chk::chk_string(x)
    helper1(y)
    helper2(z)
  }

  # Add helpers to global env
  assign("helper1", helper1, envir = .GlobalEnv)
  assign("helper2", helper2, envir = .GlobalEnv)
  on.exit(
    {
      rm(helper1, envir = .GlobalEnv)
      rm(helper2, envir = .GlobalEnv)
    },
    add = TRUE
  )

  expect_snapshot({
    extract_chk_calls(test_fun, fun_name = "test_fun")
  })
})

test_that("extract_chk_calls snapshot: single argument analysis", {
  test_fun <- function(x, y, z) {
    chk::chk_string(x)
    chk::chk_flag(y)
    chk::chk_number(z)
    paste(x, y, z)
  }

  expect_snapshot({
    extract_chk_calls(test_fun, arg = "y", fun_name = "test_fun")
  })
})

test_that("extract_chk_calls snapshot: nested function calls", {
  inner_validate <- function(val) {
    chk::chk_gt(val, 0)
    val
  }

  middle_validate <- function(val) {
    inner_validate(val)
  }

  test_fun <- function(x) {
    middle_validate(x)
  }

  # Add helpers to global env
  assign("inner_validate", inner_validate, envir = .GlobalEnv)
  assign("middle_validate", middle_validate, envir = .GlobalEnv)
  on.exit(
    {
      rm(inner_validate, envir = .GlobalEnv)
      rm(middle_validate, envir = .GlobalEnv)
    },
    add = TRUE
  )

  expect_snapshot({
    extract_chk_calls(test_fun, fun_name = "test_fun")
  })
})
