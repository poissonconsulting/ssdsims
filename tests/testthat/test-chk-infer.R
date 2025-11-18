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

test_that("is_chk_call detects chk namespace calls", {
  expr1 <- quote(chk::chk_string(x))
  expect_true(is_chk_call(expr1))

  expr2 <- quote(chk::chk_flag(y))
  expect_true(is_chk_call(expr2))

  expr3 <- quote(print(x))
  expect_false(is_chk_call(expr3))

  expr4 <- quote(other::fun(x))
  expect_false(is_chk_call(expr4))
})

test_that("find_chk_calls_in_body extracts chk calls", {
  fun_args <- c("x", "y")
  arg_results <- setNames(lapply(fun_args, function(x) list()), fun_args)

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
