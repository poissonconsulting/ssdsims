# extract_chk_calls snapshot: simple function with direct non-namespaced chk calls

    Code
      extract_chk_calls(test_fun, fun_name = "test_fun")
    Message
      i Non-generic function `test_fun()` with 3 arguments: `x`, `y`, and `z`
      > Step 2: Searching for chk:: calls in function body
      v Found 3 chk calls across 3 arguments: `x`, `y`, and `z`
      i All arguments have chk calls
      > Returning results for 3 arguments
    Output
      $x
      $x[[1]]
      chk::chk_string(x)
      
      
      $y
      $y[[1]]
      chk::chk_flag(y)
      
      
      $z
      $z[[1]]
      chk::chk_number(z)
      
      

# extract_chk_calls snapshot: simple function with direct chk calls

    Code
      extract_chk_calls(test_fun, fun_name = "test_fun")
    Message
      i Non-generic function `test_fun()` with 3 arguments: `x`, `y`, and `z`
      > Step 2: Searching for chk:: calls in function body
      v Found 3 chk calls across 3 arguments: `x`, `y`, and `z`
      i All arguments have chk calls
      > Returning results for 3 arguments
    Output
      $x
      $x[[1]]
      chk::chk_string(x)
      
      
      $y
      $y[[1]]
      chk::chk_flag(y)
      
      
      $z
      $z[[1]]
      chk::chk_number(z)
      
      

# extract_chk_calls snapshot: function with indirect chk calls

    Code
      extract_chk_calls(test_fun, fun_name = "test_fun")
    Message
      i Non-generic function `test_fun()` with 2 arguments: `x` and `y`
      > Step 2: Searching for chk:: calls in function body
      v Found 1 chk call across 1 argument: `y`
      i No chk calls found for 1 argument: `x`
      > Step 3: Tracing indirect argument usage for 1 argument
      >   Tracing `x`...
      i     First usage found in call to `validate_input()`
      i     Function has 1 formal: `val`
      i     Argument `x` maps to parameter `val`
      >     Recursively analyzing `validate_input()` for `val`...
      i   Non-generic function `validate_input()`, analyzing argument: `val`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 2 chk calls across 1 argument: `val`
      i   All arguments have chk calls
      >   Returning results for 1 argument
      v     Found 2 indirect chk calls for `x`
      > Returning results for 2 arguments
    Output
      $x
      $x[[1]]
      chk::chk_string(val)
      
      $x[[2]]
      chk::chk_not_empty(val)
      
      
      $y
      $y[[1]]
      chk::chk_flag(y)
      
      

# extract_chk_calls snapshot: function with mixed validation

    Code
      extract_chk_calls(test_fun, fun_name = "test_fun")
    Message
      i Non-generic function `test_fun()` with 3 arguments: `x`, `y`, and `z`
      > Step 2: Searching for chk:: calls in function body
      v Found 1 chk call across 1 argument: `x`
      i No chk calls found for 2 arguments: `y` and `z`
      > Step 3: Tracing indirect argument usage for 2 arguments
      >   Tracing `y`...
      i     First usage found in call to `helper1()`
      i     Function has 1 formal: `a`
      i     Argument `y` maps to parameter `a`
      >     Recursively analyzing `helper1()` for `a`...
      i   Non-generic function `helper1()`, analyzing argument: `a`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 1 chk call across 1 argument: `a`
      i   All arguments have chk calls
      >   Returning results for 1 argument
      v     Found 1 indirect chk call for `y`
      >   Tracing `z`...
      i     First usage found in call to `helper2()`
      i     Function has 1 formal: `b`
      i     Argument `z` maps to parameter `b`
      >     Recursively analyzing `helper2()` for `b`...
      i   Non-generic function `helper2()`, analyzing argument: `b`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 1 chk call across 1 argument: `b`
      i   All arguments have chk calls
      >   Returning results for 1 argument
      v     Found 1 indirect chk call for `z`
      > Returning results for 3 arguments
    Output
      $x
      $x[[1]]
      chk::chk_string(x)
      
      
      $y
      $y[[1]]
      chk::chk_whole_number(a)
      
      
      $z
      $z[[1]]
      chk::chk_range(b, c(0, 1))
      
      

# extract_chk_calls snapshot: single argument analysis

    Code
      extract_chk_calls(test_fun, arg = "y", fun_name = "test_fun")
    Message
      i Non-generic function `test_fun()`, analyzing argument: `y`
      > Step 2: Searching for chk:: calls in function body
      v Found 1 chk call across 1 argument: `y`
      i All arguments have chk calls
      > Returning results for 1 argument
    Output
      $y
      $y[[1]]
      chk::chk_flag(y)
      
      

# extract_chk_calls snapshot: nested function calls

    Code
      extract_chk_calls(test_fun, fun_name = "test_fun")
    Message
      i Non-generic function `test_fun()` with 1 argument: `x`
      > Step 2: Searching for chk:: calls in function body
      ! No chk calls found
      i No chk calls found for 1 argument: `x`
      > Step 3: Tracing indirect argument usage for 1 argument
      >   Tracing `x`...
      i     First usage found in call to `middle_validate()`
      i     Function has 1 formal: `val`
      i     Argument `x` maps to parameter `val`
      >     Recursively analyzing `middle_validate()` for `val`...
      i   Non-generic function `middle_validate()`, analyzing argument: `val`
      >   Step 2: Searching for chk:: calls in function body
      !   No chk calls found
      i   No chk calls found for 1 argument: `val`
      >   Step 3: Tracing indirect argument usage for 1 argument
      >     Tracing `val`...
      i       First usage found in call to `inner_validate()`
      i       Function has 1 formal: `val`
      i       Argument `val` maps to parameter `val`
      >       Recursively analyzing `inner_validate()` for `val`...
      i     Non-generic function `inner_validate()`, analyzing argument: `val`
      >     Step 2: Searching for chk:: calls in function body
      v     Found 1 chk call across 1 argument: `val`
      i     All arguments have chk calls
      >     Returning results for 1 argument
      v       Found 1 indirect chk call for `val`
      >   Returning results for 1 argument
      v     Found 1 indirect chk call for `x`
      > Returning results for 1 argument
    Output
      $x
      $x[[1]]
      chk::chk_gt(val, 0)
      
      

