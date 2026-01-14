# extract_chk_calls snapshot: S3 generic with multiple methods

    Code
      extract_chk_calls(validate_obj, fun_name = "validate_obj")
    Message
      i Function `validate_obj()` is an S3 generic
      i Found 3 methods: `validate_obj.character()`, `validate_obj.default()`, and `validate_obj.numeric()`
      i Collected 5 unique arguments across all methods: `x`, `pattern`, `strict`, `min_val`, and `max_val`
      i Analyzing all arguments
      > Recursively analyzing each method...
      i   Non-generic function `validate_obj.character()` with 2 arguments: `x` and `pattern`
      >   Step 2: Searching for chk:: calls in function body
      i Loaded 182 chk package exports
      v   Found 3 chk calls across 2 arguments: `x` and `pattern`
      i   All arguments have chk calls
      >   Returning results for 2 arguments
      i   Non-generic function `validate_obj.default()` with 2 arguments: `x` and `strict`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 2 chk calls across 1 argument: `x`
      i   No chk calls found for 1 argument: `strict`
      >   Step 3: Tracing indirect argument usage for 1 argument
      >     Tracing `strict`...
      i       First usage found in call to `if()`
      i       Function is a primitive (base)
      >   Returning results for 2 arguments
      i   Non-generic function `validate_obj.numeric()` with 3 arguments: `x`, `min_val`, and `max_val`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 5 chk calls across 3 arguments: `x`, `min_val`, and `max_val`
      i   All arguments have chk calls
      >   Returning results for 3 arguments
      > Combining results from all methods
    Output
      $x
      $x[[1]]
      chk::chk_string(x)
      
      $x[[2]]
      chk::chk_match(x, pattern)
      
      $x[[3]]
      chk::chk_not_null(x)
      
      $x[[4]]
      chk::chk_not_empty(x)
      
      $x[[5]]
      chk::chk_number(x)
      
      $x[[6]]
      chk::chk_gte(x, min_val)
      
      $x[[7]]
      chk::chk_lte(x, max_val)
      
      
      $pattern
      $pattern[[1]]
      chk::chk_match(x, pattern)
      
      
      $strict
      list()
      
      $min_val
      $min_val[[1]]
      chk::chk_gte(x, min_val)
      
      
      $max_val
      $max_val[[1]]
      chk::chk_lte(x, max_val)
      
      

# extract_chk_calls snapshot: S3 generic analyzing specific argument

    Code
      extract_chk_calls(process_data, arg = "verbose", fun_name = "process_data")
    Message
      i Function `process_data()` is an S3 generic
      i Found 2 methods: `process_data.data.frame()` and `process_data.default()`
      i Collected 4 unique arguments across all methods: `x`, `verbose`, `nrows`, and `timeout`
      i Analyzing specific argument: `verbose`
      > Recursively analyzing each method...
      i   Non-generic function `process_data.data.frame()`, analyzing argument: `verbose`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 1 chk call across 1 argument: `verbose`
      i   All arguments have chk calls
      >   Returning results for 1 argument
      i   Non-generic function `process_data.default()`, analyzing argument: `verbose`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 1 chk call across 1 argument: `verbose`
      i   All arguments have chk calls
      >   Returning results for 1 argument
      > Combining results from all methods
    Output
      $verbose
      $verbose[[1]]
      chk::chk_flag(verbose)
      
      

# extract_chk_calls snapshot: S3 generic with indirect validation

    Code
      extract_chk_calls(compute, fun_name = "compute")
    Message
      i Function `compute()` is an S3 generic
      i Found 2 methods: `compute.default()` and `compute.numeric()`
      i Collected 3 unique arguments across all methods: `x`, `scale`, and `offset`
      i Analyzing all arguments
      > Recursively analyzing each method...
      i   Non-generic function `compute.default()` with 2 arguments: `x` and `scale`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 1 chk call across 1 argument: `x`
      i   No chk calls found for 1 argument: `scale`
      >   Step 3: Tracing indirect argument usage for 1 argument
      >     Tracing `scale`...
      i       First usage found in call to `check_positive()`
      i       Function has 1 formal: `val`
      i       Argument `scale` maps to parameter `val`
      >       Recursively analyzing `check_positive()` for `val`...
      i     Non-generic function `check_positive()`, analyzing argument: `val`
      >     Step 2: Searching for chk:: calls in function body
      v     Found 2 chk calls across 1 argument: `val`
      i     All arguments have chk calls
      >     Returning results for 1 argument
      v       Found 2 indirect chk calls for `scale`
      >   Returning results for 2 arguments
      i   Non-generic function `compute.numeric()` with 3 arguments: `x`, `scale`, and `offset`
      >   Step 2: Searching for chk:: calls in function body
      v   Found 2 chk calls across 2 arguments: `x` and `offset`
      i   No chk calls found for 1 argument: `scale`
      >   Step 3: Tracing indirect argument usage for 1 argument
      >     Tracing `scale`...
      i       First usage found in call to `check_positive()`
      i       Function has 1 formal: `val`
      i       Argument `scale` maps to parameter `val`
      >       Recursively analyzing `check_positive()` for `val`...
      i     Non-generic function `check_positive()`, analyzing argument: `val`
      >     Step 2: Searching for chk:: calls in function body
      v     Found 2 chk calls across 1 argument: `val`
      i     All arguments have chk calls
      >     Returning results for 1 argument
      v       Found 2 indirect chk calls for `scale`
      >   Returning results for 3 arguments
      > Combining results from all methods
    Output
      $x
      $x[[1]]
      chk::chk_not_null(x)
      
      $x[[2]]
      chk::chk_number(x)
      
      
      $scale
      $scale[[1]]
      chk::chk_number(val)
      
      $scale[[2]]
      chk::chk_gt(val, 0)
      
      
      $offset
      $offset[[1]]
      chk::chk_number(offset)
      
      

# combine_method_results snapshot: deduplicates across methods

    Code
      combine_method_results(results_list, fun_args)
    Output
      $verbose
      $verbose[[1]]
      chk::chk_flag(verbose)
      
      
      $x
      $x[[1]]
      chk::chk_number(x)
      
      
      $name
      $name[[1]]
      chk::chk_string(name)
      
      

