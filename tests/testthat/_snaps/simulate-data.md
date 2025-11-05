# ssd_simulate_data.function works

    Code
      data
    Output
      # A tibble: 10 x 5
           sim stream  nrow args       data            
         <int>  <int> <dbl> <list>     <list>          
       1     1      1     5 <list [0]> <tibble [5 x 1]>
       2     2      1     5 <list [0]> <tibble [5 x 1]>
       3     3      1     5 <list [0]> <tibble [5 x 1]>
       4     4      1     5 <list [0]> <tibble [5 x 1]>
       5     5      1     5 <list [0]> <tibble [5 x 1]>
       6     6      1     5 <list [0]> <tibble [5 x 1]>
       7     7      1     5 <list [0]> <tibble [5 x 1]>
       8     8      1     5 <list [0]> <tibble [5 x 1]>
       9     9      1     5 <list [0]> <tibble [5 x 1]>
      10    10      1     5 <list [0]> <tibble [5 x 1]>

# ssd_simulate_data.function vectorized

    Code
      data
    Output
      # A tibble: 4 x 4
          sim stream  nrow data             
        <int>  <int> <dbl> <list>           
      1     1      1     5 <tibble [5 x 1]> 
      2     1      1    10 <tibble [10 x 1]>
      3     2      1     5 <tibble [5 x 1]> 
      4     2      1    10 <tibble [10 x 1]>

# ssd_simulate_data.character works

    Code
      data
    Output
      # A tibble: 10 x 5
           sim stream  nrow args       data            
         <int>  <int> <dbl> <list>     <list>          
       1     1      1     5 <list [0]> <tibble [5 x 1]>
       2     2      1     5 <list [0]> <tibble [5 x 1]>
       3     3      1     5 <list [0]> <tibble [5 x 1]>
       4     4      1     5 <list [0]> <tibble [5 x 1]>
       5     5      1     5 <list [0]> <tibble [5 x 1]>
       6     6      1     5 <list [0]> <tibble [5 x 1]>
       7     7      1     5 <list [0]> <tibble [5 x 1]>
       8     8      1     5 <list [0]> <tibble [5 x 1]>
       9     9      1     5 <list [0]> <tibble [5 x 1]>
      10    10      1     5 <list [0]> <tibble [5 x 1]>

# ssd_simulate_data.character vectorized

    Code
      data
    Output
      # A tibble: 4 x 4
          sim stream  nrow data             
        <int>  <int> <dbl> <list>           
      1     1      1     5 <tibble [5 x 1]> 
      2     1      1    10 <tibble [10 x 1]>
      3     2      1     5 <tibble [5 x 1]> 
      4     2      1    10 <tibble [10 x 1]>

# ssd_simulate_data.fitdists works top

    Code
      data
    Output
      # A tibble: 10 x 5
           sim stream  nrow args             data            
         <int>  <int> <dbl> <list>           <list>          
       1     1      1     5 <named list [2]> <tibble [5 x 1]>
       2     2      1     5 <named list [2]> <tibble [5 x 1]>
       3     3      1     5 <named list [2]> <tibble [5 x 1]>
       4     4      1     5 <named list [2]> <tibble [5 x 1]>
       5     5      1     5 <named list [2]> <tibble [5 x 1]>
       6     6      1     5 <named list [2]> <tibble [5 x 1]>
       7     7      1     5 <named list [2]> <tibble [5 x 1]>
       8     8      1     5 <named list [2]> <tibble [5 x 1]>
       9     9      1     5 <named list [2]> <tibble [5 x 1]>
      10    10      1     5 <named list [2]> <tibble [5 x 1]>

# ssd_simulate_data.fitdists works multi

    Code
      data
    Output
      # A tibble: 10 x 5
           sim stream  nrow args             data            
         <int>  <int> <dbl> <list>           <list>          
       1     1      1     5 <named list [1]> <tibble [5 x 1]>
       2     2      1     5 <named list [1]> <tibble [5 x 1]>
       3     3      1     5 <named list [1]> <tibble [5 x 1]>
       4     4      1     5 <named list [1]> <tibble [5 x 1]>
       5     5      1     5 <named list [1]> <tibble [5 x 1]>
       6     6      1     5 <named list [1]> <tibble [5 x 1]>
       7     7      1     5 <named list [1]> <tibble [5 x 1]>
       8     8      1     5 <named list [1]> <tibble [5 x 1]>
       9     9      1     5 <named list [1]> <tibble [5 x 1]>
      10    10      1     5 <named list [1]> <tibble [5 x 1]>

# ssd_simulate_data.fitdists works name

    Code
      data
    Output
      # A tibble: 10 x 5
           sim stream  nrow args             data            
         <int>  <int> <dbl> <list>           <list>          
       1     1      1     5 <named list [2]> <tibble [5 x 1]>
       2     2      1     5 <named list [2]> <tibble [5 x 1]>
       3     3      1     5 <named list [2]> <tibble [5 x 1]>
       4     4      1     5 <named list [2]> <tibble [5 x 1]>
       5     5      1     5 <named list [2]> <tibble [5 x 1]>
       6     6      1     5 <named list [2]> <tibble [5 x 1]>
       7     7      1     5 <named list [2]> <tibble [5 x 1]>
       8     8      1     5 <named list [2]> <tibble [5 x 1]>
       9     9      1     5 <named list [2]> <tibble [5 x 1]>
      10    10      1     5 <named list [2]> <tibble [5 x 1]>

# ssd_simulate_data.tmbfit works

    Code
      data
    Output
      # A tibble: 10 x 5
           sim stream  nrow args             data            
         <int>  <int> <dbl> <list>           <list>          
       1     1      1     5 <named list [2]> <tibble [5 x 1]>
       2     2      1     5 <named list [2]> <tibble [5 x 1]>
       3     3      1     5 <named list [2]> <tibble [5 x 1]>
       4     4      1     5 <named list [2]> <tibble [5 x 1]>
       5     5      1     5 <named list [2]> <tibble [5 x 1]>
       6     6      1     5 <named list [2]> <tibble [5 x 1]>
       7     7      1     5 <named list [2]> <tibble [5 x 1]>
       8     8      1     5 <named list [2]> <tibble [5 x 1]>
       9     9      1     5 <named list [2]> <tibble [5 x 1]>
      10    10      1     5 <named list [2]> <tibble [5 x 1]>

# ssd_simulate_data.data.frame works

    Code
      data
    Output
      # A tibble: 10 x 5
           sim stream  nrow replace data            
         <int>  <int> <dbl> <lgl>   <list>          
       1     1      1     5 FALSE   <tibble [5 x 5]>
       2     2      1     5 FALSE   <tibble [5 x 5]>
       3     3      1     5 FALSE   <tibble [5 x 5]>
       4     4      1     5 FALSE   <tibble [5 x 5]>
       5     5      1     5 FALSE   <tibble [5 x 5]>
       6     6      1     5 FALSE   <tibble [5 x 5]>
       7     7      1     5 FALSE   <tibble [5 x 5]>
       8     8      1     5 FALSE   <tibble [5 x 5]>
       9     9      1     5 FALSE   <tibble [5 x 5]>
      10    10      1     5 FALSE   <tibble [5 x 5]>

# ssd_simulate_data.data.frame vectorized

    Code
      data
    Output
      # A tibble: 4 x 5
          sim stream replace  nrow data             
        <int>  <int> <lgl>   <dbl> <list>           
      1     1      1 FALSE       5 <tibble [5 x 5]> 
      2     1      1 FALSE      10 <tibble [10 x 5]>
      3     2      1 FALSE       5 <tibble [5 x 5]> 
      4     2      1 FALSE      10 <tibble [10 x 5]>

