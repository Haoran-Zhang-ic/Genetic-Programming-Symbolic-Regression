
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpsr

<!-- badges: start -->

<!-- badges: end -->

The goal of gpsr is to solve symbolic regression problem using genetic
programming technique. Genetic programming is a widely used machine
learning method in a variety of tasks: optimization, regression and so
on. It begins by building a population of naive random tree-like
formulas to represent a relationship between known independent variables
and their dependent variable targets in order to predict new data. And
the population of formulas keep evolving by selecting optimal
individuals from themselves.

## Installation

You can install the development version of gpsr like so:

``` r
# use remotes 
remotes::install_github("Haoran-Zhang-ic/Genetic-Programming-Symbolic-Regression")
# or use devtools
devtools::install_github("Haoran-Zhang-ic/Genetic-Programming-Symbolic-Regression")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gpsr)
## basic example code
set.seed(0)
# randomly generate samples
data <- matrix(runif(100, -1, 1), nrow = 50, ncol = 2)
# our target formula: X1^2 - X2^2 + X1 - 1
y <- data[, 1]**2 - data[, 2]**2 + data[, 1] - 1
sr <- SymbolicRegressor$new(
  pop_size = 2000,
  generations = 10,
  p_crossover = 0.7,
  p_subtree_mutation = 0.1,
  p_hoist_mutation = 0.05,
  p_point_mutation = 0.1,
  p_point_replace = 0.1,
  parsimony_coefficient = 0.001
)
# fit the target
sr$fit(data, y)
#>       | Population              | Best Individual         |
#> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#> | Gen | Length | Fitness        | Length | Best Fitness   |
#> |   1 |  31.78 |             NA |      7 |    0.144511048 |
#> |   2 |  12.58 |             NA |      3 |    0.140808962 |
#> |   3 |  10.67 |             NA |      7 |    0.078291445 |
#> |   4 |   6.79 |    0.401222643 |     11 |    0.039638880 |
#> |   5 |   6.25 |    0.140839794 |     11 |    0.000484338 |
#> |   6 |  10.15 |    0.467877422 |     11 |    0.000387067 |
#> |   7 |   9.41 |    0.510928363 |     11 |    0.000387067 |
#> |   8 |  10.81 |    0.474339488 |     11 |    0.000387067 |
#> |   9 |  11.05 |    0.474339488 |     11 |    0.000387067 |
#> |  10 |  10.98 |    0.475791023 |     11 |    0.000387067 |
# find the best program
paste("Best program:",sr$best_program$tree_expression())
#> [1] "Best program: sub(add(-0.980325982905924,X1),sub(mul(X2,X2),mul(X1,X1)))"
```

Our target formula is: $`y = X_1^2 - X_2^2 + X_1 - 1`$.

The best expression we get is $`y = (X_1 - 0.98) - (X_2^2 - X_1^2)`$.

After reform our expression $`y = X_1^2 - X_2^2 + X_1 - 0.98`$, we
(almost) find the formula we are looking for!

Next, let’s try the changeable parameters!

``` r
set.seed(0)
# randomly generate samples
data <- matrix(runif(100, -1, 1), nrow = 50, ncol = 2)
# set a parameter changeable function shift
.shift = function(x,a){
  return(c(rep(1,a),x[-c((length(x) - a + 1):length(x))]))
}
shift = make_function(
  fun = .shift, 
  arity = 2, 
  name = "shift",
  param = 1,
  param_pool = list(c('2','3','4')))
# define our target: shift(X1,3) + shift(X2,3) - X1 - X2
y <- .shift(data[,1],3) + .shift(data[,2],3) - data[,1] - data[,2]
# set our function set
my_function_set = list("add","sub","div","mul",shift)
sr <- SymbolicRegressor$new(
  pop_size = 2000,
  generations = 10,
  p_crossover = 0.7,
  p_subtree_mutation = 0.1,
  p_hoist_mutation = 0.05,
  p_point_mutation = 0.1,
  p_point_replace = 0.1,
  parsimony_coefficient = 0.001,
  function_set = my_function_set,
  metric = 'mse',
  const_range = NULL
)
# fit the target
sr$fit(data, y)
#>       | Population              | Best Individual         |
#> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#> | Gen | Length | Fitness        | Length | Best Fitness   |
#> |   1 |  23.97 |             NA |     15 |    0.352297132 |
#> |   2 |  10.75 |             NA |     15 |    0.000000000 |
#> |   3 |   9.42 |             NA |     11 |    0.000000000 |
#> |   4 |  15.39 |             NA |     11 |    0.000000000 |
#> |   5 |  18.35 |             NA |     11 |    0.000000000 |
#> |   6 |  16.46 |             NA |     11 |    0.000000000 |
#> |   7 |  12.18 |    0.917365133 |     11 |    0.000000000 |
#> |   8 |  11.06 |    0.691931377 |     11 |    0.000000000 |
#> |   9 |  11.23 |             NA |     11 |    0.000000000 |
#> |  10 |  11.11 |    0.917365133 |     11 |    0.000000000 |
# find the best program
paste("Best program:",sr$best_program$tree_expression())
#> [1] "Best program: add(sub(shift(X1,3),add(X2,X1)),shift(X2,3))"
```

Our target formula is: $`y = shift(X_1,3) + shift(X_2,3) - X_1 - X_2`$.

The best expression we get is
$`y = (shift(X_1,3) - (X_2 + X_1)) + shift(X_2,3)`$.

After reform our expression
$`y = shift(X_1,3) + shift(X_2,3) - X_1 - X_2`$.

Fantastic! We find our target form!
