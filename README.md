
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
#> |   2 |  12.62 |             NA |      3 |    0.140839794 |
#> |   3 |  11.38 |             NA |     17 |    0.123656140 |
#> |   4 |   6.98 |             NA |      9 |    0.081347816 |
#> |   5 |   4.27 |    0.141099405 |      9 |    0.081347816 |
#> |   6 |   6.09 |    0.380456541 |     17 |    0.008354066 |
#> |   7 |   9.28 |             NA |     11 |    0.000653386 |
#> |   8 |  10.73 |    0.558225524 |     11 |    0.000653386 |
#> |   9 |  11.70 |    0.477692843 |     11 |    0.000653386 |
#> |  10 |  14.20 |    0.483064974 |     11 |    0.000653386 |
# find the best program
paste("Best program:",sr$best_program$tree_expression())
#> [1] "Best program: sub(sub(X1,0.974),sub(mul(X2,X2),mul(X1,X1)))"
```

Our target formula is: $`y = X_1^2 - X_2^2 + X_1 - 1`$.

The best expression we get is $`y = (X_1 - 0.974) - (X_2^2 - X_1^2)`$.

After reform our expression $`y = X_1^2 - X_2^2 + X_1 - 0.974`$, we
(almost) find the formula we are looking for!
