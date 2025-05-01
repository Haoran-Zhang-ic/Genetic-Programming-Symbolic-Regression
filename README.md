
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
remotes::install_github("yourname/mypackage")
# or use devtools
devtools::install_github("yourname/mypackage")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gpsr)
## basic example code
set.seed(0)
# randomly generate samples
data <- matrix(runif(100, -1, 1), nrow = 50, ncol = 2)
# our target formula: X1^2 - X2^2 - X1 - 1
y <- data[, 1]**2 - data[, 2]**2 - data[, 1] - 1
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
#> |   1 |  15.57 |    4.656874494 |      3 |    0.140916707 |
#> |   2 |   8.95 |             NA |      9 |    0.079212302 |
#> |   3 |   6.43 |    0.644043514 |      9 |    0.079212302 |
#> |   4 |   5.25 |    0.405801412 |      9 |    0.079212302 |
#> |   5 |   5.14 |    0.201578943 |     11 |    0.001533950 |
#> |   6 |   7.88 |    0.498960368 |     11 |    0.001533950 |
#> |   7 |  10.22 |    0.498960368 |     11 |    0.001533950 |
#> |   8 |  12.10 |    0.503263607 |     11 |    0.001533950 |
#> |   9 |  12.18 |    0.437107603 |     11 |    0.001533950 |
#> |  10 |  11.19 |    0.449602368 |     11 |    0.001533950 |
# find the best program
paste("Best program:",sr$best_program$tree_expression())
#> [1] "Best program: sub(add(mul(X1,X1),sub(-0.961,X1)),mul(X2,X2))"
```

Our target formula is: $`y = X_1^2 - X_2^2 - X_1 - 1`$.

The best expression we get is $`y = (X_1^2 + (-0.961 - X_1)) - X_2^2`$.

After reform our expression $`y = X_1^2 - X_2^2 - X_1 - 0.961`$, we
(almost) find the formula we are looking for!
