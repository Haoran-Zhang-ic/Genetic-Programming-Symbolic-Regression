test_that("give type error", {
  expect_error(sr <- SymbolicRegressor$new(
    pop_size = "X",
    generations = 20,
    p_crossover = 0.7,
    p_subtree_mutation = 0.1,
    p_hoist_mutation = 0.05,
    p_point_mutation = 0.1,
    p_point_replace = 0.1,
    parsimony_coefficient = 0.001
  )
  )
})
test_that("give input type error", {
  expect_error(sr <- SymbolicRegressor$new(
    pop_size = 2000,
    generations = 20,
    p_crossover = 0.7,
    p_subtree_mutation = 0.1,
    p_hoist_mutation = 0.05,
    p_point_mutation = 0.1,
    p_point_replace = 0.1,
    parsimony_coefficient = 0.001
  )$fit(c("x","1"),c(1,2))
  )
})
test_that("give input type error", {
  expect_error(sr <- SymbolicRegressor$new(
    pop_size = 2000,
    generations = 20,
    p_crossover = 0.7,
    p_subtree_mutation = 0.1,
    p_hoist_mutation = 0.05,
    p_point_mutation = 0.1,
    p_point_replace = 0.1,
    parsimony_coefficient = 0.001
  )$fit(matrix(c(1,2,3,4,5,6),ncol = 2),c(1,2))
  )
})
