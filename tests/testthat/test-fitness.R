test_that("example works", {
  expect_equal(make_fitness(function_ = function(x,y){sum(x - y)},
                             greater_is_better = FALSE),
               list(function_ = function(x,y){sum(x - y)},
                    sign = -1))
})
test_that("non-logical error", {
  expect_error(make_fitness(function_ = function(x,y){sum(x - y)},
                            greater_is_better = 1))
})
test_that("return error", {
  expect_error(make_fitness(function_ = function(x,y){return("x")},
                            greater_is_better = TRUE))
})
test_that("return error", {
  expect_error(make_fitness(function_ = function(x,y){return(c(1,2,3))},
                            greater_is_better = TRUE))
})
