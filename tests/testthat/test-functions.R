test_that("example works", {
  expect_equal(make_function(fun = function(x,a){return(a*x)},
                             arity = 2,
                             param = 1,
                             param_pool = list(c(1,2,3)),
                             name = "test function"),
               list(fun = function(x,a){return(a*x)},
                    arity = 2,
                    param = 1,
                    param_pool = list(c(1,2,3)),
                    name = "test function"))
})
test_that("give negative arity or param error", {
  expect_error(make_function(fun = function(x,a){return(a*x)},
                             arity = -2,
                             param = 1,
                             param_pool = list(c(1,2,3)),
                             name = "test function"))
  expect_error(make_function(fun = function(x,a){return(a*x)},
                             arity = 2,
                             param = -1,
                             param_pool = list(c(1,2,3)),
                             name = "test function"))
})
test_that("give type error", {
  expect_error(make_function(fun = function(x,a){return(a*x)},
                             arity = 2,
                             param = 1,
                             param_pool = c(1,2,3),
                             name = "test function"))
})
test_that("give different shape error", {
  expect_error(make_function(fun = function(x,a){return(a*x[1:3])},
                             arity = 2,
                             param = 1,
                             param_pool = list(c(1,2,3)),
                             name = "test function"))
})
