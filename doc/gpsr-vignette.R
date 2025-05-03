## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gpsr)

## -----------------------------------------------------------------------------
.perf_sqr = function(a,b){
  return((a+b)^2)
}

## -----------------------------------------------------------------------------
perf_sqr = make_function(
  fun = .perf_sqr, 
  arity = 2, 
  name = "perfect_square")
# test the function
perf_sqr$fun(c(1,2,3),c(2,3,4))

## -----------------------------------------------------------------------------
gpsr_es = SymbolicRegressor$new(function_set = list("add","sub","div","mul",perf_sqr))
# check function names
for (i in 1:length(gpsr_es$function_set)){
  cat(gpsr_es$function_set[[i]]$name," ")
}

## -----------------------------------------------------------------------------
.perf_sqr_plus = function(x,y,a,b){
  return(a*(x+y)^2 + b)
}

## -----------------------------------------------------------------------------
perf_sqr_plus = make_function(
  fun = .perf_sqr_plus, 
  arity = 4, 
  name = "perfect_square_plus",
  param = 2,
  param_pool = list(c(1,2,3),c(2,3,4)))
# test the function
perf_sqr_plus$fun(c(1,2,3),c(2,3,4),2,3)

## -----------------------------------------------------------------------------
gpsr_es = SymbolicRegressor$new(function_set = list("add","sub","div","mul",perf_sqr,perf_sqr_plus))
# check function names
for (i in 1:length(gpsr_es$function_set)){
  cat(gpsr_es$function_set[[i]]$name," ")
}

## -----------------------------------------------------------------------------
.rmse = function(y,y_hat){
  return(sqrt(mean((y-y_hat)^2)))
}

## -----------------------------------------------------------------------------
rmse = make_fitness(function_ = .rmse, greater_is_better = FALSE)
# check rmse function
rmse$function_(c(1,2,3),c(2,3,4))

## -----------------------------------------------------------------------------
gpsr_es = SymbolicRegressor$new(
  function_set = list("add","sub","div","mul",perf_sqr,perf_sqr_plus),
  metric = rmse)
gpsr_es$metric

