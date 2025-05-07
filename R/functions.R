#' Making a function for use in programs
#'
#' @description
#' make_function is used to create customized functions for user to apply in the algorithm.
#'
#' @param fun A function to operate the computation in execute algorithm.
#' @param arity The number of arguments that the function takes.
#' @param name The name of the function.
#' @param param The number of arguments that the function takes and varied during the training.
#' @param param_pool The possible values the param can take.
#'
#' @return The list return includes the function itself and the attributes of it.
#' @export
#'
#' @examples
#' # define a function
#' my_function <- function(x, a, b) a * x^b
#' my_customized_function <- make_function(
#'   fun = my_function,
#'   name = "My first function",
#'   arity = 3,
#'   param = 2,
#'   param_pool = list(c('1', '2', '3'), c('4', '5', '6'))
#' )
#' # check the function return the correct number
#' my_customized_function$fun(5, as.numeric('2'), as.numeric('5'))
make_function <- function(fun, arity, name, param = 0, param_pool = list()) {
  # Making a function for use in programs
  # Parameters ---------------------------------------------------------
  # fun : function
  #   A function to operate the computation in execute algorithm.
  # arity : numeric
  #   The number of arguments that the function takes.
  # name : character
  #   The name of the function.
  # param : numeric
  #   The number of arguments that the function takes and varied during the training.
  # param_pool : numeric
  #   The possible values the param can take. character.
  # Returns ------------------------------------------------------------
  # function_list : list
  #   The list return includes the function itself and the attributes of it.
  if (!is.function(fun)) {
    stop(paste("The fun parameter expects function value. Got type:", class(fun)))
  }
  if (!is.numeric(arity)) {
    stop(paste("The arity parameter expects numeric value. Got type:", class(arity)))
  } else if (length(arity) > 1) {
    stop("Got multiple arity parameters.")
  } else if (is.na(arity) | arity < 0) {
    stop("The arity parameter is NA or negative arity.")
  }

  if (!is.numeric(param)) {
    stop(paste("The param parameter expects numeric value. Got type:", class(param)))
  } else if (length(param) > 1) {
    stop("Got multiple param parameters.")
  } else if (is.na(param) | param < 0) {
    stop("The param parameter is NA or negative param.")
  }
  if (!is.list(param_pool)) {
    stop(paste("The param_pool parameter expects list value. Got type:", class(param_pool)))
  } else if (length(param_pool) != param) {
    stop(paste("The number of param pools is not consistent with the number of params. Got params:", param, "Got param pools:", length(param_pool)))
  }
  for (i in param_pool) {
    if (!is.character(i)) {
      stop(paste("The param pool expects character value. Got type:", class(i)))
    }
  }
  for (i in param_pool) {
    if (sum(is.na(as.numeric(i))) > 0) {
      stop(paste("The param pool expects Non-NA numeric after transform the character value. Please check :", paste(i, collapse = "\n - "),sep = "\n - "))
    }
  }
  if (!is.character(name)) {
    stop(paste("The name parameter expects character value. Got type:", class(name)))
  } else if (length(name) > 1) {
    stop("Got multiple name parameters.")
  }
  # check if function return the same shape as input vectors
  sample_feature <- arity - param
  sample_terminals <- list()
  for (i in c(1:sample_feature)) {
    sample_terminals[[i]] <- rep(1, 10)
  }
  if (param > 0) {
    for (i in c(1:param)) {
      sample_terminals[[length(sample_terminals) + 1]] <- as.numeric(sample(param_pool[[i]], size = 1))
    }
  }
  sample_intermediate_result <- do.call(fun, sample_terminals)
  if (length(sample_intermediate_result) != 10) {
    stop("supplied function does not return same shape as input vectors.")
  }
  # check if function is closed under zero input
  sample_terminals <- list()
  for (i in c(1:sample_feature)) {
    sample_terminals[[i]] <- rep(0, 10)
  }
  if (param > 0) {
    for (i in c(1:param)) {
      sample_terminals[[length(sample_terminals) + 1]] <- as.numeric(sample(param_pool[[i]], size = 1))
    }
  }
  sample_intermediate_result <- do.call(fun, sample_terminals)
  if (sum(is.infinite(sample_intermediate_result)) > 0) {
    stop("supplied function does not have closure against zero vectors.")
  }
  # check if function is closed under negative input
  sample_terminals <- list()
  for (i in c(1:sample_feature)) {
    sample_terminals[[i]] <- rep(-1, 10)
  }
  if (param > 0) {
    for (i in c(1:param)) {
      sample_terminals[[length(sample_terminals) + 1]] <- as.numeric(sample(param_pool[[i]], size = 1))
    }
  }
  sample_intermediate_result <- do.call(fun, sample_terminals)
  if (sum(is.infinite(sample_intermediate_result)) > 0) {
    stop("supplied function does not have closure against negative vectors.")
  }
  function_list <- list(fun = fun, arity = arity, param = param, param_pool = param_pool, name = name)
  return(function_list)
}
.functions_map <- list(
  add = list(fun = function(x, y) x + y, arity = 2, param = 0, param_pool = list(), name = "add"),
  sub = list(fun = function(x, y) x - y, arity = 2, param = 0, param_pool = list(), name = "sub"),
  mul = list(fun = function(x, y) x * y, arity = 2, param = 0, param_pool = list(), name = "mul"),
  div = list(fun = function(x, y) ifelse(is.infinite(x / y), 1, x / y), arity = 2, param = 0, param_pool = list(), name = "div")
)
