#' Making a fitness function for use in algorithms
#'
#' @description
#' make_fitness is used to create fitness function for user to apply in the algorithm.
#'
#' @param function_ A function to operate the fitness computation.
#' @param greater_is_better The direction of the fitness function.
#'
#' @return The list return includes the function itself and the sign of objective
#' @export
#'
#' @examples
#' my_fitness <- function(x, y) {
#'   return(sum((x - y)^2))
#' }
#' my_customized_fitness <- make_fitness(my_fitness, FALSE)
#' my_customized_fitness
make_fitness <- function(function_, greater_is_better) {
  # Making a fitness function for use in algorithms
  # Parameters ---------------------------------------------------------
  # function_ : function
  #   A function to operate the fitness computation.
  # greater_is_better : bool
  #   The direction of the fitness function.
  # Returns ------------------------------------------------------------
  # function_list : list
  #   The list return includes the function itself and the sign of objective

  # check parameter type
  if (!is.function(function_)) {
    stop(paste("The function_ parameter expects function value. Got type:", class(function_)))
  }
  if (!is.logical(greater_is_better)) {
    stop(paste("The greater_is_better parameter expects bool value. Got type:", class(greater_is_better)))
  }
  # check return single numeric
  check_sample = matrix(c(1,2,1,1,2,1),byrow = FALSE,ncol = 2)
  check_terminals = list(check_sample[,1],check_sample[,2])
  check_result = do.call(function_,check_terminals)
  if (length(check_result) != 1 | !is.numeric(check_result)){
    stop("Function must return a single numeric.")
  }
  if (greater_is_better) {
    sign <- 1
  } else {
    sign <- -1
  }
  function_list <- list(function_ = function_, sign = sign)
  return(function_list)
}
.pearson <- function(x, y) {
  # compute the pearson correlation as fitness function
  # check x and y have same length
  if (length(x) != length(y)) {
    stop("Input x and y do not have the same length.")
  }
  x_mean <- mean(x)
  y_mean <- mean(y)
  if ((sqrt(sum((
    x - x_mean
  )**2)) * sqrt(sum((
    y - y_mean
  )**2))) == 0) {
    return(0)
  } else {
    return(sum((x - x_mean) * (y - y_mean)) / (sqrt(sum((
      x - x_mean
    )**2)) * sqrt(sum((
      y - y_mean
    )**2))))
  }
}
.spearman <- function(x, y) {
  # compute the spearman correlation as fitness function
  # check x and y have same length
  if (length(x) != length(y)) {
    stop("Input x and y do not have the same length.")
  }
  x <- order(x)
  y <- order(y)
  return(.pearson(x, y))
}
.mean_absolute_error <- function(x, y) {
  # compute the mean absolute error as fitness function
  # check x and y have same length
  if (length(x) != length(y)) {
    stop("Input x and y do not have the same length.")
  }
  return(mean(abs(x - y)))
}
.mean_square_error <- function(x, y) {
  # compute the mean square error as fitness function
  # check x and y have same length
  if (length(x) != length(y)) {
    stop("Input x and y do not have the same length.")
  }
  return(mean((x - y)**2))
}
pearson <- make_fitness(function_ = .pearson, greater_is_better = TRUE)
spearman <- make_fitness(function_ = .spearman, greater_is_better = TRUE)
mean_absolute_error <- make_fitness(function_ = .mean_absolute_error, greater_is_better = FALSE)
mean_square_error <- make_fitness(function_ = .mean_square_error, greater_is_better = FALSE)
.fitness_map <- list(
  "pearson" = pearson,
  "spearman" = spearman,
  "mae" = mean_absolute_error,
  "mse" = mean_square_error
)
