#' A Genetic Programming symbolic regressor using R6 class.
#'
#' @description
#' A symbolic regressor is an estimator that begins by building a population
#' of naive random formulas to represent a relationship.
#' The formulas are represented by tree-like structures with formulas as the nodes and variables as the leaves.
#'
#' @details
#' A regressor can fit the target values.
#'
#' @import R6
#' @export
SymbolicRegressor <- R6Class(
  "SymbolicRegressor",
  public = list(
    #' @field pop_size The number of programs in each generation.
    pop_size = NULL,
    #' @field generations The number of generations to evolve.
    generations = NULL,
    #' @field tournament_size The number of programs that will compete to become part of the next generation.
    tournament_size = NULL,
    #' @field const_range The range of constant can take for the program.
    const_range = NULL,
    #' @field init_depth The range of tree depths for the initial population of naive formulas.
    init_depth = NULL,
    #' @field init_method The method define how the tree grows.
    init_method = NULL,
    #' @field function_set A list of valid functions in list form to use in the program.
    function_set = NULL,
    #' @field metric The raw fitness metric.
    metric = NULL,
    #' @field parsimony_coefficient The constant penalizes large programs by adjusting their fitness to be less favorable for selection.
    parsimony_coefficient = NULL,
    #' @field p_crossover The probability of performing crossover on a tournament winner.
    p_crossover = NULL,
    #' @field p_subtree_mutation The probability of performing subtree mutation on a tournament winner.
    p_subtree_mutation = NULL,
    #' @field p_hoist_mutation The probability of performing hoist mutation on a tournament winner.
    p_hoist_mutation = NULL,
    #' @field p_point_mutation The probability of performing point mutation on a tournament winner.
    p_point_mutation = NULL,
    #' @field p_point_replace The probability that any given node will be mutated during point mutation.
    p_point_replace = NULL,
    #' @field best_program The flattened tree representation of the best program.
    best_program = NULL,
    #' @field feature_names The vector of names of features of input data.
    feature_names = NULL,
    #' @field method_probs The probabilities to perform the crossover or mutations.
    method_probs = NULL,
    #' @field programs All the programs.
    programs = NULL,
    #' @field fitness Fitness of all the programs.
    fitness = NULL,

    #' Initialize method
    #'
    #' @description
    #' Start a new regressor object.
    #'
    #' @param pop_size The field pop_size.
    #' @param generations The field generations.
    #' @param tournament_size The field tournament_size.
    #' @param const_range The field const_range.
    #' @param init_depth The field init_depth.
    #' @param init_method The field init_method.
    #' @param function_set The field function_set.
    #' @param metric The field metric.
    #' @param parsimony_coefficient The field parsimony_coefficient.
    #' @param p_crossover The field p_crossover.
    #' @param p_subtree_mutation The field p_subtree_mutation.
    #' @param p_hoist_mutation The field p_hoist_mutation.
    #' @param p_point_mutation The field p_point_mutation.
    #' @param p_point_replace The field p_point_replace.
    #'
    #' @return A new regressor object.

    initialize = function(pop_size = 2000,
                          generations = 20,
                          tournament_size = 20,
                          const_range = c(-1,1),
                          init_depth = c(2:6),
                          init_method = "half and half",
                          function_set = list("add", "sub", "mul", "div"),
                          metric = "mse",
                          parsimony_coefficient = 1e-3,
                          p_crossover = 0.7,
                          p_subtree_mutation = 0.1,
                          p_hoist_mutation = 0.05,
                          p_point_mutation = 0.1,
                          p_point_replace = 0.05) {
      # check parameter type
      if (!is.numeric(pop_size)) {
        stop(paste("The pop_size parameter expects numeric value. Got type:", class(pop_size)))
      } else if (length(pop_size) > 1) {
        stop("Got multiple pop_size parameters.")
      } else if (is.na(pop_size) | pop_size < 0) {
        stop("The pop_size parameter is NA or negative.")
      }

      if (!is.numeric(tournament_size)) {
        stop(paste("The tournament_size parameter expects numeric value. Got type:", class(tournament_size)))
      } else if (length(tournament_size) > 1) {
        stop("Got multiple tournament_size parameters.")
      } else if (is.na(tournament_size) | tournament_size < 0) {
        stop("The tournament_size parameter is NA or negative.")
      }

      if (!is.numeric(generations)) {
        stop(paste("The generations parameter expects numeric value. Got type:", class(generations)))
      } else if (length(generations) > 1) {
        stop("Got multiple generations parameters.")
      } else if (is.na(generations) | generations < 0) {
        stop("The generations parameter is NA or negative.")
      }

      if (!is.numeric(const_range)) {
        stop(paste("The const_range parameter expects numeric value. Got type:", class(const_range)))
      } else if (sum(is.na(const_range)) > 0) {
        stop("The const_range parameter contains NA.")
      }

      if (!is.numeric(init_depth)) {
        stop(paste("The init_depth parameter expects numeric value. Got type:", class(init_depth)))
      } else if (sum(is.na(init_depth)) > 0) {
        stop("The init_depth parameter contains NA.")
      }

      if (!is.character(init_method)) {
        stop(paste("The init_method parameter expects character value. Got type:", class(init_method)))
      } else if (!(init_method %in% c("half and half","grow","full"))) {
        stop("Unknown init_method.")
      }

      if (!is.numeric(parsimony_coefficient)) {
        stop(paste("The parsimony_coefficient parameter expects numeric value. Got type:", class(parsimony_coefficient)))
      } else if (length(parsimony_coefficient) > 1) {
        stop("Got multiple parsimony_coefficient parameters.")
      } else if (is.na(parsimony_coefficient) | parsimony_coefficient < 0) {
        stop("The parsimony_coefficient parameter is NA or negative.")
      }

      if (!is.numeric(p_crossover)) {
        stop(paste("The p_crossover parameter expects numeric value. Got type:", class(p_crossover)))
      } else if (length(p_crossover) > 1) {
        stop("Got multiple p_crossover parameters.")
      } else if (is.na(p_crossover) | p_crossover < 0) {
        stop("The p_crossover parameter is NA or negative.")
      }

      if (!is.numeric(p_subtree_mutation)) {
        stop(paste("The p_subtree_mutation parameter expects numeric value. Got type:", class(p_subtree_mutation)))
      } else if (length(p_subtree_mutation) > 1) {
        stop("Got multiple p_subtree_mutation parameters.")
      } else if (is.na(p_subtree_mutation) | p_subtree_mutation < 0) {
        stop("The p_subtree_mutation parameter is NA or negative.")
      }

      if (!is.numeric(p_hoist_mutation)) {
        stop(paste("The p_hoist_mutation parameter expects numeric value. Got type:", class(p_hoist_mutation)))
      } else if (length(p_hoist_mutation) > 1) {
        stop("Got multiple p_hoist_mutation parameters.")
      } else if (is.na(p_hoist_mutation) | p_hoist_mutation < 0) {
        stop("The p_hoist_mutation parameter is NA or negative.")
      }

      if (!is.numeric(p_point_mutation)) {
        stop(paste("The p_point_mutation parameter expects numeric value. Got type:", class(p_point_mutation)))
      } else if (length(p_point_mutation) > 1) {
        stop("Got multiple p_point_mutation parameters.")
      } else if (is.na(p_point_mutation) | p_point_mutation < 0) {
        stop("The p_point_mutation parameter is NA or negative.")
      }

      if (!is.numeric(p_point_replace)) {
        stop(paste("The p_point_replace parameter expects numeric value. Got type:", class(p_point_replace)))
      } else if (length(p_point_replace) > 1) {
        stop("Got multiple p_point_replace parameters.")
      } else if (is.na(p_point_replace) | p_point_replace < 0) {
        stop("The p_point_replace parameter is NA or negative.")
      }

      self$function_set <- list()
      for (.function in function_set) {
        if (is.list(.function)) {
          self$function_set[[.function$name]] <- .function
        } else if (is.character(.function) & .function %in% names(.functions_map)) {
          self$function_set[[.function]] <- .functions_map[[.function]]
        } else {
          stop(paste("Got unknown function:", .function))
        }
      }
      if (is.list(metric)) {
        self$metric <- metric
      } else if (is.character(metric) & metric %in% names(.fitness_map)) {
        self$metric <- .fitness_map[[metric]]
      } else {
        stop("Got unknown fitness.")
      }
      self$init_depth <- init_depth
      self$init_method <- init_method
      self$pop_size <- pop_size
      self$tournament_size <- tournament_size
      self$generations <- generations
      self$p_crossover <- p_crossover
      self$p_subtree_mutation <- p_subtree_mutation
      self$p_hoist_mutation <- p_hoist_mutation
      self$p_point_mutation <- p_point_mutation
      self$p_point_replace <- p_point_replace
      self$parsimony_coefficient <- parsimony_coefficient
      self$const_range <- const_range
      self$method_probs <- cumsum(c(self$p_crossover, self$p_subtree_mutation, self$p_hoist_mutation, self$p_point_mutation))
    },
    #' @description
    #' Use tournament selection method to choose one program from the programs as the parent to evolve.
    #'
    #' @param programs The field programs.
    #' @param tournament_size The field tournament_size.
    #'
    #' @return A program.
    tournament = function(programs, tournament_size) {
      # Find the fittest individual from a sub-population.
      idx <- sample(1:self$pop_size, size = tournament_size)
      # get all the fitness of the programs
      fitnesses <- numeric(length = length(programs))
      for (i in 1:length(programs)) {
        fitnesses[i] <- programs[[i]]$fitness_
      }
      # find the index of the best program
      if (self$metric$sign == 1) {
        best_idx <- idx[order(fitnesses[idx], decreasing = T)][1]
      } else {
        best_idx <- idx[order(fitnesses[idx])][1]
      }
      return(programs[[best_idx]])
    },
    #' @description
    #' Function used to build a batch of programs within a job.
    #'
    #' @param parents List of programs
    #' @param X The given feature data.
    #' @param y The target values.
    #'
    #' @return The programs of current generation.
    evolve = function(parents, X, y) {
      programs <- list()
      for (i in 1:self$pop_size) {
        if (length(parents) == 0) {
          program <- NULL
        } else {
          method <- runif(1)
          # select parent program
          parent <- self$tournament(parents, self$tournament_size)
          # implement selected method
          if (method < self$method_probs[1]) {
            donor <- self$tournament(parents, self$tournament_size)
            program <- parent$crossover(donor$program)
          } else if (method < self$method_probs[2]) {
            program <- parent$subtree_mutation()
          } else if (method < self$method_probs[3]) {
            program <- parent$hoist_mutation()
          } else if (method < self$method_probs[4]) {
            program <- parent$point_mutation()
          } else {
            program <- parent$program
          }
        }
        # construct new program
        program <- Program$new(
          metric = self$metric,
          function_set = self$function_set,
          feature_names = self$feature_names,
          init_depth = self$init_depth,
          init_method = self$init_method,
          p_point_replace = self$p_point_replace,
          parsimony_coefficient = self$parsimony_coefficient,
          const_range = self$const_range,
          program = program
        )
        # compute its raw fitness
        program$raw_fitness_ <- program$raw_fitness(X, y)
        # add it in the programs
        programs <- append(programs, program)
      }
      return(programs)
    },
    #' @description
    #' A report of the progress of the evolution process.
    #'
    #' @param gen Current generation.
    #' @param programs The programs of current generation.
    #'
    #' @return None
    report_result = function(gen, programs) {
      if (gen == 1) {
        cat(sprintf(
          "      | %-23s | %-23s |\n",
          "Population", "Best Individual"
        ))
        cat(rep("-", 30), "\n")
        cat(sprintf(
          "| %3s | %-6s | %-14s | %-6s | %-14s |\n",
          "Gen", "Length", "Fitness", "Length", "Best Fitness"
        ))
      }
      # compute all the length and raw_fitness
      med_fitness_vec <- numeric(length = self$pop_size)
      avg_length_vec <- numeric(length = self$pop_size)
      for (i in 1:length(programs)) {
        avg_length_vec[i] <- length(programs[[i]]$program)
        med_fitness_vec[i] <- programs[[i]]$raw_fitness_
      }
      med_pop_fitness <- median(med_fitness_vec)
      avg_pop_length <- mean(avg_length_vec)
      # store the attributes of the best program
      best_program <- self$tournament(programs, self$pop_size)
      best_program_length <- length(best_program$program)
      best_program_fitness <- best_program$raw_fitness_
      # report
      cat(sprintf(
        "| %3d | %6.2f | %14.9f | %6d | %14.9f |\n",
        gen, avg_pop_length, med_pop_fitness, best_program_length, best_program_fitness
      ))
    },
    #' @description
    #' The fit process of the estimator
    #'
    #' @param X The feature data.
    #' @param y The target values.
    #'
    #' @return None
    fit = function(X, y) {
      # check Input data type
      if (is.matrix(X)) {
        X <- data.frame(X)
      } else if (is.data.frame(X)) {
        X <- X
      } else {
        stop(paste("The Input X expects data.frame or matrix value. Got type:", class(X)))
      }
      # check na values
      if (sum(is.na(y)) > 0) {
        stop("Input y contains NA values.")
      }
      if (sum(is.na(X)) > 0) {
        warning("Input X contains NA values.")
      }
      # check X and y have same sample size
      if (dim(X)[1] != length(y)){
        stop("Input feature X and target value y have different legnth at dim = 1.")
      }
      self$feature_names <- colnames(X)
      programs <- list()
      for (gen in 1:self$generations) {
        # batch evolve
        programs <- self$evolve(programs, X, y)
        # store fitness
        fitness <- numeric(length = self$pop_size)
        for (id in 1:self$pop_size) {
          # infinite protection
          fitness[id] <- ifelse(is.na(programs[[id]]$raw_fitness_), -Inf * self$metric$sign, programs[[id]]$raw_fitness_)
          programs[[id]]$fitness_ <- programs[[id]]$fitness()
        }
        self$programs <- programs
        self$fitness <- fitness
        self$report_result(gen, programs)
        self$best_program <- self$tournament(programs, self$pop_size)
      }
    }
  )
)
