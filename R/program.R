#' Program class for symbolic regression using genetic programming.
#'
#' @description
#' This is the underlying data-structure used by the public classes in the genetic programming.
#'
#' @details
#' Object used by the SymbolicRegressor.
#'
#' @import R6
Program <- R6Class(
  "Program",
  public = list(
    #' @field init_depth The range of tree depths for the initial population of naive formulas.
    init_depth = NULL,
    #' @field init_method The method define how the tree grows.
    init_method = NULL,
    #' @field function_set A list of valid functions in list form to use in the program.
    function_set = NULL,
    #' @field const_range The range of constant can take for the program.
    const_range = NULL,
    #' @field metric The raw fitness metric.
    metric = NULL,
    #' @field p_point_replace The probability that any given node will be mutated during point mutation.
    p_point_replace = NULL,
    #' @field parsimony_coefficient The constant penalizes large programs by adjusting their fitness to be less favorable for selection.
    parsimony_coefficient = NULL,
    #' @field feature_names The vector of names of features of input data.
    feature_names = NULL,
    #' @field program The programs.
    program = NULL,
    #' @field raw_fitness_ The raw fitness of the program.
    raw_fitness_ = NULL,
    #' @field fitness_ The fitness of the program after penalized.
    fitness_ = NULL,

    #' Initialize method
    #'
    #' @param function_set The field function_set
    #' @param init_depth The field init_depth
    #' @param init_method The field init_method
    #' @param feature_names The field feature_names
    #' @param const_range The field const_range
    #' @param metric The field metric
    #' @param p_point_replace The field p_point_replace
    #' @param parsimony_coefficient The field parsimony_coefficient
    #' @param program The field program
    #'
    #' @return The program object
    initialize = function(function_set = NULL,
                          init_depth = NULL,
                          init_method = NULL,
                          feature_names = NULL,
                          const_range = NULL,
                          metric = NULL,
                          p_point_replace = NULL,
                          parsimony_coefficient = NULL,
                          program = NULL) {
      self$function_set <- function_set
      self$init_depth <- init_depth
      self$init_method <- init_method
      self$feature_names <- feature_names
      self$const_range <- const_range
      self$metric <- metric
      self$p_point_replace <- p_point_replace
      self$parsimony_coefficient <- parsimony_coefficient
      self$program <- program
      if (is.null(self$program)) {
        self$program <- self$build_program()
      } else {
        self$program <- program
      }
    },
    #' @description
    #' Build a naive program randomly.
    #'
    #' @return a raw program in list form.
    build_program = function() {
      if (self$init_method == "half and half"){
        method = ifelse(round(runif(1),0),"full","grow")
      }else{
        method = self$init_method
      }
      max_depth <- sample(self$init_depth, size = 1)
      # Start a program with a function to avoid degenerative programs
      init_function <- sample(names(self$function_set), size = 1)
      program <- list(init_function)
      terminal_stack <- list(self$function_set[[init_function]]$arity)
      param_stack <- list(self$function_set[[init_function]]$param)
      param_pool_stack <- list(self$function_set[[init_function]]$param_pool)
      while (length(terminal_stack) != 0) {
        depth <- length(terminal_stack)
        choice <- sample(c(names(self$function_set), self$feature_names), size = 1)
        # Determine if we are adding a function or terminal
        if (depth < max_depth && (choice %in% names(self$function_set) || method == "full")) {
          choice <- sample(names(self$function_set), size = 1)
          program <- append(program, choice)
          terminal_stack <- append(terminal_stack, self$function_set[[choice]]$arity)
          param_stack <- append(param_stack, self$function_set[[choice]]$param)
          param_pool_stack[[length(param_pool_stack) + 1]] <- self$function_set[[choice]]$param_pool
        } else {
          # Determine if we add a constant or variable
          if (!is.null(self$const_range)) {
            terminal <- sample(c(self$feature_names, "const"), size = 1)
            if (terminal == "const") {
              terminal <- runif(1, min = min(self$const_range), max = max(self$const_range))
            }
          } else {
            terminal <- sample(self$feature_names, size = 1)
          }
          program <- append(program, terminal)
          terminal_stack[[length(terminal_stack)]] <- terminal_stack[[length(terminal_stack)]] - 1
          # Determine if we add param for the function
          while (terminal_stack[[length(terminal_stack)]] <= param_stack[[length(param_stack)]] & param_stack[[length(param_stack)]] != 0) {
            param_idx <- 1
            terminal <- sample(param_pool_stack[[length(param_pool_stack)]][[param_idx]], size = 1)
            program <- append(program, terminal)
            param_stack[[length(param_stack)]] <- param_stack[[length(param_stack)]] - 1
            terminal_stack[[length(terminal_stack)]] <- terminal_stack[[length(terminal_stack)]] - 1
            param_idx <- param_idx + 1
          }
          # Determine if the current function is terminated
          while (terminal_stack[[length(terminal_stack)]] == 0) {
            terminal_stack <- terminal_stack[-length(terminal_stack)]
            param_stack <- param_stack[-length(param_stack)]
            param_pool_stack <- param_pool_stack[-length(param_pool_stack)]
            if (length(terminal_stack) == 0) {
              return(program)
            }
            terminal_stack[[length(terminal_stack)]] <- terminal_stack[[length(terminal_stack)]] - 1
            # Determine if we add param for the function
            while (terminal_stack[[length(terminal_stack)]] <= param_stack[[length(param_stack)]] & param_stack[[length(param_stack)]] != 0) {
              param_idx <- 1
              terminal <- sample(param_pool_stack[[length(param_pool_stack)]][[param_idx]], size = 1)
              program <- append(program, terminal)
              param_stack[[length(param_stack)]] <- param_stack[[length(param_stack)]] - 1
              terminal_stack[[length(terminal_stack)]] <- terminal_stack[[length(terminal_stack)]] - 1
              param_idx <- param_idx + 1
            }
          }
        }
      }
    },
    #' @description
    #' Execute the program according to X
    #' @param X Training vectors.
    #'
    #' @return The result of executing the program on X.
    execute = function(X) {
      node <- self$program[[1]]
      # Check for single-node programs
      if (is.numeric(node)) {
        return(rep(node, dim(X)[1]))
      }
      if (node %in% self$feature_names) {
        return(X[[node]])
      }
      apply_stack <- list()
      for (node in 1:length(self$program)) {
        # Apply functions that have sufficient arguments
        if (self$program[[node]] %in% names(self$function_set)) {
          apply_stack[[length(apply_stack) + 1]] <- list(self$program[[node]])
        } else {
          apply_stack[[length(apply_stack)]][[length(apply_stack[[length(apply_stack)]]) + 1]] <- self$program[[node]]
        }
        function_arity <- self$function_set[[apply_stack[[length(apply_stack)]][[1]]]]$arity
        function_param <- self$function_set[[apply_stack[[length(apply_stack)]][[1]]]]$param
        while (length(apply_stack[[length(apply_stack)]]) == function_arity + 1) {
          my_function <- apply_stack[[length(apply_stack)]][[1]]
          terminals <- list()
          # calculate terminals
          for (i in 2:length(apply_stack[[length(apply_stack)]])) {
            if (length(apply_stack[[length(apply_stack)]][[i]]) == 1 && is.numeric(apply_stack[[length(apply_stack)]][[i]]) && i <= (function_arity - function_param + 1)) {
              terminals[[i - 1]] <- rep(apply_stack[[length(apply_stack)]][[i]], dim(X)[1])
            } else if (length(apply_stack[[length(apply_stack)]][[i]]) == 1 && apply_stack[[length(apply_stack)]][[i]] %in% self$feature_names) {
              terminals[[i - 1]] <- X[[apply_stack[[length(apply_stack)]][[i]]]]
            } else {
              terminals[[i - 1]] <- as.numeric(apply_stack[[length(apply_stack)]][[i]])
            }
          }
          # apply terminals to function
          intermediate_result <- do.call(self$function_set[[my_function]]$fun, terminals)
          if (length(apply_stack) != 1) {
            apply_stack <- apply_stack[1:(length(apply_stack) - 1)]
            function_arity <- self$function_set[[apply_stack[[length(apply_stack)]][[1]]]]$arity
            apply_stack[[length(apply_stack)]][[length(apply_stack[[length(apply_stack)]]) + 1]] <- intermediate_result
          } else {
            return(intermediate_result)
          }
        }
      }
    },
    #' @description
    #' Evaluate the raw fitness of the program according to X, y
    #' @param X Training vectors.
    #' @param y Target values.
    #'
    #' @return The raw fitness of the program.
    raw_fitness = function(X, y) {
      y_pred <- self$execute(X)
      raw_fitness_value <- self$metric$function_(y_pred, y)
      return(raw_fitness_value)
    },
    #' Fitness method
    #' @description
    #' Evaluate the penalized fitness of the program according to X, y.
    #' @return The penalized fitness of the program.
    fitness = function() {
      penalty <- self$parsimony_coefficient * length(self$program) * self$metric$sign
      return(self$raw_fitness_ - penalty)
    },
    #' @description
    #' Get a random subtree from the program.
    #'
    #' @param program The flattened tree representation of the program. If None, the embedded tree in the object will be used.
    #'
    #' @return The indices of the start and end of the random subtree.
    get_subtree = function(program = NULL) {
      if (is.null(program)) {
        program <- self$program
      }
      probs <- numeric(length = length(program))
      for (i in 1:length(program)) {
        if (program[[i]] %in% names(self$function_set)) {
          probs[i] <- 0.9
        } else if(is.character(program[[i]])){
          probs[i] <- 0
        }else{
          probs[i] <- 0.1
        }
      }
      probs <- cumsum(probs / sum(probs))
      start_p <- findInterval(runif(1), probs) + 1
      stack_p <- 1
      end_p <- start_p
      while (stack_p > end_p - start_p) {
        node <- program[[end_p]]
        if (node %in% names(self$function_set)) {
          function_arity <- self$function_set[[node]]$arity
          stack_p <- stack_p + function_arity
        }
        end_p <- end_p + 1
      }
      return(list(start = start_p, end = end_p - 1))
    },
    #' @description
    #' Perform the crossover genetic operation on the program.

    #' @param donor The flattened tree representation of the donor program.
    #'
    #' @return The flattened tree representation of the program.
    crossover = function(donor) {
      point_list_parent <- self$get_subtree()
      point_list_donor <- self$get_subtree(donor)
      start_p <- point_list_parent$start
      end_p <- point_list_parent$end
      start_d <- point_list_donor$start
      end_d <- point_list_donor$end
      child <- list()
      child[0:(start_p - 1)] <- self$program[0:(start_p - 1)]
      child[start_p:(start_p + end_d - start_d)] <- donor[start_d:end_d]
      if (end_p == length(self$program)) {
        return(child)
      } else {
        child[(length(child) + 1):(length(child) + 1 + length(self$program) - (end_p + 1))] <- self$program[(end_p + 1):length(self$program)]
        return(child)
      }
    },
    #' @description
    #' Perform the subtree mutation operation on the program.
    #' @return The flattened tree representation of the program.
    subtree_mutation = function() {
      donor <- self$build_program()
      child <- self$crossover(donor)
      return(child)
    },
    #' The hoist mutation method.
    #' @description
    #' Perform the hoist mutation operation on the program.
    #' @return The flattened tree representation of the program.
    hoist_mutation = function() {
      donor_point <- self$get_subtree()
      donor <- self$program[donor_point$start:donor_point$end]
      child <- self$crossover(donor)
      return(child)
    },
    #' @description
    #' Perform the point mutation operation on the program.
    #' @return The flattened tree representation of the program.
    point_mutation = function() {
      program <- self$program
      mutate <- c(1:length(program))[runif(length(program)) < self$p_point_replace]
      for (i in mutate) {
        node <- program[[i]]
        if (node %in% names(self$function_set)) {
          function_arity <- self$function_set[[node]]$arity
          function_param <- self$function_set[[node]]$param
          sample_pool <- c()
          for (sample_candidate in names(self$function_set)) {
            if (self$function_set[[sample_candidate]]$arity == function_arity & self$function_set[[sample_candidate]]$param == function_param) {
              sample_pool <- c(sample_pool, sample_candidate)
            }
          }
          program[[i]] <- sample(sample_pool, size = 1)
        } else {
          if (is.character(node)){
            terminal <- node
          }else if(!is.null(self$const_range)) {
            terminal <- sample(c(self$feature_names, "const"), size = 1)
            if (terminal == "const") {
              terminal <- runif(1, min = min(self$const_range), max = max(self$const_range))
            }
          } else {
            terminal <- sample(self$feature_names, size = 1)
          }
          program[[i]] <- terminal
        }
      }
      return(program)
    },
    #' @description
    #' Generate the readable tree expression of program
    #' @return The characters of a tree.
    tree_expression = function() {
      node <- self$program[[1]]
      # Check for single-node programs
      if (is.numeric(node)) {
        return(node)
      }
      if (node %in% self$feature_names) {
        return(node)
      }
      apply_stack <- list()
      for (node in 1:length(self$program)) {
        # Apply functions that have sufficient arguments
        if (self$program[[node]] %in% names(self$function_set)) {
          apply_stack[[length(apply_stack) + 1]] <- list(self$program[[node]])
        } else {
          apply_stack[[length(apply_stack)]][[length(apply_stack[[length(apply_stack)]]) + 1]] <- self$program[[node]]
        }
        function_arity <- self$function_set[[apply_stack[[length(apply_stack)]][[1]]]]$arity
        function_param <- self$function_set[[apply_stack[[length(apply_stack)]][[1]]]]$param
        while (length(apply_stack[[length(apply_stack)]]) == function_arity + 1) {
          my_function <- apply_stack[[length(apply_stack)]][[1]]
          terminals <- c()
          # add terminals
          for (i in 2:length(apply_stack[[length(apply_stack)]])) {
            terminals <- c(terminals, as.character(apply_stack[[length(apply_stack)]][[i]]))
          }
          # add terminals to function
          intermediate_result <- paste0(self$function_set[[my_function]]$name, "(", paste(terminals,collapse = ","),")",sep = "")
          if (length(apply_stack) != 1) {
            apply_stack <- apply_stack[1:(length(apply_stack) - 1)]
            function_arity <- self$function_set[[apply_stack[[length(apply_stack)]][[1]]]]$arity
            apply_stack[[length(apply_stack)]][[length(apply_stack[[length(apply_stack)]]) + 1]] <- intermediate_result
          } else {
            return(intermediate_result)
          }
        }
      }
    },
    #' @description
    #' Compute the depth of a tree.
    #' @return The depth of a tree.
    depth = function() {
      terminals <- list(0)
      depth <- 1
      for (i in 1:length(self$program)) {
        if (self$program[[i]] %in% names(self$function_set)) {
          terminals <- append(terminals, self$function_set[[self$program[[i]]]]$arity)
          depth <- max(length(terminals), depth)
        } else {
          terminals[[length(terminals)]] <- terminals[[length(terminals)]] - 1
          while (terminals[[length(terminals)]] == 0) {
            terminals <- terminals[-length(terminals)]
            terminals[[length(terminals)]] <- terminals[[length(terminals)]] - 1
          }
        }
      }
      return(depth - 1)
    }
  )
)
