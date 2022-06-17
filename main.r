################################################################################

#  install.packages("GA")
library(GA)

numbers_operators <-
  c("10", "25", "100", "5", "3", "+", "-", "/", "*")
target_number <- 2512

########################## 1: POPULATION #######################################

# Function that accepts GA object and returns generated population
generate_population <- function(object) {
  # Get length of expression and size of population
  numbers_operators_length <- object@upper
  population_size <- object@popSize
  
  # Get number of numbers and operators
  operators_length <- numbers_operators_length %/% 2
  numbers_length <- numbers_operators_length - operators_length
  
  population <-
    matrix(nrow = population_size, ncol = numbers_operators_length)
  
  # Create population_size instances of generation
  for (instance in 1:population_size) {
    # Create permutation of numbers and operators
    numbers <-
      sample(x = 1:numbers_length,
             numbers_length,
             replace = FALSE)
    operators <-
      sample(
        x = (numbers_length + 1):numbers_operators_length,
        operators_length,
        replace = TRUE
      )
    
    # Create instance I (interweaving between numbers and operators)
    I <- c(1:numbers_operators_length)
    I[1:numbers_operators_length %% 2 == 1] = numbers
    I[1:numbers_operators_length %% 2 == 0] = operators
    
    population[instance, ] <- I
  }
  
  return(population)
}

########################## 2: FITNESS FUNCTION #################################

# Function that accepts mathematical expression and calculates result
fitness <- function(expression) {
  result <-
    eval(parse(text = paste(numbers_operators[expression], collapse = "")))
  
  # Return negative absolute value, because GA maximize results
  return(-abs(target_number - result))
}

########################## 3: CROSSOVER AND MUTATION FUNCTIONS #################

# Function that accepts object and two parents and returns crossover children
# This crossover crossovers operators only
simple_crossover <- function(object, parents) {
  numbers_operators_length = object@upper
  operators_length <- numbers_operators_length %/% 2
  numbers_length <- numbers_operators_length - operators_length
  
  parent1 = object@population[parents[1],]
  parent2 = object@population[parents[2],]
  
  children = matrix(0, nrow = 2, ncol = numbers_operators_length)
  
  # Keep numbers and crossover operators
  children[1, ] <- parent1
  children[1, ][children[1, ] > numbers_length] <-
    parent2[parent2 > numbers_length]
  children[2, ] <- parent2
  children[2, ][children[2, ] > numbers_length] <-
    parent1[parent1 > numbers_length]
  
  return(list(children = children, fitness = rep(NA, 2)))
}

# Function that accepts object and two parents and returns crossover children
# This crossover crossovers operators only from random cross point
crossover <- function(object, parents) {
  numbers_operators_length = object@upper
  operators_length <- numbers_operators_length %/% 2
  numbers_length <- numbers_operators_length - operators_length
  
  parent1 = object@population[parents[1],]
  parent2 = object@population[parents[2],]
  
  children = matrix(0, nrow = 2, ncol = numbers_operators_length)
  
  # Crossover point value
  crossover_point <- sample(1:operators_length, 1) * 2
  
  # Keep numbers and crossover operators
  children[1, ] <- parent1
  children[2, ] <- parent2
  for (i in seq(from = crossover_point, to = numbers_operators_length, by = 2)) {
    children[1, i] <- parent2[i]
    children[2, i] <- parent1[i]
  }
  
  return(list(children = children, fitness = rep(NA, 2)))
}

# Function that accepts object and two parents and returns crossover children
# This crossover crossovers random numbers and operators (if possible)
advanced_crossover <- function(object, parents) {
  numbers_operators_length = object@upper
  operators_length <- numbers_operators_length %/% 2
  numbers_length <- numbers_operators_length - operators_length
  
  # Default crossover point value
  crossover_point <- 0
  
  # Get parents from population
  parent1 = object@population[parents[1],]
  parent2 = object@population[parents[2],]
  
  # Matrix of two crossover children
  children = matrix(0, nrow = 2, ncol = numbers_operators_length)
  
  numbers_operators_parent1 <- numbers_operators[parent1]
  numbers_operators_parent2 <- numbers_operators[parent2]
  # Iterate through numbers and operator and find crossover point
  start_index <- sample(1:numbers_operators_length, 1)
  # start -> right side -> left side
  for (i in c(start_index:numbers_operators_length, 1:(start_index - 1))) {
    # If operator, randomly check for crossover point
    operator_check = sample(1:2, 1)
    if (i %% 2 == 0 & operator_check == 2) {
      next
    }
    
    # Check if all numbers match (if not, find new crossover point)
    # Always check in shorter half of the parents
    if (i >= numbers_operators_length %% 2) {
      range <- i:numbers_operators_length
    } else {
      range <- 1:i
    }
    numbers_parent1 <-
      as.numeric(grep('^-?[0-9.]+$', numbers_operators_parent1[range], val = TRUE))
    numbers_parent1 <- order(numbers_parent1)
    numbers_parent2 <-
      as.numeric(grep('^-?[0-9.]+$', numbers_operators_parent2[range], val = TRUE))
    numbers_parent2 <- order(numbers_parent2)
    
    # If values are the same, i is new crossover point
    if (all(numbers_parent1 == numbers_parent2)) {
      crossover_point = i
      break
    }
  }
  
  # If crossover point is 0, return parents
  if (crossover_point == 0) {
    children[1,] <- parent1
    children[2,] <- parent2
  } else {
    children[1,] <-
      c(parent1[1:(crossover_point)], parent2[(crossover_point + 1):numbers_operators_length])[1:numbers_operators_length]
    children[2,] <-
      c(parent2[1:(crossover_point)], parent1[(crossover_point + 1):numbers_operators_length])[1:numbers_operators_length]
    
    # TODO: Replace same indices with indices of another number
  }
  
  return(list(children = children, fitness = rep(NA, 2)))
}

# Function that accepts object and a parent and returns mutated child
# Mutate on numbers or operators
simple_mutation <- function(object, parent) {
  # Select a parent from the population
  mutate <- parent <- as.vector(object@population[parent,])
  numbers_operators_length <- length(parent) %/% 2
  
  # Mutate on: number (1) or operator (2)
  mutation_score <- sample(1:2, size = 1)
  
  if (mutation_score == 1) {
    # Mutate on numbers (swap two numbers)
    m <- sample(0:numbers_operators_length, 2) * 2 + 1
    mutate[m[1]] <- parent[m[2]]
    mutate[m[2]] <- parent[m[1]]
  } else {
    # Mutate on operators (change operator)
    m <- sample(1:numbers_operators_length, 1) * 2
    mutate[m] <- parent[m]
  }
  
  return(mutate)
}

# Function that accepts object and a parent and returns mutated child
# Mutate on operators, numbers, operators and numbers or don't mutate
mutation <- function(object, parent) {
  # Select a parent from the population
  mutate <- parent <- as.vector(object@population[parent,])
  numbers_operators_length <- length(parent) %/% 2
  
  # Mutate on: none (1), number (2), operator (3) or number and operator (4)
  mutation_score <- sample(1:4, size = 1)
  
  if (mutation_score == 2) {
    # Mutate on numbers (swap two numbers)
    m <- sample(0:numbers_operators_length, 2) * 2 + 1
    mutate[m[1]] <- parent[m[2]]
    mutate[m[2]] <- parent[m[1]]
  } else if (mutation_score == 3) {
    # Mutate on operators (change operator)
    m <- sample(1:numbers_operators_length, 1) * 2
    mutate[m] <- parent[m]
  } else if (mutation_score == 4) {
    # Mutate on numbers and operators (swap two numbers and change operator)
    m <- sample(1:numbers_operators_length, 2) * 2
    mutate[m[1] - 1] <- parent[m[2] - 1]
    mutate[m[2] - 1] <- parent[m[1] - 1]
    m_operator <- sample(1:2, 1) # Choose 1 or 2 randomly
    mutate[m[m_operator]] <- parent[m[m_operator]]
  }
  
  return(mutate)
}

# Function that accepts object and a parent and returns mutated child
# Mutate random times on random values
advanced_mutation <- function(object, parent) {
  # Select a parent from the population
  mutate <- parent <- as.vector(object@population[parent,])
  numbers_operators_length <- length(parent) %/% 2
  
  # Times of mutation
  mutation_number <- sample(1:numbers_operators_length, size = 1)
  for (i in seq(mutation_number)) {
    # Mutate on: none (1), number (2), operator (3) or number and operator (4)
    mutation_score <- sample(1:4, size = 1)
    
    if (mutation_score == 2) {
      # Mutate on numbers (swap two numbers)
      m <- sample(0:numbers_operators_length, 2) * 2 + 1
      mutate[m[1]] <- parent[m[2]]
      mutate[m[2]] <- parent[m[1]]
    } else if (mutation_score == 3) {
      # Mutate on operators (change operator)
      m <- sample(1:numbers_operators_length, 1) * 2
      mutate[m] <- parent[m]
    } else if (mutation_score == 4) {
      # Mutate on numbers and operators (swap two numbers and change operator)
      m <- sample(1:numbers_operators_length, 2) * 2
      mutate[m[1] - 1] <- parent[m[2] - 1]
      mutate[m[2] - 1] <- parent[m[1] - 1]
      m_operator <- sample(1:2, 1) # Choose 1 or 2 randomly
      mutate[m[m_operator]] <- parent[m[m_operator]]
    }
  }
  
  return(mutate)
}

########################## 4: EVALUATION #######################################

# Function that accepts numbers and operators and calculates the result
random_search <- function (maxiter, run) {
  # Get length of expression
  numbers_operators_length <- length(numbers_operators)
  
  # Get number of numbers and operators
  numbers_length <-
    length(as.numeric(grep('^-?[0-9.]+$', numbers_operators, val = TRUE)))
  operators_length <-
    length(numbers_operators[(numbers_length + 1):numbers_operators_length])
  
  result_old <- 0
  result_best <- -target_number
  instance_best <- c()
  result_number <- 0
  iteration <- 1
  
  # History of evolution
  history <- c()
  
  while (TRUE) {
    # Create permutation of numbers and vectors
    numbers <-
      sample(x = 1:numbers_length,
             numbers_length,
             replace = FALSE)
    operators <-
      sample(
        x = (numbers_length + 1):numbers_operators_length,
        operators_length,
        replace = TRUE
      )
    
    # Create instance I (interweaving between numbers and operators)
    instance <- c(1:numbers_operators_length)
    instance[1:numbers_operators_length %% 2 == 1] = numbers
    instance[1:numbers_operators_length %% 2 == 0] = operators
    
    # Calculate fitness value
    result = fitness(instance)
    
    # Add iteration to history
    history[length(history) + 1] <- result
    
    # If result is equal to 0, we found solution
    if (result == 0) {
      return(
        list(
          iteration = iteration,
          solution = instance,
          result = result,
          history = history
        )
      )
    }
    
    # Update best instance
    if (result >= result_best) {
      result_best <- result
      instance_best <- instance
    }
    
    # Check if we get same result
    if (result == result_old) {
      result_number <- result_number + 1
    } else {
      result_number = 0
    }
    
    # If we have more than maxiter iterations, return best solution
    # If our result is same for run times, return best solution
    if (iteration >= maxiter || result_number >= run) {
      return(
        list(
          iteration = iteration,
          solution = instance_best,
          result = result_best,
          history = history
        )
      )
    }
    
    result_old <- result
    iteration <- iteration + 1
  }
}

# Compare different results
# Genetic algorithm result
genetic_algorithm_result <-
  ga(
    type = 'permutation',
    fitness = fitness,
    lower = 1,
    upper = length(numbers_operators),
    run = 50,
    maxiter = 1000,
    population = generate_population,
    popSize = 100,
    pmutation = 0.5,
    mutation = mutation,
    crossover = crossover
  )
plot(genetic_algorithm_result)
summary(genetic_algorithm_result)

# Simple genetic algorithm result
simple_genetic_algorithm_result <-
  ga(
    type = 'permutation',
    fitness = fitness,
    lower = 1,
    upper = length(numbers_operators),
    run = 50,
    maxiter = 1000,
    population = generate_population,
    popSize = 100,
    pmutation = 0.5,
    mutation = simple_mutation,
    crossover = simple_crossover
  )
plot(simple_genetic_algorithm_result)
summary(simple_genetic_algorithm_result)

# Advanced genetic algorithm result
advanced_genetic_algorithm_result <-
  ga(
    type = 'permutation',
    fitness = fitness,
    lower = 1,
    upper = length(numbers_operators),
    run = 50,
    maxiter = 1000,
    population = generate_population,
    popSize = 100,
    pmutation = 0.5,
    mutation = advanced_mutation,
    crossover = advanced_crossover
  )
plot(advanced_genetic_algorithm_result)
summary(advanced_genetic_algorithm_result)

# Random search result
random_search_result <- random_search(1000, 50)
plot(random_search_result$history,
     xlab = "Iteration",
     ylab = "Result")
points(random_search_result$iteration,
       random_search_result$result,
       col = "red")

# Comparison of genetic algorithm and random search
times_genetic_algorithm <- c()
times_random_search <- c()
for (iteration in 1:100) {
  times_genetic_algorithm[iteration] <- measure_time(
    ga(
      type = 'permutation',
      fitness = fitness,
      lower = 1,
      upper = length(numbers_operators),
      run = 50,
      maxiter = 1000,
      population = generate_population,
      popSize = 100,
      pmutation = 0.5,
      mutation = mutation,
      crossover = crossover
    )
  )
  times_random_search[iteration] <-
    measure_time(random_search(1000, 50))
}

plot(
  1:100,
  times_random_search,
  type = "l",
  col = "red",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
)
par(new = TRUE)
plot(
  1:100,
  times_genetic_algorithm,
  type = "l",
  col = "blue",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
)

# Comparison of crossover and mutation functions
times_genetic_algorithm_crossover <- c()
times_simple_genetic_algorithm_crossover <- c()
times_advanced_genetic_algorithm_crossover <- c()
times_genetic_algorithm_mutation <- c()
times_simple_genetic_algorithm_mutation <- c()
times_advanced_genetic_algorithm_mutation <- c()
for (iteration in 1:100) {
  # Compare different crossovers
  times_genetic_algorithm_crossover[iteration] <- measure_time(
    ga(
      type = 'permutation',
      fitness = fitness,
      lower = 1,
      upper = length(numbers_operators),
      run = 50,
      maxiter = 1000,
      population = generate_population,
      popSize = 100,
      pmutation = 0.5,
      mutation = mutation,
      crossover = crossover
    )
  )
  times_simple_genetic_algorithm_crossover[iteration] <-
    measure_time(
      ga(
        type = 'permutation',
        fitness = fitness,
        lower = 1,
        upper = length(numbers_operators),
        run = 50,
        maxiter = 1000,
        population = generate_population,
        popSize = 100,
        pmutation = 0.5,
        mutation = mutation,
        crossover = simple_crossover
      )
    )
  times_advanced_genetic_algorithm_crossover[iteration] <-
    measure_time(
      ga(
        type = 'permutation',
        fitness = fitness,
        lower = 1,
        upper = length(numbers_operators),
        run = 50,
        maxiter = 1000,
        population = generate_population,
        popSize = 100,
        pmutation = 0.5,
        mutation = mutation,
        crossover = advanced_crossover
      )
    )
  
  # Compare different mutations
  times_genetic_algorithm_mutation[iteration] <- measure_time(
    ga(
      type = 'permutation',
      fitness = fitness,
      lower = 1,
      upper = length(numbers_operators),
      run = 50,
      maxiter = 1000,
      population = generate_population,
      popSize = 100,
      pmutation = 0.5,
      mutation = mutation,
      crossover = crossover
    )
  )
  times_simple_genetic_algorithm_mutation[iteration] <-
    measure_time(
      ga(
        type = 'permutation',
        fitness = fitness,
        lower = 1,
        upper = length(numbers_operators),
        run = 50,
        maxiter = 1000,
        population = generate_population,
        popSize = 100,
        pmutation = 0.5,
        mutation = simple_mutation,
        crossover = crossover
      )
    )
  times_advanced_genetic_algorithm_mutation[iteration] <-
    measure_time(
      ga(
        type = 'permutation',
        fitness = fitness,
        lower = 1,
        upper = length(numbers_operators),
        run = 50,
        maxiter = 1000,
        population = generate_population,
        popSize = 100,
        pmutation = 0.5,
        mutation = advanced_mutation,
        crossover = crossover
      )
    )
}

plot(
  1:100,
  times_genetic_algorithm_crossover,
  type = "l",
  col = "red",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
)
par(new = TRUE)
plot(
  1:100,
  times_simple_genetic_algorithm_crossover,
  type = "l",
  col = "blue",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
)
par(new = TRUE)
plot(
  1:100,
  times_advanced_genetic_algorithm_crossover,
  type = "l",
  col = "green",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
)

plot(
  1:100,
  times_genetic_algorithm_mutation,
  type = "l",
  col = "red",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
)
par(new = TRUE)
plot(
  1:100,
  times_simple_genetic_algorithm_mutation,
  type = "l",
  col = "blue",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
  
)
par(new = TRUE)
plot(
  1:100,
  times_advanced_genetic_algorithm_mutation,
  type = "l",
  col = "green",
  xaxt = 'n',
  yaxt = 'n',
  ann = FALSE
)

########################## TIME MEASUREMENT FUNCTION ###########################

# Function that accepts another function and measures computation time
measure_time <- function (fun) {
  # Start timing
  start_time <- Sys.time()
  result = fun
  # End timing
  end_time <- Sys.time()
  # Time difference
  return(end_time - start_time)
}

################################################################################
