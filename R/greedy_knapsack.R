#' Greedy Heuristic
#' 
#' @name greedy_knapsack
#' 
#' @description
#' Knapsack greedy knapsack function that takes a data frame x with two variables v and w. 
#' Returns the maximum knapsack value and which elements (rows in the data.frame).
#'
#' @param x a data frame
#' @param W knapsack size, positive number
#'
#' @return the maximum knapsack value and which elements (rows in the data.frame)
#'
#' @references https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#' 
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#' 
#' @export greedy_knapsack

greedy_knapsack <- function(x, W){
  
  # Control Inputs
  stopifnot(is.data.frame(x),
            length(x) == 2,
            all(names(x) == c("w", "v")),
            typeof(x$w) == "integer",
            typeof(x$v) == "double",
            is.numeric(W),
            length(W) == 1,
            W > 0)
  
  v_w_ratio <- x$v/x$w
  values <- 0
  weights <- 0
  a <- c()
  
  result <- list(value=0, elements=NULL)
  
  for (i in order(-v_w_ratio)) {
    if (weights + x$w[i] <= W){
      a <- c(a, i)
      values <- c(values + x$v[i])
      weights <- c(weights + x$w[i])
    }
    else{
      result$value <- round(values)
      result$elements <- a
      return(result)
    }
    
  }
}

#suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#n <- 2000 
#knapsack_objects <- data.frame(
#  w=sample(1:4000, size = n, replace = TRUE),
#  v=runif(n = n, 0, 10000))
#greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))

#library(profvis)
#profvis(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))
