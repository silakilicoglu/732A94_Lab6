knapsack_dynamic <-  function(x, W){
  
  # https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem
  
  # Control Inputs
  stopifnot(is.data.frame(x),
            length(x) == 2,
            all(names(x) == c("w", "v")),
            typeof(x$w) == "integer",
            typeof(x$v) == "double",
            is.numeric(W),
            length(W) == 1,
            W > 0)
  
  value <- matrix(-1, nrow = nrow(x)+1, ncol = W+1)
  m <- function(i,j){
    if (i==0 || j<=0){
      value[i+1,j+1] <<- 0
      return()
    }
    
    if (value[i,j+1] == -1){
      m(i-1, j)
    }
    
    if (x$w[i] > j){
      value[i+1,j+1] <<- value[i,j+1]
    }
    
    else{
      if (value[i, j+1-x$w[i]] == -1){
        m(i-1,j-x$w[i])
      }
      value[i+1,j+1] <<- max(value[i,j+1], value[i,j+1-x$w[i]]+x$v[i])
    }
    return(value)
  }
  
  # Returns the indices of the items of the optimal knapsack
  
  optimal <- c()
  result <- list(value=0, elements=NULL)
  
  optimal_knapsack <- function(i,j){
    if(i==0){
      return(optimal)
    }
    
    if(value[i+1,j+1] > value[i,j+1]){
      optimal <<- c(optimal,i)
      return(optimal_knapsack(i-1,j-x$w[i]))
    } 
    else{
      return(optimal_knapsack(i-1,j))
    }
    
    return(optimal)
  }
  
  a<-m(nrow(x),W)[nrow(x)+1,W+1]
  result$value <- round(a)
  result$elements <- sort(optimal_knapsack(nrow(x),W))
  return(result)
}

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000 
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))