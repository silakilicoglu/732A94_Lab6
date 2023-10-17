# Brute Force Search
brute_force_knapsack <- function(x, W, parallel = FALSE){
  
  # Control inputs
  stopifnot(is.data.frame(x),
            length(x) == 2,
            all(names(x) == c("w", "v")),
            typeof(x$w) == "integer",
            typeof(x$v) == "double",
            is.numeric(W),
            length(W) == 1)
  
  stopifnot(W > 0)
  
  # W: çantanın ağırlığı (kapasite)
  # x in içinde 2 değer var (w, v)
  # w: eşyaların ağırlığı (ağırlık)
  # v: eşyaların değeri (değerler)
  
  n <- nrow(x)
  max_val <- 0
  result <- list(value=0, elements=0)
  
  
  if(parallel==TRUE){
    s=1:(2^n-1)
    sub_func <- function(s,x,n,W){
      value=0
      elements=0
      binary_number=as.numeric(intToBits(s))
      binary_number=binary_number[1:n]
      sub_weight=sum(binary_number*x$w)
      if(sub_weight<=W){
        value=sum(binary_number*x$v)
        elements=which(binary_number==1)
      }
      return(value)
    }
    
    core <- parallel::detectCores()
    clstr <- makeCluster(core, type = "PSOCK")
    par_result <- parLapply(clstr, s, fun = sub_func, x, n, W)
    stopCluster(clstr)
    par_result1= as.vector(unlist(par_result))
    k=which(par_result1==max(par_result1),arr.ind=TRUE)
    elements=which(as.numeric(intToBits(k))==1)
    return(list(value=round((max(par_result1)),0),elements=elements))
    }
  
  else{
  temp_matrix <- matrix(0, nrow = 2^n, ncol = n)
  temp_vector <- vector(length = n)
  
  for(i in 1:nrow(temp_matrix)){
    binary <- intToBits(i)
    for(j in 1:n){
      temp_vector[j] <- as.numeric(binary[j])
    }
    
    temp_matrix[i,] <- temp_vector
  }
  
  for(i in 1:nrow(temp_matrix)){
    temp <- which(temp_matrix[i,]==1)
    total_w <- sum(x[temp,1])
    total_v <- sum(x[temp,2])
    
    if(total_w<=W & total_v>max_val){
      result_elements <- temp
      max_val <- total_v
      #act_w <- total_w #bak
    }
  }
  
  result <- list(value=round(max_val,0), elements=result_elements)
  
  return(result)
  }
}

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3"))) 
##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel = TRUE)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000, parallel = TRUE)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000, parallel = TRUE)
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000, parallel = TRUE))
