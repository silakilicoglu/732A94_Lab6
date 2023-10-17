#' Data frame of knapsack
#'
#' Data frame x with two variables v and w for using knapsack algorithm functions.
#' @format ## knapsack_objects
#' A data frame with 2000 rows and 2 columns:
#' \describe{
#'   \item{w}{weight to put item into the knapsack}
#'   \item{v}{value of the item}
#' }
#' 
#' @examples
#' library(knapsack)
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

"knapsack_objects"