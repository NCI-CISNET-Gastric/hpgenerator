#' Logit function
#'
#' \code{logit} computes the log odds of a probability \code{p}.
#'
#' @param p probability
#' @return 
#' The log-odds of \code{p}.
#' @export
logit <- function(p){
  if(p < 0 | p > 1){
    stop("The probability parameter `p` should be [0, 1]")  
  }
  
  x <- log(p/(1- p))
  
  return(x)
}

#' Inverse logit unction
#'
#' \code{ilogit} computes the probability \code{p} from a log-odds.
#'
#' @param logodds log-odds
#' @return 
#' The probability of log-odds.
#' @export
ilogit <- function(logodds){
  p <- 1/(1 + exp(-logodds))
  
  return(p)
}