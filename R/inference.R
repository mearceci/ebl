#'mom
#'
#'Task
#'
#'@param
#'@return gammna
#'@export
mom_gamma <- function(sample_g){
  x_bar <- mean(sample_g)
  x2_bar <- mean(sample_g^2)
  mom_theta <- (x2_bar - x_bar^2) / x_bar
  mom_k <- x_bar / mom_theta

  list(k_hat = mom_k, theta_hat = mom_theta)
  }

