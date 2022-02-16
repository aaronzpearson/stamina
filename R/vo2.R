#' VO2 Max Model Fit
#'
#' @param alpha alpha
#' @param beta beta
#' @param theta theta
#' @param duration duration
#' @param cf correction factor
#'
#' @export
vo2.model.fit <- function(alpha, beta, theta, duration, cf = 1.25) {

  t <- theta/cf
  f <- alpha * exp(beta * duration) + t

  return(f)

}

#' @export
#' @describeIn vo2.model.fit Returns the integral of the velocity duration curve
vo2.model.integral <- function(alpha, beta, theta, duration, cf = 1.25) {

  a_b <- alpha/ beta
  t <- theta/cf
  f <- t * duration + a_b * exp(beta * duration)

  return(f)

}
