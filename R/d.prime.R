#' D' Balance
#'
#' Returns an athlete's D' balance based on their current speed and critical speed. All arguments must be known to use this function.
#'
#' If you do not know an athlete's critical speed or D', please use the `stamina.player.profile()` function.
#'
#' @param speed Athlete's speed
#' @param cs Critical speed
#' @param d0 D'
#' @param t.diff Sample rate in seconds
#'
#' @export
dp.bal <- function(speed,
                   cs = 2,
                   d0 = 100,
                   t.diff = 0.1,
                   d.bal.min = -50) {

  d.prime <- function(dbal, speed, cv = 2, d0 = d0, t.diff = 0.1) {

    speed_diff = speed - cv

    if(speed_diff > 0) {
      d = dbal - speed_diff * t.diff
    } else {
      d = d0 - (d0 - dbal) * exp(speed_diff/d0 * t.diff)
    }

    d = ifelse(d > d0, d0, d)
    d = ifelse(d > d.bal.min, d, d.bal.min)

  }

  if(sum(is.na(speed)) > 0) {
    message("Error: The data must not contain missing observations")
    stop()
  } else {
    d.bal <- unlist(purrr::accumulate(speed, d.prime, cv = cs, t.diff = t.diff, d0 = d0, .init = d0))[1:length(speed)]
    return(d.bal)
  }

}

#' @export
#' @describeIn dp.bal Estimate of an athlete's D'
dp.estim <- function(speed, max.speed, crit.speed, duration) {

  # duration <- 0.75 * duration

  if(missing(max.speed)) {max.speed = max(speed, na.rm = TRUE)}

  d0 <- (duration * (crit.speed - max.speed) * (crit.speed - speed)) / (max.speed - speed)

  d0 <- ifelse(d0 < 50, 50, d0)
  d0 <- ifelse(d0 > 200, 200, d0)

  d0

}
