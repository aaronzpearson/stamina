#' Player Load
#'
#' This function is built off Catapult Sports (R) player load model. Since some player tracking data returns a single acceleration vector, `ay` and `az` are optional.
#'
#' `cumulative` returns a vector with the summation of the player load over time. Defualt is set to `FALSE`
#'
#' @param ax Acceleration on the x-axis
#' @param ay Acceleration on the y-axis (optional)
#' @param az Acceleration on the z-axis (optional)
#' @param cumulative Summed player load over time
#' @param sample.rate Sample rate in Hz
#'
#' @export
pl.player.load <- function(ax, ay, az, cumulative = FALSE, sample.rate = 100) {

  # adapted from Catapult Sports
  # returns values in AU (arbitrary units)

  fill_missing <- rep(0, length(ax))

  if(missing(ay)) {ay <- fill_missing}
  if(missing(az)) {az <- fill_missing}

  ax[is.na(ax)] <- 0
  ay[is.na(ay)] <- 0
  az[is.na(az)] <- 0

  ##

  diff_ax <- diff(ax)
  diff_ay <- diff(ay)
  diff_az <- diff(az)

  player_load = sqrt(diff_ax^2 + diff_ay^2 + diff_az^2)/sample.rate
  player_load = c(0, player_load)


  if(cumulative == FALSE) {

    return(player_load)

  } else {

    return(cumsum(player_load))

    }

}
