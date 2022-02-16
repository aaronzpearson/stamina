#' Critical Speed
#'
#' Models a player's critical speed and returns a `data.frame` with the player's raw and modelled data over time.
#'
#' `dur` is set to 600 s (10 minutes). It is not advised to change this value.
#'
#' `algo` is the algorithm used. Default is slow which can take 10 - 60 s to run, depending on the machine. When set to fast, the algorithm will return a rough estimate of an athlete's critical speed.
#'
#' @param speed Athlete's speed
#' @param dur Duration of the velocity-duration curve
#' @param sample.rate Sample rate in Hz
#' @param algo Speed of the function
#'
#' @export
cs.results.model <- function(speed, dur = 600, sample.rate = 10, algo = "slow") {

  # build data.frame
  dat <- data.frame(dur = 1:length(speed)/ sample.rate,
                    speed = speed)

  # smooth data to 1Hz
  dat$speed.mean <- cs.move.ave(dat$speed, sample.rate)
  dat.1hz <- dat[seq(1, nrow(dat), sample.rate), ]

  if(algo == "fast") {
    dur = seq(0, dur, by = round(dur/ 40, 2))
  } else { dur = 1:dur
    }


  # build velocity duration data
  roll_ave_final <- vector(length = length(dur))
  for(i in seq_along(dur)) { # loop should take < 1min

    temp_data <- cs.move.ave(dat.1hz$speed.mean, i)

    max_roll_ave <- max(temp_data, na.rm = T) # select the maximal value per window size
    rm(temp_data) # decrease memory usage

    roll_ave_final[i] <- max_roll_ave

  }

  # rebuild data.frame
  dat <- data.frame(dur = dur,
                    speed = roll_ave_final)

  # model velocity duration
  theta.0 <- min(dat$speed) * 0.5 # set an initial velocity duration asymptote
  model.0 <- lm(log(speed - theta.0) ~ dur, data = dat)  # provide model formula
  alpha.0 <- exp(coef(model.0)[1]) # initial y-intercept before optimization
  beta.0 <- coef(model.0)[2] # initial exponential decay before optimization

  model <- nls(speed ~ alpha * exp(beta * dur) + theta, # fit the model
               data = dat,
               start = list(alpha = alpha.0,
                            beta = beta.0,
                            theta = theta.0),
               control = nls.control(maxiter = 1000))

  alpha <- coef(model)[[1]]
  beta <- coef(model)[[2]]
  theta <- coef(model)[[3]]

  dat$pred <- predict(model)

  dat

}

#' @export
#' @describeIn cs.results.model A moving average helper function
cs.move.ave <- function(x, n) {

  as.vector(stats::filter(x, rep(1/n, n), sides = 2))

}
