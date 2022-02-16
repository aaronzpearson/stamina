#' Stamina Model Results
#'
#' Returns a data.frame that includes their speed, acceleration, player load, cumulative player load, metabolic energy, metabolic power, V(dot)O2, approximate kcal expenditure, cumulative kcal expenditure, and D' balance.
#'
#' If `crit.speed` or `d.prime` are unknown, set the argument to `auto` and the function will estimate these values. To return the estimated values, call `stamina.player.profile()`.
#'
#' If `player.load` is set to `FALSE`, the function assumes that acceleration is not provided and will override values in the `ax` argument. You can omit the `ax` argument to achieve the same results.
#'
#' `vo2` can take on one of the following: `"vo2.walk"`, `"vo2.jog"`, `"vo2.track"`, `"vo2.treadmill"`. The default is set to `"vo2.track"` with the assumption that is the nearest approximation to actual V(dot)O2.
#'
#'
#' @param speed Athlete's speed
#' @param ax Athlete's acceleration on the x-axis
#' @param ay Athlete's acceleration on the y-axis (optional)
#' @param az Athlete's acceleration on the z-axis (optional)
#' @param vo2 VO2 model
#' @param crit.speed Athlete's critical speed
#' @param d.prime Athlete's initial D' value
#' @param player.load If FALSE, overrides ax and sets it to zero
#' @param sample.rate Sample rate in Hz
#'
#' @export
stamina.results.model <- function(speed,
                       ax,
                       ay = NA,
                       az = NA,
                       vo2 = "auto",
                       crit.speed = "auto",
                       d.prime = "auto",
                       player.load = TRUE,
                       sample.rate = 10) {

  if(missing(ax)) {ax = rep(0, length(speed))}
  if(player.load == FALSE) {ax = rep(0, length(speed))}

  if(crit.speed == "auto" | d.prime == "auto") {

    auto.df <- stamina.player.profile(speed = speed,
                                      sample.rate = sample.rate)

  }

  if(crit.speed == "auto") {crit.speed = auto.df$crit.speed}
  if(d.prime == "auto") {d.prime = auto.df$d.prime}
  d.prime = ifelse(d.prime < 50, 50, d.prime)
  d.prime = ifelse(d.prime > 200, 200, d.prime)


  stamina <- data.frame(duration = 0:(length(speed) - 1)/ sample.rate,
                        player.speed = speed,
                        ax = ax,
                        ay = ay,
                        az = az)

  stamina$player.load = pl.player.load(stamina$ax,
                                    stamina$ay,
                                    stamina$az,
                                    cumulative = FALSE,
                                    sample.rate = sample.rate)
  stamina$player.load.sum = cumsum(stamina$player.load)

  stamina$met.energy = met.energy(stamina$ax,
                                  kcal = FALSE)
  stamina$met.power = met.power(stamina$player.speed,
                                stamina$ax)

  stamina$vo2 = vo2.auto(stamina$player.speed * 3.6)

  if(vo2 == "vo2.walk") {
      stamina$vo2 = vo2.walk(stamina$player.speed * 3.6)
  } else if(vo2 == "vo2.jog") {
      stamina$vo2 = vo2.jog(stamina$player.speed * 3.6)
  } else if(vo2 == "vo2.treadmill") {
      stamina$vo2 = vo2.treadmill(stamina$player.speed * 3.6)
  } else if(vo2 == "vo2.track") {
    stamina$vo2 = vo2.track(stamina$player.speed * 3.6)
  }

  stamina$vo2.kcal = vo2.kcal.dist(stamina$player.speed * 3.6,
                                   sample.rate = sample.rate)
  stamina$vo2.kcal.sum = cumsum(stamina$vo2.kcal)

  stamina$d.prime.bal = dp.bal(stamina$player.speed,
                              d0 = d.prime,
                              cs = crit.speed,
                              t.diff = 1/ sample.rate)

  return(stamina)

}


#' Stamina Player Profile
#'
#' Returns approximations of an athlete's critical speed, D', max aerobic speed, and VO2 max.
#'
#' `algo` is the algorithm used. Default is slow which can take 10 - 60 s to run, depending on the machine. When set to fast, the algorithm will return a rough estimate of an athlete's critical speed.
#'
#' `dur` is set to 600 s (10 minutes). It is not advised to change this value.
#'
#' **Note** The VO2 max algorithm is an approximation and has not been validated. Results are typically +/- 10%. If the data is very messy, results can be highly erratic.
#'
#' @param speed Athete's speed
#' @param sample.rate Sample rate in Hz
#' @param algo Speed of the function
#' @param dur Duration of the velocity-duration curve
#'
#' @export
stamina.player.profile <- function(speed, dur = 600, sample.rate = 10, algo = "slow") {

  # build data.frame
  dat <- data.frame(dur = 1:length(speed)/ sample.rate,
                    speed = speed)

  # smooth data to 1Hz
  dat$speed.mean <- cs.move.ave(dat$speed, sample.rate)
  dat.1hz <- dat[seq(1, nrow(dat), sample.rate), ]

  if(algo == "fast") {
    dur = seq(0, dur, by = round(dur/ 40, 2))
  } else {
    dur = 1:dur
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

  dat$pred <- vo2.model.fit(alpha,
                        beta,
                        theta,
                        dat$dur,
                        cf = 1.25)

  # model max aerobic speed
  dat$pred.integral <- vo2.model.integral(alpha,
                                      beta,
                                      theta,
                                      duration = dat$dur,
                                      cf = 1.25)

  dat$intersect <- abs(dat$pred - dat$pred.integral)
  least.diff <- min(dat$intersect)

  intersect.pt <- subset(dat, intersect == least.diff)
  MAS <- intersect.pt$pred

  player <- data.frame(crit.speed = round(theta, 2),
                       d.prime = round(dp.estim(speed = MAS,
                                                     max.speed = max(dat$pred, na.rm = TRUE),
                                                     crit.speed = theta,
                                                     duration = intersect.pt$dur), 2),
                       max.aerobic.speed = round(MAS, 2),
                       vo2.max = round(vo2.estim(MAS), 2))

  return(player)

}
