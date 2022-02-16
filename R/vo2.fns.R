#' VO2 Max
#'
#' Estimates an athlete's V(dot)O2. `vo2.auto` selects the most appropriate algorithm based on the athlete's speed. VO2 max is capped at 60 ml/kg/minute if it isn't specified.
#'
#' Algorithms are built for individuals who are walking (< 10 kph), jogging (10 to 17 kph), running on a track (8 to 25 kph), and running on a treadmill (8 to 25 kph).
#'
#' To convert m/s to kps, multiple by 3.6
#'
#' VO2 is returned in ml/kg/min
#'
#' @param speed.kph The player's speed in kph
#'
#' @export
#'
#'
vo2.auto <- function(speed.kph, vo2.max = 60) {

  # this function selects the most-appropriate V(dot)O2 formula based on the input speed

  vo2 = ifelse(speed.kph < 8, vo2.walk(speed.kph), vo2.track(speed.kph))

  vo2 = ifelse(vo2 > vo2.max, vo2.max, vo2)

  return(vo2)

}

#' @export
#' @describeIn vo2.auto VdotO2 at a walking pace
vo2.walk <- function(speed.kph) {

  # Bunc & Dlouha (1997)
  # walking (< 10 km/h or 6 mi/h)
  vo2 = 4.501 - 0.108*speed.kph + 0.379*speed.kph^2

  return(vo2)


}

#' @export
#' @describeIn vo2.auto VO2 at a jogging pace
vo2.jog <- function(speed.kph) {

  # Brisswalter (1996)
  # jogging (10 km/h to 17 km/h)
  vo2 = 17.94 + 0.00845*speed.kph + 0.2093*speed.kph^2

  return(vo2)


}

#' @export
#' @describeIn vo2.auto VO2 while running on a track
vo2.track <- function(speed.kph) {

  # Leger & Mercier (1984)
  # track running (8 km/h to 25 km/h)
  vo2 = 3.5*speed.kph

  return(vo2)


}

#' @export
#' @describeIn vo2.auto VO2 while running on a treadmill
vo2.treadmill <- function(speed.kph) {

  # Leger & Mercier (1984)
  # treadmill running (8 km/h to 25 km/h)
  vo2 = 2.209 + 3.163*speed.kph + 0.000525542*speed.kph^3

  return(vo2)


}

#' @export
#' @describeIn vo2.auto The kcal equivalent expenditure in kcal/kg/km
vo2.kcal <- function(speed.kph) {
  # kcal/kg/km

  vo2 = vo2.auto(speed.kph)

  ee = vo2 * 5 * 60/1 * 1/1000 * 1/speed.kph

  ee = ifelse(speed.kph == 0, 0, ee)

  return(ee)

}

#' @export
#' @describeIn vo2.auto The kcal equivalent expenditure for a distance travelled
vo2.kcal.dist <- function(speed.kph, sample.rate = 10) {
  # kcal/kg for a given distance

  ee <- vo2.kcal(speed.kph) * speed.kph/60/60/sample.rate

  return(ee)

}


#' VO2 Max Estimates
#'
#' An estimation of an athlete's VO2 max in ml/kg/min
#'
#' @param MAS Max aerobic speed
#'
#' @export
vo2.estim <- function(MAS) {

  MAS * 3.5 * 3.6

}

#' @export
#' @describeIn vo2.estim Estimate of an athlete's VO2 max v.2
vo2.estim.2 <- function(MAS) {

  MAS = MAS * 3.6
  v <- 2.209 + 3.163 * MAS

  return(v)

}

#' @export
#' @describeIn vo2.estim Estimate of an athlete's VO2 max v.3
vo2.estim.3 <- function(MAS) {

  MAS = MAS * 3.6
  v = 2.209 + 3.163 * MAS + 0.000525542 * MAS^3

  return(v)

}
