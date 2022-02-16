#' Metabolic Energy and Power
#'
#' This function returns a player's instantaneous metabolic energy or power.
#'
#' @param speed Athlete's speed
#' @param accel Athlete's acceleration on the x-axis
#'
#' @export
met.power <- function(speed, accel) {

  # returns values in W/kg

  torso <- atan(9.81/ accel)
  equiv_slope <- tan(pi/2 - torso)

  met_energy <- (155.4*equiv_slope^5 - 30.4*equiv_slope^4 -
                   43.3*equiv_slope^3 + 46.3*equiv_slope^2 + 19.5*equiv_slope + 3.6) * 1.29

  met_power <- met_energy * speed

  return(met_power)

}

#' @export
#' @describeIn met.power Updated metabolic power algorithm
met.power.update <- function(speed, accel) {

  # returns values in W/kg

  torso <- atan(9.81/ accel)
  equiv_slope <- tan(pi/2 - torso)

  met_energy <- 30.4*equiv_slope^4 - 5.0975*equiv_slope^3 + 46.3*equiv_slope^2 + 17.696*equiv_slope + 4.66

  met_power <- met_energy * speed

  return(met_power)

}

#' @export
#' @describeIn met.power Metabolic energy
met.energy <- function(accel, kcal = FALSE) {

  # returns values in J/kg/m

  torso <- atan(9.81/ accel)
  equiv_slope <- tan(pi/2 - torso)

  met_energy = (155.4*equiv_slope^5 - 30.4*equiv_slope^4 -
                  43.3*equiv_slope^3 + 46.3*equiv_slope^2 + 19.5*equiv_slope + 3.6) * 1.29


  if(kcal == FALSE) {

    return(met_energy)

  } else {

    return(met_energy/ 4200)

  }

}

#' @export
#' @describeIn met.power Updated metabolic energy
met.energy.update <- function(accel, kcal = FALSE) {

  # returns values in kcal/kg/m

  torso <- atan(9.81/ accel)
  equiv_slope <- tan(pi/2 - torso)

  met_energy <- 30.4*equiv_slope^4 - 5.0975*equiv_slope^3 +
    46.3*equiv_slope^2 + 17.696*equiv_slope + 4.66

  if(kcal == FALSE) {

    return(met_energy)

  } else {

    return(met_energy/ 4200)

  }

}

#' @export
#' @describeIn met.power Rough kcal estimate from an athlete's metabolic energy
met.energy.kcal <- function(met.energy) {

  ee = met.energy/ 4200

  return(ee)

}
