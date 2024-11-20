#' @title Simulate Ice Cream Party
#' @description Simulates the number of guests and calculates the total volume and surface area of ice cream cones required.
#' @return A numeric vector with total ice cream volume and surface area.
#' @examples
#' simulate_party()
#' @export
simulate_party <- function(lambda) {
  n_guests <- stats::rpois(1, lambda)

  cones_per_guest <- sample(c(1, 2), n_guests, replace = TRUE, prob = c(0.67, 0.33))
  total_cones <- sum(cones_per_guest)

  h_variation <- function(x) {
    x + stats::rnorm(1, mean = 0, sd = 0.1)
  }

  volume_integrand <- function(x) h_variation(x)^2
  surface_integrand <- function(x) {
    h <- h_variation(x)
    dh_dx <- (h_variation(x + 1e-5) - h_variation(x)) / 1e-5
    h * sqrt(1 + dh_dx^2)
  }

  total_volume <- total_cones * pi * stats::integrate(volume_integrand, lower = 0, upper = 10)$value
  total_surface <- total_cones * 2 * pi * stats::integrate(surface_integrand, lower = 0, upper = 10)$value

  c(volume = total_volume, surface = total_surface)
}
