#' @title Volume Summary
#' @description Calculates and returns the volume summary statistics based on simulation results.
#' @param sim_results_df A data frame with `volume` and `surface` columns.
#' @return A named list with 99th percentile, mean, and standard deviation of volume and surface.
#' @examples
#' sim_results_df <- data.frame(volume = runif(100, 300, 1000), surface = runif(100, 50, 150))
#' volume_summary(sim_results_df)
#' @author MarcelaChoque
#' @export
volume_summary <- function(sim_results_df) {
  if (!all(c("volume", "surface") %in% names(sim_results_df))) {
    stop("Data frame must contain 'volume' and 'surface' columns.")
  }
  volume_99 <- stats::quantile(sim_results_df$volume, 0.99)
  surface_99 <- stats::quantile(sim_results_df$surface, 0.99)
  mean_volume <- mean(sim_results_df$volume)
  mean_surface <- mean(sim_results_df$surface)
  sd_volume <- sd(sim_results_df$volume)
  sd_surface <- sd(sim_results_df$surface)

  list(
    volume_99 = volume_99,
    surface_99 = surface_99,
    mean_volume = mean_volume,
    mean_surface = mean_surface,
    sd_volume = sd_volume,
    sd_surface = sd_surface
  )
}
