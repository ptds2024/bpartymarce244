#' @title Volume Summary
#' @description Calculates and returns the volume summary statistics based on simulation results.
#' @param sim_results_df A data frame with `volume` and `surface` columns.
#' @return A list with 99th percentile, mean, and standard deviation of volume and surface.
#' @examples
#' volume_summary(sim_results_df)
#' @author MarcelaChoque
#' @export
volume_summary <- function(sim_results_df) {
  volume_99 <- stats::quantile(sim_results_df$volume, 0.99)
  surface_99 <- stats::quantile(sim_results_df$surface, 0.99)
  list(volume_99 = volume_99, surface_99 = surface_99)
}
