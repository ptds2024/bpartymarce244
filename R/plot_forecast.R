#' @title Plot Weather Forecast
#' @description Creates a plot of the 5-day weather forecast for a specified parameter.
#' @param forecast_df A data frame containing forecast data, including `date_time`, `Temperature`, `Humidity`, and `Pressure`.
#' @param parameter A character string specifying the weather parameter to plot. Should be one of `Temperature`, `Humidity`, or `Pressure`.
#' @return A `ggplot2` plot object.
#' @examples
#' \dontrun{
#' plot_forecast(forecast_df, parameter = "Temperature")
#' }
#' @author MarcelaChoque
#' @export
plot_forecast <- function(forecast_df, parameter) {
  valid_parameters <- c("Temperature", "Humidity", "Pressure")
  if (!parameter %in% valid_parameters) {
    stop("Invalid parameter. Must be one of: ", paste(valid_parameters, collapse = ", "))
  }

  ggplot2::ggplot(forecast_df, ggplot2::aes(x = date_time, y = !!rlang::sym(parameter))) +
    ggplot2::geom_line(color = "#439EB7", linewidth = 1) +
    ggplot2::labs(
      title = paste("5-Day", parameter, "Forecast"),
      x = "Date and Time",
      y = parameter
    ) +
    ggplot2::theme_minimal()
}
