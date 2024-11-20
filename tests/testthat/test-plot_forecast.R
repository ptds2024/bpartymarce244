test_that("plot_forecast throws error for invalid parameter", {
  forecast_df <- data.frame(
    date_time = Sys.time() + seq(1, 5) * 3600,
    Temperature = runif(5, 0, 30),
    Humidity = runif(5, 50, 100),
    Pressure = runif(5, 900, 1100)
  )

  expect_error(
    plot_forecast(forecast_df, parameter = "InvalidParam"),
    "Invalid parameter. Must be one of: Temperature, Humidity, Pressure"
  )
})

test_that("plot_forecast creates a plot for valid parameter", {
  forecast_df <- data.frame(
    date_time = Sys.time() + seq(1, 5) * 3600,
    Temperature = runif(5, 0, 30),
    Humidity = runif(5, 50, 100),
    Pressure = runif(5, 900, 1100)
  )

  plot <- plot_forecast(forecast_df, parameter = "Temperature")
  expect_s3_class(plot, "gg")
})
