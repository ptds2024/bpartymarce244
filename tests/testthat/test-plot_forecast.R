
library(testthat)

test_that("plot_forecast creates a ggplot object", {
  # Create a sample forecast_df
  forecast_df <- data.frame(
    date_time = Sys.time() + seq(0, by = 3600, length.out = 5),
    Temperature = runif(5, min = 10, max = 20),
    Humidity = runif(5, min = 50, max = 100),
    Pressure = runif(5, min = 1000, max = 1020)
  )

  plot <- plot_forecast(forecast_df, parameter = "Temperature")
  expect_s3_class(plot, "ggplot")
})

test_that("plot_forecast handles invalid parameter", {
  forecast_df <- data.frame(
    date_time = Sys.time() + seq(0, by = 3600, length.out = 5),
    Temperature = runif(5, min = 10, max = 20)
  )

  expect_error(plot_forecast(forecast_df, parameter = "Wind"), "object 'Wind' not found")
})
