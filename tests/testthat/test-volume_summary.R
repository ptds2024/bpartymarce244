test_that("volume_summary calculates correct statistics", {
  sim_results_df <- data.frame(
    volume = runif(100, min = 300, max = 1000),
    surface = runif(100, min = 50, max = 150)
  )

  summary <- volume_summary(sim_results_df)
  expect_named(summary, c("volume_99", "surface_99", "mean_volume", "mean_surface", "sd_volume", "sd_surface"))
  expect_true(summary$volume_99 > 300)
  expect_true(summary$surface_99 > 50)
})

test_that("volume_summary throws error for invalid input", {
  expect_error(volume_summary(data.frame()), "must contain 'volume' and 'surface' columns")
  expect_error(volume_summary(data.frame(volume = 1:10)), "must contain 'volume' and 'surface' columns")
})
