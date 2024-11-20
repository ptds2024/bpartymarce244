test_that("simulate_party handles edge cases for lambda", {
  expect_error(simulate_party(lambda = -1), "must be a non-negative numeric value")
  expect_error(simulate_party(lambda = "text"), "must be a non-negative numeric value")
})

test_that("simulate_party returns correct structure", {
  result <- simulate_party(lambda = 10)
  expect_type(result, "double")
  expect_named(result, c("volume", "surface"))
  expect_true(result["volume"] > 0)
  expect_true(result["surface"] > 0)
})
