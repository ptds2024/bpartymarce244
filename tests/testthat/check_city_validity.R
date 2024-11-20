# Ensure the required packages are installed
if (!requireNamespace("httptest", quietly = TRUE)) install.packages("httptest")
if (!requireNamespace("magrittr", quietly = TRUE)) install.packages("magrittr")
# tests/testthat/test-check_city_validity.R
library(testthat)
library(httptest)

test_that("check_city_validity returns TRUE for valid city", {
  with_mock_api({
    # Simulate a successful API response
    expect_true(check_city_validity("London", api_key = "2e24e9882da038c0d423b67af00ddaa4"))
  })
})

test_that("check_city_validity returns FALSE for invalid city", {
  with_mock_api({
    # Simulate a 404 Not Found response
    expect_false(check_city_validity("InvalidCity", api_key = "fake_api_key"))
  })
})

test_that("check_city_validity handles missing API key", {
  expect_error(check_city_validity("London", api_key = ""), "API key not found")
})
