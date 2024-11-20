test_that("setup_api_key throws error for invalid key", {
  expect_error(setup_api_key(key = ""), "API key not found")
  expect_error(setup_api_key(key = "YOUR_API_KEY_HERE"), "API key not found")
})

test_that("setup_api_key returns a valid key", {
  valid_key <- "valid_api_key"
  expect_equal(setup_api_key(key = valid_key), valid_key)
})
