test_that("check_city_validity handles invalid inputs", {
  expect_error(check_city_validity(city_name = "", api_key = "valid_api_key"), "Invalid city_name")
  expect_error(check_city_validity(city_name = "London", api_key = ""), "Invalid api_key")
})

test_that("check_city_validity returns TRUE for valid city", {
  # Mock API response
  with_mock_api({
    httptest::stub_request("GET", "http://api.openweathermap.org/data/2.5/weather") %>%
      httptest::respond_with(status = 200)
    expect_true(check_city_validity("London", api_key = "valid_api_key"))
  })
})

test_that("check_city_validity returns FALSE for invalid city", {
  with_mock_api({
    httptest::stub_request("GET", "http://api.openweathermap.org/data/2.5/weather") %>%
      httptest::respond_with(status = 404)
    expect_false(check_city_validity("InvalidCity", api_key = "valid_api_key"))
  })
})
