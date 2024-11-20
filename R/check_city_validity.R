#' @title Check City Validity
#' @description Verifies if a given city name is valid in the OpenWeatherMap API dataset.
#' @param city_name A character string representing the name of the city.
#' @param api_key A character string containing the OpenWeatherMap API key.
#' @return A logical value: `TRUE` if the city is valid, `FALSE` otherwise.
#' @examples
#' \dontrun{
#' check_city_validity("London", api_key = "your_api_key")
#' }
#' @export
check_city_validity <- function(city_name, api_key) {
  response <- httr::GET(
    "http://api.openweathermap.org/data/2.5/weather",
    query = list(q = city_name, appid = api_key)
  )
  status <- httr::status_code(response)
  return(status == 200)
}
