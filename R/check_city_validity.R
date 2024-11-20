#' @title Check City Validity
#' @description Verifies if a given city name is valid in the OpenWeatherMap API dataset.
#' @param city_name A character string representing the name of the city.
#' @param api_key A character string containing the OpenWeatherMap API key.
#' @return A logical value: `TRUE` if the city is valid, `FALSE` otherwise.
#' @examples
#' \dontrun{
#' check_city_validity("London", api_key = "your_api_key")
#' }
#' @author MarcelaChoque
#' @export
check_city_validity <- function(city_name, api_key) {
  if (missing(city_name) || !is.character(city_name) || nchar(city_name) == 0) {
    stop("Invalid city_name: Please provide a valid city name as a non-empty character string.")
  }

  if (missing(api_key) || nchar(api_key) == 0 || api_key == "YOUR_API_KEY_HERE") {
    stop("Invalid api_key: Please provide a valid OpenWeatherMap API key.")
  }

  response <- tryCatch(
    httr::GET(
      "http://api.openweathermap.org/data/2.5/weather",
      query = list(q = city_name, appid = api_key)
    ),
    error = function(e) {
      stop("Error while making API request: ", e$message)
    }
  )

  status <- httr::status_code(response)
  return(status == 200)
}
