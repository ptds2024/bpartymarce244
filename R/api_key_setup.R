#' @title Set OpenWeatherMap API Key
#' @description Verifies and sets the OpenWeatherMap API key.
#' @param key A character string representing the OpenWeatherMap API key. If not provided, it attempts to read the key from the environment variable `OPENWEATHERMAP_API_KEY`.
#' @return The API key as a character string if it is valid.
#' @examples
#' \dontrun{
#' setup_api_key(key = "your_api_key_here")
#' }
#' @author MarcelaChoque
#' @export
setup_api_key <- function(key = Sys.getenv("OPENWEATHERMAP_API_KEY")) {
  if (nchar(key) == 0 || key == "YOUR_API_KEY_HERE") {
    stop("API key not found. Please provide a valid OpenWeatherMap API key or set it in the environment variable 'OPENWEATHERMAP_API_KEY'.")
  }
  return(key)
}
