#' @title Set OpenWeatherMap API Key
#' @description Verifies and sets the OpenWeatherMap API key.
#' @return No return value; raises an error if the API key is missing.
#' @examples
#' setup_api_key()
#' @author MarcelaChoque
#' @export
setup_api_key <- function() {
  api_key <- "YOUR_API_KEY_HERE"
  if (nchar(api_key) == 0 || api_key == "YOUR_API_KEY_HERE") {
    stop("API key not found. Please set your OpenWeatherMap API key.")
  }
  return(api_key)
}
