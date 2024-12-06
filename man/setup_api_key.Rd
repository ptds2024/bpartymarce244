% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_key_setup.R
\name{setup_api_key}
\alias{setup_api_key}
\title{Set OpenWeatherMap API Key}
\usage{
setup_api_key(key = Sys.getenv("OPENWEATHERMAP_API_KEY"))
}
\arguments{
\item{key}{A character string representing the OpenWeatherMap API key. If not provided, it attempts to read the key from the environment variable `OPENWEATHERMAP_API_KEY`.}
}
\value{
The API key as a character string if it is valid.
}
\description{
Verifies and sets the OpenWeatherMap API key.
}
\examples{
\dontrun{
setup_api_key(key = "your_api_key_here")
}
}
\author{
MarcelaChoque
}
