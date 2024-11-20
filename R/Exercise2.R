#' @title Run the Shiny Application
#' @description Launches the Shiny app that displays current weather data, forecasts, and simulations for multiple locations.
#' @return No return value. This function starts the Shiny application.
#' @examples
#' \dontrun{
#' run_app()
#' }
#' @export
run_app <- function() {

  # Set the OpenWeatherMap API key
  api_key <- "YOUR_API_KEY_HERE"
  if (nchar(api_key) == 0 || api_key == "YOUR_API_KEY_HERE") {
    stop("API key not found. Please set your OpenWeatherMap API key.")
  }

  # Function to verify if a city is valid in OpenWeatherMap's dataset
  #' @title Check City Validity
  #' @description Verifies if a given city name is valid in OpenWeatherMap's dataset.
  #' @param city_name A character string representing the name of the city to verify.
  #' @param api_key Your OpenWeatherMap API key as a character string.
  #' @return A logical value: \code{TRUE} if the city is valid, \code{FALSE} otherwise.
  #' @examples
  #' \dontrun{
  #' valid <- check_city_validity("London", api_key = "your_api_key")
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

  # Function to create dynamic forecast plots
  #' @title Plot 5-Day Weather Forecast
  #' @description Creates a dynamic plot of the 5-day weather forecast for a specified parameter.
  #' @param forecast_df A data frame containing forecast data with columns \code{date_time}, \code{Temperature}, \code{Humidity}, and \code{Pressure}.
  #' @param parameter A character string specifying the weather parameter to plot. Must be one of \code{"Temperature"}, \code{"Humidity"}, or \code{"Pressure"}.
  #' @return A \code{ggplot} object representing the forecast plot.
  #' @examples
  #' \dontrun{
  #' plot_forecast(forecast_df, parameter = "Temperature")
  #' }
  #' @export
  plot_forecast <- function(forecast_df, parameter) {
    ggplot2::ggplot(forecast_df, ggplot2::aes(x = date_time, y = !!rlang::sym(parameter))) +
      ggplot2::geom_line(color = "#439EB7", size = 1) +
      ggplot2::labs(
        title = paste("5-Day", parameter, "Forecast"),
        x = "Date and Time",
        y = parameter
      ) +
      ggplot2::theme_minimal()
  }

  # Define the user interface for the Shiny app
  ui <- shiny::fluidPage(
    shiny::titlePanel("Climatic Factors Today and in the Next 5 Days for Multiple Locations"),

    # Create a tabset panel to separate each location's inputs and outputs
    shiny::tabsetPanel(
      # Location 1
      shiny::tabPanel("Location 1",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::textInput("city_1", "City", value = "London"), # Input for city name
                          shiny::selectInput("parameter_1", "Parameter",        # Dropdown for weather parameter selection
                                             choices = c("Temperature", "Humidity", "Pressure")),
                          shiny::actionButton("submit_1", "Submit"),            # Submit button to fetch weather data

                          shiny::numericInput("n_simulations_1", "Number of Simulations", value = 10000, min = 1),
                          shiny::actionButton("run_simulations_1", "Run Simulations")     # Button to run simulations
                        ),

                        shiny::mainPanel(
                          shiny::textOutput("current_weather_1"),               # Display current weather text output
                          shiny::plotOutput("forecast_plot_1"),                 # Display plot for 5-day forecast

                          shiny::textOutput("volume_summary_text_1"),           # Text for ice cream volume
                          shiny::plotOutput("volume_histogram_1"),              # Histogram for total volume

                          shiny::textOutput("surface_summary_text_1"),          # Text for coating surface area
                          shiny::plotOutput("surface_histogram_1")              # Histogram for total surface area
                        )
                      )
      ),

      # Location 2
      shiny::tabPanel("Location 2",
                      # Repeat the same structure as for Location 1, changing identifiers to _2
                      # (You can copy and adapt the code from Location 1, changing suffixes accordingly)
      ),

      # Location 3
      shiny::tabPanel("Location 3",
                      # Repeat the same structure as for Location 1, changing identifiers to _3
                      # (You can copy and adapt the code from Location 1, changing suffixes accordingly)
      )
    )
  )

  # Define server logic to process inputs and generate outputs
  server <- function(input, output, session) {

    # Repeat the server code for each location, for example, for Location 1:

    # ========== Location 1 ==========
    # Event to validate city and retrieve weather data when "Submit" is clicked
    weather_data_1 <- shiny::eventReactive(input$submit_1, {
      city <- input$city_1

      # Check if the entered city name is valid
      if (!check_city_validity(city, api_key)) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid City",
          paste("City", city, "not found. Please enter a valid city name."),
          easyClose = TRUE
        ))
        return(NULL)
      }

      # Fetch current weather data for the specified city
      current_weather <- jsonlite::fromJSON(httr::content(httr::GET(
        "http://api.openweathermap.org/data/2.5/weather",
        query = list(q = city, APPID = api_key, units = "metric")
      ), "text", encoding = "UTF-8"))

      # Fetch 5-day forecast data for the specified city
      forecast_data <- jsonlite::fromJSON(httr::content(httr::GET(
        "http://api.openweathermap.org/data/2.5/forecast",
        query = list(q = city, APPID = api_key, units = "metric")
      ), "text", encoding = "UTF-8"))

      list(current = current_weather, forecast = forecast_data)
    })

    # Display current weather information
    output$current_weather_1 <- shiny::renderText({
      data <- weather_data_1()
      shiny::req(data)

      city <- input$city_1
      current <- data$current
      parameter <- input$parameter_1

      # Display message based on the selected parameter
      if (parameter == "Temperature") {
        paste("Welcome to", city, "! The Temperature in", city, "is", current$main$temp, "°C.")
      } else if (parameter == "Humidity") {
        paste("Welcome to", city, "! The Humidity in", city, "is", current$main$humidity, "%.")
      } else if (parameter == "Pressure") {
        paste("Welcome to", city, "! The Pressure in", city, "is", current$main$pressure, "hPa.")
      }
    })

    # Render plot for 5-day forecast based on the selected parameter
    output$forecast_plot_1 <- shiny::renderPlot({
      data <- weather_data_1()
      shiny::req(data)

      forecast <- data$forecast$list

      # Convert forecast data into a data frame
      forecast_df <- data.frame(
        date_time = as.POSIXct(sapply(forecast, function(x) x$dt_txt), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
        Temperature = sapply(forecast, function(x) x$main$temp),
        Humidity = sapply(forecast, function(x) x$main$humidity),
        Pressure = sapply(forecast, function(x) x$main$pressure)
      )

      # Call plot_forecast function with forecast data and user-selected parameter
      plot_forecast(forecast_df, input$parameter_1)
    })

    # Observe event for running simulations
    shiny::observeEvent(input$run_simulations_1, {

      # Basic validation of the number of simulations
      n_simulations <- as.integer(input$n_simulations_1)
      if (is.na(n_simulations) || n_simulations <= 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid Input",
          "Please enter a positive integer for the number of simulations.",
          easyClose = TRUE
        ))
        return(NULL)
      }

      # Get the current weather data
      data <- weather_data_1()
      shiny::req(data)

      current <- data$current
      temperature <- current$main$temp            # Temperature in Celsius
      humidity <- current$main$humidity / 100     # Humidity as a decimal (percentage / 100)
      pressure <- current$main$pressure           # Pressure in hPa

      # Calculate lambda for the Poisson distribution
      lambda <- exp(0.5 + 0.5 * temperature - 3 * humidity + 0.001 * pressure)

      # Define the simulation function
      #' @title Simulate Party Resources
      #' @description Simulates the number of guests, required cones, and calculates total ice cream volume and coating surface area.
      #' @return A numeric vector with total volume and total surface area.
      simulate_party <- function() {
        # Simulate number of guests
        n_guests <- stats::rpois(1, lambda)

        # Simulate number of cones per guest (1 or 2)
        cones_per_guest <- sample(c(1, 2), n_guests, replace = TRUE, prob = c(0.67, 0.33))
        total_cones <- sum(cones_per_guest)

        # Random variations in cone dimensions
        h_variation <- function(x) {
          x + stats::rnorm(1, mean = 0, sd = 0.1)
        }

        # Calculate total volume and surface area for all cones
        volume_integrand <- function(x) h_variation(x)^2
        surface_integrand <- function(x) {
          h <- h_variation(x)
          dh_dx <- (h_variation(x + 1e-5) - h_variation(x)) / 1e-5
          h * sqrt(1 + dh_dx^2)
        }

        total_volume <- total_cones * pi * stats::integrate(volume_integrand, lower = 0, upper = 10)$value
        total_surface <- total_cones * 2 * pi * stats::integrate(surface_integrand, lower = 0, upper = 10)$value

        c(volume = total_volume, surface = total_surface)
      }

      # Run the simulations
      sim_results <- replicate(n_simulations, simulate_party())

      # Convert results into a data frame
      sim_results_df <- data.frame(volume = sim_results[1, ], surface = sim_results[2, ])

      # Calculate the 99th percentile for volume and surface area
      volume_99 <- stats::quantile(sim_results_df$volume, 0.99)
      surface_99 <- stats::quantile(sim_results_df$surface, 0.99)

      # Render the volume summary text
      output$volume_summary_text_1 <- shiny::renderText({
        paste0(
          "Ice Cream Volume: To satisfy the guests with a 99% chance, ",
          "we will need approximately ", format(round(volume_99, 2), big.mark = ","), " cm³ of ice cream."
        )
      })

      # Generate and display the histogram of total volume
      output$volume_histogram_1 <- shiny::renderPlot({
        hist(sim_results_df$volume,
             main = "Histogram of Total Volume",
             xlab = "Total Volume (cm³)",
             col = "#E28B55",
             border = "white")
      })

      # Render the surface area summary text
      output$surface_summary_text_1 <- shiny::renderText({
        paste0(
          "Coating Surface Area: To coat the cones with a 99% chance of sufficiency, ",
          "we will need approximately ", format(round(surface_99, 2), big.mark = ","), " cm² of coating material."
        )
      })

      # Generate and display the histogram of total surface area
      output$surface_histogram_1 <- shiny::renderPlot({
        hist(sim_results_df$surface,
             main = "Histogram of Total Surface Area",
             xlab = "Total Surface Area (cm²)",
             col = "#DCB64D",
             border = "white")
      })
    })
  }

  # Run the Shiny app
  shiny::shinyApp(ui, server)
}




